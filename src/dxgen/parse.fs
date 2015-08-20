module parse

open FSharpx
open libclang
open cdesc
open salparser

let filtermapMap (f: 'k -> 'v -> Option<'v1>) (m: Map<'k,'v>)=
  m |> Map.filter (fun k v -> Option.isSome(f k v)) 
    |> Map.map (fun k v -> match f k v with Some(v1) -> v1 |_ -> raise <| new System.Exception("Unreachable"))

let annotations (cursor:Cursor) =
  let annots=ref []
  let annotationVisitor (cursor:Cursor) _ _ =
    if getCursorKind cursor = CursorKind.AnnotateAttr then
      let dn=getCursorSpellingFS cursor
      annots := dn :: !annots
    ChildVisitResult.Recurse
  visitChildrenFS cursor annotationVisitor () |> ignore
  List.rev !annots

open annotations
open annotations_autogen

let parse (headerLocation: System.IO.FileInfo) (pchLocation: System.IO.FileInfo option) (includePaths : string seq)=
  // Let's use clean C interface. Those fancy C++ classes with inheritance and whistles aren't good for rust codegen.
  let options = 
    seq {
       yield "-x"
       yield "c"
       yield "-std=c11"
       yield "-fms-extensions"
       yield "-fms-compatiblity"
       yield "-fmsc-version=1800"
       yield! includePaths |> Seq.map (fun p -> "-I"+p)
    } |> Array.ofSeq
  let index = createIndex(0, 0)

  let pchTempLocation = Option.maybe {
    let! pchLocation = pchLocation
    let translationUnit = parseTranslationUnit(index, pchLocation.FullName, options, options.Length, [||], 0u, TranslationUnitFlags.None)

    return 
      try
        let fileLocation = System.IO.Path.GetTempFileName()
        saveTranslationUnit(translationUnit, fileLocation, 0u)
        System.IO.FileInfo(fileLocation)
      finally
        translationUnit |> disposeTranslationUnit
  }

  let options = Array.concat [options
                              pchTempLocation |> Option.map (fun pch -> [| "-include-pch"; pch.FullName |]) |> Option.getOrElse [||]]

  let translationUnit = parseTranslationUnit(index, headerLocation.FullName, options, options.Length, [||], 0u, TranslationUnitFlags.None)


  let types=ref Map.empty
  let enums=ref Map.empty
  let structs=ref Map.empty
  let funcs=ref Map.empty
  let iids=ref Set.empty

  let parseEnumConsts (cursor:Cursor)=
    let listOfConsts=ref []
    let addConst cursor _ _=
      if getCursorKind cursor=CursorKind.EnumConstantDecl then
        listOfConsts := (cursor |> getCursorDisplayNameFS, cursor |> getEnumConstantDeclUnsignedValue) :: !listOfConsts
      ChildVisitResult.Continue
    visitChildrenFS cursor addConst () |> ignore
    List.rev !listOfConsts

  let parseFunction (cursor:Cursor) (fType:Type)=
    let nArgs=getNumArgTypes(fType)
    let cc=getFunctionTypeCallingConv(fType)
    let rety=getResultType(fType) |> typeDesc
    let args=ref []
    let argsVisitor (cursor:Cursor) _ _=
      if cursor.kind=CursorKind.ParmDecl then
        let cannot=
          match annotations cursor with
            |[] -> NoAnnotation
            |[ann] -> parseSAL ann
            |_ -> raise <| new System.Exception("Multiple annotations")
        let (pname,ptype,pannot)=(getCursorDisplayNameFS cursor, getCursorType cursor |> typeDesc, cannot)
        let pdesc=
          match ptype with
          |Array(_,_) as arr -> (pname, Ptr(arr), pannot)
//          |Ptr(TypedefRef "IUnknown")
//          |Ptr(Const(TypedefRef "IUnknown")) ->
//              (pname, Ptr(Ptr(Primitive Void)), pannot)
          |_ -> (pname, ptype, pannot)
        args := pdesc :: !args
      ChildVisitResult.Continue
    visitChildrenFS cursor argsVisitor () |> ignore
    if (List.length !args <> nArgs) then
      raise <| System.Exception("Number of parmDecls doesn't match number of arguments. " :: tokenizeFS cursor |> String.concat " ")
    let retyname=getTypeSpellingFS(getCanonicalType(getResultType(fType)))
    let retysize=getSizeOfType(getResultType(fType))
    if retysize>0L && retysize<=8L && (retyname.StartsWith("struct ") || (retyname.Contains(" struct ") && retyname.Contains("*")=false)) then
      // C returns those structs thru EAX:EDX, C++ thru reference
      args := ("__ret_val",Ptr(rety), Out) :: !args
      Function(CFuncDesc(List.rev !args, Ptr(rety),cc))
    else
      Function(CFuncDesc(List.rev !args,rety,cc))


  let parseStruct (cursor:Cursor)=
    let tokens=tokenizeFS cursor
    //printfn "%A %s %s" cursor.kind (cursor |> getCursorSpellingFS) (String.concat " " tokens)
    let fields=ref []
    let parseFieldDecl cursor _ _=
      if getCursorKind cursor=CursorKind.FieldDecl then
        let ctype=getCursorType cursor
        let nm=getCursorDisplayNameFS cursor
        let pointee=getPointeeType ctype
        let nArgs=getNumArgTypes(pointee)
        let ty=
          if nArgs<> -1 then
            // function pointer
            Ptr(parseFunction cursor pointee)
          else
            if nm="pArgumentDescs" then
              ctype |> typeDesc
            else
              ctype |> typeDesc
        let bw=if isBitFieldFS cursor then Some(getFieldDeclBitWidth cursor) else None
        fields := CStructElem(nm, ty, bw) :: !fields
      ChildVisitResult.Continue
    visitChildrenFS cursor parseFieldDecl () |> ignore
    Struct(List.rev !fields)

  // main parser callback
  let rec childVisitor cursor _ _ =
    let cursorKind=getCursorKind cursor

    if cursorKind=CursorKind.EnumDecl then
      enums := !enums |> Map.add (cursor |> getCursorDisplayNameFS) (Enum(enumCType cursor,parseEnumConsts cursor))

    if cursorKind=CursorKind.TypedefDecl then
      let tokens=tokenizeFS cursor
      //printfn "%A %s %s" cursor.kind (cursor |> getCursorSpellingFS) (String.concat " " tokens)
      let uty=cursor |> getTypedefDeclUnderlyingType
      let pty=uty |> getPointeeType
      let nArgs=pty |> getNumArgTypes
      let tdesc=
        if nArgs<> -1 then
          // it is a function pointer
          Ptr(parseFunction cursor pty)
        else
          uty |> typeDesc
      types := !types |> Map.add (cursor |> getCursorDisplayNameFS) (Typedef(tdesc))
      // typedefs can contain other definitions
      visitChildrenFS cursor childVisitor () |> ignore

    if cursorKind=CursorKind.StructDecl then
      let structName=cursor |> getCursorDisplayNameFS
      structs := !structs |> Map.add structName (parseStruct cursor)

    if cursorKind=CursorKind.FunctionDecl then
      funcs := !funcs |> Map.add (cursor |> getCursorSpellingFS) (parseFunction cursor (getCursorType cursor))

    if cursorKind=CursorKind.VarDecl then
      let nm=cursor |> getCursorSpellingFS
      let tys=cursor |> getCursorType |> getTypeSpellingFS
      if tys="const IID" then
        iids := !iids |> Set.add nm

    if cursorKind=CursorKind.UnexposedDecl then // dip into extern "C"
      visitChildrenFS cursor childVisitor () |> ignore
    
    ChildVisitResult.Continue

  let cursor = getTranslationUnitCursor(translationUnit)

  try
    visitChildrenFS cursor childVisitor () |> ignore

    for KeyValue(en,ty) in !enums do
      match ty with
      |Enum(values=vals) ->
        let (_,isect)=vals |> List.map snd |> List.filter (fun n -> n<>0xffffUL && n<>0xffffffffUL && n<>0xffffffffffffffffUL) |> List.fold (fun (acc,isec) n -> (acc|||n,isec || (acc&&&n<>0UL) )) (0x0UL,false)
        if isect then
          printfn "  (\"%s\",EAEnum);" en
        else 
          if en.Contains("FLAGS") then
            printfn "  (\"%s\",EAFlags);" en
          else if (List.length vals)<4 then
            printfn "  (\"%s\",EAEnum);" en
          else
            printfn "  (\"%s\",EAFlags);" en
      | _ -> ()

    // let's replace all "typedef struct BlaSmth {} Bla;" with "struct Bla {};"
    // No need to follow C quirks.
    let typedef2struct=
      !types |> filtermapMap
        (fun k v -> 
          match v with 
          |Typedef (StructRef s) -> Some(s)
          |_ -> None
          )

    let struct2typedef = typedef2struct |> Map.toSeq |> Seq.map swap |> Map.ofSeq
    
    structs := !structs |> Map.toSeq |> Seq.map (fun (k,v) -> match Map.tryFind k struct2typedef with Some(n) -> (n,v) |None -> (k,v) ) |> Map.ofSeq

    let removeTypedefStructs m=
        m  |> Map.map 
            (fun n ty ->
                ty |> recursiveTransform 
                  (fun ty ->
                      match ty with
                      |Typedef(StructRef s) ->
                        match Map.tryFind s struct2typedef with
                        |Some(s) ->
                          Some(StructRef s)
                        |None -> None
                      |TypedefRef s ->
                        match Map.tryFind s typedef2struct with
                        |Some(_) ->
                          Some(StructRef s)
                        |None -> None
                      |StructRef s ->
                        match Map.tryFind s struct2typedef with
                        |Some(s) ->
                          Some(StructRef s)
                        |None -> None
                      |_ -> None
                  )
            )
    types := removeTypedefStructs (!types |> Map.filter (fun n _ -> Map.containsKey n typedef2struct |> not))

    structs := removeTypedefStructs !structs

    (!types, !enums, !structs, !funcs, !iids)

  finally
    translationUnit |> disposeTranslationUnit
    index |> disposeIndex
        
    Option.maybe {
        let! pch = pchTempLocation
        pch.Delete()
    } |> ignore

