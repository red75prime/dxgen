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

let tryParse (s:System.String)=
  let invcul=System.Globalization.CultureInfo.InvariantCulture
  let ignorecase=System.StringComparison.InvariantCultureIgnoreCase
  if s.StartsWith("0x", ignorecase) then
    if s.EndsWith("ULL", ignorecase) then
      let s'=s.Substring(2,s.Length-5)
      try Some(MCUInt64(System.Convert.ToUInt64(s',16)), "0x"+s') with
      |_ -> raise <| new System.Exception(sprintf "%s should be UInt64, but it isn't" s)
    else if s.EndsWith("LL", ignorecase) then
      let s'=s.Substring(2,s.Length-4)
      try Some(MCInt64(System.Convert.ToInt64(s',16)), "0x"+s') with
      |_ -> raise <| new System.Exception(sprintf "%s should be Int64, but it isn't" s)
    else if s.EndsWith("UL", ignorecase) then
      let s'=s.Substring(2,s.Length-4)
      try Some(MCUInt32(System.Convert.ToUInt32(s',16)), "0x"+s') with
      |_ -> raise <| new System.Exception(sprintf "%s should be UInt32, but it isn't" s)
    else if s.EndsWith("L", ignorecase) then
      let s'=s.Substring(2,s.Length-3)
      try Some(MCUInt32(System.Convert.ToUInt32(s',16)), "0x"+s') with
      |_ ->
        try Some(MCInt32(System.Convert.ToInt32(s',16)), "0x"+s') with
        |_ -> raise <| new System.Exception(sprintf "%s should be Int32, but it isn't" s)
    else 
      let s'=s.Substring(2)
      try Some(MCUInt32(System.Convert.ToUInt32(s',16)), "0x"+s') with
      |_ -> 
        try Some(MCInt32(System.Convert.ToInt32(s',16)), "0x"+s') with
        |_ -> 
          try Some(MCUInt64(System.Convert.ToUInt64(s',16)), "0x"+s') with                
          |_ -> 
            try Some(MCInt64(System.Convert.ToInt64(s',16)), "0x"+s') with
            |_ -> None
  else
    if s.EndsWith("f", ignorecase) then
      let s'=s.Substring(0,s.Length-1)
      try Some(MCFloat(System.Convert.ToDouble(s', invcul) |> float), s') with
      |_ -> None
    else
      if System.String.IsNullOrEmpty(s) || not <| (System.Char.IsDigit(s.[0]) || s.StartsWith("-") || s.StartsWith("+")) then
        None
      else if s.EndsWith("ULL", ignorecase) then
        let s'=s.Substring(0,s.Length-3)
        try Some(MCUInt64(System.Convert.ToUInt64(s',10)), s') with
        |_ -> raise <| new System.Exception(sprintf "%s should be UInt64, but it isn't" s)
      else if s.EndsWith("LL", ignorecase) then
        let s'=s.Substring(0,s.Length-2)
        try Some(MCInt64(System.Convert.ToInt64(s', 10)), s') with
        |_ -> raise <| new System.Exception(sprintf "%s should be Int64, but it isn't" s)
      else if s.EndsWith("UL", ignorecase) then
        let s'=s.Substring(0,s.Length-2)
        try Some(MCUInt32(System.Convert.ToUInt32(s', 10)), s') with
        |_ -> 
          printfn "%s should be UInt32, but it isn't" s
          None
      else if s.EndsWith("L", ignorecase) then // 'L' suffix doesn't mean that the number is signed
        let s'=s.Substring(0, s.Length-1)
        try Some(MCDouble(System.Convert.ToDouble(s', invcul)), s') with
        |_ ->
          try Some(MCUInt32(System.Convert.ToUInt32(s',10)), s') with
          |_ ->
            try Some(MCInt32(System.Convert.ToInt32(s',10)), s') with
            |_ ->
              printfn "  %s should be UInt32, Int32 or Double, but it isn't" s
              None
      else
        try Some(MCUInt32(System.Convert.ToUInt32(s,10)), s) with
        |_ -> 
          try Some(MCInt32(System.Convert.ToInt32(s,10)), s) with
          |_ -> 
            try Some(MCUInt64(System.Convert.ToUInt64(s,10)), s) with                
            |_ -> 
              try Some(MCInt64(System.Convert.ToInt64(s,10)), s) with
              |_ ->
                try Some(MCDouble(System.Convert.ToDouble(s, invcul)), s) with
                |_ -> None

let parse (headerLocation: System.IO.FileInfo) (pchLocation: System.IO.FileInfo option) (includePaths : string seq)=
  // Let's use clean C interface. Those fancy C++ classes with inheritance and whistles aren't good for rust codegen.
  let options = 
    seq {
       yield "-x"
       yield "c"
       yield "--target=x86_64-pc-win32"
       yield ""
       yield "-std=c11"
       yield "-fms-extensions"
       yield "-fms-compatiblity"
       yield "-fmsc-version=1800"
       yield! includePaths |> Seq.map (fun p -> "-I"+p)
    } |> Array.ofSeq
  let index = createIndex(0, 1)

  let pchTempLocation = Option.maybe {
    let! pchLocation = pchLocation
    let translationUnit = parseTranslationUnit(index, pchLocation.FullName, options, options.Length, [||], 0u, TranslationUnitFlags.DetailedProcessingRecord)

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

  let translationUnit = parseTranslationUnit(index, headerLocation.FullName, options, options.Length, [||], 0u, TranslationUnitFlags.DetailedProcessingRecord)


  let types=ref Map.empty
  let enums=ref Map.empty
  let structs=ref Map.empty
  let funcs=ref Map.empty
  let iids=ref Map.empty
  let defines=ref Map.empty

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
        let (pname,ptype,pannot)=(getCursorDisplayNameFS cursor |> (fun pname -> if pname="type" then "type_" else pname), getCursorType cursor |> typeDesc, cannot)
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
    if (retyname.StartsWith("struct ") || (retyname.Contains(" struct ") && retyname.Contains("*")=false)) then
      // C returns those structs thru EAX:EDX, C++ thru reference
      // and Rust do something different
      args := ("__ret_val",Ptr(rety), Out) :: !args
      Function(CFuncDesc(List.rev !args, Ptr(rety),cc))
    else
      Function(CFuncDesc(List.rev !args,rety,cc))

  let rec parseUnion (cursor:Cursor)=
    let tokens=tokenizeFS cursor
    //printfn "%A %s %s" cursor.kind (cursor |> getCursorSpellingFS) (String.concat " " tokens)
    let fields=ref []
    let parseFieldDecl cursor _ _=
      let ckind=getCursorKind cursor
      if ckind=CursorKind.FieldDecl then
        let ctype=getCursorType cursor
        let nm=getCursorDisplayNameFS cursor
        let pointee=getPointeeType ctype
        let ty=ctype |> typeDesc
        let sz=ctype |> getSizeOfType
        let bw=if isBitFieldFS cursor then Some(getFieldDeclBitWidth cursor) else None
        fields := (CStructElem(nm, ty, bw),sz) :: !fields
      else if ckind=CursorKind.StructDecl then
        let ctype=getCursorType cursor
        let sz=ctype |> getSizeOfType
        fields := (CStructElem("", parseStruct cursor, None),sz) :: !fields
      ChildVisitResult.Continue
    visitChildrenFS cursor parseFieldDecl () |> ignore
    let fields = 
      !fields |> List.rev
        |> utils.seqPairwise 
        |> Seq.choose
          (function
            |[(CStructElem("", (Struct(_) as stc), _), sz); (CStructElem(fname, StructRef sref, bw),_)] when sref.StartsWith("(anonymous struct") ->
              Some((CStructElem(fname,stc,bw),sz))
            |[(CStructElem(fname, StructRef sref, bw), _); _] when sref.StartsWith("(anonymous struct") -> 
              None
            |[se;_] -> 
              Some(se)
            |[(CStructElem(fname, StructRef sref, bw), _)] when sref.StartsWith("(anonymous struct") ->
              None
            |[se] ->
              Some(se)
            |_ -> raise <| new System.Exception("unreachable")
          )
    Union(List.ofSeq fields)
  and parseStruct (cursor:Cursor)=
    let tokens=tokenizeFS cursor
    //printfn "%A %s %s" cursor.kind (cursor |> getCursorSpellingFS) (String.concat " " tokens)
    let fields=ref []
    let parseFieldDecl cursor _ _=
      let ckind=getCursorKind cursor
      if ckind=CursorKind.FieldDecl then
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
      else if ckind=CursorKind.UnionDecl then
        fields := CStructElem("", parseUnion cursor, None) :: !fields
      ChildVisitResult.Continue
    visitChildrenFS cursor parseFieldDecl () |> ignore
    Struct(List.rev !fields)

  // main parser callback
  let rec childVisitor cursor _ _ =
    let mutable curFile = File(0)
    let mutable curLine=0u
    let mutable curColumn=0u
    let mutable curOffset=0u
    getExpansionLocation( cursor |> getCursorExtent |> getRangeStart, &curFile, &curLine, &curColumn, &curOffset)
    let curFileName = getFileNameFS curFile
    let locInfo=(curFileName, curLine, curColumn, curOffset)


    let cursorKind=getCursorKind cursor

    if cursorKind=CursorKind.MacroDefinition then
      let mname :: tokens = (tokenizeFS cursor)
      // rudimentary parsing of macro defined constants
      // expressions aren't supported
      let ignorecase=System.StringComparison.InvariantCultureIgnoreCase
      if mname.StartsWith("D3D", ignorecase) || mname.StartsWith("DXGI", ignorecase) then
        let cv=
          match tokens with
          |"(" :: value :: ")" :: _ :: [] // last token doesn't belong to macro. libclang bug?
          |value :: _ :: [] ->
            tryParse value
          |"(" :: "-" :: value :: ")" :: _ :: [] 
          |"-" :: value :: _ :: [] ->
            tryParse ("-" + value)
          |_ -> None
        match cv with
        |Some(v,orgs) ->
          //printfn "%A %s" v (System.String.Join(" ", tokens))
          defines := !defines |> Map.add mname (v, orgs, locInfo)
        |None ->
          ()

    if cursorKind=CursorKind.EnumDecl then
      enums := !enums |> Map.add (cursor |> getCursorDisplayNameFS) (Enum(enumCType cursor, parseEnumConsts cursor), locInfo)

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
      types := !types |> Map.add (cursor |> getCursorDisplayNameFS) (Typedef(tdesc), locInfo)
      // typedefs can contain other definitions
      visitChildrenFS cursor childVisitor () |> ignore

    if cursorKind=CursorKind.StructDecl then
      let structName=cursor |> getCursorDisplayNameFS
      structs := !structs |> Map.add structName (parseStruct cursor, locInfo)

    if cursorKind=CursorKind.FunctionDecl then
      funcs := !funcs |> Map.add (cursor |> getCursorSpellingFS) (parseFunction cursor (getCursorType cursor), locInfo)

    if cursorKind=CursorKind.VarDecl then
      let nm=cursor |> getCursorSpellingFS
      let tys=cursor |> getCursorType |> getTypeSpellingFS
      if tys="const IID" then
        iids := !iids |> Map.add nm locInfo

    if cursorKind=CursorKind.UnexposedDecl then // dip into extern "C"
      visitChildrenFS cursor childVisitor () |> ignore
    
    ChildVisitResult.Continue

  let cursor = getTranslationUnitCursor(translationUnit)

  try
    visitChildrenFS cursor childVisitor () |> ignore

    for KeyValue(en,ty) in !enums do
      match ty with
      |Enum(values=vals), _ ->
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
          |Typedef (StructRef s), _ -> Some(s)
          |_ -> None
          )

    let struct2typedef = typedef2struct |> Map.toSeq |> Seq.map swap |> Map.ofSeq
    
    structs := !structs |> Map.toSeq |> Seq.map (fun (k,v) -> match Map.tryFind k struct2typedef with Some(n) -> (n,v) |None -> (k,v) ) |> Map.ofSeq

    let removeTypedefStructs m=
        m  |> Map.map 
            (fun n (ty, loc) ->
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
                  ) |> fun ty -> (ty, loc)
            )
    types := removeTypedefStructs (!types |> Map.filter (fun n _ -> Map.containsKey n typedef2struct |> not))

    structs := removeTypedefStructs !structs

    (!types, !enums, !structs, !funcs, !iids, !defines)

  finally
    translationUnit |> disposeTranslationUnit
    index |> disposeIndex
        
    Option.maybe {
        let! pch = pchTempLocation
        pch.Delete()
    } |> ignore

