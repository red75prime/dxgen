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

let getLocInfo (cursor: Cursor) = 
    let mutable curFile = File(0)
    let mutable curLine=0u
    let mutable curColumn=0u
    let mutable curOffset=0u
    getExpansionLocation( cursor |> getCursorExtent |> getRangeStart, &curFile, &curLine, &curColumn, &curOffset)
    let curFileName = getFileNameFS curFile
    (curFileName, curLine, curColumn, curOffset)

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

let keys m=
  m |> Map.toSeq |> Seq.map fst |> Set.ofSeq

let parse (headerLocation: System.IO.FileInfo) (pchLocation: System.IO.FileInfo option) (includePaths : string seq) (target:string)=
  let options = 
    seq {
       yield "-x"
       yield "c++"
       yield "-std=c++11"
       yield "--target="+target
       yield "-fms-extensions"
       yield "-fms-compatibility"
       yield "-Wno-ignored-attributes"
       yield "-Wno-microsoft"
       yield! includePaths |> Seq.map (fun p -> "-I\""+p+"\"")
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
  let typedefloc = ref Map.empty

  let rec registerTypeLocation (ctype:Type) = 
      let tdesc = typeDesc ctype
      match tdesc with
      |Ptr(_) -> 
        registerTypeLocation <| getPointeeType ctype
      |_ ->
          match Map.tryFind tdesc !typedefloc with
          |Some(_) -> ()
          |None ->
              let decl = getTypeDeclaration ctype
              let declLocInfo = getLocInfo decl
              typedefloc := Map.add tdesc declLocInfo !typedefloc

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
    let typedef = ref None
    let args=ref []
    let argsVisitor (cursor:Cursor) _ _=
      if cursor.kind=CursorKind.ParmDecl then
        let cannot=
          match annotations cursor with
            |[] -> NoAnnotation
            |[ann] -> parseSAL ann
            |_ -> raise <| new System.Exception("Multiple annotations")
        let ctype = getCursorType cursor
        registerTypeLocation ctype
        let (pname,ptype,pannot)=(getCursorDisplayNameFS cursor |> (fun pname -> if pname="type" then "type_" else pname), ctype |> typeDesc, cannot)
        let pdesc=
          match ptype with
          |Array(Const(_),_) as arr -> (pname, Ptr(Const(arr)), pannot)
          |Array(_,_) as arr -> (pname, Ptr(arr), pannot)
          |_ -> (pname, ptype, pannot)
        args := pdesc :: !args
      ChildVisitResult.Continue
    visitChildrenFS cursor argsVisitor () |> ignore
    match !typedef with
    |Some(td) ->
      TypedefRef td
    |None ->
      if (List.length !args <> nArgs) then
        raise <| System.Exception("Number of parmDecls doesn't match number of arguments. " :: tokenizeFS cursor |> String.concat " ")
      let crety = getResultType(fType)
      registerTypeLocation crety
      let rec getretyname (crety:Type) =
        if crety.kind = TypeKind.Typedef then
          let cretydecl = getTypeDeclaration crety
          let urety = getTypedefDeclUnderlyingType(cretydecl)
          if getTypeSpellingFS(urety).Contains("*") then
            (getretyname urety)+" *"
          else
            getretyname urety
        else
          getTypeSpellingFS(crety)
      let retyname = getretyname crety
          
      if (retyname.StartsWith("struct ") || retyname.Contains(" struct ")) && (retyname.Contains("*")=false) then
        // C returns those structs thru EAX:EDX, C++ thru reference
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
        registerTypeLocation ctype
        let nm=getCursorDisplayNameFS cursor
        let pointee=getPointeeType ctype
        let ty=ctype |> typeDesc
        let sz=ctype |> getSizeOfType
        let bw=if isBitFieldFS cursor then Some(getFieldDeclBitWidth cursor) else None
        fields := (CStructElem(nm, ty, bw), [TargetUnknown, sz]) :: !fields
      else if ckind=CursorKind.StructDecl then
        let ctype=getCursorType cursor
        let sz=ctype |> getSizeOfType
        fields := (CStructElem("", parseStruct cursor "" |> fst, None), [TargetUnknown, sz]) :: !fields
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
  and parseStruct (cursor:Cursor) (structName: string) : (CTypeDesc*bool)=
    let tokens=tokenizeFS cursor
    //printfn "%A %s %s" cursor.kind (cursor |> getCursorSpellingFS) (String.concat " " tokens)
    let fields=ref []
    let bas = ref ""
    let itIsAClass = ref false
    let parseFieldDecl cursor _ _=
      let ckind=getCursorKind cursor
      //printfn "    %s: %A %s" structName ckind (getCursorDisplayNameFS cursor)
      if ckind=CursorKind.CxxBaseSpecifier then
        itIsAClass := true // crude. TODO: something
        let basename = getCursorDisplayNameFS cursor
        if basename.StartsWith("struct ") then
          bas := basename.Substring(7)
        else 
            //printfn "Warning: expected C++ base specifier in the form 'struct XXX', found '%s'" basename
            bas := basename
            //raise <| new System.Exception("Base specifier is not struct: "+basename)
      if ckind=CursorKind.FieldDecl then
          let ctype=getCursorType cursor
          registerTypeLocation ctype
          let nm=getCursorDisplayNameFS cursor
          let pointee=getPointeeType ctype
          let nArgs=getNumArgTypes(pointee)
          let ty=
            if nArgs<> -1 then
              // function pointer
              Ptr(parseFunction cursor pointee)
            else
              ctype |> typeDesc
          let bw=if isBitFieldFS cursor then Some(getFieldDeclBitWidth cursor) else None
          fields := CStructElem(nm, ty, bw) :: !fields
      else if ckind=CursorKind.CxxMethod then
          itIsAClass := true
          let nm=getCursorSpellingFS cursor
          if isPureVirtualFS cursor then
            let ctype=getCursorType cursor
            let ty = 
              match parseFunction cursor ctype with
              |Function(CFuncDesc(parms, rty, cc)) ->
                Ptr(Function(CFuncDesc(("This", (Ptr(StructRef structName)), NoAnnotation)::parms, rty, cc)))
              |_ -> raise <| new System.Exception("Unreachable")
            fields := CStructElem(nm, ty, None) :: !fields
          else
            // Skip implementation
            //printfn "Skipping non pure virtual %s::%s" structName nm
            ()
      else if ckind=CursorKind.UnionDecl then
        fields := CStructElem("", parseUnion cursor, None) :: !fields
      else if ckind=CursorKind.UnexposedAttr then
        //let tokens = tokenizeFS cursor
        //let what = getCursorSpellingFS cursor
        //printfn "      Attribute: %s %A" what tokens
        ()
      ChildVisitResult.Continue
    visitChildrenFS cursor parseFieldDecl () |> ignore
    (Struct(List.rev !fields, !bas), !itIsAClass)
  
  let parseMacro (cursor:Cursor) locInfo =
    match tokenizeFS cursor with
    |[] -> raise <| new System.Exception("unreachable")
    |mname :: tokens ->
        // rudimentary parsing of macro defined constants
        // expressions aren't supported
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
    
  // main parser callback
  let rec childVisitor cursor _ _ =
    let (curFileName,_,_,_) as locInfo = getLocInfo cursor

    if curFileName.EndsWith(headerLocation.Name) then
        let cursorKind=getCursorKind cursor
        //printfn "%A %s" cursorKind (getCursorDisplayNameFS cursor)
    
        if cursorKind=CursorKind.MacroDefinition then
          parseMacro cursor locInfo

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
              registerTypeLocation uty
              uty |> typeDesc
          types := !types |> Map.add (cursor |> getCursorDisplayNameFS) (Typedef(tdesc), locInfo)
          // typedefs can contain other definitions
          visitChildrenFS cursor childVisitor () |> ignore

        if cursorKind=CursorKind.StructDecl then
          let structName=cursor |> getCursorDisplayNameFS
          match parseStruct cursor structName with
          |(Struct(ses, bas) as strct, itIsAClass) ->
              if itIsAClass then
                // Transform class into struct and vtable
                let vtblName = structName+"Vtbl"

                structs := !structs |> Map.add vtblName (strct, locInfo)
                structs := !structs |> Map.add (structName) (Struct([CStructElem("pVtbl", Ptr(StructRef vtblName), None)], ""), locInfo)
              else
                structs := !structs |> Map.add structName (strct, locInfo)
          |_ -> raise <| new System.Exception("unreachable")

        if cursorKind=CursorKind.FunctionDecl then
          let funcName = cursor |> getCursorSpellingFS
          if funcName.StartsWith("operator") then
            ()
          else
            funcs := !funcs |> Map.add funcName (parseFunction cursor (getCursorType cursor), locInfo)

        if cursorKind = CursorKind.MacroInstantiation then
            let tokens = tokenizeFS cursor
            match tokens with
            |["DEFINE_GUID"; "("; nm; ","; p1; ","; p2; ","; p3; ","; b1;
                             ","; b2; ","; b3; ","; b4; ","; b5; ","; b6; ","; b7; ","; b8; ")"; ";"] ->
                iids := !iids |> Map.add nm (locInfo, IID11 [| p1;p2;p3;b1;b2;b3;b4;b5;b6;b7;b8 |])
            |["DX_DECLARE_INTERFACE"; "("; iid; ")"; iname] ->
                iids := !iids |> Map.add ("IID_"+iname) (locInfo, IID1 (iid.Substring(1, iid.Length-2)))
            |_ -> 
                // whatever
                ()

//        if cursorKind=CursorKind.VarDecl then
//          let nm=cursor |> getCursorSpellingFS
//          let tys=cursor |> getCursorType |> getTypeSpellingFS
//          if tys="const IID" then
//            iids := !iids |> Map.add nm locInfo

        if cursorKind=CursorKind.UnexposedDecl then // dip into extern "C"
          visitChildrenFS cursor childVisitor () |> ignore
    
    ChildVisitResult.Continue

  let cursor = getTranslationUnitCursor(translationUnit)

  try
    visitChildrenFS cursor childVisitor () |> ignore

//    for KeyValue(en,ty) in !enums do
//      match ty with
//      |Enum(values=vals), _ ->
//        let (_,isect)=vals |> List.map snd |> List.filter (fun n -> n<>0xffffUL && n<>0xffffffffUL && n<>0xffffffffffffffffUL) |> List.fold (fun (acc,isec) n -> (acc|||n,isec || (acc&&&n<>0UL) )) (0x0UL,false)
//        if isect then
//          printfn "  (\"%s\",EAEnum);" en
//        else 
//          if en.Contains("FLAGS") then
//            printfn "  (\"%s\",EAFlags);" en
//          else if (List.length vals)<4 then
//            printfn "  (\"%s\",EAEnum);" en
//          else
//            printfn "  (\"%s\",EAFlags);" en
//      | _ -> ()
//
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

let zipStructs () = ()

let rec setTarget ty target =
  ty |> recursiveTransform 
    (fun ty ->
      match ty with
      |Union ues ->
        let ues'=
          ues |> List.map 
            (fun (CStructElem(ename, ty, bw), s) ->
              (CStructElem(ename, setTarget ty target, bw), s |> List.map (fun (_, sz) -> (target, sz)))
            )
        Some(Union(ues'))
      |_ -> None)

let rec combineTargets ty1 ty2 =
  let combineUnions ues1 ues2=
    List.map2 
      (fun (CStructElem(fname1, ty1, bw1) ,sz1) (CStructElem(fname2, ty2, bw2), sz2) -> 
        assert(fname1=fname2)
        assert(bw1=bw2)
        (CStructElem(fname1, combineTargets ty1 ty2, bw1), List.concat [sz1;sz2])
         ) ues1 ues2
  match (ty1, ty2) with
  |(Union ues1, Union ues2) ->
    Union(combineUnions ues1 ues2)
  |_ ->
    let (sty1, gen) = subtypes ty1
    let (sty2, _) = subtypes ty2
    gen <| List.map2 combineTargets sty1 sty2

// It parses code as 32-bit then as 64-bit and then it zips results
// Some unions need different rust representation in 32/64 bits
let combinedParse (headerLocation: System.IO.FileInfo) (pchLocation: System.IO.FileInfo option) (includePaths : string seq) =
  let (types32, enums32, structs32', funcs32, iids32, defines32) as p32=
    parse headerLocation pchLocation includePaths "i686-pc-win32"
  let (types64, enums64, structs64', funcs64, iids64, defines64) as p32=
    parse headerLocation pchLocation includePaths "x86_64-pc-win32"
  // ensure that parse results contain same items
  assert (keys types32 = keys types64)
  assert (keys enums32 = keys enums64)
  assert (keys structs32' = keys structs64')
  assert (keys funcs32 = keys funcs64)
  assert (keys iids32 = keys iids64)
  assert (keys defines32 = keys defines64)
  let structs32 = 
    structs32' |>
      Map.map (fun _ (ty,locInfo) -> (setTarget ty TargetX86, locInfo))

  let structs64 = 
    structs64' |>
      Map.map (fun _ (ty,locInfo) -> (setTarget ty TargetX64, locInfo))

  let structsCombined=
    let combine (k1,(v1, locInfo)) (k2, (v2, _)) =
      assert(k1=k2)
      (k1, (combineTargets v1 v2, locInfo))
    Seq.map2 combine (Map.toSeq structs32) (Map.toSeq structs64) |> Map.ofSeq

  (types64, enums64, structsCombined, funcs64, iids64, defines64)
