module parse

open FSharpx
open libclang
open cdesc
open salparser

let filtermapMap (f: 'k -> 'v -> Option<'v1>) (m: Map<'k,'v>)=
    m |> Map.filter (fun k v -> Option.isSome(f k v)) 
      |> Map.map (fun k v -> match f k v with Some(v1) -> v1 |_ -> failwith "Unreachable")

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

let filename2module file =
    match file with
    |"" -> "ctypes"
    |_ ->
        let parts = file.Split([|'/';'\\'|])
        let len = parts.Length
        let file = 
            let file = parts.[len-1]
            if file.EndsWith(".h") then
                file.Substring(0, file.Length - 2).ToLowerInvariant()
            else
                failwithf "Wrong header file %s" file
        parts.[len-2]+"::"+file

let getSLInfo (sl: SourceLocation) = 
    let mutable curFile = File(0)
    let mutable curLine=0u
    let mutable curColumn=0u
    let mutable curOffset=0u
    getExpansionLocation( sl, &curFile, &curLine, &curColumn, &curOffset)
    let curFileName = getFileNameFS curFile
    (curFileName, curLine, curColumn, curOffset)


let getLocInfo (cursor: Cursor) = 
    cursor |> getCursorExtent |> getRangeStart |> getSLInfo

let uuidRegexp = System.Text.RegularExpressions.Regex("\"([0-9a-fA-F]{8})-([0-9a-fA-F]{4})-([0-9a-fA-F]{4})-([0-9a-fA-F]{2})([0-9a-fA-F]{2})-([0-9a-fA-F]{2})([0-9a-fA-F]{2})([0-9a-fA-F]{2})([0-9a-fA-F]{2})([0-9a-fA-F]{2})([0-9a-fA-F]{2})\"")

let readFromSource (sr: SourceRange) =
    let (fileName, _, _, startOffset) = getRangeStart sr |> getSLInfo
    let (_, _, _, endOffset) = getRangeEnd sr |> getSLInfo
    if System.String.IsNullOrEmpty fileName then
        ""
    else
        let fs = new System.IO.FileStream(fileName, System.IO.FileMode.Open, System.IO.FileAccess.Read)
        let pos = fs.Seek(int64(startOffset), System.IO.SeekOrigin.Begin) 
        assert(pos = int64(startOffset))
        let tr = new System.IO.StreamReader(fs, System.Text.Encoding.UTF8)
        let len = int(endOffset - startOffset)
        let buf = Array.init len (fun _ -> ' ')
        let rlen = tr.Read(buf, 0, len)
        assert(len = rlen)
        System.String buf        

let parseUuid (s: string) =
    let m = uuidRegexp.Match(s)
    if m.Success then
        let gs = m.Groups |> Seq.cast<System.Text.RegularExpressions.Group> |> Seq.skip 1 
                          |> Seq.map(fun g -> "0x" + g.Value.ToLowerInvariant())
        Some("#[uuid("+System.String.Join(", ", gs)+")]")
    else
        None

let tryParse (s:System.String)=
    let invcul=System.Globalization.CultureInfo.InvariantCulture
    let ignorecase=System.StringComparison.InvariantCultureIgnoreCase
    if s.StartsWith("0x", ignorecase) then
        if s.EndsWith("ULL", ignorecase) then
            let s'=s.Substring(2,s.Length-5)
            try Some(MCUInt64(System.Convert.ToUInt64(s',16)), "0x"+s') with
            |_ -> failwithf "%s should be UInt64, but it isn't" s
        else if s.EndsWith("LL", ignorecase) then
            let s'=s.Substring(2,s.Length-4)
            try Some(MCInt64(System.Convert.ToInt64(s',16)), "0x"+s') with
            |_ -> failwithf "%s should be Int64, but it isn't" s
        else if s.EndsWith("UL", ignorecase) then
            let s'=s.Substring(2,s.Length-4)
            try Some(MCUInt32(System.Convert.ToUInt32(s',16)), "0x"+s') with
            |_ -> failwithf "%s should be UInt32, but it isn't" s
        else if s.EndsWith("L", ignorecase) then
            let s'=s.Substring(2,s.Length-3)
            try Some(MCUInt32(System.Convert.ToUInt32(s',16)), "0x"+s') with
            |_ ->
                try Some(MCInt32(System.Convert.ToInt32(s',16)), "0x"+s') with
                |_ -> failwithf "%s should be Int32, but it isn't" s
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
                |_ -> failwithf "%s should be UInt64, but it isn't" s
            else if s.EndsWith("LL", ignorecase) then
                let s'=s.Substring(0,s.Length-2)
                try Some(MCInt64(System.Convert.ToInt64(s', 10)), s') with
                |_ -> failwithf "%s should be Int64, but it isn't" s
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


let rec getFieldsList (ccur: Cursor) =
    let ct = getCanonicalType <| getCursorType ccur
    if getSizeOfType ct = TypeLayoutError_Incomplete then
        failwith "Cannot get method list on incomplete type"
    let typedecl = getTypeDeclaration ct
    let name = getTypeSpellingFS ct
    let locinfo = getLocInfo typedecl
    let fieldslist = ref []
    let baselist = ref []
    let visitor cursor _ _ =
        let ckind = getCursorKind cursor
        match ckind with
        |CursorKind.CxxBaseSpecifier ->
            if not <| List.isEmpty !baselist then
                failwith "Multiple inheritance isn't supported"
            baselist := getFieldsList cursor
        |CursorKind.FieldDecl->
            let mname = getCursorSpellingFS cursor
            let mtype = getCanonicalType <| getCursorType cursor
            let ty = typeDesc <| getCursorType cursor
            let bw = if isBitFieldFS cursor then Some(getFieldDeclBitWidth cursor, getSizeOfType mtype) else None
            fieldslist := CStructElem(mname, ty, bw) :: !fieldslist
        |_ -> ()
        ChildVisitResult.Continue
    ignore <| visitChildrenFS typedecl visitor ()
    List.concat [!baselist; List.rev !fieldslist]

let rec getMethodList (ccur: Cursor) =
    let ct = getCanonicalType <| getCursorType ccur
    if getSizeOfType ct = TypeLayoutError_Incomplete then
        failwith "Cannot get method list on incomplete type"
    let typedecl = getTypeDeclaration ct
    let name = getTypeSpellingFS ct
    let locinfo = getLocInfo typedecl
    let methodlist = ref []
    let baselist = ref []
    let visitor cursor _ _ =
        let ckind = getCursorKind cursor
        match ckind with
        |CursorKind.CxxBaseSpecifier ->
            if not <| List.isEmpty !baselist then
                failwith "Multiple inheritance isn't supported"
            baselist := getMethodList cursor
        |CursorKind.CxxMethod ->
            let mname = getCursorSpellingFS cursor
            let mtype = getCanonicalType <| getCursorType cursor
            methodlist := (mname, mtype) :: !methodlist
        |_ -> ()
        ChildVisitResult.Continue
    ignore <| visitChildrenFS typedecl visitor ()
    List.concat [!baselist; List.rev !methodlist]


let parse   (headerLocation: System.IO.FileInfo) 
            (pchLocation: System.IO.FileInfo option) 
            (includePaths: string seq) (target: string) =
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
            yield! includePaths |> Seq.map (fun p -> p.Replace("\\","/")) |> Seq.map (fun p -> "-I"+p+"")
        } |> Array.ofSeq
    let index = createIndex(0, 1)

    // Location of precompiled header data, if any
    let pchTempLocation = 
        match pchLocation with
        |Some(pchLocation) ->
            let translationUnit = parseTranslationUnitFS index pchLocation.FullName options [||] TranslationUnitFlags.DetailedProcessingRecord
            try
                let fileLocation = System.IO.Path.GetTempFileName()
                saveTranslationUnit(translationUnit, fileLocation, 0u)
                Some(System.IO.FileInfo(fileLocation))
            finally
                translationUnit |> disposeTranslationUnit
        |None -> None

    let optionsPCH = 
        match pchTempLocation with
        |Some(pch) -> Array.concat [options; [|"-include-pch"; pch.FullName.Replace("\\","/")|]]
        |None -> options

    let translationUnit = parseTranslationUnitFS index headerLocation.FullName optionsPCH [||] TranslationUnitFlags.DetailedProcessingRecord

    let types = ref Map.empty
    let enums = ref Map.empty
    let structs = ref Map.empty
    let funcs = ref Map.empty
    let iids = ref Map.empty
    let defines = ref Map.empty
    // locations of type definitions
    let typedefloc = ref Map.empty;
    // storage for named union definitions 
    // TODO: implement parsing standalone unions
    let namedUnions = ref Map.empty
    // Attributes of structs
    let attribs = ref Map.empty

    let registerAttribute name atts =
        match Map.tryFind name !attribs with
        |Some(atts') -> attribs := Map.add name (List.concat [atts'; atts]) !attribs
        |None -> attribs := Map.add name atts !attribs

    let registerForwardDeclLocation typeName modName =
        typedefloc := Map.add typeName modName !typedefloc

    let rec registerTypeLocation (ctype:Type) (isComInterface: bool) = 
        let tdesc =
            match typeDesc ctype with
            |Const(t) -> t
            |StructRef(t) -> TypedefRef(t)
            |UnionRef(t) -> TypedefRef(t)
            |t -> t
        match tdesc with
        |Unimplemented _ ->
            utils.coloredText System.ConsoleColor.Red (fun () -> 
                printfn "Warning: cannot register %A" tdesc)
        |Ptr(_) -> 
            registerTypeLocation (getPointeeType ctype) isComInterface
        |Array(_,_) -> 
            registerTypeLocation (getArrayElementType ctype) isComInterface
        |Primitive CPrimitiveType.Char8 
        |Primitive CPrimitiveType.Int32 ->
            ()
        |_ ->
            let tstr =
                match tdesc with
                |StructRef n -> n
                |TypedefRef n -> n
                |EnumRef n -> n
                |Primitive _ -> tyToRust tdesc
                |_ -> failwith "Unexpected type"
            let decl = getTypeDeclaration ctype
            let (file,_,_,_) = getLocInfo decl
            let modname = filename2module file
            typedefloc := Map.add tstr modname !typedefloc
            if isComInterface then
                typedefloc := Map.add (tstr+"Vtbl") modname !typedefloc


    let parseEnumConsts (cursor:Cursor)=
        let listOfConsts=ref []
        let addConst cursor _ _=
            if getCursorKind cursor=CursorKind.EnumConstantDecl then
                listOfConsts := (cursor |> getCursorDisplayNameFS, cursor |> getEnumConstantDeclUnsignedValue) :: !listOfConsts
            ChildVisitResult.Continue
        visitChildrenFS cursor addConst () |> ignore
        List.rev !listOfConsts

    let parseFunction (cursor:Cursor) (fType:Type)=
        let nArgs = getNumArgTypes(fType)
        let cc = getFunctionTypeCallingConv(fType)
        let rety = getResultType(fType) |> typeDesc
        let typedef = ref None
        let args=ref []
        let argsVisitor (cursor:Cursor) _ _=
            if cursor.kind=CursorKind.ParmDecl then
                let cannot=
                    match annotations cursor with
                    |[] -> NoAnnotation
                    |[ann] -> parseSAL ann
                    |_ -> failwith "Multiple annotations"
                let ctype = getCursorType cursor
                registerTypeLocation ctype false
                let (pname,ptype,pannot) =
                    (getCursorDisplayNameFS cursor |> (fun pname -> if pname="type" then "type_" else pname), ctype |> typeDesc, cannot)
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
                failwith ("Number of parmDecls doesn't match number of arguments. " :: tokenizeFS cursor |> String.concat " ")
            let crety = getResultType(fType)
            if rety <> Primitive Void then
                registerTypeLocation crety false
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
            if cursor.kind = CursorKind.CxxMethod then
                if (retyname.StartsWith("struct ") || retyname.Contains(" struct ")) && (retyname.Contains("*")=false) then
                    // C returns those structs thru EAX:EDX, C++ thru reference
                    // order of parameters for C call: this, pointer to return value, rest of parameters
                    Function(CFuncDesc(("__ret_val",Ptr(rety), Out) :: List.rev !args, Ptr(rety), cc))
                else
                    Function(CFuncDesc(List.rev !args, rety, cc))
            else
                Function(CFuncDesc(List.rev !args, rety, cc))

    let rec parseUnion (cursor:Cursor)=
        let tokens = tokenizeFS cursor
        //printfn "%A %s %s" cursor.kind (cursor |> getCursorSpellingFS) (String.concat " " tokens)
        let fields = ref []
        let parseFieldDecl cursor _ _=
            let ckind = getCursorKind cursor
            if ckind = CursorKind.FieldDecl then
                let ctype = getCursorType cursor
                registerTypeLocation ctype false
                let nm = getCursorDisplayNameFS cursor
                let pointee = getPointeeType ctype
                let ty = ctype |> typeDesc
                let sz = ctype |> getSizeOfType
                let bw = if isBitFieldFS cursor then Some(getFieldDeclBitWidth cursor, getSizeOfType ctype) else None
                fields := (CStructElem(nm, ty, bw), [TargetUnknown, sz]) :: !fields
            else if ckind = CursorKind.StructDecl then
                let nm = getCursorDisplayNameFS cursor
                let ctype = getCursorType cursor
                let sz = ctype |> getSizeOfType
                if System.String.IsNullOrEmpty nm then
                    // unnamed struct inside union
                    fields := (CStructElem("", parseStruct cursor "" |> fst, None), [TargetUnknown, sz]) :: !fields
                else
                    // named struct defined inside union
                    let (s, isAClass) = parseStruct cursor nm
                    assert(isAClass = false)
                    structs := Map.add nm (s, getLocInfo cursor) !structs
            else
                utils.coloredText System.ConsoleColor.Red (fun () ->
                    printfn "  Warning! Unexpected cursor kind '%A' in union" ckind)
                printfn "  Location: %A" (getLocInfo cursor)
                printfn "  Text: %s" (readFromSource <| getCursorExtent cursor)
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
                        |_ -> failwith "unreachable"
                    )
        Union(List.ofSeq fields)
    and parseStruct (cursor:Cursor) (structName: string) : (CTypeDesc*bool)=
        let tokens = tokenizeFS cursor
        //printfn "%A %s %s" cursor.kind (cursor |> getCursorSpellingFS) (String.concat " " tokens)
        let fields = ref []
        let bas = ref ""
        let itIsAClass = ref false
        let hasDataMembers = ref false
        let inheritedMethods = ref []
        let inheritedFields = ref []
        let baseType = ref None
        let parseFieldDecl cursor _ _=
            let ckind=getCursorKind cursor
            //printfn "    %s: %A %s" structName ckind (getCursorDisplayNameFS cursor)
            match ckind with
            |CursorKind.CxxBaseSpecifier ->
                if !itIsAClass then
                    failwith "Multiple inheritance isn't supported"
                itIsAClass := true // crude. TODO: something
                let ctype = getCursorType cursor
                let cxxbasevisitor (cur:Cursor) _ _ =
                    if cur.kind = CursorKind.TypeRef then
                        inheritedMethods := getMethodList cur
                        inheritedFields := getFieldsList cur
                        ChildVisitResult.Break
                    else
                        ChildVisitResult.Continue
                visitChildrenFS cursor cxxbasevisitor () |> ignore
                baseType := Some(ctype)
                let basename = getCursorDisplayNameFS cursor
                if basename.StartsWith("struct ") then
                    bas := basename.Substring(7)
                else 
                    //printfn "Warning: expected C++ base specifier in the form 'struct XXX', found '%s'" basename
                    bas := basename
                    //fail("Base specifier is not struct: "+basename)
            |CursorKind.FieldDecl ->
                let ctype = getCursorType cursor
                let nm = getCursorDisplayNameFS cursor
                let pointee = getPointeeType ctype
                let nArgs = getNumArgTypes(pointee)
                let ty=
                    if nArgs<> -1 then
                        // function pointer
                        Ptr(parseFunction cursor pointee)
                    else
                        hasDataMembers := true
                        ctype |> typeDesc
                match ty with
                |UnionRef(nm) ->
                    fields := CStructElem("", Map.find nm !structs |> fst, None) :: !fields
                |_ ->
                    registerTypeLocation ctype false
                    let bw=if isBitFieldFS cursor then Some(getFieldDeclBitWidth cursor, getSizeOfType ctype) else None
                    fields := CStructElem(nm, ty, bw) :: !fields
            |CursorKind.CxxMethod ->
                itIsAClass := true
                let nm = getCursorSpellingFS cursor
                if isPureVirtualFS cursor then
                    let ctype = getCursorType cursor
                    let canonicalType = getCanonicalType ctype
                    // Sometimes Microsoft adds inherited pure virtual methods to the derived class
                    // Let's get rid of them
                    let isInherited (mname, mtype) =
                        mname = nm && equalTypesFs mtype canonicalType
                    let methodIsInherited = 
                        List.exists isInherited !inheritedMethods
                    if not methodIsInherited then
                        let ty = 
                            match parseFunction cursor ctype with
                            |Function(CFuncDesc(parms, rty, cc)) ->
                                Ptr(Function(CFuncDesc(("This", (Ptr(StructRef structName)), NoAnnotation)::parms, rty, cc)))
                            |_ -> failwith "Unreachable"
                        fields := CStructElem(nm, ty, None) :: !fields
                    else 
                        ()
                else
                    // Skip implementation
                    //printfn "Skipping non pure virtual %s::%s" structName nm
                    ()
            |CursorKind.UnionDecl ->
                let nm = getCursorSpellingFS cursor
                if nm <> "" then
                    // named union definition inside struct
                    structs := Map.add nm (parseUnion cursor, getLocInfo cursor) !structs
                    ()
                else
                    // unnamed union definition inside struct
                    fields := CStructElem("", parseUnion cursor, None) :: !fields
            |CursorKind.UnexposedAttr ->
                //let tokens = tokenizeFS cursor
                //let what = getCursorSpellingFS cursor
                //printfn "      Attribute: %s %A" what tokens
                let sl = getCursorExtent cursor
                let text = readFromSource sl
                match (parseUuid text) with
                |Some(uuid) -> registerAttribute structName [Attribute.AttrUuid uuid] 
                |None -> ()
            |_ -> 
                ()
            ChildVisitResult.Continue
        visitChildrenFS cursor parseFieldDecl () |> ignore
        if !bas <> "" && !hasDataMembers then
            // struct, which inherits another struct
            (Struct(List.concat [!inheritedFields; List.rev !fields], "", []), false)
        else
            match !baseType with
            |Some(ctype) ->
                registerTypeLocation ctype (!itIsAClass && not !hasDataMembers)
            |None -> ()
            let attrs = 
                match Map.tryFind structName !attribs with
                |Some(attrs) -> attrs
                |None -> []

            (Struct(List.rev !fields, !bas, attrs), !itIsAClass && not !hasDataMembers)
  
    let parseMacro (cursor:Cursor) locInfo =
        match tokenizeFS cursor with
        |[] -> failwith "unreachable"
        |mname :: tokens ->
            // rudimentary parsing of macro defined constants
            // expressions aren't supported
            let cv =
                match tokens with
                |"(" :: value :: ")" :: _ :: [] // last token doesn't belong to macro. libclang bug?
                |value :: _ :: [] ->
                    tryParse value
                |"(" :: "-" :: value :: ")" :: _ :: [] 
                |"-" :: value :: _ :: [] ->
                    tryParse ("-" + value)
                |_ -> Some(MCExpression(tokens), "")
            match cv with
            |Some(v,orgs) ->
                //printfn "%A %s" v (System.String.Join(" ", tokens))
                defines := !defines |> Map.add mname (v, orgs, locInfo)
            |None ->
                ()
    
      // main parser callback
    let rec childVisitor cursor parentCursor _ =
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
                let uty = cursor |> getTypedefDeclUnderlyingType
                let utysz = getSizeOfType uty
                let utyname = uty |> getTypeSpellingFS
                let pty = uty |> getPointeeType
                let nArgs = pty |> getNumArgTypes
                let tdesc =
                    if nArgs<> -1 then
                        // it is a function pointer
                        Ptr(parseFunction cursor pty)
                    else
                        let uts = getTypeSpellingFS uty
                        if utysz = TypeLayoutError_Incomplete then
                            ForwardDecl uts
                        else
                            registerTypeLocation uty false
                            uty |> typeDesc
                let typedefname = cursor |> getCursorDisplayNameFS
                if typedefname <> utyname then
                    types := !types |> Map.add typedefname (Typedef(tdesc), locInfo)
                // typedefs can contain other definitions
                visitChildrenFS cursor childVisitor () |> ignore

            if cursorKind = CursorKind.StructDecl then
                let structName = cursor |> getCursorDisplayNameFS
                let typesz = cursor |> getCursorType |> getSizeOfType
                if typesz = TypeLayoutError_Incomplete then
                    structs := !structs |> Map.add structName (ForwardDecl structName, locInfo)
                else
                    match parseStruct cursor structName with
                    |(Struct(ses, bas, attrs) as strct, itIsAClass) ->
                        if itIsAClass then
                            // Transform class into struct and vtable
                            let vtblName = structName+"Vtbl"

                            structs := !structs |> Map.add vtblName (strct, locInfo)
                            structs := !structs |> Map.add (structName) (Struct([CStructElem("pVtbl", Ptr(StructRef vtblName), None)], "", attrs), locInfo)
                        else
                            structs := !structs |> Map.add structName (strct, locInfo)
                    |_ -> failwith "unreachable"

            if cursorKind = CursorKind.UnionDecl then
                let unionName = cursor |> getCursorDisplayNameFS
                let typesz = cursor |> getCursorType |> getSizeOfType
                if typesz = TypeLayoutError_Incomplete then
                    structs := !structs |> Map.add unionName (ForwardDecl unionName, locInfo)
                else
                    match parseUnion cursor with
                    |Union(ses) ->
                        structs := !structs |> Map.add unionName (Struct([CStructElem("", Union(ses), None)], "", []), locInfo)
                    |_ -> failwith "unreachable"


            if cursorKind=CursorKind.FunctionDecl then
                let funcName = cursor |> getCursorSpellingFS
                if funcName.StartsWith("operator") then
                    ()
                else
                    match getCursorKind parentCursor with
                    |CursorKind.TranslationUnit ->
                        // function is not defined inside 'extern "C"' block. Skip it
                        utils.coloredText System.ConsoleColor.Red (fun () -> 
                            printfn "  Warning! Skipping non-extern-c function %s" funcName)
                        ()
                    |CursorKind.UnexposedDecl ->
                        let text = readFromSource <| getCursorExtent parentCursor
                        let func = parseFunction cursor (getCursorType cursor)
                        funcs := !funcs |> Map.add funcName (func, locInfo)
                    |ck ->
                        utils.coloredText System.ConsoleColor.Red (fun () -> 
                            printfn "  Warning! Unexpected parent cursor kind %A for function %s" ck funcName)

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

            if cursorKind=CursorKind.UnexposedDecl then // dip into extern "C"
                visitChildrenFS cursor childVisitor () |> ignore
    
        ChildVisitResult.Continue

    let cursor = getTranslationUnitCursor(translationUnit)

    try
        visitChildrenFS cursor childVisitor () |> ignore

        // remove forward decls defined in current module
        types := 
            !types |> Map.map (fun name ty ->
                                match ty with
                                |(Typedef(ForwardDecl(n)), loc) ->
                                    match Map.tryFind n !structs with
                                    |Some((ty1,_)) -> 
                                        (ty1, loc)
                                    |None -> ty
                                |_ -> ty
                            )

        // let's replace all "typedef struct BlaSmth {} Bla;" with "struct Bla {};"
        // No need to follow C quirks. Enums too
        let typedef2struct =
            !types |> filtermapMap
                (fun k v -> 
                    match v with 
                    |Typedef (StructRef s), _ -> Some(s)
                    |_ -> None
                )

        let typedef2enum =
            !types |> filtermapMap
                (fun k v -> 
                    match v with 
                    |Typedef (EnumRef s), _ -> Some(s)
                    |_ -> None
                )


        let struct2typedef = typedef2struct |> Map.toSeq |> Seq.map swap |> Map.ofSeq
        let enum2typedef = typedef2enum |> Map.toSeq |> Seq.map swap |> Map.ofSeq
    
        structs := !structs |> Map.toSeq |> Seq.map (fun (k,v) -> match Map.tryFind k struct2typedef with Some(n) -> (n,v) |None -> (k,v) ) |> Map.ofSeq
        enums := !enums |> Map.toSeq |> Seq.map (fun (k,v) -> match Map.tryFind k enum2typedef with Some(n) -> (n,v) |None -> (k,v) ) |> Map.ofSeq

        let removeTypedefStructs m=
            m |> Map.map 
                (fun n (ty, loc) ->
                    ty |> recursiveTransform 
                        (fun ty ->
                            match ty with
                            |Typedef(StructRef s) ->
                                match Map.tryFind s struct2typedef with
                                |Some(s) -> Some(StructRef s)
                                |None -> None
                            |Typedef(EnumRef s) ->
                                match Map.tryFind s enum2typedef with
                                |Some(s) -> Some(EnumRef s)
                                |None -> None
                            |TypedefRef s ->
                                match Map.tryFind s typedef2struct with
                                |Some(_) ->
                                    Some(StructRef s)
                                |None -> match Map.tryFind s typedef2enum with
                                            |Some(_) ->
                                            Some(EnumRef s)
                                            |None -> None
                            |StructRef s ->
                                match Map.tryFind s struct2typedef with
                                |Some(s) -> Some(StructRef s)
                                |None -> None
                            |EnumRef s ->
                                match Map.tryFind s enum2typedef with
                                |Some(s) -> Some(EnumRef s)
                                |None -> None
                            |_ -> None
                        ) |> fun ty -> (ty, loc)
                )
        types := !types |> Map.filter (fun n _ -> Map.containsKey n typedef2struct |> not)
        types := !types |> Map.filter (fun n _ -> Map.containsKey n typedef2enum |> not)
        types := removeTypedefStructs !types

        structs := removeTypedefStructs !structs
        enums := removeTypedefStructs !enums

        // remove local types
        let curmod = headerLocation.FullName |> filename2module
        let typedef =
            !typedefloc
            |> Map.filter
                (fun _ modname ->
                    System.StringComparer.InvariantCultureIgnoreCase.Compare(curmod, modname) <> 0
                )

        (!types, !enums, !structs, !funcs, !iids, !defines, typedef)

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
    // TODO: calling convention for function pointers is always C. Fix it.
    let (types32, enums32, structs32', funcs32, iids32, defines32, typedefloc32) as p32=
        parse headerLocation pchLocation includePaths "i686-pc-win32"
    let (types64, enums64, structs64', funcs64, iids64, defines64, typedefloc64) as p64=
        parse headerLocation pchLocation includePaths "x86_64-pc-win32"
    // ensure that parse results contain same items
    assert (keys types32 = keys types64)
    assert (keys enums32 = keys enums64)
    assert (keys structs32' = keys structs64')
    assert (keys funcs32 = keys funcs64)
    assert (keys iids32 = keys iids64)
    assert (keys defines32 = keys defines64)
    assert (keys typedefloc32 = keys typedefloc64)
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
    let debugSC = structsCombined |> Array.ofSeq
    // all extern funcs for 64-bit build seem to have cdecl cc, so I use funcs32
    (types32, enums64, structsCombined, funcs32, iids64, defines64, typedefloc64)
