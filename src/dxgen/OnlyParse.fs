module OnlyParse

open System.Runtime.InteropServices
open Microsoft.FSharp.NativeInterop
open FSharpx
open libclang

type CPrimitiveType=
  |Void
  |Int8
  |UInt8
  |Int16
  |UInt16
  |Int32
  |UInt32
  |IntPtr
  |UIntPtr
  |Int64
  |UInt64
  |Float32
  |Float64
  |Float80
  |Bool
  |Char8
  |Char16
  |Char32

type CCallingConv=
  |StdCall


let ccToRust (cc:CallingConv)=
  match cc with
  |CallingConv.X86StdCall -> "\"stdcall\""
  |_ -> raise <| new System.Exception(sprintf "Unimplemented calling convention %A in ccToRust" cc)


type CParamAnnotation=
  | NoAnnonation
  | In
  | InOptional
  | Out
  | OutOptional
  | InOut
  | InReads of string
  | InReadsBytes of string
  | OutWrites of string
  | OutWritesBytes of string
  | OutWritesOpt of string * string


type CTypeDesc=
  |Primitive of CPrimitiveType
  |Unimplemented of string
  |Typedef of CTypeDesc // type alias.
  |TypedefRef of string //
  |Enum of underlyingType:CPrimitiveType * values:((string * uint64) list)  // 
  |EnumRef of string
  |Ptr of CTypeDesc
  |Const of CTypeDesc
  |Array of CTypeDesc*int64
  |Struct of CStructElem list
  |StructRef of string
  |Union of CStructElem
  |UnionRef of string
  |UnsizedArray of CTypeDesc
  |Function of CFuncDesc
  |Interface of (string*CFuncDesc) list // 
and CFuncDesc=CFuncDesc of ((string*CTypeDesc*CParamAnnotation) list)*CTypeDesc*CallingConv
and CStructElem=CStructElem of name:string*typeDesc:CTypeDesc*bitWidth:Option<int32>

// True iff ty is a typedef of struct that contains only function pointers
let getVtbl (structs:Map<string, CTypeDesc>) ty=
  let isStructVtbl ses=
    if List.forall (function |CStructElem(_,Ptr(Function(_)),_) -> true |_ -> false) ses then
      Some(ses)
    else
      None

  match ty with
  |Typedef(StructRef(sname)) -> 
    match Map.find sname structs with
    |Struct(ses) -> isStructVtbl ses
    |_ -> None
  |_-> None

let rec isStruct n2ty ty=
  match ty with
  | Struct _ -> true
  | StructRef _ -> true
  | Typedef uty -> isStruct n2ty uty
  | TypedefRef nm -> isStruct n2ty (Map.find nm n2ty)
  | Const uty -> isStruct n2ty uty
  | _ -> false

let enumCType cursor=
  let tySize=getEnumDeclIntegerType cursor |> getSizeOfType
  match tySize with 
  |1L -> UInt8
  |2L -> UInt16
  |4L -> UInt32
  |8L -> UInt64
  | _ -> Void

let rec typeDesc (ty: Type)=
  match ty.kind with
  |TypeKind.Void ->   Primitive Void
  |TypeKind.Bool ->   Primitive Bool
  |TypeKind.Char16 -> Primitive Char16
  |TypeKind.Char32 -> Primitive Char32
  |TypeKind.Char_S -> Primitive Char8
  |TypeKind.Char_U -> Primitive Char16
  |TypeKind.Double -> Primitive Float64
  |TypeKind.Float ->  Primitive Float32
  |TypeKind.Int ->    
    let ts=getTypeSpellingFS ty
    Primitive Int32
  |TypeKind.Long ->   Primitive Int32
  |TypeKind.LongDouble -> Primitive Float64 
  |TypeKind.LongLong -> Primitive Int64
  |TypeKind.SChar -> Primitive Int8
  |TypeKind.Short -> Primitive Int16
  |TypeKind.UChar -> Primitive UInt8
  |TypeKind.UInt -> Primitive UInt32
  |TypeKind.ULong -> Primitive UInt32
  |TypeKind.ULongLong -> Primitive UInt64
  |TypeKind.UShort -> Primitive UInt16
  |TypeKind.ConstantArray ->
    Array(getArrayElementType ty |> typeDesc, getArraySize ty)
  |TypeKind.Enum -> 
    EnumRef (getTypeSpellingFS ty)
  |TypeKind.Record ->
    StructRef (getTypeSpellingFS ty)
  |TypeKind.FunctionProto ->
    Unimplemented(getTypeSpellingFS ty)
  |TypeKind.Complex -> Unimplemented (getTypeSpellingFS ty)
  |TypeKind.Typedef -> 
    //let und1=getTypedefDeclUnderlyingType ty |> getTypeSpellingFS
    //let canon=getCanonicalType ty |> getTypeSpellingFS
    let ts=getTypeSpellingFS ty
    if ts.StartsWith("const ") then
      Const(TypedefRef(ts.Substring(String.length("const "))))
    else
      TypedefRef(ts)
  |TypeKind.Pointer -> Ptr(getPointeeType ty |> typeDesc)
  |TypeKind.Unexposed ->
    match getTypeSpellingFS ty with
    | tname when tname.StartsWith("struct ") -> StructRef(tname.Substring("struct ".Length))
    | tname when tname.StartsWith("enum ") -> EnumRef(tname.Substring("enum ".Length))
    | tname -> Unimplemented(tname)
  |_ -> Unimplemented(getTypeSpellingFS ty)
  

let rec tyToRust (ty:CTypeDesc)=
  match ty with
  |Primitive t ->
    match t with
    |Void -> "c_void"
    |UInt16 -> "u16"
    |UInt8 -> "u8"
    |UInt32 -> "u32"
    |UInt64 -> "u64"
    |UIntPtr -> "usize" 
    |Int16 -> "i16"
    |Int32 -> "i32"
    |Int64 -> "i64"
    |Int8 -> "i8"
    |IntPtr -> "isize"
    |Float32 -> "f32"
    |Float64 -> "f64"
    |Float80 -> "f80"
    |Bool -> "bool"
    |Char8 -> "u8"
    |Char16 -> "u16"
    |Char32 -> "u32"
  |Unimplemented v-> "Unimplemented("+v+")"
  |Typedef uty -> tyToRust uty
  |TypedefRef tyn-> tyn
  |StructRef tyn -> tyn
  |EnumRef tyn -> tyn
  |Array(uty,size) -> "["+(tyToRust uty)+";"+size.ToString()+"]"
  |Ptr(Const(uty)) -> "*const "+(tyToRust uty)
  |Ptr(Function(CFuncDesc(args,rty,cc))) ->
    "extern "+(ccToRust cc)+" fn ("+((List.map funcArgToRust args) |> String.concat(", "))+") -> "+(tyToRust rty)
  |Ptr uty -> "*mut " + (tyToRust uty)
  |Const(uty) -> tyToRust uty
  |_ -> "NoRepresentationYet("+(sprintf "%A" ty)+")"
and
  funcArgToRust(name,ty,_) =
              if name="type" then
                "ty"+" : "+(tyToRust ty)
              else if name="" then
                "_ : "+(tyToRust ty)
              else
                name+" : "+(tyToRust ty)

let parse (headerLocation: System.IO.FileInfo) (pchLocation: System.IO.FileInfo option) =
  // Let's use clean C interface. Those fancy C++ classes with inheritance and whistles aren't good for rust codegen.
  let options = [| "-x"; "c"; "-std=c11"; "-fms-extensions"; "-fms-compatiblity"; "-fmsc-version=1800" |]
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
        args := (getCursorDisplayNameFS cursor, getCursorType cursor |> typeDesc, NoAnnonation) :: !args
      ChildVisitResult.Continue
    visitChildrenFS cursor argsVisitor () |> ignore
    if (List.length !args <> nArgs) then
      raise <| System.Exception("Number of parmDecls doesn't match number of arguments. " :: tokenizeFS cursor |> String.concat " ")
    let retyname=getTypeSpellingFS(getCanonicalType(getResultType(fType)))
    let retysize=getSizeOfType(getResultType(fType))
    if retysize>0L && retysize<=8L && (retyname.StartsWith("struct ") || (retyname.Contains(" struct ") && retyname.Contains("*")=false)) then
      // C returns those structs thru EAX:EDX, C++ thru reference
      args := ("__ret_val",Ptr(rety), Out) :: !args
      Function(CFuncDesc(List.rev !args,Primitive Void,cc))
    else
      Function(CFuncDesc(List.rev !args,rety,cc))


  let parseStruct (cursor:Cursor)=
    let tokens=tokenizeFS cursor
    printfn "%A %s %s" cursor.kind (cursor |> getCursorSpellingFS) (String.concat " " tokens)
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

  let rec childVisitor cursor _ _ =
    let cursorKind=getCursorKind cursor

    if cursorKind=CursorKind.EnumDecl then
      enums := !enums |> Map.add (cursor |> getCursorDisplayNameFS) (Enum(enumCType cursor,parseEnumConsts cursor))

    if cursorKind=CursorKind.TypedefDecl then
      let tokens=tokenizeFS cursor
      printfn "%A %s %s" cursor.kind (cursor |> getCursorSpellingFS) (String.concat " " tokens)
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
      structs := !structs |> Map.add (cursor |> getCursorDisplayNameFS) (parseStruct cursor)

    if cursorKind=CursorKind.FunctionDecl then
      funcs := !funcs |> Map.add (cursor |> getCursorSpellingFS) (parseFunction cursor (getCursorType cursor))

    if cursorKind=CursorKind.VarDecl then
      let nm=cursor |> getCursorSpellingFS
      let tys=cursor |> getCursorType |> getTypeSpellingFS
      if tys="const IID" then
        iids := !iids |> Set.add nm
      printfn "%s %s" nm tys

    if cursorKind=CursorKind.UnexposedDecl then // dip into extern "C"
      visitChildrenFS cursor childVisitor () |> ignore
    
    ChildVisitResult.Continue

  let cursor = getTranslationUnitCursor(translationUnit)

  try
    visitChildrenFS cursor childVisitor () |> ignore
    for en in !types do
      match en.Value with
      | Typedef(under) -> printfn "Typedef %s is %A" en.Key under
      | _ -> printfn "Some other type %A" en.Value
    ()
    for en in !enums do
      match en.Value with
      | Enum(underType, consts)  -> 
          printfn "Enum %s of %A" en.Key underType
          for (nm,vl) in consts do
            printfn "  %s = %d (0x%X)," nm vl vl
      | _ -> printfn "Shouldn't be here: %A" en.Value

    for en in !structs do
      match en.Value with
      | Struct(elems)  -> 
          printfn "Struct %s of" en.Key 
          for CStructElem(name,ty,bw) in elems do
            printfn "  %s : %A (%A)," name ty bw
      | _ -> printfn "Shouldn't be here: %A" en.Value

    for en in !funcs do
      printfn "%s %A" en.Key en.Value

    (!types, !enums, !structs, !funcs, !iids)

  finally
    translationUnit |> disposeTranslationUnit
    index |> disposeIndex
        
    Option.maybe {
        let! pch = pchTempLocation
        pch.Delete()
    } |> ignore

let codeGen (types:Map<string,CTypeDesc>,enums:Map<string,CTypeDesc>,structs:Map<string,CTypeDesc>,funcs:Map<string,CTypeDesc>, iids:Set<string>) : string=
  let createEnums (sb:System.Text.StringBuilder)=
    sb.AppendLine("use std::ops::BitOr;").AppendLine() |> ignore
    for KeyValue(name,Enum(uty, vals)) in enums do
      sb.AppendLine(@"#[repr(C)]") |> ignore
      sb.AppendLine(@"#[derive(Clone,Copy,PartialEq,Eq,Debug,Default)]") |> ignore
      sb.AppendFormat(@"pub struct {0}(u32);",name) |> ignore
      sb.AppendLine() |> ignore
      if name.Contains("FLAG") then
        sb.AppendFormat(@"
impl BitOr for {0} {{
  type Output={0};

  fn bitor(self, rhs: {0}) -> Self::Output {{
    match (self,rhs) {{
      ({0}(a),{0}(b)) => {0}(a|b)
    }}
  }}
}}
      ", name) |> ignore
      sb.AppendLine() |> ignore
      for (cn,v) in vals do
        sb.AppendFormat("const {0}: {1} = {1}({2});", cn, name, v) |> ignore
        sb.AppendLine() |> ignore
      sb.AppendLine() |> ignore
      sb.AppendLine() |> ignore


  let libcTypeNames=Set.ofList ["BOOL";"LPCWSTR";"HMODULE";"GUID";"LARGE_INTEGER";"LPVOID";"WCHAR";"BYTE";"LPCVOID";"LONG_PTR";"WORD";"SIZE_T";"SECURITY_ATTRIBUTES";"HANDLE";"DWORD";"LPCSTR";"LONG"]
  let createStructs (sb:System.Text.StringBuilder)=
    for KeyValue(name, Struct(sfields)) in structs do
      if Set.contains name libcTypeNames then
        ()
      else
        sb.AppendLine("#[repr(C)]") |> ignore
        if sfields.IsEmpty then
          sb.AppendFormat("struct {0};",name).AppendLine() |> ignore;
        else
          sb.AppendFormat("struct {0} {{",name).AppendLine() |> ignore;
          for CStructElem(fname,fty,None) in sfields do
            sb.AppendFormat("  {0} : {1},", fname, tyToRust fty).AppendLine() |> ignore
          sb.AppendLine("}").AppendLine() |> ignore
  
  let createTypes (sb:System.Text.StringBuilder)=
    for KeyValue(name,ty) in types do
      // Some types are defined in rust's libc module
      if Set.contains name libcTypeNames then
        ()
      else
        match ty with
        |Typedef(EnumRef(ename)) when ename=name -> ()
        |Typedef(StructRef(sname)) when sname=name -> ()
        |_ -> sb.AppendFormat("type {0} = {1};", name, tyToRust ty).AppendLine() |> ignore
    sb.AppendLine() |> ignore

  let createFunctions (sb:System.Text.StringBuilder)=
     // TODO: use 
    for KeyValue(name, Function(CFuncDesc(args,rty,cc))) in funcs do
      sb.AppendLine("#[link(name=\"d3d12\")]") |> ignore       
      sb.AppendFormat("extern {3} {{ fn {0}({1}) -> {2}; }}",name,((List.map funcArgToRust args) |> String.concat(", ")),(tyToRust rty), (ccToRust cc)).AppendLine() |> ignore
    sb.AppendLine("").AppendLine() |> ignore

  let createIIDs (sb:System.Text.StringBuilder)=
    sb.AppendLine("#[link(name=\"dxguid\")]").AppendLine("extern {") |> ignore
    for iid in iids do
      sb.AppendFormat("  static {0}: IID;",iid).AppendLine() |> ignore
    sb.AppendLine("}").AppendLine() |> ignore

  let sb=new System.Text.StringBuilder()
  sb.AppendLine("// This file is autogenerated.").AppendLine() |> ignore
  sb.AppendLine("#![feature(libc)]") |> ignore
  sb.AppendLine("extern crate libc;").AppendLine("use libc::*;").AppendLine() |> ignore
  createEnums sb
  createTypes sb
  createStructs sb
  createIIDs sb
  createFunctions sb
  sb.ToString()

let appendLine (sb:System.Text.StringBuilder) line=
  sb.AppendLine(line) |> ignore

let iUnknownFuncs=["QueryInterface";"AddRef";"Release"] |> Set.ofList

let o2l xs= 
  match xs with
  |Some(x) -> [x]
  |None -> []

// returns string of f# code that contains empty annotations for all COM interfaces contained in types parameter
let emptyAnnotationsGen (types:Map<string,CTypeDesc>,enums:Map<string,CTypeDesc>,structs:Map<string,CTypeDesc>,funcs:Map<string,CTypeDesc>, iids:Set<string>) : string=
  let sb=new System.Text.StringBuilder()
  let apl=appendLine sb
  apl("module annotations_autogen")
  apl("open annotations")
  apl("")
  apl("let d3d12annotations=[")
  let vtbls=types |> List.ofSeq |> List.collect (fun (KeyValue(name,ty)) -> getVtbl structs ty |> o2l |> List.map (fun vtbl -> (name,vtbl)))
  for (name, mths) in vtbls do
    apl(sprintf "  (\"%s\",IAAutogen,[" name)
    for CStructElem(mname, Ptr(Function(CFuncDesc(args,ty,_))), _) in mths do
      if Set.contains mname iUnknownFuncs then
        apl(sprintf "    (\"%s\",[],MAIUnknown);" mname)
      else
        apl(sprintf "    (\"%s\",[" mname)
        for (aname, ty, sPAn) in args do
          let pa=
            if aname="This" then
              "AThis"
            else
              "ANone" 
          apl(sprintf "      (\"%s\",%s);" aname pa)
        apl("    ],MANone);")
    apl(sprintf "  ]);")
  apl("  ]")
  sb.ToString()

open annotations
open annotations_autogen


// returns Some(vs) iff for all (x1,x2) in xs1,xs2 (f x1 x2 = Some(v))
// map2L : ('x1 -> 'x2 -> Option<'y>) -> ['x1] -> ['x2] -> Option<['y]>
let map2L f xs1 xs2=
  let ys=List.map2 f xs1 xs2 |> List.collect o2l 
  if List.length ys = List.length xs1 then
    Some(ys)
  else
    None
  

// This function performs generic list comparison, by checking that lists have equal set of keys, 
//  lists have equal number of matching keys in the same order, 
//  corrensponding list elements has (subMatch key e1 e2) = Some _
// Return Some([_]) or None
let isListsMatch key1 name1 list1 key2 name2 list2 subMatch=
  let keys1=List.map key1 list1 |> Set.ofList
  let keys2=List.map key2 list2 |> Set.ofList
  if keys1=keys2 then
    // set of keys are equal
    let subCmp e1 e2=
      let (k1,k2) = (key1 e1, key2 e2)
      if k1 = k2 then
        subMatch k1 e1 e2
      else
        printfn "%s has %s, while %s has %s"  name1 k1 name2 k2
        None
    if List.length list1 = List.length list2 then
      map2L subCmp list1 list2
    else
      // Set of keys are equal, but lists have different size. What it means? Key duplication
      let dup ls kf=ls |> Seq.countBy kf |> Seq.filter (fun (_,b) -> b>1) |> Seq.map fst
      let printdup dup name=
        if not <| Seq.isEmpty dup then 
          printfn "Duplicate keys in %s" name
          for key in dup do
            printfn "  %s" key
      let dup1=dup list1 key1
      printdup dup1 name1
      let dup2=dup list2 key2
      printdup dup2 name2
      None
  else
    // set of keys aren't equal. show the difference
    let diff1=Set.difference keys1 keys2
    let diff2=Set.difference keys2 keys1
    if not (Set.isEmpty diff1) then
      printfn "%s lack those keys:" name2
      for key in diff1 do
        printfn "  %s" key
    if not (Set.isEmpty diff2) then
      printfn "%s lack those keys:" name1
      for key in diff2 do
        printfn "  %s" key
    None

// merges annotation and description of native parameter
// returns Some(parameterName, parameterAnnotation, parameterNativeType) or None
// iname - interface name
// mname - method name
// parms - list of native parameters' descriptions
// (parameterName, parameterType,parameterNativeAnnotation) - native parameter description
// (parameterName, parameterAnnotation) - annotated parameter
let mergeParameter iname mname parms pname (_,pty,_) (_,pan)=
  let parameterExists p = 
    let ret=List.exists (fun (pname,_,_) -> pname=p) parms
    if not ret then
      printfn "Error. Annotation of parameter %s of method %s::%s refers to absent parameter %s" pname iname mname p
    ret
  let parameterMatch= 
    match pan with
    |AThis|ANone|InOut|InOutReturn|OutReturn|InIUnknown|InOptional -> true
    |InOutOfSize p -> parameterExists p
    |OutOfSize p -> parameterExists p
    |OutReturnCombine (_,_) -> true
    |OutReturnInterface p -> parameterExists p // TODO: check for type of p
    |OutReturnKnownInterface (p,_) -> parameterExists p // TODO: check for type of p
    |InOfSize p -> parameterExists p
    |InOptionalArrayOfSize p -> parameterExists p
    |InArrayOfSize p -> parameterExists p
    |InByteArrayOfSize p -> parameterExists p
    |TypeSelector (p,_) -> 
      parameterExists p // TODO: check constants and types
  if parameterMatch then
    Some(pname, pan, pty)
  else
    None

// Recursively check if interfaces', methods', parameters' names are equal in native and annotated 
// Returns Some(annotaded native) or None
let sanityCheck vtbls annotations= 
  isListsMatch fst "Native interfaces" vtbls (fun (name,_,_) -> name) "Annotated interfaces" annotations 
    (fun inamevtbl (_, structElems) (_,interfaceAnnotation,methodAnnotations) -> 
      // inamevtbl is in the form I***Vtbl, let's strip 'I' and 'Vtbl'
      let iname=inamevtbl.Substring(1,inamevtbl.Length-5)
      if interfaceAnnotation=IAManual then
        // Don't bother matching interfaces marked for manual implementation
        Some(iname, interfaceAnnotation, [])
      else
        // check if interface iname matches, by checking all methods
        let mergedMethods=
          isListsMatch (fun (CStructElem(mname,_,_)) -> mname) (sprintf "Native methods of %s" iname) structElems 
                        (fun (mname,_,_) -> mname) (sprintf "Annotated methods of %s" iname) methodAnnotations
            (fun mname (CStructElem(_,Ptr(Function(CFuncDesc(parms, rty, cc))),_)) (_,aparms,methodAnnotation) ->
              // check if method mname matches
              if methodAnnotation=MAIUnknown then
                // don't check parameters of IUnknown methods
                Some(mname, methodAnnotation, [], rty)
              else
                // by checking all parameters
                let mergedParms=
                  isListsMatch (fun (pname,_,_) -> pname) (sprintf "Parameters of native method %s::%s" iname mname) parms
                                (fun (pname,_) -> pname) (sprintf "Parameters of annotated method %s::%s" iname mname) aparms 
                                  (mergeParameter iname mname parms)
                match mergedParms with
                |Some(mp) -> Some(mname, methodAnnotation, mp, rty)
                |None -> None
              )
        match mergedMethods with
        |Some(mm) -> Some(iname, interfaceAnnotation, mm)
        |None -> None
    )
    
// rust code generation
// AThis : self.0
// ANone : depends on other annotations
// InOut : mutable borrow, C-type should be a pointer. e.g.
//    GetPrivateData(AThis ID3D* this, ANone REFGUID guid, InOut UINT* pDataSize) -> HRESULT =>  
//    fn get_private_data(&self, guid : REFGUID, data_size : &mut u32) -> HResult<()>

// InOutReturn : for Copy types only, in by value, out by function result e.g.
//    GetPrivateData(AThis ID3D* this, ANone REFGUID guid, InOutReturn UINT* pDataSize) -> HRESULT =>
//    fn get_private_data(&self, guid: REFGUID, u32 data_size) -> HResult<u32> {
//      let mut p1=data_size;
//      let hr=(...GetPrivateData)(..., guid, &mut p1);
//      hr2ret(hr,p1)
//    }

// OutReturn : same as InOutReturn, but without "in" part
//    GetPrivateData(AThis ID3D* this, ANone REFGUID guid, OutReturn UINT* pDataSize) -> HRESULT =>
//    fn get_private_data(&self, guid: REFGUID) -> HResult<u32> {
//      let mut p1=Default::<u32>::default();
//      let hr=(...GetPrivateData)(..., guid, &mut p1);
//      hr2ret(hr,p1)
//    }

// InIUnknown : makes function generic, transforms struct that has HasIID trait into *mut *mut IUnknownVtbl
//    SetPrivateInterface(AThis ID3D* this, ANone REFGUID guid, InIUnknown IUnknown* pInterface) =>
//    fn set_private_interface<T:HasIID>(&self, REFGUID guid, interface : T) -> () {
//      (...SetPrivateData)(..., guid, interface.expose_iptr());
//    }

// InOptional : for C-type T, *T => Option<&T>

// InOutOfSize s : C-type should be void*, makes a function generic on T, *mut c_void => &mut T, routes mem::size_of::<T>() to parameter s

// OutOfSize s : same as InOutOfSize s, Rust forbids uninitialized values anyway.

// InOfSize s : same as InOutOfSize s, but borrow is immutable

// OutReturnCombine(StructName, fieldName) : return value will be StructName or HResult(StructName), e.g.
//  GetClockCalibration(This : *mut ID3D12CommandQueue, OutReturnCombine(GPUCPUTimestamp,gpu) pGpuTimestamp : *mut UINT64, OutReturnCombine(GPUCPUTimestamp,cpu) pCpuTimestamp : *mut UINT64) -> HRESULT =>
//  struct GPUCPUTimestamp { gpu : u64, cpu: u64, }
//  fn get_clock_calibration(&self) -> HResult(GPUCPUTimestamp) {
//    let ret=GPUCPUTimestamp {gpu:0,cpu:0};
//    let hr=(...GetClockCalibration)(..., &mut ret.gpu as *mut UINT64, &mut ret.cpu as *mut UINT64);
//    hr2ret(hr, ret)
//  }
// Maybe I should just drop it. 

// OutReturnInterface riid : makes a function generic on T:HasIID, return type T or HResult<T>, routes HasIID::iid() to parameter riid

// OutReturnKnownInterface(riid, InterfaceName) : InterfaceName should implement HasIID, return type InterfaceName or HResult<InterfaceName>, routes InterfaceName::iid() to parameter riid

// InOptionalArrayOfSize s : *T => Option<&[T]>, routes param.map_or(0,|v|v.len()) to parameter s

// InArrayOfSize s : *T => &[T], routes param.len() to parameter s

// InByteArrayOfSize s : makes function generic over T, *c_void => &T, routes mem::size_of::<T>() to parameter s

// TypeSelector(buf, [(suffix, const, type)]) : annotated parameter select type that native method accepts in void* buf, buf parameter should be annotated InOutOfSize s
// suffix : rust function suffix for type
// const : const value for type
// type : accepted type
// 
// For each tuple (suffix, const, type) in list create method "native_method_<suffix>", route const to annotated parameter, route type to buf type
//

// ------------------
// Using annotations, we should determine source of each native parameter, source of Rust's function return value, presence of generic parameters

type RustType=
  |RType of string
  |RBorrow of RustType
  |RMutBorrow of RustType
  |RGeneric of string
  |RHResult of RustType

let rec rustTypeToString rt=
  match rt with
  |RType s -> s
  |RBorrow t -> "&" + (rustTypeToString t)
  |RMutBorrow t-> "&mut " + (rustTypeToString t)
  |RGeneric s -> s
  |RHResult t -> "HResult<" + (rustTypeToString t) + ">"

type NParmSource=
  |RustSelf // *(self.0) as *mut _ as *mut InterfaceVtbl
  |RustParameter of string*RustType // rustparameter as native type
  |RustParameterSizeOf of string // mem::size_of::<rust parameter type>()
  |RustSliceLenghtOf of string // rustparameter.len()
  |RustIIDOf of string // rustparameter.iid()
  |RustLocalVar of string*RustType // local variable
  |RustStructField of string*string*RustType // (local var name, field name) &mut (tempstruct.field)
  |RustConst of string // some constant


type ReturnValueSource=
  |RVNative // use native return type (void -> (), HRESULT -> HResult<()>, type -> rust_type)
  |RVLocalVar of string*RustType // local variable (default initialized). Uninitialized is better, but there's no stable support for this yet.



// parms is list of tuples (parameterName, parameterAnnotation, nativeParameterType, parameterSource)
let generateMethodFromEquippedAnnotation (mname, mannot, parms, rty, rvsource)=
  // let's find return type and return expression
  let (rrt,rexpr)=
    match rvsource with
      |RVNative ->
        match rty with
        |Primitive Void -> (RType "()", "hr")
        |TypedefRef name when name="HRESULT" -> (RHResult(RType "()"), "hr")
        | _ -> (RType (tyToRust rty), "hr")
      |RVLocalVar(name,rtype) -> 
        match rty with
        |Primitive Void -> (rtype, name)
        |TypedefRef name when name="HRESULT" -> (RHResult rtype, "hr2ret(hr,"+name+")")
        |_ -> // error. native return type is incompatible with annotated.
          let err=sprintf "Native return type is %s, but annotated is %A" (tyToRust rty) rtype
          printfn "%s" err
          raise <| new System.Exception(err)
          (rtype, name)
  let rtype=rustTypeToString rrt
  // let's find generic types
  let genericTypes=
    parms |> List.collect 
      (fun (_,_,_,psource) -> 
        match psource with 
        |RustParameter(_,RGeneric g) -> [g]
        |RustLocalVar(_,RGeneric g) -> [g]
        |RustSelf |RustParameter _ | RustLocalVar _ |RustParameterSizeOf _ | RustSliceLenghtOf _ | RustIIDOf _ | RustStructField _ | RustConst _ -> []
        ) |> Set.ofList 
  let generics=
    if Set.isEmpty genericTypes then
      ""
    else
      "<"+System.String.Join(",",genericTypes)+">"
  // let's find local variables and their types
  let localVars=
    parms |> List.collect
      (fun (_,_,_,psource) ->
        match psource with
        |RustLocalVar (name, rt) -> [(name,rt)]
        |RustStructField (name, _, rt) -> [(name,rt)]
        |RustSelf | RustParameter _ |RustParameterSizeOf _ | RustSliceLenghtOf _ | RustIIDOf _ | RustConst _ -> []
        ) |> Set.ofList
  let locals=""
  // let's find rust method parameters and their types
  let rustParms=
    parms |> List.collect
      (fun (_,_,_,psource) ->
        match psource with
        |RustParameter (name,rtype) -> [(name,rtype)]
        |RustSelf |RustParameterSizeOf _ | RustSliceLenghtOf _ | RustIIDOf _ | RustConst _ |RustLocalVar _ |RustStructField _ -> []
      ) |> List.map (fun (name, rtype) -> name+" : "+(rustTypeToString rtype))
  let parameters=System.String.Join(", ", rustParms)
  let unsafe=if mannot=MAUnsafe then "unsafe " else ""
  let nativeInvocation=""
  // we are ready to generate method
  "
"+unsafe+"fn "+mname+generics+"("+parameters+") -> "+rtype+" {
"+locals+"
  let hr=unsafe {
    "+nativeInvocation+"
  };
  "+rexpr+"
}
"

let indentBy indentationString (source:System.String)=
  indentationString+source.Replace("\n","\n"+indentationString)

open CaseConverter
let generateMethod (mname, mannot, parms, rty)=
  if mannot=MAIUnknown || mannot=MADontImplement then
    ""
  else
    let eparms=
      parms |> List.map 
        (fun (pname, pannot, ptype) -> 
          let psource=
            match pannot with
              |AThis -> RustSelf
              |_ -> RustParameter (toSnake pname, RType (tyToRust ptype))
          (pname, pannot, ptype, psource))
    let trivialEquip=(toSnake mname, mannot, eparms, rty, RVNative)
    generateMethodFromEquippedAnnotation trivialEquip |> indentBy "  "

let generateMethods methods=
  let sb=new System.Text.StringBuilder()
  for methoddesc in methods do
    sb.Append(generateMethod methoddesc) |> ignore
  sb.ToString()

let safeInterfaceGen (types:Map<string,CTypeDesc>,enums:Map<string,CTypeDesc>,structs:Map<string,CTypeDesc>,funcs:Map<string,CTypeDesc>, iids:Set<string>) : string=
  let sb=new System.Text.StringBuilder()
  let vtbls=types |> List.ofSeq |> List.collect (fun (KeyValue(name,ty)) -> getVtbl structs ty |> o2l |> List.map (fun vtbl -> (name,vtbl))) 
  match sanityCheck vtbls d3d12annotations with
  |Some(interfaceAnnotations) ->
    let apl = appendLine sb
    apl "\
extern crate iid;
extern crate d3d12_sys;
use iid::*;
use d3d12_sys::*;
"
    for (iname, iannot, methods) in interfaceAnnotations do
      if iannot=IAManual then
        ()
      else
        apl <| sprintf "\
pub struct %s(*mut *mut IUnknownVtbl);

pub impl HasIID for %s {
  fn iid() -> &'static IID { IID_I%s };
  fn new(ppVtbl : *mut *mut IUnknownVtbl) -> Self { %s(ppVtbl) };
  fn expose_iptr(&self) -> *mut *mut IUnknownVtbl { self.0 };
}

pub impl %s {
%s
}
"          iname iname iname iname iname (generateMethods methods)

    sb.ToString()
  |None -> 
    printfn "Sanity check failed. No Rust interfaces generated"
    ""
