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
    funcArgToRust=
      function (name,ty,_) -> 
                if name="type" then
                  "ty"+" : "+(tyToRust ty)
                else if name="" then
                  "_ : "+(tyToRust ty)
                else
                  name+" : "+(tyToRust ty)
      

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

// This function performs generic list comparison, by checking that lists have equal set of keys, 
//  lists have equal number of matching keys in the same order, 
//  corrensponding list elements has (subMatch key e1 e2) = true
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
        false
    if List.length list1 = List.length list2 then
      List.forall2 subCmp list1 list2
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
      false
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
    false

let sanityCheck vtbls annotations= 
  // Recursively check if interfaces', methods', parameters' names are equal in native and annotated 
  isListsMatch fst "Native interfaces" (Map.toList vtbls) (fun (name,_,_) -> name) "Annotated interfaces" annotations 
    (fun iname (_, structElems) (_,interfaceAnnotation,methodAnnotations) -> 
      // check if interface iname matches, by checking all methods
      isListsMatch (fun (CStructElem(mname,_,_)) -> mname) (sprintf "Native methods of %s" iname) structElems 
                    (fun (mname,_,_) -> mname) (sprintf "Annotated methods of %s" iname) methodAnnotations
        (fun mname (CStructElem(_,Ptr(Function(CFuncDesc(parms, _, _))),_)) (_,aparms,methodAnnotation) ->
          // check if method mname matches
          if methodAnnotation=MAIUnknown then
            // don't check parameters of IUnknown methods
            true
          else
            // by checking all parameters
            isListsMatch (fun (pname,_,_) -> pname) (sprintf "Parameters of native method %s::%s" iname mname) parms
                       (fun (pname,_) -> pname) (sprintf "Parameters of annotated method %s::%s" iname mname) aparms 
              (fun pname _ (_,pan) -> 
                // let's check validity of method annotations also
                let parameterExists p = 
                  let ret=List.exists (fun (pname,_,_) -> pname=p) parms
                  if not ret then
                    printfn "Error. Annotation of parameter %s of method %s::%s refers to absent parameter %s" pname iname mname p
                  ret
                match pan with
                |AThis|ANone|InOut|InOutReturn|OutReturn|InIUnknown|InOptional -> true
                |InOutOfSize p -> parameterExists p
                |OutPointerToSizedType (_,p) -> parameterExists p
                |OutReturnCombine (_,_) -> true
                |OutReturnInterface p -> parameterExists p // TODO: check for type of p
                |OutReturnKnownInterface (p,_) -> parameterExists p // TODO: check for type of p
                |InPointerToSizedType (_,p) -> parameterExists p
                |InOptionalArrayOfSize p -> parameterExists p
                |InArrayOfSize p -> parameterExists p
                |InByteArrayOfSize p -> parameterExists p
                |TypeSelector (p,_) -> 
                  parameterExists p // TODO: check constants and types
                )
          )
      )
    

let safeInterfaceGen (types:Map<string,CTypeDesc>,enums:Map<string,CTypeDesc>,structs:Map<string,CTypeDesc>,funcs:Map<string,CTypeDesc>, iids:Set<string>) : string=
  let sb=new System.Text.StringBuilder()
  let vtbls=types |> List.ofSeq |> List.collect (fun (KeyValue(name,ty)) -> getVtbl structs ty |> o2l |> List.map (fun vtbl -> (name,vtbl))) |> Map.ofList
  if sanityCheck vtbls d3d12annotations then
    let apl = appendLine sb
    apl "\
extern crate iid;
extern crate d3d12_sys;
use iid::*;
use d3d12_sys::*;
"
    

    sb.ToString()
  else
    printfn "Sanity check failed. No Rust interfaces generated"
    ""
