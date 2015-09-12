module cdesc

open FSharp.Reflection
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
  |CallingConv.X86StdCall -> "\"system\""
  |_ -> raise <| new System.Exception(sprintf "Unimplemented calling convention %A in ccToRust" cc)

type CodeLocation=System.String*uint32*uint32*uint32

type CParamAnnotation=
  | NoAnnotation
  | In
  | InZ
  | InOpt
  | Out
  | OutOpt
  | InOut
  | InOutOpt
  | InReads of string
  | InReadsOpt of string
  | InReadsBytes of string
  | InReadsBytesOpt of string
  | InOutUpdatesBytes of string
  | OutWrites of string
  | OutWritesOpt of string
  | OutWritesBytes of string
  | OutWritesBytesOpt of string
  | OutWritesTo of string*string
  | OutWritesToOpt of string*string
  | COMOutptr
  | COMOutptrOpt
  | InRange of string*string
  | FieldSize of string
  | OutptrOptResultBytebuffer
  | OutptrResultBytebuffer
  | OutptrOptResultMayBeNull
  | COMOutptrOptResultMayBeNull

let isParamOptional pa=
  match pa with
  |InOpt |OutOpt |InOutOpt |InReadsOpt _ | InReadsBytesOpt _ |OutWritesOpt _ 
  |OutWritesBytesOpt _ |OutWritesToOpt _ |COMOutptrOpt |OutptrOptResultBytebuffer 
  |COMOutptrOptResultMayBeNull |OutptrOptResultMayBeNull -> true

  |NoAnnotation |In |InZ |Out |InOut |InReads _ |InReadsBytes _ |InOutUpdatesBytes _ 
  |OutWrites _ |OutWritesBytes _ | OutWritesTo _ |COMOutptr |InRange _ 
  |FieldSize _ |OutptrResultBytebuffer  -> false

let removeOpt pa=
  match pa with
  |InOpt -> In
  |OutOpt -> Out
  |InOutOpt -> InOut
  |InReadsOpt p -> InReads p
  |InReadsBytesOpt p -> InReadsBytes p
  |OutWritesOpt p -> OutWrites p
  |OutWritesBytesOpt p -> OutWritesBytes p
  |OutWritesToOpt(p,c) -> OutWritesTo(p,c)
  |OutptrOptResultBytebuffer -> OutptrResultBytebuffer
  |COMOutptrOpt -> COMOutptr
  |COMOutptrOptResultMayBeNull | OutptrOptResultMayBeNull -> 
    raise <| new System.Exception("Cannot removeOpt on COMOutptrOptResultMayBeNull")

  |NoAnnotation |In |InZ |Out |InOut |InReads _ |InReadsBytes _ 
  |InOutUpdatesBytes _ |OutWrites _ |OutWritesBytes _ | OutWritesTo _ 
  |COMOutptr |InRange _ |FieldSize _ |OutWritesBytes _ |OutptrResultBytebuffer  -> pa

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
  |Union of CStructElem list
  |UnionRef of string
  |UnsizedArray of CTypeDesc
  |Function of CFuncDesc
  |Interface of (string*CFuncDesc) list // 
and CFuncDesc=CFuncDesc of ((string*CTypeDesc*CParamAnnotation) list)*CTypeDesc*CallingConv
and CStructElem=CStructElem of name:string*typeDesc:CTypeDesc*bitWidth:Option<int32>

let rec recursiveTransform f ty=
  let transformStructElems ses=
    ses |> List.map (fun (CStructElem(name, sty, bw)) -> CStructElem(name, recursiveTransform f sty, bw))
  let transformFuncDesc (CFuncDesc(plist,rety,cc))=
    CFuncDesc(plist |> List.map (fun (n,t,a) -> (n,recursiveTransform f t,a)) ,recursiveTransform f rety,cc)
  match f ty with
  |None ->
    match ty with
    |Primitive _ -> ty
    |Unimplemented _ -> ty
    |Typedef sty -> Typedef(recursiveTransform f sty)
    |TypedefRef _ -> ty
    |Enum _ -> ty
    |EnumRef _ -> ty
    |Ptr sty -> Ptr(recursiveTransform f sty)
    |Const sty -> Const(recursiveTransform f sty)
    |Array(sty,num) -> Array(recursiveTransform f sty, num)
    |Struct ses -> Struct(transformStructElems ses)
    |StructRef _ -> ty
    |Union ses -> Union(transformStructElems ses)
    |UnionRef _ -> ty
    |UnsizedArray sty -> UnsizedArray(recursiveTransform f sty)
    |Function(fd) -> Function(transformFuncDesc fd)
    |Interface meths -> Interface(meths |> List.map (fun (n,fd) -> (n,transformFuncDesc fd)))
  |Some v -> v

let rec removeConst ty=
  match ty with
  |Const(sty) -> removeConst sty
  |Ptr(sty) -> Ptr(removeConst sty)
  |_ -> ty

let isVoidPtr ty=
  let rec isVoidPtrRec ty=
    match ty with
    |Primitive Void -> true
    |Ptr(ty) -> isVoidPtrRec ty
    |_ -> false
  isVoidPtrRec (removeConst ty)

// True iff ty is a typedef of struct that contains only function pointers
let getVtbl (structs:Map<string, CTypeDesc*CodeLocation>) ty=
  let isStructVtbl ses=
    if List.forall (function |CStructElem(_,Ptr(Function(_)),_) -> true |_ -> false) ses then
      Some(ses)
    else
      None

  match ty with
  |Typedef(StructRef(sname)) |StructRef(sname) -> 
    match Map.find sname structs with
    |Struct(ses), _ -> isStructVtbl ses
    |_ -> None
  |Struct(ses) ->
    isStructVtbl ses
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
  |TypeKind.Void ->   
    if isConstQualifiedTypeFS ty then
      Const(Primitive Void)
    else
      Primitive Void
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
  |TypedefRef tyn-> 
    match tyn with
    |_ -> tyn
  |StructRef tyn -> tyn
  |EnumRef tyn -> tyn
  |Array(uty,size) -> "["+(tyToRust uty)+";"+size.ToString()+"]"
  |Ptr(Const(uty)) -> "*const "+(tyToRust uty)
  |Ptr(Function(CFuncDesc(args,rty,cc))) ->
    "extern "+(ccToRust cc)+" fn ("+((List.map funcArgToRust args) |> String.concat(", "))+") -> "+(if rty=Primitive Void then "()" else tyToRust rty)
  |Ptr uty -> "*mut " + (tyToRust uty)
  |Const(uty) -> tyToRust uty
  |_ -> "NoRepresentationYet("+(sprintf "%A" ty)+")"
and
  funcArgToRust(name,ty,_) =
    match name with
    |"type" -> "ty"+": "+(tyToRust ty)
    |"" -> "_ : "+(tyToRust ty)
    |_ -> 
      match ty with
      |Array(_,_) -> name+": &"+(tyToRust ty)
      |_ -> name+": "+(tyToRust ty)

let rec tyToRustGlobal (ty:CTypeDesc)=
  match ty with
  |Primitive t ->
    match t with
    |Void -> "::c_void"
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
    |Float32 -> "::c_float"
    |Float64 -> "::c_double"
    |Float80 -> "f80"
    |Bool -> "bool"
    |Char8 -> "u8"
    |Char16 -> "u16"
    |Char32 -> "u32"
  |Unimplemented v-> "Unimplemented("+v+")"
  |Typedef uty -> tyToRustGlobal uty
  |TypedefRef tyn-> 
    match tyn with
    |_ -> "::"+tyn
  |StructRef tyn -> "::"+tyn
  |EnumRef tyn -> "::"+tyn
  |Array(uty,size) -> "["+(tyToRustGlobal uty)+"; "+size.ToString()+"]"
  |Ptr(Const(uty)) -> "*const "+(tyToRustGlobal uty)
  |Ptr(Function(CFuncDesc(args,rty,cc))) ->
    "extern "+(ccToRust cc)+" fn ("+((List.map funcArgToRustGlobal args) |> String.concat(", "))+") -> "+(if rty=Primitive Void then "()" else tyToRustGlobal rty)
  |Ptr uty -> "*mut " + (tyToRustGlobal uty)
  |Const(uty) -> tyToRustGlobal uty
  |_ -> "NoRepresentationYet("+(sprintf "%A" ty)+")"
and
  funcArgToRustGlobal(name,ty,_) =
    match name with
    |"type" -> "ty"+": "+(tyToRustGlobal ty)
    |"" -> "_ : "+(tyToRustGlobal ty)
    |_ -> 
      match ty with
      |Array(_,_) -> name+": &"+(tyToRustGlobal ty)
      |_ -> name+": "+(tyToRustGlobal ty)

type MacroConst=
  |MCInt32 of int32
  |MCUInt32 of uint32
  |MCInt64 of int64
  |MCUInt64 of uint64
  |MCFloat of float
  |MCDouble of double

