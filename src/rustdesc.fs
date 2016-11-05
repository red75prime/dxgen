module rustdesc

type RustCC=
  |RCCNative
  |RCCExtern of string

type RustRepr=
  |RRNative
  |RRepr of string

type RustAccess=
  |RAPublic
  |RAPrivate

type RName=string
type RVal=string
type RBaseTraits=string

open annotations

type RustType=
  |RType of RName
  |RBorrow of RustType
  |RMutBorrow of RustType
  |RConstPtr of RustType
  |RMutPtr of RustType
  |RGeneric of RName*RBaseTraits
  |RHResult of RustType
  |RResult of RustType*RustType
  |ROption of RustType
  |RArray of RustType*int64
  |RVec of RustType
  |RSlice of RustType
  |RTupleStruct of RustType list
  |RStruct of RustStruct
  |RFn of RustFn
and RustFn={
  cc: RustCC;
  args: (RName*RustType*ParamAnnotation) list
  rettype: RustType
  annot : MethodAnnotation
}
and RustStruct=(RName*RustType*RustAccess) list

let rec rustTypeToString rt :string=
  match rt with
  |RType s -> s
  |RBorrow t -> "&" + (rustTypeToString t)
  |RMutBorrow t-> "&mut " + (rustTypeToString t)
  |RConstPtr(t) -> "*const " + (rustTypeToString t)
  |RMutPtr(t) -> "*mut " + (rustTypeToString t)
  |RGeneric (s,sub) -> s
  |RHResult t -> "HResult<" + (rustTypeToString t) + ">"
  |RResult(t,e) -> "Result<" + (rustTypeToString t) + "," + (rustTypeToString e) + ">"
  |ROption t -> "Option<"+(rustTypeToString t) + ">"
  |RVec t -> "Vec<"+(rustTypeToString t)+">"
  |RArray(t,sz) -> sprintf "[%s;%d]" (rustTypeToString t) sz
  |RSlice(t) -> "["+(rustTypeToString t)+"]"
  |RTupleStruct(ts) ->
    raise <| new System.Exception("Tuple struct cannot be represented in this context")    
  |RStruct(fields) ->
    raise <| new System.Exception("Struct cannot be represented in this context")
  |RFn {cc=cc;args=args;rettype=rt} ->
    let extrn=
      match cc with
      |RCCNative -> ""
      |RCCExtern abi -> "extern \""+abi+"\" "
    extrn + System.String.Join(", ", List.map (fun (nm,ty,_) -> nm + (rustTypeToString ty)) args) + " -> "+(rustTypeToString rt)

type RustAttribute=RName*((RName*RVal)list)
type RustAttributes=RustAttribute list

type RustMethod=
  |RMSelf
  |RMBSelf
  |RMMBSelf

type RustElem=
  |RETypeDef of RustType
  |REStruct of RustStruct
  |REInterface of ((RName*RustMethod*RustFn) list)*InterfaceAnnotation
  |REEnum of RustType
  |REFn of RustFn*MethodAnnotation
  |REConst of RustType*RVal*uint64
  |REStatic of RustType*RVal

type RustItem=
  |RustItem of RName*RustElem*RustAccess*RustAttributes

let sndOf3 (_,v,_) = v

let rec extractGenerics rt=
  match rt with
  |RType s -> []
  |RBorrow t |RMutBorrow t |RHResult t |RResult (t,_) |ROption t |RArray (t,_) |RSlice t |RConstPtr t |RMutPtr t |RVec t-> extractGenerics t
  |RFn {args=args;rettype=rt} -> List.collect (sndOf3 >> extractGenerics) (("",rt,ANone)::args)
  |RTupleStruct fields -> List.collect extractGenerics fields
  |RStruct fields -> List.collect (sndOf3 >> extractGenerics) fields
  |RGeneric(s,sub) -> [(s,sub)]

type LVInit=
  |LVDefault
  |LVCOMWrapper
  |LVSizeOf of string
  |LVStringConversion of string

type NParmSource=
  |RustSelf // *(self.0) as *mut _ as *mut InterfaceVtbl
  |RustParameter of string*RustType // rustparameter as native type
  |RustParameterSizeOf of string // mem::size_of::<rust parameter type>()
  |RustSliceLenghtOf of string list // multiple sources mean that all array lengths should be equal
  |RustIIDOf of string // rustparameter.iid()
  |RustLocalVar of string*RustType*LVInit*RustType // local variable (default init)
  |RustStructField of string*string*RustType // (local var name, field name) &mut (tempstruct.field)
  |RustConst of string // some constant

let typeOfSource s=
  match s with
  |RustSelf -> RType "Don't know"
  |RustParameter(_,rty) -> rty
  |RustParameterSizeOf _ -> RType "usize"
  |RustSliceLenghtOf _ -> RType "usize"
  |RustIIDOf _ -> RType "REFGUID"
  |RustLocalVar(_,_,_,rty) -> rty
  |RustStructField(_,_,rty) -> rty
  |RustConst _ -> RType "Don't know"

type ReturnValueSource=
  |RVNative // use native return type (void -> (), HRESULT -> HResult<()>, type -> rust_type)
  |RVLocalVar of string*RustType*LVInit*RName // local variable (default initialized). Uninitialized is better, but there's no stable support for this yet.

open cdesc

let ccToRCC cc=
  match cc with
  |libclang.CallingConv.X86StdCall -> RCCExtern "system"
  |_ -> raise <| new System.Exception("NotSupported")

let rec tyToRty (ty:CTypeDesc)=
  match ty with
  |Primitive t ->
    match t with
    |Void -> RType "c_void"
    |UInt16 -> RType "u16"
    |UInt8 -> RType "u8"
    |UInt32 -> RType "u32"
    |UInt64 -> RType "u64"
    |UIntPtr -> RType "usize" 
    |Int16 -> RType "i16"
    |Int32 -> RType "i32"
    |Int64 -> RType "i64"
    |Int8 -> RType "i8"
    |IntPtr -> RType "isize"
    |Float32 -> RType "f32"
    |Float64 -> RType "f64"
    |Float80 -> RType "f80"
    |Bool -> RType "bool"
    |Char8 -> RType "u8"
    |Char16 -> RType "u16"
    |Char32 -> RType "u32"
  |Unimplemented v-> RType ("Unimplemented("+v+")")
  |Typedef uty -> tyToRty uty
  |TypedefRef tyn-> RType tyn
  |StructRef tyn -> RType tyn
  |EnumRef tyn -> RType tyn
  |Array(uty,size) -> RArray ((tyToRty uty), size)
  |Ptr(Const(uty)) -> RConstPtr (tyToRty uty)
  |Ptr(Function(CFuncDesc(args,rty,cc))) ->
    RFn {cc=(ccToRCC cc); args=(List.map funcArgToRty args); rettype=(tyToRty rty); annot=MANone}
  |Ptr uty -> RMutPtr(tyToRty uty)
  |Const(uty) -> tyToRty uty
  |_ -> raise <| new System.Exception(sprintf "NoRepresentationYet(%A)" ty)
and
  funcArgToRty(name,ty,_) =
    let nm=
      match name with
      |"type" -> "ty"
      |"" -> "_"
      |_ -> name
    let rty=
      match ty with
      |Array(_,_) -> RMutBorrow(tyToRty ty)
      |_ -> tyToRty ty
    (nm, rty, ANone)

// General notes of mapping native interfaces into safe ones
//
// 1. Native method can correspond to multiple safe methods (TypeSelector)
//      
// 2. Native parameter can be taken from:
//   a. Safe parameter (type conversion on call-site)
//   b. Local variable
//     I)   uninitialized (for native _out_ parameter)
//     II)  default initialized (?) (can't see a use for that)
//     III) converted from safe parameter (for native _in_ parameter)
//     IV)  struct (for returning multiple values from safe fn) (tuple? no names. inconvenient)
//     

type LocalVar={
  mut : bool
  ty : RustType
  initExpression : RVal
}

type MethodRouting={
  className: RName
  implClassName: RName
  nativeName: RName
  safeName: RName
  unsafe: bool
  genericTypes: Map<RName, RVal> // map from generic type name to constraints
  localVars: Map<RName, LocalVar> // map form local var name to local var desc.
  nativeParms: (RName*RVal) list // name of native parm, conversion expression 
  safeParms: (RName*RustType*Option<RVal>) list // name of safe parm, type of safe parm, initialization expression 
  returnVal: RVal // return expression
  returnType: RustType
}
