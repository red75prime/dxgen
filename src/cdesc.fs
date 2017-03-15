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

let ccToRustNoQuotes (cc:CallingConv)=
    match cc with
    |CallingConv.X86StdCall -> "stdcall"
    |CallingConv.CXCallingConv_C -> "cdecl"
    |_ -> failwithf "Unimplemented calling convention %A in ccToRust" cc

let ccToRust (cc:CallingConv)=
    "\"" + (ccToRustNoQuotes cc) + "\""

// File path, line, column, 
type CodeLocation=System.String*uint32*uint32*uint32

type CParamAnnotation=
    |NoAnnotation
    |In
    |InZ
    |InOpt
    |Out
    |OutOpt
    |InOut
    |InOutOpt
    |InReads of string
    |InReadsOpt of string
    |InReadsBytes of string
    |InReadsBytesOpt of string
    |InOutUpdatesBytes of string
    |OutWrites of string
    |OutWritesOpt of string
    |OutWritesBytes of string
    |OutWritesBytesOpt of string
    |OutWritesTo of string*string
    |OutWritesToOpt of string*string
    |COMOutptr
    |COMOutptrOpt
    |InRange of string*string
    |FieldSize of string
    |OutptrOptResultBytebuffer
    |OutptrResultBytebuffer
    |OutptrOptResultMayBeNull
    |COMOutptrOptResultMayBeNull

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
        failwith "Cannot removeOpt on COMOutptrOptResultMayBeNull"

    |NoAnnotation |In |InZ |Out |InOut |InReads _ |InReadsBytes _ 
    |InOutUpdatesBytes _ |OutWrites _ |OutWritesBytes _ | OutWritesTo _ 
    |COMOutptr |InRange _ |FieldSize _ |OutWritesBytes _ |OutptrResultBytebuffer  -> pa

type Target = 
    |TargetUnknown
    |TargetX86
    |TargetX64

type Attribute =
    |AttrUuid of string

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
    |Struct of (CStructElem list)*string*(Attribute list) // elems + base (for c++)
    |ForwardDecl of string 
    |StructRef of string
    |Union of (CStructElem*((Target*int64) list)) list // snd is list of (target, elem size)
    |UnionRef of string
    |UnsizedArray of CTypeDesc
    |Function of CFuncDesc
and CFuncDesc=CFuncDesc of ((string*CTypeDesc*CParamAnnotation) list)*CTypeDesc*CallingConv
and CStructElem=CStructElem of name:string*typeDesc:CTypeDesc*bitWidth:Option<int32*int64>

// retuns tuple (list of subtypes, function to generate type from list of subtypes)
let rec subtypes ty = 
    let subtypesUnion ses=
        let elems = ses |> List.map (fun (CStructElem(name, sty, bw), sz) -> sty)
        let gen sts = List.map2 (fun (CStructElem(name, _, bw), sz) ty -> (CStructElem(name, ty, bw), sz)) ses sts
        (elems, gen)
    let subtypesStruct ses=
        let elems = ses |> List.map (fun (CStructElem(name, sty, bw)) -> sty)
        let gen sts = List.map2 (fun (CStructElem(name, _, bw)) ty -> (CStructElem(name, ty, bw))) ses sts
        (elems, gen)
    let subtypesFunc (CFuncDesc(plist,rety,cc))=
        let elems = rety :: (plist |> List.map (fun (_,ty,_) -> ty))
        let gen list =
            match list with
            |[] -> failwith "Unreachable"
            |rety :: sts -> CFuncDesc(List.map2 (fun (pname, _ , pannot) ty -> (pname, ty, pannot)) plist sts,rety,cc)
        (elems, gen)
    let unwrap sts=
        match sts with
        |[ty] -> ty
        |_ -> failwith "Error in type processing"
    match ty with
    |Primitive _ -> ([], (fun _ -> ty))
    |Unimplemented _ -> ([], (fun _ -> ty))
    |Typedef sty -> ([sty], unwrap >> Typedef)
    |TypedefRef _ -> ([], (fun _ -> ty))
    |Enum _ -> ([], (fun _ -> ty))
    |EnumRef _ -> ([], (fun _ -> ty))
    |Ptr sty -> ([sty], unwrap >> Ptr)
    |Const sty -> ([sty], unwrap >> Const)
    |Array(sty, num) -> ([sty], unwrap >> (fun ty -> Array(ty, num)))
    |Struct (ses,bas,attrs) -> let (sts, fn) = subtypesStruct ses in (sts, fun sts -> Struct(fn sts, bas, attrs))
    |StructRef _ -> ([], (fun _ -> ty))
    |Union ues -> let (sts, fn) = subtypesUnion ues in (sts, fun sts -> Union(fn sts))
    |UnionRef _ -> ([], (fun _ -> ty))
    |UnsizedArray sty -> ([sty], unwrap >> UnsizedArray)
    |Function(fd) -> let (sts, fn) = subtypesFunc fd in (sts, fun sts -> Function(fn sts))
    |ForwardDecl _ -> ([], (fun _ -> ty))


let rec recursiveTransform f ty=
    match f ty with
    |Some(ty) -> ty
    |_ ->
        let (elems, gen)=subtypes ty
        gen <| (elems |> List.map (fun ty -> match f ty with |Some(ty') -> ty' |None -> recursiveTransform f ty ))

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

// returns Some(ses) iff ty is a typedef of struct that has base struct or (contains only function pointers and is not empty)
let getVtbl (structs:Map<string, CTypeDesc*CodeLocation>) ty=
    let isStructVtbl ses bas=
        if List.exists (function |CStructElem(_,Ptr(Function(_)),_) -> false |_ -> true) ses then
            // Struct, which contains data, cannot be interface
            None
        else 
            if not(System.String.IsNullOrEmpty bas) then
                Some(ses,bas)
            else
                if (ses <> []) && (List.forall (function |CStructElem(_,Ptr(Function(_)),_) -> true |_ -> false) ses) then
                    Some(ses,bas)
                else
                    None

    match ty with
    |Typedef(StructRef(sname)) |StructRef(sname) -> 
        match Map.find sname structs with
        |Struct(ses, bas, _), _ -> isStructVtbl ses bas
        |_ -> None
    |Struct(ses, bas, _) ->
        isStructVtbl ses bas
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

let rec typeDesc (ty0: Type)=
    let ty = 
        if ty0.kind = TypeKind.Elaborated then
            getNamedType ty0
        else
            ty0
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
        if isConstQualifiedTypeFS (getArrayElementType ty) then
            Array(Const(getArrayElementType ty |> typeDesc), getArraySize ty)
        else
            Array(getArrayElementType ty |> typeDesc, getArraySize ty)
    |TypeKind.Enum -> 
        EnumRef (getTypeSpellingFS ty)
    |TypeKind.Record ->
        let ts0 = getTypeSpellingFS ty
        let ts = 
            if ts0.Contains("::") then
                ts0.Split([|" "|], System.StringSplitOptions.RemoveEmptyEntries) 
                    |> Array.map(
                        fun (s:string) -> 
                            let li = s.LastIndexOf("::")
                            if li = -1 then s else s.Substring(li+2) 
                        )
                    |> fun sa -> System.String.Join(" ", sa)
            else
                ts0
        
        let cnst = "const "
        if ts.StartsWith(cnst) then
            Const(StructRef (ts.Substring(cnst.Length)))
        else
            StructRef (ts)
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
        | tname when tname.StartsWith("union ") -> UnionRef(tname.Substring("union ".Length))
        | tname -> Unimplemented(tname)
    |TypeKind.LValueReference ->
        Ptr(getPointeeType ty |> typeDesc)
    |TypeKind.Elaborated ->
        failwith "unreachable"
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
        |Float32 -> "c_float"
        |Float64 -> "c_double"
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
    |Ptr(Array(Const(uty),size)) -> "*const ["+(tyToRust uty)+"; "+size.ToString()+"]"
    |Ptr(Array(uty,size)) -> "*mut ["+(tyToRust uty)+"; "+size.ToString()+"]"
    |Ptr(Const(uty)) -> "*const "+(tyToRust uty)
    |Ptr(Function(CFuncDesc(args,rty,cc))) ->
        let eol = System.Environment.NewLine
        "Option<unsafe extern \"system\" fn(" + eol + "    " + 
            ((List.map funcArgToRust args) |> String.concat(","+eol+"    ")) + eol + 
            "    ) -> " + (if rty = Primitive Void then "c_void" else tyToRust rty) + ">"
    |Ptr uty -> "*mut " + (tyToRust uty)
    |Const(uty) -> tyToRust uty
    |ForwardDecl uty -> uty
    |_ -> "NoRepresentationYet("+(sprintf "%A" ty)+")"
and
    funcArgToRust(name,ty,_) =
        let rty = 
            match ty with
            |Array(Const(_),_) -> "*"+(tyToRust ty)
            |Array(_,_) -> "*mut "+(tyToRust ty)
            |_ -> tyToRust ty
        match name with
        |"type" -> "ty"+": "+rty
        |"" -> rty
        |_ -> name+": "+rty

let rec tyToRustGlobal (ty:CTypeDesc)=
    tyToRust ty

type MacroConst=
    |MCInt32 of int32
    |MCUInt32 of uint32
    |MCInt64 of int64
    |MCUInt64 of uint64
    |MCFloat of float
    |MCDouble of double
    |MCExpression of string list

type IID = 
    |IID11 of string array
    |IID1 of string

let iid2array iid =
    match iid with
    |IID11 ar -> ar
    |IID1 s ->
        // "12345678-1234-1234-1234-123456789012" => [| "0x12345678"; "0x1234"; "0x1234"; "0x12"; "0x34"; "0x12"; "0x34"; "0x56"; "0x78"; "0x90"; "0x12" |]
        let a = s.Split('-')
        let b4 = a.[3]
        let a2 = Array.init 2 (fun i -> b4.Substring(i*2,2))
        let b5 = a.[4]
        let a6 = Array.init 6 (fun i -> b5.Substring(i*2, 2))
        let ar = Array.concat [[| a.[0]; a.[1]; a.[2]; a.[3] |]; a2; a6]
        Array.map (fun s -> "0x"+s) ar

