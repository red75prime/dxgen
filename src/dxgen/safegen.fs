module safegen

open System.Runtime.InteropServices
open Microsoft.FSharp.NativeInterop
open FSharpx
open libclang
open cdesc

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

open rustdesc

// Generates rust code to check that sizes of all arrays in arrs are same. Some arrays can be optional.
//
// locVar - name of local variable that receives size of array
// parms - list of annotated and equipped parameters of native method
// arrs - list of native names of arrays
let generateArraySizeCheck locVar parms arrs=
  let arrList=
    parms |> List.filter (fun (pname,_,_,_) -> List.contains pname arrs) 
          |> List.map (fun (pname,_,_,psource) -> 
              match psource with
              |RustParameter (rpname, ROption _) ->
                rpname+".as_ref().map(|a|{a.len()})"
              |RustParameter (rpname, _) ->
                "Some("+rpname+".len())"
              |_ -> 
                printfn "Error in creating constraint for array sizes" 
                "ERROR"
            )
  "  let "+locVar+" = same_lenght(&["+System.String.Join(",",arrList)+"]).expect(\"Arrays must have equal sizes.\");"

let native2rustName parms name=
  match List.find (function |(nname,_,_,RustParameter _) when nname=name -> true |_ -> false ) parms with
  |(_,_,_,RustParameter(rname,_)) -> rname
  |_ -> raise <| new System.Exception("Unreachable")

let deKeyword name=
  match name with
  |"type" -> "type_"
  |_ -> name

// parms is list of tuples (parameterName, parameterAnnotation, nativeParameterType, parameterSource)
let generateMethodFromEquippedAnnotation (mname, nname, mannot, parms_k, rty, rvsource)=
  let parms=List.map (function |(nname,annot,ctype,RustParameter (rname,rtype)) -> (nname,annot,ctype,RustParameter (deKeyword rname, rtype)) |other -> other) parms_k
  // let's find return type and return expression
  let (rrt,rexpr)=
    match rvsource with
      |RVNative ->
        match rty with
        |Primitive Void -> (RType "()", "()")
        |TypedefRef name when name="HRESULT" -> (RHResult(RType "()"), "hr2ret(hr,())")
        | _ -> (RType (tyToRust rty), "hr")
      |RVLocalVar(name,rtype,init,rettypestr) -> 
        match init with
        |LVCOMWrapper ->
          match rty with
          |Primitive Void -> (RType rettypestr, rettypestr+"::new("+name+")")
          |TypedefRef tname when tname="HRESULT" -> (RHResult (RType rettypestr), "hr2ret(hr,"+rettypestr+"::new("+name+"))")
          |_ -> 
            let err=sprintf "Native return type is %s, but annotated is %A" (tyToRust rty) rtype
            printfn "%s" err
            raise <| new System.Exception(err)
            (rtype, name)
        |_ ->
          match rty with
          |Primitive Void -> (rtype, name)
          |TypedefRef tname when tname="HRESULT" -> (RHResult rtype, "hr2ret(hr,"+name+")")
          |Ptr(TypedefRef s) ->
            (rtype, name)
          |_ -> // error. native return type is incompatible with annotated.
            let err=sprintf "Native return type is %s, but annotated is %A" (tyToRust rty) rtype
            printfn "%s" err
            raise <| new System.Exception(err)
            (rtype, name)
  let rtype=rrt
  // let's find generic types
  let genericTypes=
    parms |> List.collect 
      (fun (_,_,_,psource) -> 
        match psource with 
        |RustParameter(_,rty) -> extractGenerics rty
        |RustLocalVar(_,rty,_,rty1) -> List.concat [extractGenerics rty;extractGenerics rty1]
        |RustSelf |RustParameterSizeOf _ |RustSliceLenghtOf _ |RustIIDOf _ |RustStructField _ |RustConst _ -> []
        ) |> Set.ofList |> Set.map (fun (s,sub) -> if System.String.IsNullOrEmpty sub then s else s+": "+sub)
  let generics=
    if Set.isEmpty genericTypes then
      ""
    else
      "<"+System.String.Join(",",genericTypes)+">"
  // let's find rust method parameters and their types
  let rustParms=
    parms |> List.collect
      (fun (_,_,_,psource) ->
        match psource with
        |RustParameter (name,rtype) -> [(name,rtype)]
        |RustLocalVar (_,_,LVStringConversion name,_) -> [(name, RType "&OsStr")]
        |RustSelf |RustParameterSizeOf _ | RustSliceLenghtOf _ | RustIIDOf _ | RustConst _ |RustLocalVar _ |RustStructField _ -> []
      ) |> List.map (fun (name, rtype) -> name+": "+(rustTypeToString rtype))
  let parameters=System.String.Join(", ", rustParms)
  let unsafe=if mannot=MAUnsafe then "unsafe " else ""
  // generate parameter check (array lenghts should be equal if there's just one native parameter for their length)
  let csb=new System.Text.StringBuilder()
  let hasConstraints=ref false
  let cNum=ref 1
  let parms_constraint=
    parms |> 
      List.map (
        fun ((a,b,c,psource) as parm) ->
          match psource with
          |RustSliceLenghtOf ((_::_::_) as ps) ->
            hasConstraints := true
            let locVar=sprintf "len%d" !cNum
            cNum := !cNum+1
            csb.AppendLine(generateArraySizeCheck locVar parms ps) |> ignore
            (a,b,c,RustLocalVar(locVar,RType "usize",LVSizeOf "", RType "usize"))
          |_ -> parm
        )
  let constraints=csb.ToString()
  // let's find local variables and their types
  let localVars=
    parms |> List.collect // parms instead of parms_constraint, because constraint's local vars is in constraints block alreay
      (fun (_,_,_,psource) ->
        match psource with
        |RustLocalVar (name, rt, init, _) -> [(name, rt, init)]
        |RustStructField (name, _, rt) -> [(name, rt, LVDefault)]
        |RustSelf | RustParameter _ |RustParameterSizeOf _ | RustSliceLenghtOf _ | RustIIDOf _ | RustConst _ -> []
        ) |> Set.ofList
  // if there's parameter check, then modify return type and return expression
  let rtype_constraint=
    if !hasConstraints then 
      rtype // I decided to panic if array sizes do not match
    else
      rtype
  let rexpr_constraint=
    if !hasConstraints then
      rexpr
    else
      rexpr
  // generate local variables
  let sbloc=new System.Text.StringBuilder()
  for (vname,rt,init) in localVars do
    match init with
    |LVCOMWrapper ->
      sbloc.AppendLine(sprintf "  let mut %s: %s = ::std::ptr::null_mut();" vname (rustTypeToString rt)) |> ignore
    |LVDefault -> 
        sbloc.AppendLine(sprintf "  let mut %s: %s = unsafe{ ::std::mem::uninitialized() };" vname (rustTypeToString rt)) |> ignore
    |LVSizeOf aname ->
      let raname=native2rustName parms aname
      sbloc.AppendLine(sprintf "  let mut %s = size_of_val(%s) as u32;" vname raname) |> ignore
    |LVStringConversion sname ->
      sbloc.AppendLine(sprintf "  let %s = os_str_to_vec_u16(%s);" vname sname) |> ignore
  let locals=sbloc.ToString()
  // TODO: generate call to native function
  let native_parms=
    parms_constraint |> 
      List.map 
        (fun (_,_,cty,src) ->
          match src with
          |RustConst s ->
            s
          |RustIIDOf p ->
            let pty=parms_constraint |> List.find (fun (n,_,_,_) -> n=p) |> (fun (_,_,_,src) -> typeOfSource src)
            sprintf "%s::iid() as REFGUID" (rustTypeToString pty)
          |RustLocalVar (l,rty,init,_) ->
            match init with
            |LVStringConversion _ ->
              l+".as_ptr()"
            |LVSizeOf _ ->
              match cty with
              |Ptr(_) ->
                "&mut "+l+" as "+(tyToRust cty)
              |_ ->
                l+" as "+(tyToRust cty)
            |LVCOMWrapper ->
              "&mut "+l+" as *mut *mut _ as "+(tyToRust cty)
            |_ -> 
              "&mut "+l+" as "+(tyToRust cty)
          |RustParameter (p,rty) ->
            match cty with
            |Ptr(Array(_,_)) ->
              p+" as "+(tyToRust cty)
            |Ptr(Primitive Void) ->
              p+" as *mut _ as "+(tyToRust cty)
            |Ptr(Const(Primitive Void)) ->
              p+" as *const _ as "+(tyToRust cty)
            |Ptr(Ptr(Primitive Void)) ->
              match rty with
              |RBorrow(RGeneric(_,"HasIID"))
              |RMutBorrow(RGeneric(_,"HasIID")) -> 
                p+".expose_iptr() as *mut *mut _ as *mut *mut c_void"
              |_ -> p+" as "+(tyToRust cty)
            |Ptr(TypedefRef _) |Ptr(Ptr(TypedefRef _)) |Ptr(Const(TypedefRef _)) ->
              match rty with
              |ROption(RMutBorrow(RSlice(_))) ->
                "opt_slice_to_mut_ptr("+p+")"
              |ROption(RMutBorrow(RType _)) ->
                p+".map(|p|p as *mut _).unwrap_or(::std::ptr::null_mut())"
              |RMutBorrow(RSlice(_)) ->
                p+".as_mut_ptr() as "+(tyToRust cty)
              |_ -> p+" as "+(tyToRust cty)
            |Ptr(Const(TypedefRef _)) ->
              match rty with
              |ROption(RBorrow(RSlice(_))) ->
                "opt_slice_to_ptr("+p+")"
              |RBorrow(RSlice(_)) ->
                p+".as_ptr()"
              |_ -> p+" as "+(tyToRust cty)
            |_ -> p+" as "+(tyToRust cty)
          |RustParameterSizeOf p ->
            "size_of_val("+p+") as "+(tyToRust cty)
          |RustSelf ->
            "self.0"
          |RustSliceLenghtOf [p] ->
            p+".len() as "+(tyToRust cty)
          |RustSliceLenghtOf _ ->
            raise <| new System.Exception("Unreachable")
          |RustStructField(s,f,_) ->
            "&mut "+s+"."+f+" as "+(tyToRust cty)
            ) |> (fun seq -> System.String.Join(", ", seq))
  let nativeInvocation="((**self.0)."+nname+")("+native_parms+")"
  // we are ready to generate method
  "
pub "+unsafe+"fn "+mname+generics+"(&self"+(if parameters="" then "" else ", "+parameters)+") -> "+(rustTypeToString rtype_constraint)+" {
"+constraints+"
"+locals+"
  let hr=unsafe {
    "+nativeInvocation+"
  };
  "+rexpr_constraint+"
}
"

let getReferencedParameter parameterAnnotation=
  match parameterAnnotation with
  |InOutOfSize p -> p
  |OutOfSize p -> p
  |InOfSize p -> p
  |OutReturnInterface p -> p
  |OutReturnKnownInterface (p,_) -> p
  |InOptionalArrayOfSize p -> p
  |InArrayOfSize p -> p
  |InByteArrayOfSize p -> p
  |TypeSelector (p,_) -> p
  |_ -> ""


let indentBy indentationString (source:System.String)=
  indentationString+source.Replace("\n","\n"+indentationString)

let derefCType pty=
  match pty with
  |Ptr(Const(ty)) -> ty
  |Ptr(ty) -> ty
  |_ -> raise <| new System.Exception("Type isn't a pointer")

let rec convertTypeToRustNoArray ty pannot=
  match ty with
  |Array(Const(uty), sz) ->
    RArray((convertTypeToRustNoArray uty pannot),sz)
  |TypedefRef typename ->
    RType typename // struct or enum or something that is already defined in libc or in d3d12_sys.rs
  |Const(TypedefRef typename) ->
    RType typename
  |Ptr(Const(uty)) -> 
    match pannot with
    |InOptional ->
      ROption(RBorrow (convertTypeToRustNoArray uty pannot))
    |_ -> RBorrow (convertTypeToRustNoArray uty pannot)
  |Ptr(uty) ->
    match pannot with
    |InOptional ->
      ROption(RMutBorrow (convertTypeToRustNoArray uty pannot))
    |_ -> RMutBorrow (convertTypeToRustNoArray uty pannot)
  |Primitive _ -> RType(tyToRust ty)
  |_ ->
    raise <| new System.Exception(sprintf "convertToRustTypeNoArray: unsupported type %A" ty)

open CaseConverter
let generateMethod (mname, nname, mannot, parms, rty)=
  // let's iterate list of parametes, filling list of local varibles,
  // generic types, equipping parameter with source
  let genNum=ref 1
  let locNum=ref 1
  let eqpParms=ref []
  let retParm=ref RVNative
  let cty2locvar=ref Map.empty 
  for (pname, pannot, pty) in parms do
    let ensureNoRefs()=
        let refs=parms |> List.filter (fun (_, pannot, _) -> getReferencedParameter pannot = pname)
        match refs with
        |[] -> ()
        |_ -> raise <| new System.Exception(sprintf "Parameter %s in %s shouldn't be referenced by annotation" pname mname)
    let processRefs()=
        // let's find references to this parameter
        let refs=parms |> List.filter (fun (_, pannot, _) -> getReferencedParameter pannot = pname)
        match refs with
        |[] ->
          // No references. Just convert type
          // C pointer can be a pointer to a value or an array.
          // Arrays should be annotated, this parameter is not, thus no arrays.
          match pty with
          |TypedefRef "LPCWSTR" ->
            let locVar=sprintf "tmp%d" !locNum
            locNum:= !locNum + 1
            (pname, pannot, pty, RustLocalVar (locVar, RType "LPCWSTR", LVStringConversion (toSnake pname), RType "LPCWSTR"))
          |Array(_,_) ->
            (pname, pannot, pty, RustParameter (toSnake pname, RMutBorrow(convertTypeToRustNoArray pty pannot)))
          |_ ->
            (pname, pannot, pty, RustParameter (toSnake pname, convertTypeToRustNoArray pty pannot))
        |[rf] -> 
          // one reference
          match rf with
          |(name,InOutOfSize _,refty) 
          |(name,OutOfSize _,refty) 
          |(name,InOfSize _,refty) -> 
            (pname, pannot, pty, RustParameterSizeOf(name))
          |(name, OutReturnInterface _, _) ->
            (pname, pannot, pty, RustIIDOf name)
          |(name, OutReturnKnownInterface(_,iid),_) ->
            (pname, pannot, pty, RustConst ("&IID_I"+iid))
          |(name, InOptionalArrayOfSize _, _) 
          |(name, InArrayOfSize _, _) ->
            (pname, pannot, pty, RustSliceLenghtOf [name])
          |(name, InByteArrayOfSize _, _) ->
            (pname, pannot, pty, RustParameterSizeOf(name))
          |_ ->
            raise <| new System.Exception(sprintf "Unexpected reference %A" rf)
        |_ ->
          // multiple references
          // The only case is when this parameter is a size of multiple array parameters
          if refs |> List.forall (function |(_,InOptionalArrayOfSize _,_) |(_,InArrayOfSize _, _) -> true |_ -> false) then
            (pname, pannot, pty, RustSliceLenghtOf (refs |> List.map (fun (name,_,_) -> name )))
          else
            raise <| new System.Exception("Unexpected annotations")
    let eqParm=
      match pannot with
      |AThis ->
        (pname, pannot, pty, RustSelf)
      |ANone ->
        processRefs()
      |InOut ->
        ensureNoRefs()
        (pname, pannot, pty, RustParameter (toSnake pname, convertTypeToRustNoArray pty pannot))
      |InOutReturn ->
        // This is special case for GetPrivateData.
        let (_,_,_, eqp)=processRefs()
        match eqp with
        |RustParameterSizeOf nname ->
          let locVar=sprintf "tmp%d" !locNum
          locNum:= !locNum + 1
          let rty=RType "u32"
          retParm := RVLocalVar(locVar, rty, LVSizeOf nname, "")
          (pname, pannot, pty, RustLocalVar(locVar,rty,LVSizeOf nname,rty))
        |_ ->
          raise <| new System.Exception("Unreachable")
      |InOutOfSize nname |OutOfSize nname ->
        ensureNoRefs()
        // TypeSelector could assign type to this parameter
        match pty with
        |Ptr(Primitive Void) ->
          let gname=sprintf "T%d" !genNum
          genNum := !genNum + 1
          (pname, pannot, pty, RustParameter(toSnake pname, RMutBorrow(RGeneric (gname,""))))
        |_ ->
          (pname, pannot, pty, RustParameter(toSnake pname, convertTypeToRustNoArray pty pannot))
      |InOfSize nname ->
        ensureNoRefs()
        // TypeSelector could assign type to this parameter
        match pty with
        |Ptr(Const(Primitive Void)) |Ptr(Primitive Void) ->
          let gname=sprintf "T%d" !genNum
          genNum := !genNum + 1
          (pname, pannot, pty, RustParameter(toSnake pname, RBorrow(RGeneric (gname,""))))
        |_ ->
          (pname, pannot, pty, RustParameter(toSnake pname, convertTypeToRustNoArray pty pannot))
      |OutReturn ->
        ensureNoRefs()
        let locVar=sprintf "tmp%d" !locNum
        locNum:= !locNum + 1
        let derefType=
          match pty with
          |Ptr(Const(ty)) -> ty
          |Ptr(ty) -> ty
          |_ -> raise <| new System.Exception(sprintf "Parameter %s annotated by OutReturn isn't a pointer" pname)
        let rtype=convertTypeToRustNoArray derefType pannot
        retParm := RVLocalVar(locVar,rtype,LVDefault,"")
        (pname, pannot, pty, RustLocalVar(locVar, rtype, LVDefault, rtype))
      |OutReturnCombine (stype,sfield) ->
        ensureNoRefs()
        match Map.tryFind stype !cty2locvar with
        |Some(locVar) ->
          (pname, pannot, pty, RustStructField(locVar, sfield, RType stype))
        |None ->
          let locVar=sprintf "tmp%d" !locNum
          locNum:= !locNum + 1
          cty2locvar := Map.add stype locVar !cty2locvar
          retParm := RVLocalVar(locVar, RType stype, LVDefault, "")
          (pname, pannot, pty, RustStructField(locVar, sfield, RType stype))
      |OutReturnInterface piid ->
        ensureNoRefs()
        let gname=sprintf "T%d" !genNum
        genNum := !genNum + 1
        let locVar=sprintf "tmp%d" !locNum
        locNum:= !locNum + 1
        let rty=RGeneric(gname, "HasIID")
        let tmpty=RMutPtr(RMutPtr(RType "::iid::IUnknownVtbl"))
        retParm := RVLocalVar(locVar, tmpty, LVCOMWrapper, gname)
        (pname, pannot, pty, RustLocalVar(locVar,tmpty,LVCOMWrapper,rty))
      |OutReturnKnownInterface (piid, itype) ->
        let locVar=sprintf "tmp%d" !locNum
        locNum:= !locNum + 1
        let rty=RType itype
        let tmpty=RMutPtr(RMutPtr(RType "::iid::IUnknownVtbl"))
        retParm := RVLocalVar(locVar, tmpty , LVCOMWrapper, itype)
        (pname, pannot, pty, RustLocalVar(locVar, tmpty, LVCOMWrapper, rty))
      |InIUnknown ->
        let gname=sprintf "T%d" !genNum
        genNum := !genNum + 1
        (pname, pannot, pty, RustParameter(toSnake pname, RMutBorrow(RGeneric(gname,"HasIID"))))
      |InOptionalArrayOfSize _ ->
        (pname, pannot, pty, RustParameter(toSnake pname, ROption(RMutBorrow(RSlice(convertTypeToRustNoArray (derefCType pty) pannot)))))
      |InArrayOfSize _ ->
        (pname, pannot, pty, RustParameter(toSnake pname, RMutBorrow(RSlice(convertTypeToRustNoArray (derefCType pty) pannot))))
      |InByteArrayOfSize _ ->
        (pname, pannot, pty, RustParameter(toSnake pname, RMutBorrow(RSlice(RType "u8"))))
      |InOptional ->
        (pname, pannot, pty, RustParameter(toSnake pname, ROption(RMutBorrow(convertTypeToRustNoArray (derefCType pty) pannot))))
      |AConst s ->
        (pname, pannot, pty, RustConst s)
    eqpParms := eqParm :: !eqpParms

  let eparms=List.rev !eqpParms

  let trivialEquip=(toSnake mname, nname, mannot, eparms, rty, !retParm)
  generateMethodFromEquippedAnnotation trivialEquip |> indentBy "  "

// Removes TypeSelector annotation, generating specialized methods
let preGenerateMethod ((mname, mannot, parms, rty) as methoddesc)=
  if mannot=MAIUnknown || mannot=MADontImplement then
    ""
  else
    match parms |> List.tryFind (function |(_,TypeSelector _,_) -> true |_ ->false) with
    |Some (sname, TypeSelector (tname,slist), _) ->
      let sb=new System.Text.StringBuilder()
      for (suffix, sval, stype) in slist do
        let parms1=
          parms |> List.map
            (fun (pname, pannot, pty) -> 
              if pname=sname then
                (pname, AConst sval, pty)
              else if pname=tname then
                match pty with
                |Ptr _ ->
                  (pname, pannot, pty) //Ptr (TypedefRef stype))
                |Const (Ptr _) ->
                  (pname, pannot, pty) //Const (Ptr (TypedefRef stype)))
                |_ ->
                  raise <| new System.Exception(sprintf "Unexpeted type %A of type-selected parameter" pty)
              else
                (pname, pannot, pty)
            )
        sb.AppendLine(generateMethod (mname+suffix, mname, mannot,parms1,rty)) |> ignore
      sb.ToString()
    |None ->
      generateMethod (mname, mname, mannot, parms, rty)
  

let generateMethods methods=
  let sb=new System.Text.StringBuilder()
  for methoddesc in methods do
    sb.Append(preGenerateMethod methoddesc) |> ignore
  sb.ToString()

let addCombiningStructs (sb:System.Text.StringBuilder) interfaceAnnotations=
  let apl=appendLine sb
  let structFields=
    interfaceAnnotations |> List.fold 
      (fun s (_,_,mannots) ->
        mannots |> List.fold  
          (fun s (_,_,pannots,_) ->
            pannots |> List.fold 
              (fun s (_,pannot,pty) ->
                match pannot with
                |OutReturnCombine(stype,sfield) ->
                  Set.add (stype,sfield,pty) s
                |_ -> s
              ) s          
          ) s
      ) Set.empty
  let structsByType=structFields |> Seq.groupBy (fun (stype,_,_) -> stype)
  for (stype,fields) in structsByType do
    apl "#[derive(Default, Debug)]"
    apl <| "pub struct "+stype+" {"
    for (_,field, pty) in fields do
      apl <| "  "+field+" : "+(tyToRust (derefCType pty))+","
    apl "}"
    apl ""
  ()

let safeInterfaceGen (types:Map<string,CTypeDesc>,enums:Map<string,CTypeDesc>,structs:Map<string,CTypeDesc>,funcs:Map<string,CTypeDesc>, iids:Set<string>) : string=
  let sb=new System.Text.StringBuilder()
  let vtbls=types |> List.ofSeq |> List.collect (fun (KeyValue(name,ty)) -> getVtbl structs ty |> o2l |> List.map (fun vtbl -> (name,vtbl))) 
  match sanityCheck vtbls d3d12annotations with
  |Some(interfaceAnnotations) ->
    let apl = appendLine sb
    apl "\
use iid::{HasIID, HResult, IUnknownVtbl, release_com_ptr, clone_com_ptr};
use iid::iids::*;
use d3d12_sys::*;
use std::ptr::{null, null_mut};
use std::mem::size_of_val;
use libc::*;
use std::ffi::OsStr;
use std::os::windows::ffi::OsStrExt;

fn os_str_to_vec_u16(s : &OsStr) -> Vec<u16> {
  s.encode_wide().chain(Some(0).into_iter()).collect::<Vec<_>>()
}

// Utility function. 
// Compares lengths of slices in Option<&[T]> 
// returns Ok(len) if all present slices have length len (or 0 if there's no arrays), 
//         Err(()) is arrays have different lengths.
fn same_lenght(lens:&[Option<usize>]) -> Option<usize> {
    let res=lens.iter().fold(Ok(None), 
        |sz, mlen| { 
            match sz { 
                Err(_) => sz, 
                Ok(None) => Ok(mlen.map(|l1|l1)), 
                Ok(Some(l)) => 
                    match *mlen {
                        None => sz, 
                        Some(l1) => 
                            if l1==l {
                                sz
                            } else {
                                Err(())
                            }
                    }
            }
        });
    res.map(|ms|{ms.unwrap_or(0)}).ok()
}

fn hr2ret<T>(hr : HRESULT, res:T) -> HResult<T> {
  if hr==0 { // TODO: mimic what SUCCESS macro does.
    Ok(res)
  } else {
    Err(hr)
  }
}

unsafe fn zeroinit_com_wrapper<T: HasIID>() -> T {
  T::new(null_mut())
}

fn opt_slice_to_mut_ptr<T>(os: Option<&mut [T]>) -> *mut T {
  os.map(|s|s.as_mut_ptr()).unwrap_or(::std::ptr::null_mut())
}

fn opt_slice_to_ptr<T>(os: Option<&[T]>) -> *const T {
  os.map(|s|s.as_ptr()).unwrap_or(::std::ptr::null())
}

"
    addCombiningStructs sb interfaceAnnotations
    for (iname, iannot, methods) in interfaceAnnotations do
      if iannot=IAManual then
        ()
      else
        apl <| sprintf "\
pub struct %s(*mut *mut I%sVtbl);

impl HasIID for %s {
  fn iid() -> &'static IID { &IID_I%s }
  fn new(ppVtbl : *mut *mut IUnknownVtbl) -> Self { %s(ppVtbl as *mut *mut _ as *mut *mut I%sVtbl) }
  fn expose_iptr(&self) -> *mut *mut IUnknownVtbl { self.0 as *mut *mut _ as  *mut *mut IUnknownVtbl}
}

impl Drop for %s {
  fn drop(&mut self) {
    release_com_ptr(self)
  }
}

impl Clone for %s {
  fn clone(&self) -> Self {
    clone_com_ptr(self)
  }
}

impl %s {
%s
}
"          iname iname iname iname iname iname iname iname iname (generateMethods methods)

    sb.ToString()
  |None -> 
    printfn "Sanity check failed. No Rust interfaces generated"
    ""
