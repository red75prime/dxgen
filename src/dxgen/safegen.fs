module safegen

open System.Runtime.InteropServices
open Microsoft.FSharp.NativeInterop
open FSharpx
open libclang
open cdesc

open Printf
open CaseConverter


let appendLine (sb:System.Text.StringBuilder) line=
  sb.AppendLine(line) |> ignore

let iUnknownFuncs=["QueryInterface";"AddRef";"Release"] |> Set.ofList

let o2l xs= 
  match xs with
  |Some(x) -> [x]
  |None -> []

// returns string of f# code that contains empty annotations for all COM interfaces contained in types parameter
let emptyAnnotationsGen (types:Map<string,CTypeDesc*CodeLocation>,enums:Map<string,CTypeDesc*CodeLocation>,structs:Map<string,CTypeDesc*CodeLocation>,
                            funcs:Map<string,CTypeDesc*CodeLocation>, iids:Map<string,CodeLocation>, defines:Map<string, MacroConst*string*CodeLocation>) : string=
  let sb=new System.Text.StringBuilder()
  let apl=appendLine sb
  apl("module annotations_autogen")
  apl("open annotations")
  apl("")
  apl("let d3d12annotations=[")
  let vtbls=structs |> List.ofSeq |> List.collect (fun (KeyValue(name,(ty,_))) -> getVtbl structs ty |> o2l |> List.map (fun vtbl -> (name,vtbl)))
  for (name, (mths, bas)) in vtbls do
    apl(sprintf "  (\"%s\",IAAutogen(Set.ofList []), \"%s\", [" name (if bas="" then "" else bas+"Vtbl"))
    for (mname,args,ty) in mths |> List.choose (function |CStructElem(mname, Ptr(Function(CFuncDesc(args,ty,_))), _)-> Some(mname,args,ty)  |_ -> None) do
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

  apl "/*"
  for (name, _) in vtbls do
    apl ("print_guid("+name+")")
  apl "*/"
  sb.ToString()

open annotations

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
    getReferencedParameters pan |> List.forall parameterExists
  if parameterMatch then
    Some(pname, pan, pty)
  else
    None

// Recursively check if interfaces', methods', parameters' names are equal in native and annotated 
// Returns Some(annotaded native) or None
let sanityCheck vtbls annotations= 
  isListsMatch fst "Native interfaces" vtbls (fun (name,_,_,_) -> name) "Annotated interfaces" annotations 
    (fun inamevtbl (_, (structElems, bas)) (_, interfaceAnnotation, baseInterface, methodAnnotations) -> 
      // inamevtbl is in the form I***Vtbl, let's strip 'I' and 'Vtbl'
      let iname = inamevtbl.Substring(1,inamevtbl.Length-5)
      let bname = if baseInterface="" then "" else baseInterface.Substring(0, baseInterface.Length-4)
      if bname <> bas then
        printfn "Error. Base interface of annotated is %s, but that of native is %s" bname bas
        None
      else 
        match interfaceAnnotation with
        |IAManual -> 
          // Don't bother matching interfaces marked for manual implementation
          Some(iname, bas, interfaceAnnotation, [])
        |IAAutogen _ ->
          // check if interface iname matches, by checking all methods
          let mergedMethods=
            isListsMatch (fun (CStructElem(mname,_,_)) -> mname) (sprintf "Native methods of %s" iname) structElems 
                          (fun (mname,_,_) -> mname) (sprintf "Annotated methods of %s" iname) methodAnnotations
              (fun mname se (_,aparms,methodAnnotation) ->
                match se with
                |(CStructElem(_,Ptr(Function(CFuncDesc(parms, rty, cc))),_)) ->
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
                |_ -> raise <| new System.Exception("Unreachable")
                )
          match mergedMethods with
          |Some(mm) -> Some(iname, bas, interfaceAnnotation, mm)
          |None -> None
    )
    
// ------------------
// Using annotations, we should determine source of each native parameter, source of Rust's function return value, presence of generic parameters

open rustdesc

let native2rustName parms name=
  match List.find (function |(nname,_,_,RustParameter _) when nname=name -> true |_ -> false ) parms with
  |(_,_,_,RustParameter(rname,_)) -> rname
  |_ -> raise <| new System.Exception("Unreachable")

let deKeyword name=
  match name with
  |"type" -> "type_"
  |_ -> name

let toRustParmName(s:string)=
  // TODO: process prefixes (p, pp)
  let depp=
    if s.StartsWith("pp") && s.Length>2 && System.Char.IsUpper(s.Chars(2)) then
      s.Substring(2)
    else if s.StartsWith("p") && s.Length>1 && System.Char.IsUpper(s.Chars(1)) then
      s.Substring(1)
    else if s.StartsWith("dw") && s.Length>2 && System.Char.IsUpper(s.Chars(2))  then
      s.Substring(2)
    else
      s
  depp |> toSnake |> deKeyword

let toRustMethodName=toRustParmName

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
  |TypedefRef typename |StructRef typename |EnumRef typename->
    if isOptional pannot then
      raise <| new System.Exception("Typedef, struct, enum cannot be made optional")
    else
      RType typename // struct or enum or something that is already defined in libc or in d3d12_sys.rs
  |Const(TypedefRef typename) ->
    RType typename
  |Const(StructRef typename) ->
    RType typename
  |Ptr(Const(uty)) -> 
    match pannot with
    |InOptional  ->
      ROption(RBorrow (convertTypeToRustNoArray uty pannot))
    |_ -> RBorrow (convertTypeToRustNoArray uty pannot)
  |Ptr(uty) ->
    // Typedefs can be ponters too. TODO: do something about it
    match pannot with
    |InOptional |InOutOptional ->
      ROption(RMutBorrow (convertTypeToRustNoArray uty pannot))
    |_ -> RMutBorrow (convertTypeToRustNoArray uty pannot)
  |Primitive _ -> RType(tyToRust ty)
  |_ ->
    raise <| new System.Exception(sprintf "convertToRustTypeNoArray: unsupported type %A" ty)


let generateMethodFromRouting 
        { className = clname
          implClassName = implclass
          nativeName = nname
          safeName = sname
          unsafe = unsafe
          genericTypes = gtypes
          localVars = lvs
          nativeParms = nparms
          safeParms = sparms
          returnVal = rval
          returnType = rtype
        } = 
  let generics=
    if Map.isEmpty gtypes then
      ""
    else
      "<"+System.String.Join(", ", 
        gtypes |> Map.toSeq 
          |> Seq.map 
            (fun (t,c) -> 
              if System.String.IsNullOrEmpty c 
              then t 
              else t+": "+c
              ) )+">"
  let sparams=
    System.String.Concat(
      sparms |> List.map 
        (fun (spname, ty, _) ->
          ", "+spname+": "+(rustTypeToString ty)) |> Seq.ofList)

  let rettype=rustTypeToString rtype

  let lvAndInit=
    System.String.Join(System.Environment.NewLine,
      seq {
        yield! lvs |> Map.toSeq 
          |> Seq.map 
            (fun (lv, {mut=mut;ty=ty;initExpression=init}) ->
              let init = if unsafe && init.StartsWith("unsafe ") then init.Substring("unsafe ".Length) else init
              let muts=if mut then "mut " else ""
              let lvtype=": "+(rustTypeToString ty)
              let inits=if System.String.IsNullOrEmpty init then ";" else " = "+init+";"
              "  let "+muts+lv+lvtype+inits)
        yield!
          sparms |> List.toSeq |> Seq.collect 
            (fun (_,_,minit) ->
              match minit with
              |Some(init) -> Seq.singleton init
              |None -> Seq.empty
              )
      })

  let nativeInvocation=
    System.String.Concat(
      seq{
        yield "(*(self.0 as *mut "+implclass+"))."+nname+"("
        yield System.String.Join(", ", nparms |> List.toSeq |> Seq.tail |> Seq.map snd)
        yield ")"
      })
  let signature =
    System.String.Format(
      @"{0}fn {1}{2}(&self{3}) -> {4}", (if unsafe then "unsafe " else ""), sname, generics, sparams, rettype)
  let ftext=
    System.String.Format(
      @"
#[allow(non_snake_case)]
pub {0} {{
{1}
  let _hr={4} {{ {2} }};
  {3}
}}
"       ,signature,
        lvAndInit,
        nativeInvocation,
        rval,
        if unsafe then "" else "unsafe")
  (ftext, signature)

//-------------------------------------------------------------------------------------
// This function is the core of the safe interface generator
//-=-----------------------------------------------------------------------------------
// clname - class name
// mname - Rust method name
// nname - native method name
// mannot - method's annotation
// parms - list of annotated parameters
// rty - return type
let generateRouting (clname, mname, nname, mannot, parms, rty) (noEnumConversion:bool) implclass=
  if mannot=MADontImplement then
    ([],[])
  else
    let locVarNum=ref 1
    let getNextLocVar()=
      let lvname=sprintf "lv%d" !locVarNum
      locVarNum := !locVarNum+1
      lvname
    let genTypeNum=ref 0
    let getNextGT()=
      let gtname=if !genTypeNum=0 then "T" else sprintf "T%d" !genTypeNum
      genTypeNum := !genTypeNum+1
      gtname
    
    let genTypes = ref Map.empty
    let localVars = ref Map.empty
    let nativeParms = ref Map.empty
    let safeParms = ref Map.empty
    let returnVals = ref Set.empty
    let errors = ref List.empty
    let warnings = ref List.empty

    let addError err=
      errors := err :: !errors

    let addWarning wrn=
      warnings := wrn :: !warnings

    let addReturnExpression ex rty=
      returnVals := Set.add (ex,rty) !returnVals

    let addSafeParm spname sptype =
      safeParms := Map.add spname (sptype,None) !safeParms

    let addSafeParmInit spname sptype init=
      safeParms := Map.add spname (sptype,Some(init)) !safeParms

    let addNativeParm npname spname initExpr=
      nativeParms := Map.add npname (spname, fun _ -> initExpr) !nativeParms

    let addNativeParmFun npname spname initFun=
      nativeParms := Map.add npname (spname, initFun) !nativeParms

    let addLocalVar lvname lvmut lvtype lvinit=
      localVars := Map.add lvname (lvmut, lvtype,lvinit) !localVars

    let newLocalVar lvmut lvtype lvinit=
      let lv=getNextLocVar()
      addLocalVar lv lvmut lvtype lvinit
      lv

    let newGenType constraints=
      let gtn=getNextGT()
      genTypes := Map.add gtn constraints !genTypes
      gtn

    // Let's attach list of references to every native parameter
    let parmsr=
      List.map (
        fun (pname, pannot, pty) ->
          let refs=parms |> List.filter (fun (_,pannot,_) -> List.contains pname (getReferencedParameters pannot))
          (pname, pannot, pty, refs)
        ) parms

    let sname2lv=ref Map.empty
    // Let's add local variables for OutReturnCombine
    for sname in parmsr |> Seq.choose (function |(_,OutReturnCombine(sname,_),_,_) -> Some(sname) |_ -> None) |> Set.ofSeq do
      let retty = RType(sname)
      let lv=newLocalVar true retty (fun m -> "unsafe {mem::uninitialized::<_>()}")
      addReturnExpression lv retty
      sname2lv := Map.add sname lv !sname2lv

    // Let's generate routing
    for (pname, pannot, pty, refs) in parmsr do
      let safeParmName=toRustParmName pname // TODO: Process prefixes ("p", "pp" and such)
      match pannot with
      |AThis ->
        if List.isEmpty refs then
          addNativeParm pname None "self.0"
        else
          addError "_This parameter: shouldn't be referenced in annotation"
// -----------------------------------------------------------------------------------------------------------------------------------------
      |AConst v ->
        match refs with
        |[] ->
          addNativeParm pname None v
        |_ ->
          addError (sprintf "%s parameter: shouldn't be referenced in annotation" pname)
// -----------------------------------------------------------------------------------------------------------------------------------------
      |ANone ->
        match refs with
        |[] ->
          match pty with
          |Ptr(Const(Primitive Void)) ->
            // pass pointer to input data. Due to technical problems input data should be slice
            let gt=newGenType("")
            addSafeParm safeParmName (RBorrow(RSlice(RGeneric(gt,""))))
            addNativeParm pname (Some(safeParmName)) (safeParmName+".as_ptr() as *const _") 
            if mannot<>MAUnsafe then
              addWarning (sprintf "%s parameter: ANone annotation applied to void pointer and method isn't marked as unsafe" pname)
          |Ptr(Primitive Void) ->
            // pass pointer to input data. Due to technical problems input data should be slice
            let gt=newGenType("")
            addSafeParm safeParmName (RMutBorrow(RSlice(RGeneric(gt,""))))
            addNativeParm pname (Some(safeParmName)) (safeParmName+".as_mut_ptr() as *mut _") 
            if mannot<>MAUnsafe then
              addWarning (sprintf "%s parameter: ANone annotation applied to void pointer and method isn't marked as unsafe" pname)
          |_ when isVoidPtr pty ->
            addError (sprintf "%s parameter: ANone annotation cannot be applied to void pointer" pname)
          |_ when (match (removeConst pty) with Ptr(Ptr(_)) -> true |_ -> false) ->
            addError (sprintf "%s parameter: ANone annotation cannot be used with double indirection" pname)
          |Primitive _ ->
            // No processing. Pass as is
            addSafeParm safeParmName (convertTypeToRustNoArray pty pannot) 
            addNativeParm pname (Some(safeParmName)) safeParmName
          |EnumRef e ->  
            addSafeParm safeParmName (RType e)
            if noEnumConversion then
              addNativeParm pname (Some(safeParmName)) (safeParmName) 
            else
              addNativeParm pname (Some(safeParmName)) (safeParmName+".0") 
          |TypedefRef "LPCWSTR" ->
            let locVar=getNextLocVar()
            addLocalVar locVar false (RType "Vec<u16>") (fun m -> "str_to_vec_u16("+safeParmName+")")
            addSafeParm safeParmName (RType "Cow<str>")
            addNativeParm pname (Some(safeParmName)) (locVar+".as_ptr() as LPCWSTR")
          |Ptr(Const(cty)) ->
            addSafeParm safeParmName (RBorrow(convertTypeToRustNoArray cty pannot))
            addNativeParm pname (Some(safeParmName)) safeParmName
          |Ptr(cty) ->
            addSafeParm safeParmName (RMutBorrow(convertTypeToRustNoArray cty pannot))
            addNativeParm pname (Some(safeParmName)) safeParmName
          |Array(cty,num) ->
            // C array in function parameters means pointer to first element of array
            addSafeParm safeParmName (RMutBorrow(RArray(convertTypeToRustNoArray cty pannot,num)))
            addNativeParm pname (Some(safeParmName)) (safeParmName+".as_ptr()")
          |_ -> 
            addSafeParm safeParmName (convertTypeToRustNoArray pty pannot)
            addNativeParm pname (Some(safeParmName)) safeParmName
        |[(rname, OutReturnInterface _, _)] |[(rname, OutReturnKnownInterface _, _)] ->
          () // processed in referenced paramerer
        |[(rname, InOutOfSize _, _)] |[(rname, OutOfSize _, _)] |[(rname, InOfSize _, _)] ->
          // this parameter conveys the size of another parameter
          match pty with
          |Ptr((Primitive _) as uty) |Ptr((TypedefRef _) as uty) ->
            addSafeParmInit safeParmName (convertTypeToRustNoArray pty pannot) (fun m -> "*"+safeParmName+" = mem::size_of_val("+(Map.find rname m)+") as "+(tyToRust uty))
            addNativeParm pname (Some(safeParmName)) safeParmName
          |Primitive _ |TypedefRef _ ->
            addNativeParmFun pname None (fun m -> "mem::size_of_val("+(Map.find rname m)+") as "+(tyToRust pty))
          |_ -> addError ("Unexpected type for "+pname+", referent of [InOutOfSize]"+rname)
        |[(rname, InByteArrayOfSize (_, n), _)] ->
          // this parameter conveys the size of another parameter in n-byte units
          match pty with
          |Ptr((Primitive _) as uty) |Ptr((TypedefRef _) as uty) ->
            addSafeParmInit safeParmName (convertTypeToRustNoArray pty pannot) (fun m -> "*"+safeParmName+" = (mem::size_of_val("+(Map.find rname m)+")/"+n.ToString()+") as "+(tyToRust uty))
            addNativeParm pname (Some(safeParmName)) safeParmName
          |Primitive _ |TypedefRef _ ->
            addNativeParmFun pname None (fun m -> "(mem::size_of_val("+(Map.find rname m)+")/"+n.ToString()+") as "+(tyToRust pty))
          |_ -> addError ("Unexpected type for "+pname+", referent of [InOutOfSize]"+rname)
        |refs when refs |> List.forall (function |(_,InOptionalArrayOfSize _,_) |(_,InArrayOfSize _, _) |(_,OutArrayOfSize _, _) |(_,OutOptionalArrayOfSize _,_) |(_,InComPtrArrayOfSize _,_) -> true |_ -> false)  ->
          // this parameter is the size of input array(s).
          let plist=refs |> 
                      List.map 
                        (function 
                          |(pname, InOptionalArrayOfSize _,_)
                          |(pname, OutOptionalArrayOfSize _,_) -> 
                            Choice1Of2 (fun m -> (Map.find pname m)+".as_ref().map(|a|a.len())")
                          |(pname, InArrayOfSize _,_) |(pname, OutArrayOfSize _,_) |(pname, InComPtrArrayOfSize _,_) ->  
                            Choice2Of2 (fun m -> (Map.find pname m)+".len()")
                          |_ -> raise <| new System.Exception("Unreachable")
                        )
          let init=
            fun m -> 
                match plist with
                |[Choice1Of2 f] -> (f m)+".unwrap_or(0)"
                |[Choice2Of2 f] -> f m
                |_ -> 
                    " same_length(&["+
                        System.String.Join(",",
                            plist |> List.map
                                (function 
                                    |Choice1Of2 f -> f m 
                                    |Choice2Of2 f -> "Some("+(f m)+")")
                        )+"]).expect(\"Arrays must have equal sizes\")"
          match pty with
          |Ptr((Primitive _) as uty) |Ptr((TypedefRef _) as uty) ->
            addSafeParmInit safeParmName (convertTypeToRustNoArray pty pannot) (fun m -> "let *"+safeParmName+" = "+(init m)+" as "+(tyToRust uty))
            addNativeParm pname (Some(safeParmName)) safeParmName
          |Primitive _ |TypedefRef _ ->
            addNativeParmFun pname None (fun m -> (init m)+" as "+(tyToRust pty))
          |_ -> addError ("Unexpected type for size of array parameter "+pname+". The type should be integer or pointer to integer.")
        |refs ->
          addError (sprintf "Unexpected references to parameter %s: %A" pname refs)
// -----------------------------------------------------------------------------------------------------------------------------------------
      // I hate copy/paste, but those little differences prevent me from factoring out similar parts
      |InOut |InOutOptional |InOptional |OutOptional -> 
        // convertToRustTypeNoArray handles difference between InOut and InOutOptional
        match refs with
        |[] ->
          match pty with
          |Ptr(Ptr(Primitive Void)) ->
            match pannot with
            |InOutOptional |OutOptional ->
              let gt=newGenType ""
              addSafeParm safeParmName (ROption(RMutBorrow(RMutPtr(RGeneric(gt,"")))))
              addNativeParm pname (Some(safeParmName)) ("opt_as_mut_ptr(&"+safeParmName+") as *mut *mut _")
            |_ -> 
              addError (sprintf "%s parameter: InOut, InOptional annotation cannot be applied to void pointer. Size is unknown" pname)
          |_ when isVoidPtr pty ->
            addError (sprintf "%s parameter: InOut annotation cannot be applied to void pointer. Size is unknown" pname)
          |_ when (match (removeConst pty) with Ptr(Ptr(_)) -> true |_ -> false) ->
            addError (sprintf "%s parameter: InOut annotation cannot be used with double indirection. Type of object is unknown." pname)
          |Primitive _ |EnumRef _->
            addError (sprintf "%s parameter: InOut parameter should be a pointer" pname)
          |Ptr(Const(cty)) ->
            match pannot with
            |InOptional ->
              addSafeParm safeParmName (ROption(RBorrow(convertTypeToRustNoArray cty ANone)))
              addNativeParm pname (Some(safeParmName)) (safeParmName+".as_ref().map(|p|*p as *const _ as *const _).unwrap_or(ptr::null())")
            |_ ->
              addError (sprintf "%s parameter: InOut parameter should be a pointer to non-const object" pname)
          |Ptr(cty) ->
            match pannot with 
            |InOut ->
              addSafeParm safeParmName (RMutBorrow(convertTypeToRustNoArray cty pannot))
              addNativeParm pname (Some(safeParmName)) safeParmName
            |InOutOptional |InOptional |OutOptional -> 
              addSafeParm safeParmName (ROption(RMutBorrow(convertTypeToRustNoArray cty ANone)))
              addNativeParm pname (Some(safeParmName)) ("opt_as_mut_ptr(&"+safeParmName+")")
            |_ -> raise <| new System.Exception("Unreachable")
          |Array(cty,num) ->
            // C array in function parameters means pointer to first element of array
            match pannot with 
            |InOut ->
              addSafeParm safeParmName (RMutBorrow(RArray(convertTypeToRustNoArray cty pannot,num)))
              addNativeParm pname (Some(safeParmName)) (safeParmName)
            |InOutOptional |InOptional |OutOptional -> 
              addSafeParm safeParmName (ROption((RMutBorrow(RArray(convertTypeToRustNoArray cty pannot,num)))))
              addNativeParm pname (Some(safeParmName)) ("opt_as_mut_ptr(&"+safeParmName+")")
            |_ -> raise <| new System.Exception("Unreachable")
          |_ -> 
            match (pannot, pty) with
            |(InOptional, TypedefRef "HDC") -> // TODO: make this more general
              addSafeParm safeParmName (ROption(convertTypeToRustNoArray pty ANone))
              addNativeParm pname (Some(safeParmName)) ("match "+safeParmName+"{Some(v)=>v, _=>ptr::null_mut() as HDC}")
            |_ ->
              addSafeParm safeParmName (convertTypeToRustNoArray pty pannot)
              addNativeParm pname (Some(safeParmName)) safeParmName
        |[(rname, InOutOfSize _, _)] |[(rname, OutOfSize _, _)] |[(rname, InOfSize _, _)] |[(rname, InByteArrayOfSize _, _)] ->
          // this parameter conveys the size of another parameter
          match pty with
          |Ptr((Primitive _) as uty) |Ptr((TypedefRef _) as uty) ->
            addSafeParmInit safeParmName (convertTypeToRustNoArray pty pannot) (fun m -> "*"+safeParmName+" = mem::size_of_val("+(Map.find rname m)+") as "+(tyToRust uty))
            addNativeParm pname (Some(safeParmName)) safeParmName
          |_ -> addError (sprintf "%s parameter: Unexpected type" rname)
        |refs when refs |> List.forall (function |(_,InOptionalArrayOfSize _,_) |(_,InArrayOfSize _, _) |(_,OutOptionalArrayOfSize _,_) |(_,InComPtrArrayOfSize _,_) -> true |_ -> false)  ->
          // this parameter is the size of input array(s).
          let plist=refs |> 
                      List.map 
                        (function 
                          |(pname, InOptionalArrayOfSize _,_)
                          |(pname, OutOptionalArrayOfSize _,_) -> 
                            Choice1Of2 (fun m -> (Map.find pname m)+".as_ref().map(|a|a.len())")
                          |(pname, InArrayOfSize _,_)
                          |(pname, InComPtrArrayOfSize _,_) ->  
                            Choice2Of2 (fun m -> (Map.find pname m)+".len()")
                          |_ -> 
                            raise <| new System.Exception("Unreachable")
                        )
          let init=
            fun m -> 
                match plist with
                |[Choice1Of2 f] -> (f m)+".unwrap_or(0)"
                |[Choice2Of2 f] -> f m
                |_ -> 
                    let wrap = function 
                        |Choice1Of2 f -> f m 
                        |Choice2Of2 f -> "Some("+(f m)+")"
                    " same_length(&["+System.String.Join(",",plist |> List.map wrap)+"].expect(\"Arrays must have equal sizes\")"
          match pty with
          |Ptr((Primitive _) as uty) |Ptr((TypedefRef _) as uty) ->
            addSafeParmInit safeParmName (convertTypeToRustNoArray pty pannot) (fun m -> "let *"+safeParmName+" = "+(init m)+" as "+(tyToRust uty))
            addNativeParm pname (Some(safeParmName)) safeParmName
          |_ -> addError (sprintf "%s parameter: The type should be pointer to integer." pname)
        |refs ->
          addError (sprintf "%s parameter: Unexpected references %A" pname refs)
// -----------------------------------------------------------------------------------------------------------------------------------------
      |InByteArrayOfSize (p,n) ->
        match refs with
        |[] ->
          match pty with
          |Ptr(Const(Primitive Void)) ->
            //TODO: size of generic type should be divisible by n
            let gt=newGenType("")
            addSafeParm safeParmName (RBorrow(RSlice(RGeneric(gt,""))))
            addNativeParm pname (Some(safeParmName)) (safeParmName+".as_ptr() as *const _") 
          |Ptr(Primitive Void) ->
            //TODO: size of generic type should be divisible by n
            let gt=newGenType("")
            addSafeParm safeParmName (RMutBorrow(RSlice(RGeneric(gt,""))))
            addNativeParm pname (Some(safeParmName)) (safeParmName+".as_mut_ptr() as *mut _") 
          |Ptr(Const(uty)) ->
            let ruty=convertTypeToRustNoArray uty pannot
            addSafeParm safeParmName (RBorrow(RSlice(ruty)))
            addNativeParm pname (Some(safeParmName)) (safeParmName+".as_ptr() as *const _") 
          |Ptr(uty) ->
            let ruty=convertTypeToRustNoArray uty pannot
            addSafeParm safeParmName (RMutBorrow(RSlice(ruty)))
            addNativeParm pname (Some(safeParmName)) (safeParmName+".as_mut_ptr() as *mut _") 
          |_ ->
            addError (sprintf "%s parameter: Unexpected type %A for [InByteArrayOfSize]. The type should be 'const void *'" pname pty)        
        |_ ->
          addError (sprintf "%s parameter: Should have no refs. But they are %A" pname refs)        
// -----------------------------------------------------------------------------------------------------------------------------------------
      |InOfSize p ->
        match refs with
        |[] ->
          match pty with
          |Ptr(Const(Primitive Void)) ->
            let gt=newGenType("")
            addSafeParm safeParmName (RBorrow(RGeneric(gt,"")))
            addNativeParm pname (Some(safeParmName)) (safeParmName+" as *const _ as *const _") 
          |Ptr(Const(cty)) ->
            addSafeParm safeParmName (RBorrow(convertTypeToRustNoArray cty pannot))
            addNativeParm pname (Some(safeParmName)) (safeParmName+" as *const _ as *const _") 
          |_ ->
            addError (sprintf "%s parameter: Unexpected type %A for [InByteArrayOfSize]. The type should be 'const void *'" pname pty)        
        |_ ->
          addError (sprintf "%s parameter: Should have no refs. But they are %A" pname refs)        
// -----------------------------------------------------------------------------------------------------------------------------------------
      |InOutOfSize p ->
        match refs with
        |[] ->
          addSafeParm safeParmName (convertTypeToRustNoArray pty pannot)
          addNativeParm pname (Some(safeParmName)) (safeParmName+" as *mut _ as *mut _")
        |_ ->
          addError (sprintf "%s parameter: Should have no refs. But they are %A" pname refs)
// -----------------------------------------------------------------------------------------------------------------------------------------
      |InOutReturn ->
        match refs with 
        |[(rname,OutOptionalOfSize _,_)] ->
          match pty with
          |Ptr(uty) ->
            let lvtype=convertTypeToRustNoArray uty ANone
            let lv=newLocalVar true lvtype (fun m -> (Map.find rname m)+".as_ref().map(|v|mem::size_of_val(*v)).unwrap_or(0) as "+(tyToRust uty))
            addNativeParm pname None ("&mut "+lv)
            addReturnExpression lv lvtype
          |_ ->
            addError (sprintf "%s parameter: must be a pointer." pname)
        |[(rname,OutOptionalArrayOfSize _,_)] ->
          match pty with
          |Ptr(uty) ->
            let lvtype=convertTypeToRustNoArray uty ANone
            let lv=newLocalVar true lvtype (fun m -> (Map.find rname m)+".as_ref().map(|v|v.len()).unwrap_or(0) as "+(tyToRust uty))
            addNativeParm pname None ("&mut "+lv)
            addReturnExpression lv lvtype
          |_ ->
            addError (sprintf "%s parameter: must be a pointer." pname)
        |_ ->
          addError (sprintf "%s parameter: InOutReturn should be referenced by OutOptionalOfSize or OutOptionalArrayOfSize. But references are %A" pname refs)
// -----------------------------------------------------------------------------------------------------------------------------------------
      |OutOptionalOfSize p ->
        match refs with 
        |[] ->
          let gt=newGenType ""
          addSafeParm safeParmName (ROption(RMutBorrow(RGeneric(gt,""))))
          addNativeParm pname (Some(safeParmName)) (safeParmName+".map(|v|v as *const _ as *mut _).unwrap_or(ptr::null_mut())")
        |_ ->
          addError (sprintf "%s parameter: OutOptionalOfSize shouldn't have references. But references are %A" pname refs)
// -----------------------------------------------------------------------------------------------------------------------------------------
      |InArrayOfSize p |InOptionalArrayOfSize p |OutArrayOfSize p |OutOptionalArrayOfSize p |InOutArrayOfSize p ->
        match refs with
        |[] ->
          match pty with
          |Ptr(Const(uty)) ->
            let ruty=convertTypeToRustNoArray uty pannot
            match pannot with
            |InArrayOfSize _ ->
              addSafeParm safeParmName (RBorrow(RSlice(ruty)))
              addNativeParm pname (Some(safeParmName)) ("slice_as_ptr("+safeParmName+")")
            |InOptionalArrayOfSize _ ->
              addSafeParm safeParmName (ROption(RBorrow(RSlice(ruty))))
              addNativeParm pname (Some(safeParmName)) ("opt_arr_as_ptr(&"+safeParmName+") as *const _") // TODO: Use FFI option optimization
            |_ ->
              addError (sprintf "%s parameter: out parameter can't be const" pname)
          |Ptr(uty) ->
            let ruty=convertTypeToRustNoArray uty ANone
            match pannot with
            |InArrayOfSize _ |OutArrayOfSize _ |InOutArrayOfSize _ ->
              addSafeParm safeParmName (RMutBorrow(RSlice(ruty)))
              addNativeParm pname (Some(safeParmName)) ("slice_as_mut_ptr("+safeParmName+")")
            |_ ->
              addSafeParm safeParmName (ROption(RMutBorrow(RSlice(ruty))))
              addNativeParm pname (Some(safeParmName)) ("opt_arr_as_mut_ptr(&"+safeParmName+") as *mut _") // TODO: Use FFI option optimization
          |_ -> addError (sprintf "%s parameter: Unexpected type" pname)
        |_ ->
          addError (sprintf "%s parameter: Should have no refs. But they are %A" pname refs)
// -----------------------------------------------------------------------------------------------------------------------------------------
      |InComPtrArrayOfSize p->
        // I believe Rust representation of COM-pointer is larger than a pointer, because of drop-flag.
        // So we need to convert it to array of pointers
        match refs with
        |[] ->
          match pty with
          |Ptr(Const(Ptr(StructRef s)))
          |Ptr(Ptr(StructRef s)) ->
            //let riname=s.Substring(1,s.Length-1)
            // TODO: impement traits for interfaces
            let gt=newGenType "HasIID"
            addSafeParm safeParmName (RBorrow(RSlice(RBorrow(RGeneric(gt,"HasIID")))))
            let lv=newLocalVar true (RVec(RMutPtr(RType "IUnknown"))) (fun _ -> safeParmName+".iter().map(|o|o.iptr()).collect()")
            addNativeParm pname (Some(safeParmName)) (lv+".as_mut_ptr() as *mut *mut _ as *mut *mut _")
          |_ -> addError (sprintf "%s parameter: Unexpected type" pname)
        |_ ->
          addError (sprintf "%s parameter: Should have no refs. But they are %A" pname refs)
// -----------------------------------------------------------------------------------------------------------------------------------------
      |InComPtr ->
        // Pointer to COM-object of exact type
        match refs with 
        |[] ->
          match pty with
          |Ptr(StructRef ciname) ->
            // Let's strip 'I' part
            //let iname=ciname.Substring(1,ciname.Length-1)
            // TODO: impement traits for interfaces
            let gt=newGenType "HasIID"
            let rty=RBorrow(RGeneric(gt,"HasIID"))
            addSafeParm safeParmName rty
            addNativeParm pname (Some(safeParmName)) (safeParmName+".iptr() as *mut _ as *mut _ ")
          |_ -> addError (sprintf "%s parameter: Unexpected type" pname)
        |_ ->
          addError (sprintf "%s parameter: InComPtr shouldn't have references. But references are %A" pname refs)
// -----------------------------------------------------------------------------------------------------------------------------------------
      |InOptionalComPtr ->
        // Pointer to COM-object of exact type
        match refs with 
        |[] ->
          match pty with
          |Ptr(StructRef ciname) ->
            // Let's strip 'I' part
            let iname=ciname.Substring(1,ciname.Length-1)
            let rty=ROption(RBorrow(RType iname))
            addSafeParm safeParmName rty
            addNativeParm pname (Some(safeParmName)) (safeParmName+".map(|i|i.iptr()).unwrap_or(ptr::null_mut()) as *mut _ as *mut _")
          |_ -> addError (sprintf "%s parameter: Unexpected type" pname)
        |_ ->
          addError (sprintf "%s parameter: InOptionalComPtr shouldn't have references. But references are %A" pname refs)
// -----------------------------------------------------------------------------------------------------------------------------------------
      |OutReturn ->
        match refs with 
        |[] ->
          match pty with
          |Ptr(uty) ->
            let rty=(convertTypeToRustNoArray uty pannot)
            let lv=newLocalVar true rty (fun m -> "unsafe {mem::uninitialized::<_>()}")
            addNativeParm pname None ("&mut "+lv+" as *mut _ as *mut _")
            addReturnExpression lv rty
          |_ -> addError (sprintf "%s parameter: Unexpected type" pname)
        |_ ->
          addError (sprintf "%s parameter: OutReturn shouldn't have references. But references are %A" pname refs)
// -----------------------------------------------------------------------------------------------------------------------------------------
      |OutReturnBarePointer ->
        match refs with 
        |[] ->
          match pty with
          |Ptr(Ptr(_)) ->
            let rty=RMutPtr(RType "()")
            let lv=newLocalVar true rty (fun m -> "unsafe {mem::uninitialized::<_>()}")
            addNativeParm pname None ("&mut "+lv+" as *mut _ as *mut _")
            addReturnExpression lv rty
          |_ -> addError (sprintf "%s parameter: Unexpected type" pname)
        |_ ->
          addError (sprintf "%s parameter: OutReturnBarePointer shouldn't have references. But references are %A" pname refs)
// -----------------------------------------------------------------------------------------------------------------------------------------
      |OutReturnComPtr ->
        match refs with 
        |[] ->
          match pty with
          |Ptr(Ptr(StructRef s)) ->
            // s should be a name of COM-interface. TODO: Check that it is
            let rtName=s.Substring(1,s.Length-1) // Corresponding rust wrapper have name without leading 'I'
            let rty=RType rtName
            let lv=newLocalVar true (RMutPtr(RType s)) (fun m -> "ptr::null_mut()")
            addNativeParm pname None ("&mut "+lv+" as *mut *mut _")
            addReturnExpression (rtName+"::new("+lv+" as *mut _)") rty
          |_ -> addError (sprintf "%s parameter: Unexpected type" pname)
        |_ ->
          addError (sprintf "%s parameter: OutReturnComPtr shouldn't have references. But references are %A" pname refs)
// -----------------------------------------------------------------------------------------------------------------------------------------
      |OutReturnOptionalComPtr ->
        match refs with 
        |[] ->
          match pty with
          |Ptr(Ptr(StructRef s)) ->
            // s should be a name of COM-interface. TODO: Check that it is
            let rtName=s.Substring(1,s.Length-1) // Corresponding rust wrapper have name without leading 'I'
            let rty=ROption(RType rtName)
            let lv=newLocalVar true (RMutPtr(RType s)) (fun m -> "ptr::null_mut()")
            addNativeParm pname None ("&mut "+lv+" as *mut *mut _")
            addReturnExpression ("if "+lv+"==ptr::null_mut() {None} else {Some("+rtName+"::new("+lv+" as *mut _))}") rty
          |_ -> addError (sprintf "%s parameter: Unexpected type" pname)
        |_ ->
          addError (sprintf "%s parameter: OutReturnOptionalComPtr shouldn't have references. But references are %A" pname refs)
// -----------------------------------------------------------------------------------------------------------------------------------------
      |OutReturnCombine(sname, field) ->
        match refs with 
        |[] ->
          match pty with
          |Ptr(uty) ->
            let rty=RType sname
            // local variable is added elsewhere
            let lv = Map.find sname !sname2lv
            addNativeParm pname None ("&mut ("+lv+"."+field+") as *mut _ as *mut _")
          |_ -> addError (sprintf "%s parameter: Unexpected type" pname)
        |_ ->
          addError (sprintf "%s parameter: OutReturnCombine shouldn't have references. But references are %A" pname refs)
// -----------------------------------------------------------------------------------------------------------------------------------------
      |OutReturnInterface(rname) ->
        match refs with 
        |[] ->
          match pty with
          |Ptr(uty) ->
            // TODO: check pty. 
            let gt=newGenType "HasIID"
            let rty=RGeneric(gt, "HasIID")
            let lv=newLocalVar true (RMutPtr(RType "IUnknown")) (fun m -> "ptr::null_mut()")
            addNativeParm pname None ("&mut "+lv+" as *mut *mut _ as *mut *mut c_void")
            addNativeParm rname None (gt+"::iid()")
            addReturnExpression (gt+"::new("+lv+")") rty
          |_ -> addError (sprintf "%s parameter: Unexpected type" pname)
        |_ ->
          addError (sprintf "%s parameter: OutReturnInterface shouldn't have references. But references are %A" pname refs)
// -----------------------------------------------------------------------------------------------------------------------------------------
      |OutReturnKnownInterface(rname,iname) ->
        match refs with 
        |[] ->
          match pty with
          |Ptr(uty) ->
            // TODO: check pty. 
            let rty=RType iname
            let lv=newLocalVar true (RMutPtr(RType "IUnknown")) (fun m -> "ptr::null_mut()")
            addNativeParm pname None ("&mut "+lv+" as *mut *mut _ as *mut *mut c_void")
            addNativeParm rname None (iname+"::iid()")
            addReturnExpression (iname+"::new("+lv+")") rty
          |_ -> addError (sprintf "%s parameter: Unexpected type" pname)
        |_ ->
          addError (sprintf "%s parameter: OutReturnKnownInterface shouldn't have references. But references are %A" pname refs)
// -----------------------------------------------------------------------------------------------------------------------------------------
      |InIUnknown ->
        addError (sprintf "%s parameter: InIUnknown is not implemented" pname)
// -----------------------------------------------------------------------------------------------------------------------------------------
      |_ -> raise <| new System.Exception("Unimplemented")
  
    if not <| List.isEmpty !errors then
      ([],!errors)
    else
      // map from native parm name to safeparm name
      let np2sp = !nativeParms |> Map.toSeq |> Seq.choose (function |(np,(Some(sp),_)) -> Some(np,sp) |_ -> None) |> Map.ofSeq

      let (returnVal,returnType)=
        // nrty - normalized return type
        let nrty=
          match List.last parms with
          |("__ret_val",OutReturn,Ptr(_)) ->
            // Artefact of brocken C-interface
            Primitive Void
          |_ -> rty
        // For multiple return values combine them into tuple
        // Function return tuple (return expression : string, type of return expression : RustType)
        let mrv2tuple mrv=
          ("("+System.String.Join(", ", List.map fst mrv)+")", RType("("+System.String.Join(", ", List.map (snd >> rustTypeToString) mrv)+")"))
        match nrty with
        |Ptr(Const(uty)) ->
          ("_hr", RConstPtr(RType(tyToRust uty)))
        |TypedefRef "HRESULT" ->
          match !returnVals |> Set.toList with
          |[] ->
            ("hr2ret(_hr,_hr)",RType "HResult<HRESULT>")
          |[(ex,t)] ->
            ("hr2ret(_hr,"+ex+")", RType ("HResult<"+(rustTypeToString t)+">"))
          |mrv ->
            let (ex,t)=mrv2tuple mrv
            ("hr2ret(_hr,"+ex+")", RType ("HResult<"+(rustTypeToString t)+">"))
        |StructRef sname |TypedefRef sname |EnumRef sname ->
          match !returnVals |> Set.toList with
          |[] ->
            ("_hr",RType sname)
          |[(ex,t)] ->
            ("(_hr,"+ex+")", RType ("("+sname+", "+(rustTypeToString t)+")"))
          |mrv ->
            let (ex,t)=mrv2tuple (("hr", RType sname) :: mrv)
            ("hr2ret(_hr,"+ex+")", RType ("HResult<"+(rustTypeToString t)+">"))
        |Primitive Void ->
          match !returnVals |> Set.toList with
          |[] ->
            ("()",RType "()")
          |[(ex,t)] ->
            (ex, t)
          |mrv ->
            mrv2tuple mrv
        |Ptr(StructRef retInterface) when mannot = MAReturnsInterface ->
            let rustInterfaceName = retInterface.Substring(1)
            (rustInterfaceName+"::new(_hr as *mut _)", RType rustInterfaceName)
        |_ -> raise <| new System.Exception("Unexpected return type in "+clname+"::"+mname)

      let transformedNativeParms = 
        !nativeParms |> Map.map (fun k (_,f) -> (k, f np2sp))

      let transformedSafeParms =
        !safeParms |> Map.map (fun k (ty, f) -> (k, ty, Option.map (fun f -> f np2sp) f))

      let orderedNativeParms=
        parms |> List.map (fun (nname,_,_) -> Map.find nname transformedNativeParms)

      let orderedSafeParms = 
        List.concat [
          // for each native parm add corresponding safe parm
          parms |> List.map (fun (pname,_,_) -> pname) 
            |> List.filter (fun pname -> Map.containsKey pname np2sp) 
            |> List.map (fun pname -> let sname=Map.find pname np2sp in Map.find sname transformedSafeParms);
          // add orphaned safe parms
          transformedSafeParms |> Map.toList 
            |> List.map snd 
            |> List.filter (fun (sname,_,_) -> Map.forall (fun _ v -> v <> sname) np2sp)]

      ([{ className = clname
          implClassName = implclass
          nativeName = nname
          safeName = toRustMethodName mname
          unsafe = mannot=MAUnsafe
          genericTypes = !genTypes
          localVars = !localVars |> Map.map (fun _ (mut,rty,init) -> {mut=mut;ty=rty;initExpression=init(np2sp)}) 
          nativeParms = orderedNativeParms
          safeParms = orderedSafeParms
          returnVal = returnVal
          returnType = returnType
          }, !warnings], !errors)

let bprintfn sb t=
  let pf=bprintf sb t
  (fun x -> 
    pf x  
    sb.AppendLine() |> ignore
    )

// Removes TypeSelector annotation, generates specialized methods
let preGenerateMethod clname noEnumConversion (implclass, (mname, mannot, parms, rty : CTypeDesc) as methoddesc)=
  match mannot with
  |MAIUnknown | MADontImplement -> 
    ""
  |MACustom impl -> 
    impl
  |MANone |MAMangle _ |MAReturnsInterface |MAUnsafe ->
    let rustName = 
      match mannot with
      |MAMangle name ->
        name
      |_ ->
        mname
    let routings=
      match parms |> List.tryFind (function |(_,TypeSelector _,_) -> true |_ ->false) with
      |Some (sname, TypeSelector (tname,slist), _) ->
        slist |> 
          List.map (
            fun (suffix, sval, stype, sannot) -> 
              let parms1=
                parms |> List.map
                  (fun (pname, pannot, pty) -> 
                    if pname=sname then
                      (pname, AConst sval, pty)
                    else if pname=tname then
                        (pname, sannot, stype) //Const (Ptr (TypedefRef stype)))
                    else
                      (pname, pannot, pty)
                  )
              let gparms=(clname, rustName+suffix, mname, mannot,parms1,rty)
              generateRouting gparms noEnumConversion implclass)
      |Some(_) ->
        raise <| new System.Exception("Unreachable")
      |None ->
        [generateRouting (clname, rustName, mname, mannot, parms, rty) noEnumConversion implclass]
    let sb=new System.Text.StringBuilder()
    bprintfn sb "//  Method %s" mname
    for (rws,errors) in routings do
      for e in errors do
        bprintfn sb "//  Error: %s" e
      for (routing,warnings) in rws do
        for w in warnings do
          bprintfn sb "//  Warning: %s" w
        let (mtext,_) = generateMethodFromRouting routing
        bprintfn sb "%s" mtext
    sb.ToString()
  

let generateMethods clname noEnumConversion methods=
  let sb=new System.Text.StringBuilder()
  for methoddesc in methods do
    sb.Append(preGenerateMethod clname noEnumConversion methoddesc) |> ignore
  sb.ToString()

let addCombiningStructs (sb:System.Text.StringBuilder) interfaceAnnotations=
  let apl=appendLine sb
  let structFields=
    interfaceAnnotations |> List.fold 
      (fun s (_,_,_,mannots) ->
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
      apl <| "  pub "+field+" : "+(tyToRust (derefCType pty))+","
    apl "}"
    apl ""
  ()

let autoheader = "\
// This file is autogenerated

use utils::*;
"

let joinMaps a b =
  Map.ofSeq (Seq.concat [Map.toSeq a; Map.toSeq b])

let safeInterfaceGen (header:string) (uses_CanBeFreakingNull: string seq) allInterfaces noEnumConversion (types:Map<string,CTypeDesc*CodeLocation>, enums:Map<string,CTypeDesc*CodeLocation>, structs:Map<string,CTypeDesc*CodeLocation>, funcs:Map<string,CTypeDesc*CodeLocation>, iids:Map<string,CodeLocation>, defines:Map<string, MacroConst*string*CodeLocation>) 
                        (annotations: Annotations) =
  let uses = if uses_CanBeFreakingNull = null then Seq.empty else uses_CanBeFreakingNull
  let dependencies = ref Set.empty
  let sb=new System.Text.StringBuilder()
  let vtbls=structs |> List.ofSeq |> List.collect (fun (KeyValue(name,(ty,_))) -> getVtbl structs ty |> o2l |> List.map (fun vtbl -> (name,vtbl))) 
  match sanityCheck vtbls (annotations.interfaces) with
  |Some(interfaceAnnotations) ->
    let iamap' = interfaceAnnotations |> Seq.map (fun (iname, bas, iannot, meths) -> ("I"+iname, (bas, meths, header))) |> Map.ofSeq
    let iamap = joinMaps allInterfaces iamap'
    let apl = appendLine sb
    apl autoheader
    uses |> Seq.iter (fun modul -> apl <| "use "+modul+"::*;")
    addCombiningStructs sb interfaceAnnotations
    for (iname, bas, iannot, methods) in interfaceAnnotations do
      match iannot with
      |IAManual -> ()
      |IAAutogen opts ->
        // if base class is in another module, add dependency
        match Map.tryFind bas iamap with
        |Some(_,_,basHeader) when basHeader<>header -> dependencies := Set.add (basHeader+"_safe") !dependencies
        |Some(_) -> ()
        |None -> 
          if bas<>"IUnknown" && bas<>"" then
            printfn "No header file for base class in %s:%s" iname bas
        let rec ancMeths bName = 
          if bName = "IUnknown" || bName = "" then
            []
          else 
            match Map.tryFind bName iamap with
            |Some(bas, meths, header) ->
              List.concat [ancMeths bas; meths |> List.map (fun m -> (bName,m))]
            |None -> 
              let error = sprintf "Error: Generating interface for %s. Base class %s is not defined" iname bName
              printfn "%s" error
              raise <| new System.Exception(error)
        let methods1 = ancMeths ("I"+iname)
        let implSend = 
          opts |> Set.fold (
              fun s opt ->
                match opt with
                |IOSend -> "unsafe impl Send for "+iname+" {}\r\n" + s
                |IOSync -> "unsafe impl Sync for "+iname+" {}\r\n" + s
            ) "" 
        let (derive, implDropClone) = 
            if bas="" then
                // If interface doesn't have base class, then it means it's not COM-compliant
                ("#[derive(Clone)]\r\n", "")
            else
                ("", "\
impl Drop for "+iname+" {
  fn drop(&mut self) {
    release_com_ptr(self)
  }
}

impl Clone for "+iname+" {
  fn clone(&self) -> Self {
    clone_com_ptr(self)
  }
}

"                   )
        apl <| derive 
        apl <| "pub struct "+iname+"(*mut I"+iname+");"
        apl <| implSend
        apl <| "impl HasIID for "+iname+" {"
        apl <| "  fn iid() -> REFGUID { &IID_I"+iname+" }"
        apl <| "  fn new(pp_vtbl : *mut IUnknown) -> Self { "+iname+"(pp_vtbl as *mut _ as *mut I"+iname+") }"
        apl <| "  fn iptr(&self) -> *mut IUnknown { self.0 as *mut _ as  *mut IUnknown}"
        apl <| "}"
        apl <| implDropClone
        apl <| ""
        apl <| "impl "+iname+" {"
        apl <|   (generateMethods ("I"+iname) noEnumConversion methods1 |> indentBy "  ")
        apl <| "}"

    (sb.ToString(), iamap, !dependencies)
  |None -> 
    printfn "Sanity check failed. No Rust interfaces generated"
    ("", allInterfaces, !dependencies)
