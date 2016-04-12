module sysgen

open libclang
open cdesc
open rustdesc
open annotations

let reprC: RustAttribute=("Repr",[("C","")])

let makeStruct name ses=
  let rs=
    ses |> List.map (
      fun (CStructElem(ename, ty, bw)) ->
        (ename, tyToRty ty, RAPublic)
    )
  [RustItem(name, REStruct rs,RAPublic, [reprC])]

let makeInterface annotations name ses=
  [] //RustItem(name, REInterface ,RAPublic, [reprC])

let convertToRust (types:Map<string,CTypeDesc*CodeLocation>,enums:Map<string,CTypeDesc*CodeLocation>,structs:Map<string,CTypeDesc*CodeLocation>,funcs:Map<string,CTypeDesc*CodeLocation>, iids:Set<string>, annotations)=
  let rtypes=
    types |> List.ofSeq |> 
      List.map (
        fun (KeyValue(name, (ty, _))) ->
          let rty=tyToRty ty
          RustItem (name, RETypeDef rty, RAPublic, [reprC])
        )
  let renums=
    enums |> List.ofSeq |> 
      List.collect (
        fun (KeyValue(name, ty)) ->
          match ty with
          |Enum(underlyingType=ut; values=vals), _ ->
            RustItem (name, RETypeDef(RTupleStruct([tyToRty (Primitive ut)])),RAPublic,[reprC]) :: 
              (vals |> List.map (
                fun (ename, eVal) ->
                  RustItem(ename, REConst(RType name, sprintf "%s(%d)" name eVal, eVal),RAPublic, [])
                ))
          |_ -> raise <| new System.Exception("Unexpected type of enum "+name)
      )
  let rstructs=
    let makeStructOrInterface (KeyValue(name,(ty,_))) =
      match getVtbl structs ty with
      |Some vtbl ->
        // Interface
        makeInterface annotations name vtbl
      |None ->
        // Struct
        match ty with
        |Struct (ses,bas) -> makeStruct name ses
        |_ -> raise <| new System.Exception("Unexpected type")
    structs |> List.ofSeq |>
      List.collect makeStructOrInterface
  let rfuncs=[]
  let riids=[]
  List.concat [rtypes; renums; rstructs; rfuncs; riids]

let isOnlyOneBitSet (v:uint64)=
  v<>0UL && (v &&& (v-1UL))=0UL

let maxLineLen=99
let eolAfter=80

let wrapOn items (indent:string) eolAfter maxLineLen=
  let lines = ref []
  let ll=
    Seq.tail items |> Seq.fold
      (fun cl p ->
        if cl="" then
          let c = indent + p
          if c.Length>eolAfter then
            lines := c :: !lines
            ""
          else
            c
        else
          let c=cl+" "+p
          if c.Length>maxLineLen then
            lines := cl :: !lines
            indent+p
          else if c.Length>eolAfter then
            lines := c :: !lines
            ""
          else
            c
      ) (Seq.head items)
  if ll<>"" then
    lines := ll :: !lines
  lines := List.rev !lines
  !lines  

let winapiGen (types:Map<string,CTypeDesc*CodeLocation>, 
                enums:Map<string,CTypeDesc*CodeLocation>,
                  structs:Map<string,CTypeDesc*CodeLocation>,
                    funcs:Map<string,CTypeDesc*CodeLocation>, 
                      iids:Map<string,CodeLocation>,
                       defines:Map<string, MacroConst*string*CodeLocation>) 
                        (annotations: Annotations) : Map<string,System.String>=
  let uncopyableStructs=
    let rec isStructUncopyableByItself (ses : CStructElem list)=
      ses 
        |> List.exists 
          (function 
            |CStructElem(_,Array(_,n),_) when n>32L -> true 
            |CStructElem(_,Struct(ses,bas),_) -> isStructUncopyableByItself ses
            |CStructElem(_,Union(ues),_) -> ues |> List.map fst |> isStructUncopyableByItself
            |_ -> false)
    let rec isStructUncopyable s=
      match s with
      |Struct(ses, bas) ->
        (isStructUncopyableByItself ses) || 
          ses |> List.exists (
            function 
              |CStructElem(_,StructRef sname,_) -> 
                match Map.find sname structs with 
                |(Struct(_) as s,_) -> isStructUncopyable s
                |_ -> false
              |CStructElem(_,(Struct(ses,bas) as sub),_) -> 
                isStructUncopyable sub
              |CStructElem(_,(Union(ues) as sub),_) -> 
                ues |> List.map fst |> fun ses -> Struct (ses, "") |> isStructUncopyable
              |_ -> false)
      |_ -> false
    structs |> Map.toSeq |> Seq.filter (snd >> fst >> isStructUncopyable) |> Seq.map fst |> Set.ofSeq

  let rsfilename s=
    (System.IO.Path.GetFileNameWithoutExtension s)+".rs"

  let file2sb=ref Map.empty

  let dxann=annotations.interfaces |> Seq.map (fun (a,b,c,d) -> (a,(b,c,d))) |> Map.ofSeq

  let apl (f:System.String) s=
    let sb=
      match Map.tryFind f !file2sb with
      |Some(sb:System.Text.StringBuilder) -> 
        sb
      |None ->
        let sb=new System.Text.StringBuilder()
        let text=sprintf @"// Copyright © 2016; Dmitry Roschin
// Licensed under the Apache License, Version 2.0, <LICENSE-APACHE or
// http://apache.org/licenses/LICENSE-2.0> or the MIT license <LICENSE-MIT or
// http://opensource.org/licenses/MIT>, at your option. This file may not be
// copied, modified, or distributed except according to those terms.
//! Mappings for the contents of %s.h" (f.Substring(0,f.Length-3))
        sb.AppendLine(text) |> ignore
        sb.AppendLine() |> ignore
        file2sb := Map.add f sb !file2sb
        sb
    sb.AppendLine(s) |> ignore

  let createEnums()=
    for (name,uty,vals,(fname,_,_,_)) in enums |> Seq.choose (function |KeyValue(name,(Enum(uty, vals),loc)) -> Some(name,uty,vals,loc) |_ -> None) do
      let f=rsfilename fname
      let apl=apl f
      let annot=match Map.tryFind name (annotations.enums) with |Some(v) ->v |None -> EAFlags
      let convhex=fun (v:uint64) -> "0x" + v.ToString("X")
      let convdec=fun (v:uint64) -> v.ToString()
      let (macro,convf)=if annot=EAEnum then ("ENUM!", convdec)  else ("FLAGS!", convhex)
      apl (macro+"{ enum "+name+" {")
      for (cname,v) in vals do
        if cname.Contains("_FORCE_DWORD") then
          apl ("    " + cname + " = " + (convhex v) + ",")
        else
          apl ("    " + cname + " = " + (convf v) + ",")
      apl "}}"
      apl ""

  let libcTypeNames=Set.ofList ["BOOL";"LPCWSTR";"HMODULE"
      ;"GUID";"LARGE_INTEGER";"LPVOID"
      ;"WCHAR";"BYTE";"LPCVOID";"LONG_PTR";"WORD";"SIZE_T"
      ;"SECURITY_ATTRIBUTES";"HANDLE";"DWORD";"LPCSTR";"LONG"
      ;"IUnknown"] 
// ----------------- Create structs ----------------------------------
  let outputStructDef apl name ses uncopyable=
//    if uncopyable then
//      apl "#[repr(C)] #[derive(Copy)]"
//    else
//      apl "#[repr(C)] #[derive(Clone, Copy, Debug)]"
    apl <| sprintf "STRUCT!{struct %s {" name
    for (fname,fty) in ses |> Seq.choose(function |CStructElem(fname,fty,None)->Some(fname,fty) |_-> raise <| new System.Exception("Bitfields aren't supported")) do
      apl <| System.String.Format("    {0}: {1},", fname, tyToRustGlobal fty)
    apl "}}"
    apl ""
//    if uncopyable then
//      apl <| sprintf "impl Clone for %s {" name
//      apl "    fn clone(&self) -> Self { *self }"
//      apl "}"
//      apl ""

  let createStructs()=
    for (name,bas,sfields,(fname,_,_,_)) in structs |> Seq.choose(function |KeyValue(name, (Struct(sfields,bas),loc)) -> Some(name,bas,sfields,loc) |_ -> None) do
      let f=rsfilename fname
      let apl=apl f
      if Set.contains name libcTypeNames || Map.containsKey (name+"Vtbl") dxann then
        ()
      else
        let nonInterface=List.forall (function |CStructElem(_,Ptr(Function(_)),_) -> false |_ ->true) sfields
        if nonInterface then
          let uncopyable=Set.contains name uncopyableStructs
          // check precondition. Unnamed unions within unnamed unions isn't supported
          let checkPrecond ty=
            let rec secondLevel ty=
              match ty with
              |Union(_) -> raise <| new System.Exception("Unnamed unions within unnamed unions isn't supported")
              |_ -> let (stys, _) = subtypes ty in List.iter secondLevel stys
            let rec firstLevel ty=
              let nextfn=
                match ty with
                |Union(_) -> secondLevel
                |_ -> firstLevel
              let (stys, _) = subtypes ty
              List.iter nextfn stys
            firstLevel ty
          checkPrecond (Struct(sfields,""))
          // let's create proxies for unnamed structs in unions
          let unions=ref []
          let sfields' =
            sfields |> List.map
              (fun (CStructElem(fname, fty, bw)) ->
                let fname' = if fname="" then "u" else fname
                let fty' =
                  match fty with
                  |Union(ues) ->
                    // for each union element, I need to create UNION!(base_type, field, selector, selector_mut, selector_type)
                    let ues'= 
                      ues |> List.map
                       (fun ((CStructElem(sname, stype, bw), sz) as ue) ->
                          match stype with
                          |Struct(ses,bas) ->
                            // anonymous struct in union. create proxy.
                            let proxyname=name+"_"+sname;
                            outputStructDef apl proxyname ses uncopyable
                            unions := (name, fname', sname, sname+"_mut", proxyname) :: !unions
                            (CStructElem(sname, StructRef proxyname, bw), sz)
                          |_ ->
                            unions := (name, fname', sname, sname+"_mut", tyToRustGlobal stype) :: !unions
                            ue
                          )
                    Union(ues')
                  |_ -> fty
                CStructElem(fname', fty', bw)
              )
          // I need to find largest element in union for each compile target 
          let compileTargets=
            sfields' |> List.collect (function |CStructElem(_,Union(ues),_) -> List.collect (snd >> (List.map fst)) ues  |_ -> []) |> Set.ofList |> List.ofSeq
          if List.isEmpty compileTargets then
            //no unions
            outputStructDef apl name sfields' uncopyable
          else
            // split by compile targets
            let variants=
              compileTargets 
                |> List.map 
                  (fun ct ->
                      let selectCT=
                        sfields' |> 
                          List.map 
                            (fun (CStructElem(fname, ty, bw) as el) ->
                              match ty with
                              |Union(ues) ->
                                let uname = if fname="" then "u" else fname
                                let (CStructElem(_,ty',_), _) = 
                                  ues |> List.maxBy 
                                    (fun (_, sz) ->
                                      sz |> List.find (fst >> ((=)ct)) |> snd
                                    )
                                CStructElem(uname, ty', bw)
                              |_ -> el)
                      (ct, selectCT)
                  )
            // check if all variants are the same
            if variants 
              |> List.forall
                (fun (ct1, sfs1) ->
                    variants 
                      |> List.forall 
                        (fun (ct2, sfs2) ->
                          if ct1=ct2 then true
                          else
                            List.forall2 (fun (CStructElem(_, ty1, _)) (CStructElem(_, ty2, _)) -> ty1=ty2) sfs1 sfs2
                        ) 
                ) then
              outputStructDef apl name (variants |> List.head |> snd) uncopyable
            else
              for (ct, sfs) in variants do
                match ct with
                |TargetX86 -> 
                  apl @"#[cfg(target_pointer_width = ""32"")]"
                |TargetX64 -> 
                  apl @"#[cfg(target_pointer_width = ""64"")]"
                |TargetUnknown ->
                  raise <| new System.Exception("TagetUnknown shouldn't be here")  
                outputStructDef apl name sfs uncopyable
                
          match !unions with
          |[] -> ()
          |_ ->
            for (name, fname, sname, sname_mut, ty) in !unions do
              let items=["UNION!("+name+","; fname+","; sname+","; sname_mut+","; ty+");"]
              let lines=wrapOn items "    " eolAfter maxLineLen
              for line in lines do
                apl line
            apl ""
        else
          let (_,basename,fns)=Map.find name dxann
          let fnset=fns |> Seq.map (fun (fn,_,_) -> fn) |> Set.ofSeq
          apl "RIDL!("
          if basename="" then
            apl <| sprintf "interface %s(%s) {" (name.Substring(0,name.Length-4)) name
          else
            apl <| sprintf "interface %s(%s): %s(%s) {" (name.Substring(0,name.Length-4)) name (basename.Substring(0, basename.Length-4)) basename

          let fseq=
            sfields 
              |> Seq.choose
                (function 
                  |CStructElem(fname,Ptr(Function(CFuncDesc(parms,rty,_))),_) -> 
                    if Set.contains fname fnset then
                      Some(fname,parms,rty)
                    else
                      None
                  |_ -> None
                )
          
          // incomplete pattern matches warning is ok
          for (fname,parms,rty)::next in fseq |> utils.seqPairwise do
            let p1 = "    fn "+fname+"("
            let pend = ") -> "+(if rty=Primitive Void then "()" else tyToRustGlobal rty)+(if List.isEmpty next then "" else ",")
            let parts = 
              seq {
                  yield "&mut self"
                  yield! parms |> Seq.tail |> Seq.map (fun (pname, pty, _) -> pname+": "+(tyToRustGlobal pty))
              } |> utils.seqPairwise |> Seq.map (function |[p;_] -> (p+",") |[p] -> p |_ -> "")
            let v1=p1+System.String.Join(" ",parts)+pend
            if v1.Length > eolAfter then
              let indent = "        "
              let indentm1="       "
              apl p1
              let ll=
                parts |> Seq.fold
                  (fun cl p ->
                    if cl="" then
                      let c = indent+p
                      if c.Length>eolAfter then
                        apl c
                        ""
                      else
                        c
                    else
                      let c=cl+" "+p
                      if c.Length>maxLineLen then
                        apl cl
                        indent+p
                      else if c.Length>eolAfter then
                        apl c
                        ""
                      else
                        c
                  ) ""
              if ll<>"" then
                apl ll
              apl ("    "+pend)
            else
              apl v1

          apl "});"
          apl ""
//RIDL!( 
//294 interface IDXGIFactory1(IDXGIFactory1Vtbl): IDXGIFactory(IDXGIFactoryVtbl) { 
//295     fn EnumAdapters1(&mut self, Adapter: ::UINT, ppAdapter: *mut *mut IDXGIAdapter1) -> ::HRESULT, 
//296     fn IsCurrent(&mut self) -> ::BOOL 
//297 }); 


            
  let createTypes()=
    for KeyValue(name,(ty,(fname,_,_,_))) in types do
      let f=rsfilename fname
      let apl=apl f
      match ty with
      |Typedef(EnumRef(ename)) when ename=name -> ()
      |Typedef(StructRef(sname)) when sname=name -> ()
      |_ -> 
        apl <| sprintf "pub type %s = %s;" name (tyToRustGlobal ty)

  let createDefines()=
    for KeyValue(name,(mc,orgs,(fname,_,_,_))) in defines do
      let f=rsfilename fname
      let apl=apl f
      match mc with
      |MCUInt64(_) -> 
        apl <| sprintf "pub const %s: ::UINT64 = %s;" name orgs
      |MCInt64(_) -> 
        apl <| sprintf "pub const %s: ::INT64 = %s;" name orgs
      |MCUInt32(_) -> 
        apl <| sprintf "pub const %s: ::UINT = %s;" name orgs
      |MCInt32(_) -> 
        apl <| sprintf "pub const %s: ::INT = %s;" name orgs
      |MCFloat(_) -> 
        apl <| sprintf "pub const %s: ::FLOAT = %s;" name orgs
      |MCDouble(_) -> 
        apl <| sprintf "pub const %s: ::DOUBLE = %s;" name orgs
        

  let createFunctions()=
    funcs 
      |> Seq.choose 
        (function 
          |KeyValue(name, (Function(CFuncDesc(args,rty,cc)),loc)) -> 
            Some(name,args,rty,cc,loc)
          |_ -> None)
      |> Seq.map
        (fun (name,args,rty,cc,(fname,_,_,_)) ->
          let rcc=ccToRust cc
          let text=System.String.Format("    pub fn {0}({1}) -> {2};",name,((List.map funcArgToRust args) |> String.concat(", ")),(tyToRust rty))
          let f=(rsfilename fname)
          let f'=f.Substring(0,f.Length-3)+"_sys.rs"
          (f',rcc,text))
      |> Seq.groupBy (fun (a,b,c) -> a)
      |> Seq.iter
        (fun (f,txts) ->
          let apl=apl f
          txts 
            |> Seq.groupBy (fun (a,b,c) -> b)
            |> Seq.iter 
              (fun (rcc, lines) ->
                apl <| sprintf "extern %s {" rcc
                lines |> Seq.iter (fun (_,_,t) -> apl t)
                apl "}"
                apl ""
                ))
    
  let createIIDs()=
    let apl=apl "iids.rs"
    apl "extern {"
    for KeyValue(iid,_) in iids do
      apl <| sprintf "  pub static %s: IID;" iid
    apl "}"
    apl ""
  // ----------------------- codeGen ------------------------------------------------

  createEnums()
  createStructs()
  createTypes()
  createIIDs()
  createDefines()
  createFunctions()
  !file2sb |> Map.map (fun  k v -> v.ToString())
  