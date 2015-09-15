module sysgen

open libclang
open cdesc
open rustdesc
open annotations
open annotations_autogen

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
        |Struct ses -> makeStruct name ses
        |_ -> raise <| new System.Exception("Unexpected type")
    structs |> List.ofSeq |>
      List.collect makeStructOrInterface
  let rfuncs=[]
  let riids=[]
  List.concat [rtypes; renums; rstructs; rfuncs; riids]

let isOnlyOneBitSet (v:uint64)=
  v<>0UL && (v &&& (v-1UL))=0UL

let isDXGI (fname,_,_,_)=
  //System.IO.Path.GetFileName(fname).StartsWith("dxgi",System.StringComparison.InvariantCultureIgnoreCase) 
  false //TODO: switch to winapi-rs when it's usable

let codeGen (types:Map<string,CTypeDesc*CodeLocation>,
              enums:Map<string,CTypeDesc*CodeLocation>,
                structs:Map<string,CTypeDesc*CodeLocation>,
                  funcs:Map<string,CTypeDesc*CodeLocation>, 
                    iids:Map<string,CodeLocation>,
                      defines:Map<string, MacroConst*string*CodeLocation>) : string=
  let createEnums (sb:System.Text.StringBuilder)=
    let apl s=sb.AppendLine(s) |> ignore
    apl "use std::ops::BitOr;"
    apl ""
    for (name,uty,vals,loc) in enums |> Seq.choose (function |KeyValue(name,(Enum(uty, vals),loc)) -> (if isDXGI loc then None else Some(name,uty,vals,loc)) |_ -> None) do
      let annot=Map.find name enum_annotations
      apl @"#[repr(C)]"
      apl @"#[derive(Clone,Copy,PartialEq,Eq,Default)]"
      apl <| sprintf @"pub struct %s(pub u32);" name
      apl ""
      if annot=EAFlags then
        let lcsts=vals |> List.filter (snd>>isOnlyOneBitSet) |> List.map (fun (name,v) -> sprintf @"(""%s"",%d)" name v)
        let csts=System.String.Join(", ", lcsts)
        sb.AppendFormat(@"
impl BitOr for {0} {{
  type Output={0};

  fn bitor(self, rhs: {0}) -> Self::Output {{
    match (self,rhs) {{
      ({0}(a),{0}(b)) => {0}(a|b)
    }}
  }}
}}

impl fmt::Debug for {0} {{
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {{
    debug_fmt_enum(""{0}"", self.0, &[{1}], f)
  }}
}}
      ", name, csts) |> ignore
      else 
        let (_,matches)=
          vals |> List.fold 
            (fun (vcache, str) (cname, v) -> 
              if Set.contains v vcache then
                (vcache, str)
              else
                let adds=sprintf "      %s => write!(f,\"%s\"),\r\n" cname cname
                (Set.add v vcache, str+adds)
              ) (Set.empty, "")
        sb.AppendFormat(@"
impl fmt::Debug for {0} {{
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {{
    match *self {{
{1}
      _ => write!(f, ""{0}({{}})"", self.0),
    }}
  }}
}}
      ", name, matches) |> ignore
      sb.AppendLine() |> ignore
      for (cn,v) in vals do
        apl "#[allow(dead_code)]"
        apl "#[allow(non_upper_case_globals)]"
        apl <| System.String.Format("pub const {0}: {1} = {1}({2});", cn, name, v) 
        apl ""
      apl ""
      apl ""

  let libcTypeNames=Set.ofList ["BOOL";"LPCWSTR";"HMODULE"
      ;"GUID";"LARGE_INTEGER";"LPVOID"
      ;"WCHAR";"BYTE";"LPCVOID";"LONG_PTR";"WORD";"SIZE_T"
      ;"SECURITY_ATTRIBUTES";"HANDLE";"DWORD";"LPCSTR";"LONG"
      ;"IUnknown"] 
// ----------------- Create structs ----------------------------------
  let createStructs (sb:System.Text.StringBuilder)=
    let apl s=sb.AppendLine(s) |> ignore
    for (name,sfields,loc) in structs |> Seq.choose(function |KeyValue(name, (Struct(sfields),loc)) -> (if isDXGI loc then None else Some(name,sfields,loc)) |_ -> None) do
      if Set.contains name libcTypeNames then
        ()
      else
        apl "#[allow(non_snake_case)]"
        apl "#[repr(C)]"
        let nonInterface=List.forall (function |CStructElem(_,Ptr(Function(_)),_) -> false |_ ->true) sfields
        if nonInterface && (match Map.tryFind name d3d12structs with |Some(flag,_) when flag &&& StructFlags.DeriveDefault <> StructFlags.None -> true |_ -> false  ) then
          apl "#[derive(Default)]"
        if nonInterface && (match Map.tryFind name d3d12structs with |Some(flag,_) when flag &&& StructFlags.DeriveCopy <> StructFlags.None -> true |_ -> false  ) then
          apl "#[derive(Clone,Copy)]"
        let deriveDebug=(match Map.tryFind name d3d12structs with |Some(flag,_) when flag &&& StructFlags.DeriveDebug <> StructFlags.None -> true |_ -> false  )
        if nonInterface && deriveDebug then
          apl "#[derive(Debug)]"
        if sfields.IsEmpty then
          sb.AppendFormat("pub struct {0};",name).AppendLine() |> ignore;
        else
          sb.AppendFormat("pub struct {0} {{",name).AppendLine() |> ignore;
          for (fname,fty) in sfields |> Seq.choose(function |CStructElem(fname,fty,None)->Some(fname,fty) |_ -> None) do
            sb.AppendFormat("  pub {0} : {1},", fname, tyToRust fty).AppendLine() |> ignore
          sb.AppendLine("}").AppendLine() |> ignore
        if nonInterface && (not deriveDebug) then
          let code=
            sfields |>
              Seq.map(
                function
                |CStructElem(fname,fty,None)->
                  match fty with
                  |CTypeDesc.Array(TypedefRef("WCHAR"), n) ->
                    seq {
                      yield "    try!{write!(f,\"  "+fname+": '\")};"
                      yield "    {try!{writeln!(f,\"{:}'\", wchar_array_to_string_lossy(&self."+fname+"))};}"
                    } |> fun sq -> System.String.Join(System.Environment.NewLine,sq)
                  |CTypeDesc.Array(aty, n) ->
                    seq {
                      yield "    try!{write!(f,\"  "+fname+": [\")};"
                      yield "    for i in (0.."+n.ToString()+") {try!{write!(f,\"{:?}; \", self."+fname+"[i])};}"
                      yield "    try!{writeln!(f,\"]\")};"
                    } |> fun sq -> System.String.Join(System.Environment.NewLine,sq)
                  |_ ->
                    "    try!{writeln!(f,\"  "+fname+": {:?}\", self."+fname+")};"
                |_ -> ""
                ) |> fun sq -> System.String.Join(System.Environment.NewLine,sq)
          apl (@"
impl fmt::Debug for {Struct} {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    try!{writeln!(f,""struct {Struct} "")};
{Code}
    writeln!(f,"""")
  }
}
".Replace("{Struct}",name).Replace("{Code}",code) )
            
  let createTypes (sb:System.Text.StringBuilder)=
    for KeyValue(name,(ty,loc)) in types do
      // Some types are defined in rust's libc module
      if Set.contains name libcTypeNames then
        ()
      else if name="REFGUID" then
        sb.AppendLine("pub type REFGUID=&'static GUID;") |> ignore
      else
        match ty with
        |Typedef(EnumRef(ename)) when ename=name -> ()
        |Typedef(StructRef(sname)) when sname=name -> ()
        |_ -> 
          sb.AppendLine("#[allow(non_camel_case_types)]") |> ignore
          sb.AppendFormat("pub type {0} = {1};", name, tyToRust ty).AppendLine() |> ignore
    sb.AppendLine() |> ignore

  let createFunctions (sb:System.Text.StringBuilder)=
     // TODO: use 
    for (name,args,rty,cc,loc) in funcs |> Seq.choose (function |KeyValue(name, (Function(CFuncDesc(args,rty,cc)),loc)) -> (if isDXGI loc then None else Some(name,args,rty,cc,loc)) |_ -> None) do
      //sb.AppendLine("#[link(name=\"d3d12\")]") |> ignore       
      sb.AppendFormat("extern {3} {{ pub fn {0}({1}) -> {2}; }}",name,((List.map funcArgToRust args) |> String.concat(", ")),(tyToRust rty), (ccToRust cc)).AppendLine() |> ignore
    sb.AppendLine("").AppendLine() |> ignore

  let createIIDs (sb:System.Text.StringBuilder)=
    let apl s=sb.AppendLine(s) |> ignore
    //apl "#[link(name=\"dxguid\")]"
    apl "extern {"
    for KeyValue(iid,_) in iids do
      sb.AppendFormat("  pub static {0}: IID;",iid).AppendLine() |> ignore
    apl "}"
    apl ""
  // ----------------------- codeGen ------------------------------------------------
  let sb=new System.Text.StringBuilder()
  let apl s=sb.AppendLine(s) |> ignore
  apl "// This file is autogenerated."
  apl ""
  apl ""
  apl @"
extern crate libc;
use libc::*;
use std::fmt;
use iid::IUnknown;
use std::os::windows::ffi::OsStringExt;
use std::ffi::OsString;

fn wchar_array_to_string_lossy(ws: &[u16]) -> String {
  match ws.iter().position(|c|*c==0) {
    Some(p) => {
      OsString::from_wide(&ws[0..p]).to_string_lossy().into_owned()
    },
    None => {
      OsString::from_wide(ws).to_string_lossy().into_owned()
    },
  }
}

fn debug_fmt_enum(name : &str, val: u32, opts: &[(&str,u32)], f: &mut fmt::Formatter) -> fmt::Result {
  let mut p_opts=0u32;
  let mut cval=val;
  let mut has_prev=false;
  for &(s,v) in opts {
    if (p_opts & v)==0 {
        p_opts |= v;
        if (cval & v)!=0 {
            if has_prev {
                try!{write!(f,"" | "")};
            } else {
                has_prev=true;
            }
            try!{write!(f,""{:}"",s)};
            cval &= !v;
        }
    }
  }
  if cval!=0 {
    if has_prev {
        try!{write!(f,"" | "")};
    }
    try!{write!(f, ""{}({:?})"", name, cval)}
  } else if !has_prev {
    try!{write!(f, ""{}(0)"", name)}
  }
  Ok(())
}
"

//  for KeyValue(sn,(st,loc)) in structs do
//    match st with
//    |Struct(ses) ->
//      if ses |> List.forall (function |CStructElem(_,Ptr(Function(_)),_) -> false |_ -> true) then
//        printfn "  (\"%s\",StructFlags.None,[" sn
//        for CStructElem(fname,_,_) in ses do
//          printfn "    (\"%s\",FANone);" fname
//        printfn "    ]);"
//    |_ -> ()

  createEnums sb
  createTypes sb
  createStructs sb
  createIIDs sb
  createFunctions sb
  sb.ToString()

// Returns pairs of sequense elements, except for last element
let seqPairwise (s:seq<'t>)=
  if Seq.isEmpty s then
    Seq.empty
  else
    let s1=s |> Seq.map Some
    let s2= // shifted sequence
      seq {
        yield! s |> Seq.skip 1 |> Seq.map Some
        yield None
      }
    Seq.zip s1 s2 
      |> Seq.map 
        (function 
          |(Some(a),Some(b)) -> [a;b]
          |(Some(a),None) -> [a]
          |_ -> raise <| new System.Exception("Unreachable"))
  

let maxLineLen=99
let eolAfter=80

let winapiGen (types:Map<string,CTypeDesc*CodeLocation>, 
                enums:Map<string,CTypeDesc*CodeLocation>,
                  structs:Map<string,CTypeDesc*CodeLocation>,
                    funcs:Map<string,CTypeDesc*CodeLocation>, 
                      iids:Map<string,CodeLocation>,
                       defines:Map<string, MacroConst*string*CodeLocation>) : Map<string,System.String>=
  let uncopyableStructs=
    let isStructUncopyableByItself (ses : CStructElem list)=
      ses |> List.exists (function |CStructElem(_,Array(_,n),_) when n>32L -> true |_ -> false)
    let rec isStructUncopyable s=
      match s with
      |Struct(ses) ->
        (isStructUncopyableByItself ses) || 
          ses |> List.exists (
            function 
              |CStructElem(_,StructRef sname,_) -> 
                match Map.find sname structs with 
                |(Struct(_) as s,_) -> isStructUncopyable s
                |_ -> false
              |_ -> false)
      |_ -> false
    structs |> Map.toSeq |> Seq.filter (snd >> fst >> isStructUncopyable) |> Seq.map fst |> Set.ofSeq

  let rsfilename s=
    (System.IO.Path.GetFileNameWithoutExtension s)+".rs"

  let file2sb=ref Map.empty

  let dxann=d3d12annotations_prime |> Seq.map (fun (a,b,c,d) -> (a,(b,c,d))) |> Map.ofSeq

  let apl (f:System.String) s=
    let sb=
      match Map.tryFind f !file2sb with
      |Some(sb:System.Text.StringBuilder) -> 
        sb
      |None ->
        let sb=new System.Text.StringBuilder()
        let text=sprintf @"// Copyright © 2015; Dmitry Roschin
// Licensed under the MIT License <LICENSE.md>
//! Mappings for the contents of %s.h" (f.Substring(0,f.Length-3))
        sb.AppendLine(text) |> ignore
        sb.AppendLine() |> ignore
        file2sb := Map.add f sb !file2sb
        sb
    sb.AppendLine(s) |> ignore

  let createEnums()=
    for (name,uty,vals,(fname,_,_,_)) in enums |> Seq.choose (function |KeyValue(name,(Enum(uty, vals),loc)) -> (if isDXGI loc then None else Some(name,uty,vals,loc)) |_ -> None) do
      let f=rsfilename fname
      let apl=apl f
      let annot=Map.find name enum_annotations
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
  let createStructs()=
    for (name,sfields,(fname,_,_,_)) in structs |> Seq.choose(function |KeyValue(name, (Struct(sfields),loc)) -> (if isDXGI loc then None else Some(name,sfields,loc)) |_ -> None) do
      let f=rsfilename fname
      let apl=apl f
      if Set.contains name libcTypeNames || Map.containsKey (name+"Vtbl") dxann then
        ()
      else
        let nonInterface=List.forall (function |CStructElem(_,Ptr(Function(_)),_) -> false |_ ->true) sfields
        if nonInterface then
          let uncopyable=Set.contains name uncopyableStructs
          if uncopyable then
            apl "#[repr(C)] #[derive(Copy)]"
          else
            apl "#[repr(C)] #[derive(Clone, Copy, Debug)]"
          apl <| System.String.Format("pub struct {0} {{",name)
          for (fname,fty) in sfields |> Seq.choose(function |CStructElem(fname,fty,None)->Some(fname,fty) |_-> raise <| new System.Exception("Bitfields aren't supported")) do
            apl <| System.String.Format("    pub {0}: {1},", fname, tyToRustGlobal fty)
          apl "}"
          apl ""
          if uncopyable then
            apl <| sprintf "impl Clone for %s {" name
            apl "    fn clone(&self) -> Self { *self }"
            apl "}"
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
          
          for (fname,parms,rty)::next in fseq |> seqPairwise do
            let p1 = "    fn "+fname+"("
            let pend = ") -> "+(if rty=Primitive Void then "()" else tyToRustGlobal rty)+(if List.isEmpty next then "" else ",")
            let parts = 
              seq {
                  yield "&mut self"
                  yield! parms |> Seq.tail |> Seq.map (fun (pname, pty, _) -> pname+": "+(tyToRustGlobal pty))
              } |> seqPairwise |> Seq.map (function |[p;_] -> (p+",") |[p] -> p |_ -> "")
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
            if isDXGI loc then None else Some(name,args,rty,cc,loc)
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
  