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

let codeGen (types:Map<string,CTypeDesc*CodeLocation>,enums:Map<string,CTypeDesc*CodeLocation>,structs:Map<string,CTypeDesc*CodeLocation>,funcs:Map<string,CTypeDesc*CodeLocation>, iids:Set<string>) : string=
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
    for iid in iids do
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

