module sysgen

open libclang
open cdesc
open rustdesc

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

let convertToRust (types:Map<string,CTypeDesc>,enums:Map<string,CTypeDesc>,structs:Map<string,CTypeDesc>,funcs:Map<string,CTypeDesc>, iids:Set<string>, annotations)=
  let rtypes=
    types |> List.ofSeq |> 
      List.map (
        fun (KeyValue(name, ty)) ->
          let rty=tyToRty ty
          RustItem (name, RETypeDef rty, RAPublic, [reprC])
        )
  let renums=
    enums |> List.ofSeq |> 
      List.collect (
        fun (KeyValue(name, ty)) ->
          match ty with
          |Enum(underlyingType=ut; values=vals) ->
            RustItem (name, RETypeDef(RTupleStruct([tyToRty (Primitive ut)])),RAPublic,[reprC]) :: 
              (vals |> List.map (
                fun (ename, eVal) ->
                  RustItem(ename, REConst(RType name, sprintf "%s(%d)" name eVal, eVal),RAPublic, [])
                ))
          |_ -> raise <| new System.Exception("Unexpected type of enum "+name)
      )
  let rstructs=
    let makeStructOrInterface (KeyValue(name,ty)) =
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
        sb.AppendLine("#[allow(dead_code)]") |> ignore
        sb.AppendLine("#[allow(non_upper_case_globals)]") |> ignore
        sb.AppendFormat("pub const {0}: {1} = {1}({2});", cn, name, v) |> ignore
        sb.AppendLine() |> ignore
      sb.AppendLine() |> ignore
      sb.AppendLine() |> ignore


  let libcTypeNames=Set.ofList ["BOOL";"LPCWSTR";"HMODULE";"GUID";"LARGE_INTEGER";"LPVOID";"WCHAR";"BYTE";"LPCVOID";"LONG_PTR";"WORD";"SIZE_T";"SECURITY_ATTRIBUTES";"HANDLE";"DWORD";"LPCSTR";"LONG"]
  let createStructs (sb:System.Text.StringBuilder)=
    let apl s=sb.AppendLine(s) |> ignore
    for KeyValue(name, Struct(sfields)) in structs do
      if Set.contains name libcTypeNames then
        ()
      else
        apl "#[allow(non_snake_case)]"
        apl "#[repr(C)]"
        if List.forall (function |CStructElem(_,Ptr(Function(_)),_) -> false |_ ->true) sfields then
          apl "#[derive(Debug)]"
        if sfields.IsEmpty then
          sb.AppendFormat("pub struct {0};",name).AppendLine() |> ignore;
        else
          sb.AppendFormat("pub struct {0} {{",name).AppendLine() |> ignore;
          for CStructElem(fname,fty,None) in sfields do
            sb.AppendFormat("  pub {0} : {1},", fname, tyToRust fty).AppendLine() |> ignore
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
        |_ -> 
          sb.AppendLine("#[allow(non_camel_case_types)]") |> ignore
          sb.AppendFormat("pub type {0} = {1};", name, tyToRust ty).AppendLine() |> ignore
    sb.AppendLine() |> ignore

  let createFunctions (sb:System.Text.StringBuilder)=
     // TODO: use 
    for KeyValue(name, Function(CFuncDesc(args,rty,cc))) in funcs do
      sb.AppendLine("#[link(name=\"d3d12\")]") |> ignore       
      sb.AppendFormat("extern {3} {{ pub fn {0}({1}) -> {2}; }}",name,((List.map funcArgToRust args) |> String.concat(", ")),(tyToRust rty), (ccToRust cc)).AppendLine() |> ignore
    sb.AppendLine("").AppendLine() |> ignore

  let createIIDs (sb:System.Text.StringBuilder)=
    sb.AppendLine("#[link(name=\"dxguid\")]").AppendLine("extern {") |> ignore
    for iid in iids do
      sb.AppendFormat("  pub static {0}: IID;",iid).AppendLine() |> ignore
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
