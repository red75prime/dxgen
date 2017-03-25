module sysgen

open libclang
open cdesc
open rustdesc
open annotations

let forwardDecls = 
    [
        ("ID2D1SimplifiedGeometrySink", "um::d2d1");
    ] |> Map.ofList

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

let convertToRust (types:Map<string,CTypeDesc*CodeLocation>,
                   enums:Map<string,CTypeDesc*CodeLocation>,
                   structs:Map<string,CTypeDesc*CodeLocation>,
                   funcs:Map<string,CTypeDesc*CodeLocation>,
                   iids:Set<string>,
                   annotations)=
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
          |_ -> failwith ("Unexpected type of enum "+name)
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
        |Struct (ses, bas, _) -> makeStruct name ses
        |_ -> failwith ("Unexpected type")
    structs |> List.ofSeq |>
      List.collect makeStructOrInterface
  let rfuncs=[]
  let riids=[]
  List.concat [rtypes; renums; rstructs; rfuncs; riids]

let isOnlyOneBitSet (v:uint64)=
  v<>0UL && (v &&& (v-1UL))=0UL

let maxLineLen=99
let eolAfter=90

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

let winapiGen (headername: string)
              (types:Map<string,CTypeDesc*CodeLocation>, 
                enums:Map<string,CTypeDesc*CodeLocation>,
                  structs:Map<string,CTypeDesc*CodeLocation>,
                    funcs:Map<string,CTypeDesc*CodeLocation>, 
                      iids:Map<string,CodeLocation*IID>,
                       defines:Map<string, MacroConst*string*CodeLocation>,
                         typedefs) =
  
  let def_annots = 
    match Map.tryFind headername defines_annotations.defines with
    |Some(defs) -> defs
    |None -> Map.empty

  let rsfilename s=
    (System.IO.Path.GetFileNameWithoutExtension s)+".rs"

  let file2sb: Map<string, System.Collections.Generic.SortedDictionary<uint32, System.Text.StringBuilder>> ref = ref Map.empty

  let getSbForLine (sd:System.Collections.Generic.SortedDictionary<uint32, System.Text.StringBuilder>) line =
    let sb = ref null
    if sd.TryGetValue(line, sb) then
        !sb
    else
        let sb = new System.Text.StringBuilder()
        sd.Add(line, sb)
        sb
    
  let processDefines()=
    seq {
        for KeyValue(name,(mc,orgs,(fname, linenum,_,_))) in defines do
          let f=rsfilename fname
          let defOption = 
            match Map.tryFind name def_annots with
            |Some(_) as opt -> opt
            |None -> 
                match Map.tryFind name annotations.disableRPCDefines with
                |Some(_) as opt -> opt
                |None -> None
          match defOption with
          |Some(Exclude) -> ()
          |Some(UseType(t)) ->
            match mc with
            |MCExpression(_) ->
                failwithf "Macro expression %s cannot be annotated with UseType" name
            |_ ->
                if (t = "FLOAT" || t = "DOUBLE") && (not <| orgs.Contains(".")) then
                    yield (f, sprintf "pub const %s: %s = %s.;" name t orgs, t, linenum)
                else 
                    yield (f, sprintf "pub const %s: %s = %s;" name t orgs, t, linenum)
          |Some(UseCustom(t, item)) ->
            yield (f, item, t, linenum)
          |None ->
            let t = 
              match mc with
              |MCUInt64(_) -> Choice1Of2 "UINT64"
              |MCInt64(_) -> Choice1Of2("INT64")
              |MCUInt32(_) -> Choice1Of2("UINT")
              |MCInt32(_) -> Choice1Of2("INT")
              |MCFloat(_) -> Choice1Of2("FLOAT")
              |MCDouble(_) -> Choice1Of2("DOUBLE")
              |MCExpression(tokens) -> Choice2Of2(tokens)
            match t with
            |Choice1Of2(t) -> yield (f, sprintf "pub const %s: %s = %s;" name t orgs, t, linenum)
            |Choice2Of2(tokens) ->
                match tokens with
                |"(" :: _ ->
                    let lines = 
                        (name :: tokens) 
                            |> Seq.map (fun (t:string) -> t.Replace("\r\n", " ").Replace("\n", " ")) 
                            |> utils.seqToLines 80 " " ""
                            |> Seq.map (fun line -> "// " + line)
                    // Add todo for unannotated macro
                    let todo = "// TODO: Implement macro\r\n"+System.String.Join("\r\n", lines)+"\r\n"
                    yield(f, todo, "", linenum)
                // TODO: something with aliases, like #define LONGNAME_A EVENLONGERNAME_B
                |_ -> ()
    }
        
  let registerAdditionalTypes typedefs =
    // It's not robust, but I can't find how to get type definition location by type name from libclang
    let knowntypelocations = 
        (Map.ofList [("UINT", "shared::minwindef"); ("INT", "shared::minwindef");
                     ("FLOAT", "shared::minwindef"); ("DOUBLE", "shared::ntdef");
                     ("UINT64", "shared::basetsd"); ("INT64", "shared::basetsd");
                     ("GUID", "shared::guiddef") ])
    let addType typedefs ty =
        if ty = "" then
            typedefs
        else
            match Map.tryFind ty typedefs with
            |Some(_) -> typedefs
            |None ->
                Map.add ty (Map.find ty knowntypelocations) typedefs
    //let typedefs = if Map.isEmpty iids then typedefs else addType typedefs "GUID"
    let typedefs = Seq.fold (fun tds (_,_,ty,_) -> addType tds ty) typedefs (processDefines())
    let addForwardDecl typedefs typename = 
        match Map.tryFind typename forwardDecls with
        |Some(modname) -> Map.add typename modname typedefs
        |None -> typedefs
    let typedefs = 
        structs |> Seq.choose(function |KeyValue(name, (ForwardDecl _, _ )) -> Some(name) |_ -> None) 
            |> Seq.fold addForwardDecl typedefs
    typedefs

  let typedefs = registerAdditionalTypes typedefs
//  let uncopyableStructs =
//    let rec isStructUncopyableByItself (ses : CStructElem list)=
//      ses 
//        |> List.exists 
//          (function 
//            |CStructElem(_,Array(_,n),_) when n>32L -> true 
//            |CStructElem(_,Struct(ses,bas, _),_) -> isStructUncopyableByItself ses
//            |CStructElem(_,Union(ues),_) -> ues |> List.map fst |> isStructUncopyableByItself
//            |_ -> false)
//    let rec isStructUncopyable s=
//      match s with
//      |Struct(ses, bas, _) ->
//        (isStructUncopyableByItself ses) || 
//          ses |> List.exists (
//            function 
//              |CStructElem(_,StructRef sname,_) -> 
//                let sname = sname
//                match Map.find sname structs with 
//                |(Struct(_) as s,_) -> isStructUncopyable s
//                |_ -> false
//              |CStructElem(_,(Struct(ses,bas,_) as sub),_) -> 
//                isStructUncopyable sub
//              |CStructElem(_,(Union(ues) as sub),_) -> 
//                ues |> List.map fst |> fun ses -> Struct (ses, "", []) |> isStructUncopyable
//              |_ -> false)
//      |_ -> false
//    structs |> Map.toSeq |> Seq.filter (snd >> fst >> isStructUncopyable) |> Seq.map fst |> Set.ofSeq
//
  let apl (f:System.String) (line:uint32) s=
    let sb =
      match Map.tryFind f !file2sb with
      |Some(sd) -> 
        getSbForLine sd line
      |None ->
        let sd = new System.Collections.Generic.SortedDictionary<uint32, System.Text.StringBuilder>()
        file2sb := Map.add f sd !file2sb
        let text=sprintf "\
// Copyright © 2017 winapi-rs developers
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your option.
// All files in the project carrying such notice may not be copied, modified, or distributed
// except according to those terms.
//! Mappings for the content of %s.h" (f.Substring(0,f.Length-3))
        let sb = new System.Text.StringBuilder(text)
        sb.AppendLine("") |> ignore
        sb.AppendLine("") |> ignore
        sd.Add(0u, sb)
        getSbForLine sd line
    sb.AppendLine(s) |> ignore

  let createEnums()=
    for (name,uty,vals,(fname,linenum,_,_)) in enums |> Seq.choose (function |KeyValue(name,(Enum(uty, vals),loc)) -> Some(name,uty,vals,loc) |_ -> None) do
      let f=rsfilename fname
      let apl=apl f linenum
      let convhex=fun (v:uint64) -> "0x" + v.ToString("X")
      let convdec=fun (v:uint64) -> v.ToString()
      apl ("ENUM!{enum "+name+" {")
      for (cname,v) in vals do
          apl ("    " + cname + " = " + (convhex v) + ", // "+(convdec v))
      apl "}}"
      apl ""

  let libcTypeNames=Set.ofList ["BOOL";"LPCWSTR";"HMODULE"
      ;"GUID";"LARGE_INTEGER";"LPVOID"
      ;"WCHAR";"BYTE";"LPCVOID";"LONG_PTR";"WORD";"SIZE_T"
      ;"SECURITY_ATTRIBUTES";"HANDLE";"DWORD";"LPCSTR";"LONG"
      ;"IUnknown"] 
// ----------------- Create structs ----------------------------------
  let outputStructDef apl name ses_in uncopyable encloseInIfDef =
    let hasBitfields = ses_in |> List.exists (fun (CStructElem(_, _, bw)) -> Option.isSome bw)
    // process bitfields
    // next bitfield goes into the same group if it has the same type and fits into remaining space
    // bitfield with 0 size is a delimeter
    // if bitfield doesn't fit into remaining space, bail out

    // state for fold:
    // (list of struct elems, list of bitfield groups, list of current group's bitfields, 
    //  , optional (current bitfield type, current bitfield type bit width, current bit position))

    let initialState = 
        ([], [], [], None)

    let dumpGroup state = 
        match state with
        |(ses, gs, [], None) -> // nothing to dump
            (ses, gs, [], None) 
        |(ses, gs, _, None) -> 
            failwith "unreachable"
        |(ses, gs, bs, Some(ty, _, _)) ->
            let bfGNum = List.length gs
            let bfGName = "bitfield" + (bfGNum.ToString())
            ((CStructElem(bfGName, ty, None)) :: ses, ((bfGName, ty, List.rev bs) :: gs), [], None)
    
    let foldStep (ses, gs, bs, info) se =
        let (CStructElem(name, ty, bw)) = se
        match bw with
        |None -> // it's not bitfield. just copy to output
            (se :: ses, gs, bs, info)
        |Some(fieldwidth, bytewidth) -> 
            let bytewidth = int32(bytewidth)
            match info with
            |Some(cty, bitwidth, bitpos) when cty = ty ->
                // bitfield group continues
                assert(bitwidth = bytewidth*8)
                if fieldwidth = 0 then
                    // separator. dump current group
                    let (ses, gs, bs, _) = dumpGroup (ses, gs, bs, info)
                    // start new group
                    (ses, gs, bs, Some(cty, bitwidth, 0))
                else
                    let bitpos_next = bitpos + fieldwidth
                    if bitpos_next = bitwidth then
                        // group is filled. dump it
                        dumpGroup (ses, gs, (name, bitpos, bitpos_next) :: bs, Some(cty, bitwidth, bitpos_next))
                    else if bitpos_next > bitwidth then
                        failwith "bitfield crosses type boundary"
                    else
                        (ses, gs, (name, bitpos, bitpos_next) :: bs, Some(cty, bitwidth, bitpos_next))
            |_ -> 
                let (ses, gs, bs, info) = dumpGroup (ses, gs, bs, info)
                let bitpos = 0
                let bitpos_next = fieldwidth
                if fieldwidth = 0 then
                    // skip
                    (ses, gs, bs, info)
                else
                    (ses, gs, (name, bitpos, bitpos_next) :: bs, Some(ty, bytewidth*8, bitpos_next)) |>
                        if bitpos_next > bytewidth*8 then 
                            (fun _ -> failwith "bitfield crosses type boundary") 
                        elif bitpos_next = bytewidth*8 then 
                            dumpGroup 
                        else 
                            id 


    let (ses, gs, _, _) = ses_in |> List.fold foldStep initialState |> dumpGroup
            
    let ses = List.rev ses
    let gs = List.rev gs
    // -------
    if encloseInIfDef then 
        apl <| "IFDEF!{"
    apl <| sprintf "STRUCT!{struct %s {" name
    let nametype = 
        ses 
            |> Seq.choose(
                function 
                    |CStructElem(fname,fty,None) -> 
                        Some(fname,fty) 
                    |CStructElem(fname,fty,Some(bw)) -> 
                        Some(fname, Unimplemented("bitfield_"+(tyToRustGlobal fty)+"("+(bw.ToString())+")")))

    for (fname,fty) in nametype  do
        apl <| System.String.Format("    {0}: {1},", fname, tyToRustGlobal fty)
    apl "}}"
    for (bfname, ty, bfs) in gs do
        apl <| sprintf "BITFIELD!{%s %s: %s [" name bfname (tyToRustGlobal ty)
        for (fname, startbit, endbit) in bfs do
            apl <| sprintf "    %s set_%s[%d..%d]," fname fname startbit endbit
        apl "]}"
    if encloseInIfDef then
        apl "}"
    apl ""

  let createStructs()=
    let filter (KeyValue(name, (ty, loc))) = 
        match ty with
        |Struct([], "", _) -> 
            utils.coloredText System.ConsoleColor.Red (fun () -> 
                printfn "  Warning! Skipping empty struct %s" name)
            None
        |Struct(sfields, bas, attrs) -> Some(name, bas, sfields, attrs, loc)
        |Union(ufields) -> Some(name, "", [CStructElem("u", ty, None)], [], loc)
        |_ -> None
    for (name, bas, sfields0, attrs, (fname, linenum,_,_)) in structs |> Seq.choose filter do
      // eliminate single element unnamed union
      let sfields = 
        sfields0 |> List.map (
            function
            |CStructElem("", Union([(se,_)]), None) ->
                se
            |se -> se)

      let f=rsfilename fname
      let apl=apl f linenum
      if Set.contains name libcTypeNames || Map.tryFind (name+"Vtbl") structs |> Option.isSome then
        ()
      else
        let nonInterface = bas="" && (List.isEmpty sfields || (List.exists (function |CStructElem(_,Ptr(Function(_)),_) -> false |_ ->true) sfields))
        if nonInterface then
          let uncopyable = false //Set.contains name uncopyableStructs
          // check precondition. Unnamed unions within unnamed unions aren't supported
          let checkPrecond ty=
            let rec secondLevel ty=
              match ty with
              |Union(_) -> failwith "Unnamed unions within unnamed unions isn't supported"
              |_ -> let (stys, _) = subtypes ty in List.iter secondLevel stys
            let rec firstLevel ty=
              let nextfn=
                match ty with
                |Union(_) -> secondLevel
                |_ -> firstLevel
              let (stys, _) = subtypes ty
              List.iter nextfn stys
            firstLevel ty
          checkPrecond (Struct(sfields, "", []))
          // let's create proxies for unnamed structs in unions
          let unions=ref []
          let uunum = ref 1
          let sfields' =
            sfields |> List.map
              (fun (CStructElem(fname, fty, bw)) ->
                let fname' = 
                    if fname="" then 
                        let fname = "u" + (!uunum).ToString()
                        uunum := !uunum + 1
                        fname
                    else fname
                let fty' =
                  match fty with
                  |Union(ues) ->
                    // for each union element, I need to create UNION!(base_type, field, selector, selector_mut, selector_type)
                    let ues'= 
                      ues |> List.map
                       (fun ((CStructElem(sname, stype, bw), sz) as ue) ->
                          match stype with
                          |Struct(ses, bas, _) ->
                            // anonymous struct in union. create proxy.
                            // TODO: something with nameless structures in unions
                            let sname = if sname = "" then "u" else sname
                            let proxyname=name+"_"+sname;
                            outputStructDef apl proxyname ses uncopyable false
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
            outputStructDef apl name sfields' uncopyable false
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
              outputStructDef apl name (variants |> List.head |> snd) uncopyable false
            else
              for (ct, sfs) in variants do
                match ct with
                |TargetX86 -> 
                  apl @"#[cfg(target_pointer_width = ""32"")]"
                |TargetX64 -> 
                  apl @"#[cfg(target_pointer_width = ""64"")]"
                |TargetUnknown ->
                  failwith "TagetUnknown shouldn't be here"
                outputStructDef apl name sfs uncopyable true
                
          match !unions with
          |[] -> ()
          |_ ->
            for (name, fname, sname, sname_mut, ty) in List.rev !unions do
              let items=["UNION!("+name+","; fname+","; sname+","; sname_mut+","; ty+");"]
              let lines=wrapOn items "    " eolAfter maxLineLen
              for line in lines do
                apl line
            apl ""
        else // ------------------------------- COM-Interface ---------------------------
          // rename overloaded functions
          let renameOverloads (out, prev) (CStructElem(name, ty, bw) as se) =
            match Map.tryFind name prev with
            |None ->
                (se :: out, Map.add name 1 prev)
            |Some(n) ->
                (CStructElem(name + n.ToString(), ty, bw) :: out, Map.add name (n+1) prev)
            
          let sfields = 
            sfields |> List.fold renameOverloads ([], Map.empty) |> fst |> List.rev
          let basename = 
                match Map.tryFind name structs with
                |Some(Struct(_, basename, _),_) -> basename
                |_ -> ""
          
          let uuid =
            match List.tryFind (function |Attribute.AttrUuid _ -> true |_ -> false) attrs with
            |Some(Attribute.AttrUuid uuid) -> uuid
            |_ -> 
                utils.coloredText System.ConsoleColor.Red (fun () ->
                    printfn "  Warning! %s has no uuid." name)
                "#[uuid(0x00000000, 0x0000, 0x0000, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00)]"
            
          apl <| "RIDL!{"+uuid
          if basename="" then
            apl <| sprintf "interface %s(%s) {" (name.Substring(0,name.Length-4)) name
          else
            let line = sprintf "interface %s(%s): %s(%s) {" (name.Substring(0,name.Length-4)) name basename (basename+"Vtbl")
            if line.Length >= eolAfter then
                apl <| sprintf "interface %s(%s): \r\n    %s(%s) {" (name.Substring(0,name.Length-4)) name basename (basename+"Vtbl")
            else
                apl line

          let fseq=
            sfields 
              |> Seq.choose
                (function 
                  |CStructElem(fname,Ptr(Function(CFuncDesc(parms,rty,_))),_) -> 
                    Some(fname,parms,rty)
                  |_ -> None
                )
          //Format code according to new winapi-rs rules
          for pair  in fseq |> utils.seqPairwise do
            let ((fname, parms, rty), last) =
                match pair with
                |[] -> failwith "unreachable"
                |[lastfun] -> (lastfun, true)
                |fn :: _ -> (fn, false)
            let parms = List.tail parms
            let parmToStr (pname, ty, _) = 
                sprintf "%s: %s" pname (tyToRustGlobal ty)
            let retTy = 
                match rty with
                |Primitive Void -> "()"
                |_ -> tyToRustGlobal rty
            let retTy = if last then retTy + "," else retTy + ","
            match parms with
            |[] ->
                apl <| "    fn " + fname + "() -> " + retTy
            |[parm] ->
                apl <| sprintf "    fn %s(%s,) -> %s" fname (parmToStr parm) retTy
            |_ ->
                apl <| sprintf "    fn %s(" fname
                for pair in parms |> utils.seqPairwise do
                    match pair with
                    |[] -> failwith "unreachable"
                    |[parm] -> 
                        apl <| sprintf "        %s," (parmToStr parm)
                    |parm :: _ ->
                        apl <| sprintf "        %s," (parmToStr parm)
                apl <| "    ) -> " + retTy
                    

//          // Format code according to winapi-rs rules
//          for twofield in fseq |> utils.seqPairwise do
//            match twofield with
//            |[] -> failwith "unreachable"
//            |(fname,parms,rty)::next ->
//                let p1 = "    fn "+fname+"("
//                let pend = 
//                    if rty = Primitive Void then
//                        ") -> ()" + (if List.isEmpty next then "" else ",")
//                    else
//                        ") -> "+(tyToRustGlobal rty)+(if List.isEmpty next then "" else ",")
//                let parts = 
//                  seq {
//                      //yield "&self"
//                      yield! parms |> Seq.tail |> Seq.map (fun (pname, pty, _) -> pname+": "+(tyToRustGlobal pty))
//                  } |> utils.seqPairwise |> Seq.map (function |[p;_] -> (p+",") |[p] -> p |_ -> "")
//                let v1=p1+System.String.Join(" ",parts)+pend
//                if v1.Length > eolAfter then
//                  let indent = "        "
//                  let indentm1="       "
//                  apl p1
//                  let ll=
//                    parts |> Seq.fold
//                      (fun cl p ->
//                        if cl="" then
//                          let c = indent+p
//                          if c.Length>eolAfter then
//                            apl c
//                            ""
//                          else
//                            c
//                        else
//                          let c=cl+" "+p
//                          if c.Length>maxLineLen then
//                            apl cl
//                            indent+p
//                          else if c.Length>eolAfter then
//                            apl c
//                            ""
//                          else
//                            c
//                      ) ""
//                  if ll<>"" then
//                    apl ll
//                  apl ("    "+pend)
//                else
//                  apl v1

          apl "}}"
          apl ""
//RIDL!( 
//294 interface IDXGIFactory1(IDXGIFactory1Vtbl): IDXGIFactory(IDXGIFactoryVtbl) { 
//295     fn EnumAdapters1(&mut self, Adapter: UINT, ppAdapter: *mut *mut IDXGIAdapter1) -> HRESULT, 
//296     fn IsCurrent(&mut self) -> BOOL 
//297 }); 


            
  let createTypes()=
    for KeyValue(name,(ty,(fname, linenum,_,_))) in types do
      let f=rsfilename fname
      let apl=apl f linenum
      match ty with
      |Typedef(EnumRef(ename)) when ename=name -> ()
      |Typedef(StructRef(sname)) when sname=name -> ()
      |Typedef(Ptr(Function(CFuncDesc(_, _, cc))) as ty) ->
        //TODO: cc is always "cdecl". Find out why clang does this.
        apl <| sprintf "FN!{%s %s%s}" "stdcall" name (tyToRustGlobal ty)
      |_ -> 
        apl <| sprintf "pub type %s = %s;" name (tyToRustGlobal ty)  

  let createDefines()=
    for (f, text, _, linenum) in processDefines() do
      let apl=apl f linenum
      apl <| text

  let createFunctions()=
    funcs 
      |> Seq.choose 
        (function 
          |KeyValue(name, (Function(CFuncDesc(args,rty,cc)),loc)) -> 
            Some(name,args,rty,cc,loc)
          |_ -> None)
      |> Seq.map
        (fun (name,args,rty,cc,(fname, linenum,_,_)) ->
          let rcc = ccToRustNoQuotes cc
          let sb = new System.Text.StringBuilder()
          let lib = System.IO.Path.GetFileNameWithoutExtension(fname)
          sb.AppendLine(sprintf "    pub fn %s(" name) |> ignore
          let parms = List.map (funcArgToRust >> (sprintf "        %s")) args 
                      |> Seq.ofList 
                      |> fun sq -> System.String.Join(","+System.Environment.NewLine, sq)
          sb.AppendLine(parms) |> ignore
          if rty = Primitive Void then
            sb.Append("    ) -> ();") |> ignore
          else
            sb.Append(sprintf "    ) -> %s;" (tyToRust rty)) |> ignore
          let f = (rsfilename fname)
          (f, rcc, sb.ToString(), linenum))
      |> Seq.groupBy (fun (a,b,c,d) -> a)
      |> Seq.iter
            (fun (f,txts) ->
              let apl=apl f
              txts 
                |> Seq.groupBy (fun (a,b,c,d) -> b)
                |> Seq.iter 
                  (fun (rcc, lines) ->
                    if rcc <> "stdcall" then
                        failwith "Unexpected calling convention"
                    let lastline = lines |> Seq.map (fun (_, _, _, linenum) -> linenum) |> Seq.max
                    let apl = apl lastline
                    apl "extern \"system\"{"
                    lines |> Seq.iter (fun (_,_,t,_) -> apl t)
                    apl "}"
                    ))
    
  let createIIDs()=
    iids 
        |> Map.toSeq 
        |> Seq.map (fun (iidname, ((fname, linenum,_,_), iid)) -> (fname, iidname, iid, linenum))
        |> Seq.groupBy (fun (fname, iidname, iid, linenum) -> fname) 
        |> Seq.iter 
            (fun (fname, iids) ->
                let f = rsfilename fname
                let apl = apl f
                for (_, iidname, iid, linenum) in iids do
                    apl linenum <| sprintf "DEFINE_GUID!{%s,\r\n    %s}" iidname (System.String.Join(", ", iid |> iid2array |> Array.toSeq))
                )    

  let createImports() =
    let apl = apl (headername+".rs") 1u
    let namedefs =
        typedefs
        |> Map.toSeq
        |> Seq.sortBy snd
        |> Seq.groupBy snd
    
    for (modname, sq) in namedefs do
        apl <| utils.formatUse 99 modname (sq |> Seq.map fst)
    apl ""
  // ----------------------- codeGen ------------------------------------------------
  createImports()
  createEnums()
  createStructs()
  createTypes()
  createDefines()
  createFunctions()
  createIIDs()
  (!file2sb |> Map.map (fun  k v -> System.String.Join("", v.Values)), typedefs)

