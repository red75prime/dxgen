// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.
module Program

open System.IO
open CommandLine

[<CLIMutable>]
type Options = {
    [<Option('c', "config", DefaultValue = "config.yaml", HelpText="Location of the input configuration file")>]
    ConfigurationFile: string
}
with
    static member Default with get() = { ConfigurationFile = "config.yaml" }

    static member Parse args =
        let options = Options.Default
        
        if Parser.Default.ParseArguments(args, options) then
            Some(options)
        else
            None

open annotations
open annotations_autogen
//TODO: get annotations from files
let annotations_by_module = 
  [
    ("d3d12",
      {interfaces = d3d12annotations;
      enums = d3d12enums;
      structs = d3d12structs;
      dependencies = [];
    });
    ("d3d12sdklayers",
      {interfaces = d3d12sdklayers;
      enums = d3d12enums;
      structs = d3d12structs;
      dependencies = [];
    });
    ("d3dcommon",
      {interfaces = d3dcommon;
      enums = d3d12enums;
      structs = d3d12structs;
      dependencies = [];
    });
    ("dxgi",
      {interfaces = dxgi;
      enums = d3d12enums;
      structs = d3d12structs;
      dependencies = [];
    });
    ("dxgi1_2",
      {interfaces = dxgi1_2;
      enums = d3d12enums;
      structs = d3d12structs;
      dependencies = ["dxgi"];
    });
    ("dxgi1_3",
      {interfaces = dxgi1_3;
      enums = d3d12enums;
      structs = d3d12structs;
      dependencies = ["dxgi";"dxgi1_2"];
    });
    ("dxgi1_4",
      {interfaces = dxgi1_4;
      enums = d3d12enums;
      structs = d3d12structs;
      dependencies = ["dxgi";"dxgi1_2";"dxgi1_3"];
    });
    ("dwrite",
      {interfaces = dwrite;
      enums = Map.empty;
      structs = Map.empty;
      dependencies = [];
    });
    ("d2d1",
      {interfaces = d2d1;
      enums = Map.empty;
      structs = Map.empty;
      dependencies = [];
    });
    ("d3d12shader",
      {interfaces = d3d12shader;
      enums = Map.empty;
      structs = Map.empty;
      dependencies = [];
    });
  ] |> Map.ofList
        
[<EntryPoint>]
let main argv = 
    let options = Options.Parse argv

    match options with
    | Some(opt) -> 
        use fileStream = File.OpenText(opt.ConfigurationFile)
        let config = fileStream |> Configuration.loadConfiguration
        let sdkLocation = SDKLocator.findSDKRootDirectory ()

        let allInterfaces = ref Map.empty
        let modules = ref []

        for codeModule in config.Modules do
            printfn "Processing module %s:" codeModule.Name


            let precompiledHeader = 
              if codeModule.PrecompileHeader = null then
                None
              else
                let header = FileInfo(codeModule.PrecompileHeader)
                if header.Exists then Some(header) else None

            for headerInfo in codeModule.Headers do
                let header = headerInfo.Name

                let headerPath = 
                  try 
                      codeModule.IncludePaths 
                        |> Seq.map (fun incPath -> FileInfo(Path.Combine(sdkLocation, incPath, header)))
                        |> Seq.find (fun fi -> fi.Exists)
                  with
                  | :? System.Collections.Generic.KeyNotFoundException as e -> 
                    printfn "Header file '%s' wasn't found" header
                    raise e
                  |e -> raise e
                
                let includePaths=
                  codeModule.IncludePaths |> Seq.map (fun incPath -> Path.Combine(sdkLocation, incPath))
                  
                let headerName = 
                  let lastpoint = header.LastIndexOf('.')
                  if lastpoint = -1 then
                    header
                  else
                    header.Substring(0, lastpoint)

                let types = parse.combinedParse headerPath precompiledHeader includePaths
                let atext = safegen.emptyAnnotationsGen types
                System.IO.Directory.CreateDirectory(@".\" + headerName) |> ignore 
                use swa=new System.IO.StreamWriter(@".\" + headerName + @"\annotations_autogen.fs")
                swa.Write(atext)

                match Map.tryFind (headerName) annotations_by_module with
                |None ->
                  printfn "  Error: no annotations for %s" headerName
                |Some(annotations) ->
                  if not (codeModule.NoWinapiGen) then
                    let wapi = sysgen.winapiGen types annotations
                    System.IO.Directory.CreateDirectory(@".\winapi") |> ignore //TODO: use Path
                    for KeyValue(f,t) in wapi do
                      use sw=new System.IO.StreamWriter(@".\winapi\"+f)
                      sw.Write(t)
                  let (rtext, interfaces, deps)=safegen.safeInterfaceGen headerName (headerInfo.Uses) (!allInterfaces) (codeModule.NoEnumConversion) types annotations
                  allInterfaces := interfaces
                  let moduleName = headerName+"_safe"
                  System.IO.Directory.CreateDirectory(@".\safe") |> ignore //TODO: use Path
                  use swsi=new System.IO.StreamWriter(@".\safe\"+moduleName+".rs")
//                  for modl in annotations.dependencies do
//                    swsi.WriteLine("use "+modl+"_safe::*;")
                  swsi.Write(rtext)
                  modules := moduleName :: !modules
                  printfn "Processing header %s" headerPath.FullName

            //safegen.whatDoWeHaveP()

            printfn "%s" (System.String('-', 80))

    | None -> failwith "Invalid options."

    0 
