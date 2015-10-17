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
let annotations_by_module = 
  [
    ("d3d12_dxgi",
      {interfaces = d3d12annotations_prime;
      interfacesFull = d3d12annotations;
      enums = d3d12enums;
      structs = d3d12structs
    });
    ("dwrite",
      {interfaces = [];
      interfacesFull = [];
      enums = Map.empty;
      structs = Map.empty
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

        for codeModule in config.Modules do
            printfn "Processing module %s:" codeModule.Name

            match Map.tryFind (codeModule.Name) annotations_by_module with
            |None ->
              printfn "  Error: no annotations for %s" codeModule.Name
            |Some(annotations) ->

              let precompiledHeader = 
                  let header = FileInfo(codeModule.PrecompileHeader)
                  if header.Exists then Some(header) else None

              for header in codeModule.Headers do
                  let headerPath = 
                    codeModule.IncludePaths 
                      |> Seq.map (fun incPath -> FileInfo(Path.Combine(sdkLocation, incPath, header)))
                      |> Seq.find (fun fi -> fi.Exists)
                
                  let includePaths=
                    codeModule.IncludePaths |> Seq.map (fun incPath -> Path.Combine(sdkLocation, incPath))

                  let types = parse.combinedParse headerPath precompiledHeader includePaths
                  if not (codeModule.NoWinapiGen) then
                    let wapi = sysgen.winapiGen types annotations
                    System.IO.Directory.CreateDirectory(@".\winapi") |> ignore //TODO: use Path
                    for KeyValue(f,t) in wapi do
                      use sw=new System.IO.StreamWriter(@".\winapi\"+f)
                      sw.Write(t)
                  let atext = safegen.emptyAnnotationsGen types
                  System.IO.Directory.CreateDirectory(@".\"+codeModule.Name) |> ignore 
                  use swa=new System.IO.StreamWriter(@".\" + codeModule.Name + @"\annotations_autogen.fs")
                  swa.Write(atext)
                  let rtext=safegen.safeInterfaceGen types annotations
                  use swsi=new System.IO.StreamWriter(@".\d3d12.rs")
                  swsi.Write(rtext)

                  printfn "Processing header %s" headerPath.FullName

            //safegen.whatDoWeHaveP()

            printfn "%s" (System.String('-', 80))

    | None -> failwith "Invalid options."

    0 
