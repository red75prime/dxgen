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

            let precompiledHeader = 
                let header = FileInfo(codeModule.PrecompileHeader)
                if header.Exists then Some(header) else None

            for header in codeModule.Headers do
                let headerPath = (FileInfo(Path.Combine(sdkLocation, codeModule.IncludePath, header)))
                let headerInfo = HeaderLoader.loadHeader headerPath precompiledHeader
                                 |> TypeInfoBuilder.toHeaderTypeInfo header

                printfn "Processing header %s" headerPath.FullName

            printfn "%s" (System.String('-', 80))

    | None -> failwith "Invalid options."

    0 
