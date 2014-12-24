module ConfigurationTests

open NUnit.Framework
open FsUnit

open Configuration

let testConfig = 
    """---
    modules:
        - name: dx_dxgi
          output-location: ../test/
          headers:
              - dxgi_types.h
              - dxgi.h
    """

[<Test>]
let ``loadConfiguration should return the number of expected modules`` () =
    let config = testConfig
                 |> (fun s -> new System.IO.StringReader(s))
                 |> loadConfiguration

    config.Modules |> should haveCount 1

[<Test>]  
let ``loadConfiguration should contain a module with the expected name`` () =
    let config = testConfig
                 |> (fun s -> new System.IO.StringReader(s))
                 |> loadConfiguration

    (config.Modules |> Seq.exactlyOne).Name
    |> should equal "dx_dxgi"
   
[<Test>]  
let ``loadConfiguration should contain a module with the expected output location`` () =
    let config = testConfig
                 |> (fun s -> new System.IO.StringReader(s))
                 |> loadConfiguration

    (config.Modules |> Seq.exactlyOne).OutputLocation
    |> should equal @"../test/"

[<Test>]  
let ``loadConfiguration should contain a module with the expected headers`` () =
    let config = testConfig
                 |> (fun s -> new System.IO.StringReader(s))
                 |> loadConfiguration

    (config.Modules |> Seq.exactlyOne).Headers
    |> should equal ["dxgi_types.h"; "dxgi.h"]    