#r @"packages/FAKE/tools/FakeLib.dll"
open Fake

RestorePackages()

let buildDir = "./build/"
let testDir = "./test/"

Target "Clean" (fun _ -> CleanDirs [buildDir; testDir])

Target "BuildDxgen" (fun _ ->
    !! "src/dxgen/**/*.fsproj"
        |> MSBuildRelease buildDir "Build"
        |> Log "Dxgen Build Output: "
)

Target "BuildDxgenTest" (fun _ ->
    !! "src/dxgen.test/**/*.fsproj"
        |> MSBuildDebug testDir "Build"
        |> Log "Dxgen Test Build Output: "
)

Target "Test" (fun _ ->
    let nunitRunner = findToolInSubPath "nunit-console-x86.exe" "./packages/NUnit.Runners/"

    !! (testDir + "/*.test.dll")
        |> NUnit (fun p -> 
                    { p with
                        DisableShadowCopy = true
                        OutputFile = testDir + "TestResults.xml"
                        ToolPath = ""
                        ToolName = nunitRunner })
)

Target "Default" (fun _ -> ())

"Clean"
 ==> "BuildDxgen"
 ==> "BuildDxgenTest"
 ==> "Test"
 ==> "Default"

RunTargetOrDefault "Default"