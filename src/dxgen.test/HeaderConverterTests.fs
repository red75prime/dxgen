module HeaderConverterTests

open System.IO
open HeaderConverter
open HeaderLoader
open CodeGenTree

open NUnit.Framework
open FsUnit

[<Test>]
let ``Converting a header containing an enum shield produce a CodeGenTree with the contents of the enum`` (): unit =
    loadHeader (FileInfo("./Data/enums.h")) None 
    |> toCodeGenTree 
    |> should equal { CodeGenTree.Default with Enums = [Enum("Test", [EnumVariant("A", 0L); EnumVariant("B", 1L) ; EnumVariant("C", 2L)])] }