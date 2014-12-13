module CaseConverterTests

open CaseConverter
open NUnit.Framework
open FsUnit

[<Test>]
let ``Converting pascal case to snake case should produce an identifier that has snake case`` () =
    "TestIdentifier" |> toSnakeCase SourceCase.Pascal |> should equal "test_identifier"

[<Test>]
let ``Converting enum case to snake case should produce an identifier that has snake case`` () =
    "TEST_IDENTIFIER" |> toSnakeCase SourceCase.Enum |> should equal "test_identifier"


[<Test>]
let ``Converting pascal case to pascal case should produce an identifier that has pascal case`` () =
    "TestIdentifier" |> toPascalCase SourceCase.Pascal |> should equal "TestIdentifier"

[<Test>]
let ``Converting enum case to pascal case should produce an identifier that has pascal case`` () =
    "TEST_IDENTIFIER" |> toPascalCase SourceCase.Enum |> should equal "TestIdentifier"

[<Test>]
let ``Converting single word enum case to pascal case should produce an identifier that has pascal case`` () =
    "IDENTIFIER" |> toPascalCase SourceCase.Enum |> should equal "Identifier"