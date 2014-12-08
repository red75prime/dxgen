module HeaderConverterTests

open System.IO
open HeaderConverter
open HeaderLoader
open HeaderInfo

open NUnit.Framework
open FsUnit

[<Test>]
let ``Converting a header containing an enum shield produce a HeaderTypeInfo with the contents of the enum`` (): unit =
    loadHeader (FileInfo("./Data/enums.h")) None 
    |> toHeaderTypeInfo 
    |> should equal { HeaderTypeInfo.Default with Enums = [Enum("Test", [EnumVariant("A", 0L); EnumVariant("B", 1L) ; EnumVariant("C", 2L)])] }

[<Test>]
let ``Converting a header containing a struct shield produce a HeaderTypeInfo with the contents of the struct`` (): unit =
    loadHeader (FileInfo("./Data/struct.h")) None 
    |> toHeaderTypeInfo 
    |> should equal { HeaderTypeInfo.Default with Structs = [Struct("Test", [StructField("int", "foo", None); StructField("const char *", "bar", None)])] }