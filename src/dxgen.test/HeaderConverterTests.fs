module HeaderConverterTests

open System.IO
open HeaderConverter
open HeaderLoader
open HeaderInfo

open NUnit.Framework
open FsUnit

[<Test>]
let ``Converting a header containing an enum should produce a HeaderTypeInfo with the contents of the enum`` (): unit =
    loadHeader (FileInfo("./Data/enums.h")) None 
    |> toHeaderTypeInfo 
    |> should equal { HeaderTypeInfo.Default with Enums = [Enum("Test", [EnumVariant("A", 0L); EnumVariant("B", 1L) ; EnumVariant("C", 2L)])] }

[<Test>]
let ``Converting a header containing a struct should produce a HeaderTypeInfo with the contents of the struct`` (): unit =
    loadHeader (FileInfo("./Data/struct.h")) None 
    |> toHeaderTypeInfo 
    |> should equal { HeaderTypeInfo.Default with Structs = [Struct("Test", [StructField("int", "foo", None); StructField("const char *", "bar", None)])] }

[<Test>]
let ``Converting a header containing a struct with array members should produce a HeaderTypeInfo with the contents of the struct`` (): unit =
    loadHeader (FileInfo("./Data/struct_arrays.h")) None 
    |> toHeaderTypeInfo 
    |> should equal { HeaderTypeInfo.Default with Structs = [Struct("Test", [StructField("int [64]", "foo", Some([64UL])); StructField("int [8][8]", "bar", Some([8UL; 8UL]))])] }

[<Test>]
let ``Converting a header containing an IUnknown definition should produce a HeaderTypeInfo with the contents of the interface`` (): unit =
    loadHeader (FileInfo("./Data/IUnknown.h")) None
    |> toHeaderTypeInfo
    |> should equal { HeaderTypeInfo.Default with Interfaces = [Interface("IUnknown",
                                                                          None,
                                                                          [Method("IUnknown",
                                                                                  [Parameter("const IID &", "riid", In)
                                                                                   Parameter("void * *", "ppvObject", Out)], 
                                                                                  "HRESULT")], 
                                                                          "00000000-0000-0000-C000-000000000046")]}