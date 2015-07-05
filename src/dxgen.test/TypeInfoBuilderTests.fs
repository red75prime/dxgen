﻿module HeaderConverterTests

open System.IO
open HeaderLoader
open TypeInfoBuilder
open TypeInfo

open NUnit.Framework
open FsUnit

[<Test>]
let ``Converting a header containing an enum should produce a TypeInfo with the contents of the enum`` (): unit =
    loadHeader (FileInfo("./Data/enums.h")) None 
    |> buildTypeInfo "enums.h"
    |> should equal { TypeInfo.Default with Enums = [Enum("Test", [EnumVariant("A", 0L); EnumVariant("B", 1L) ; EnumVariant("C", 2L)])] }

[<Test>]
let ``Converting a header containing a struct should produce a TypeInfo with the contents of the struct`` (): unit =
    loadHeader (FileInfo("./Data/struct.h")) None 
    |> buildTypeInfo "struct.h"
    |> should equal { TypeInfo.Default with Structs = [Struct("Test", [StructField("int", "foo", None); StructField("const char *", "bar", None)])] }

[<Test>]
let ``Converting a header containing a struct with array members should produce a TypeInfo with the contents of the struct`` (): unit =
    loadHeader (FileInfo("./Data/struct_arrays.h")) None 
    |> buildTypeInfo "struct_arrays.h"
    |> should equal { TypeInfo.Default with Structs = [Struct("Test", [StructField("int [64]", "foo", Some([64UL])); StructField("int [8][8]", "bar", Some([8UL; 8UL]))])] }

[<Test>]
let ``An IUnknown interface definition should produce a TypeInfo with the contents of the interface`` (): unit =
    (loadHeader (FileInfo("./Data/IUnknown.h")) None
    |> buildTypeInfo "IUnknown.h").Interfaces
    |> should contain (Interface("IUnknown",
                                 None,
                                 [Method("QueryInterface",
                                         [Parameter("riid", "const IID &", In)
                                          Parameter("ppvObject", "void **", Out)], 
                                         "HRESULT")], 
                                 "00000000-0000-0000-C000-000000000046"))

[<Test>]
let ``An interface definition that derives from IUnknown should produce a TypeInfo containing that interface with an IUknown base specifier`` (): unit =
    (loadHeader (FileInfo("./Data/IUnknown.h")) None
    |> buildTypeInfo "IUnknown.h").Interfaces
    |> should contain (Interface("ITest", Some("IUnknown"), [], "32A3615B-3D98-48AA-A648-3EC4BD2E0440"))

[<Test>]
let ``A free function should produce a TypeInfo containing the definition for that free function`` (): unit =
    (loadHeader (FileInfo("./Data/IUnknown.h")) None
    |> buildTypeInfo "IUnknown.h").Functions
    |> should contain (Method("CreateTest", [Parameter("ppvTest", "ITest **", Out)], "HRESULT"))