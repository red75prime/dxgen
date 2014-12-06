module HeaderConverter

open CodeGenTree

let toCodeGenTree (headerRoot: HeaderLoader.Node): CodeGenTree.CodeGenTree =
    { CodeGenTree.Default with Enums = [Enum("Test", [EnumVariant("A", 0L); EnumVariant("B", 1L) ; EnumVariant("C", 2L)])] }