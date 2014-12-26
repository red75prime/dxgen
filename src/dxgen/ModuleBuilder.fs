module ModuleBuilder

open TypeInfo

type EnumBuilder = Enum -> string
type StructBuilder = Struct -> string
type InterfaceBuilder = Interface -> string

//Builder will generate a lib.rs which will re-export all the other files generated.
//Builder will generate one file per enum, struct, and interface.
//Builder will potentially perform name transformations to make the names more idiomatic rust.
//Builder will need to translate several types into rust equivalents.