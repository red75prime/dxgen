module HeaderAST

type Enum = Enum of name: string * value: int64

type Struct = Struct of fields: StructField list
and StructField = StructField of ty: string * name: string * arrayBounds: ArrayBound list option
and ArrayBound = ArrayBound of uint64

type Interface = Interface of methods: Method list * iid: string
and Method = Method of returnType: string * parameters: Parameter
and Parameter = MethodParameter of ty: string * name: string * annotation: ParameterAnnotation
and ParameterAnnotation =
    | In
    | InOptional
    | Out
    | OutOptional
    | InOut
    | InReads of string
    | InReadsBytes of string
    | OutWrites of string
    | OutWritesBytes of string
    | OutWritesOpt of string * string

type Header = {
    Name: string
    Enums: Enum list
    Structs: Struct list
    Interfaces: Interface list
}