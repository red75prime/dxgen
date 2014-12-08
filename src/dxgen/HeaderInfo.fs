module HeaderInfo

type Enum = Enum of name: string * variants: EnumVariant list
and EnumVariant = EnumVariant of name: string * value: int64

type Struct = Struct of name: string * fields: StructField list
and StructField = StructField of ty: string * name: string * arrayBounds: ArrayBound list option
and ArrayBound = ArrayBound of uint64

type Interface = Interface of name: string * baseName: string * methods: Method list * iid: string
and Method = Method of returnType: string * parameters: Parameter
and Parameter = Parameter of ty: string * name: string * annotation: ParameterAnnotation
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
    //This will potentially need expanded.

type HeaderTypeInfo = {
    Enums: Enum list
    Structs: Struct list
    Interfaces: Interface list
}
with
    static member Default with get() = { Enums = []; Structs = []; Interfaces = [] }

    override this.ToString() = sprintf "%A" this