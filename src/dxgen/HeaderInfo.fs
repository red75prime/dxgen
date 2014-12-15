module HeaderInfo

type Enum = Enum of name: string * variants: EnumVariant list
and EnumVariant = EnumVariant of name: string * value: int64

type Struct = Struct of name: string * fields: StructField list
and StructField = StructField of ty: string * name: string * arrayBounds: uint64 list option

type Interface = Interface of name: string * baseName: string option * methods: Method list * iid: string
with override this.ToString() = sprintf "%A" this

and Method = Method of name: string * parameters: Parameter list * returnType: string
and Parameter = Parameter of name: string * parameterType: string * annotation: ParameterAnnotation
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
    Functions: Method list
}
with
    static member Default with get() = { Enums = []; Structs = []; Interfaces = []; Functions = [] }

    override this.ToString() = sprintf "%A" this