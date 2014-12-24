module Configuration

open SharpYaml.Serialization

[<CLIMutable>]
type Module = {
    [<YamlMember("name")>]
    Name : string

    [<YamlMember("headers")>]
    Headers : string seq

    [<YamlMember("output-path")>]
    OutputPath : string

    [<YamlMember("include-path")>]
    IncludePath : string

    [<YamlMember("precompiled-header")>]
    PrecompileHeader : string
}

[<CLIMutable>]
type Configuration = {
    [<YamlMember("modules")>]
    Modules : Module seq
}
with
    override this.ToString() = sprintf "%A" this

let loadConfiguration (textReader: System.IO.TextReader) =
    let deserializer = Serializer()
    deserializer.Deserialize<Configuration>(textReader)