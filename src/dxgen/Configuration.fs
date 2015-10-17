module Configuration

open SharpYaml.Serialization

[<CLIMutable>][<NoComparison>]
type Module = {
    [<YamlMember("name")>]
    Name : string

    [<YamlMember("headers")>]
    Headers : string seq

    [<YamlMember("output-path")>]
    OutputPath : string

    [<YamlMember("include-path")>]
    IncludePaths : string seq

    [<YamlMember("precompiled-header")>]
    PrecompileHeader : string

    [<YamlMember("no-winapi-gen")>]
    NoWinapiGen : bool
}

[<CLIMutable>][<NoComparison>]
type Configuration = {
    [<YamlMember("modules")>]
    Modules : Module seq
}
with
    override this.ToString() = sprintf "%A" this

let loadConfiguration (textReader: System.IO.TextReader) =
    let deserializer = Serializer()
    deserializer.Deserialize<Configuration>(textReader)