module ConfigurationLoader

open SharpYaml.Serialization

let loadConfiguration file =
    use fileReader = System.IO.File.OpenText(file)
    let yamlStream = YamlStream()

    yamlStream.Load(fileReader)

    ()