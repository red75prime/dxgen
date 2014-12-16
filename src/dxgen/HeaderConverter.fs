module HeaderConverter

open HeaderInfo
open HeaderLoader
open FSharpx

type private GuidRegex = Regex< @"GUID\(""(?<Guid>([^""]+))""\)">

let toHeaderTypeInfo (sourceFile: string) (headerRoot: Node): HeaderTypeInfo =
    let parseEnum (enumParent: Node) =
        let parseVariant =                
            function
            | { Info = NodeInfo(libclang.CursorKind.EnumConstantDecl, name); Value = Some(EnumValue(value)) } -> EnumVariant(name, value)
            | _ -> failwith "Unexpected enum variant."
                
        let rec parseVariants (accum: EnumVariant list) =
            function
            | [] -> accum |> List.rev
            | node :: nodes -> nodes |> parseVariants ((node |> parseVariant) :: accum)

        let (NodeInfo(_, name)) = enumParent.Info
        Enum(name, enumParent.Children |> parseVariants [])

    let parseStruct (structParent: Node) =
        let parseArrayBounds =
            function
            | [] -> None
            | nodes -> nodes 
                       |> List.map (function
                                    | { Info = NodeInfo(libclang.CursorKind.IntegerLiteral, _); Value = Some(LiteralValue(value)) } -> value |> System.Convert.ToUInt64
                                    | _ -> failwith "Unexpected literal value.") 
                       |> Some

        let parseField =
            function
            | { Info = NodeInfo(libclang.CursorKind.FieldDecl, name); Type = Some(NodeType(_, typeName)) } as node -> 
                StructField(typeName, name, node.Children |> parseArrayBounds)
            | _ -> failwith "Unexpected struct field."

        let rec parseFields (accum: StructField list) =
            function
            | [] -> accum |> List.rev
            | node :: nodes -> nodes |> parseFields ((node |> parseField) :: accum)

        let (NodeInfo(_, name)) = structParent.Info
        Struct(name, structParent.Children |> parseFields [])

    let parseMethod (methodParent: Node) =
        let rec getParameterAnnotation =
            function
            | [] -> In
            | { Info = NodeInfo(libclang.CursorKind.AnnotateAttr, name) } :: _ -> 
                match name with
                | "In" -> In
                | "Out" -> Out
                //TODO: Parse the remainder of the annotations.
                | _ -> failwith "Unsupported annotation."

            | _ :: nodes -> nodes |> getParameterAnnotation

        let parseParamter =
            function
            | { Info = NodeInfo(libclang.CursorKind.ParmDecl, parameterName) 
                Type = Some(NodeType(_, parameterTypeName)) } as node -> Parameter(parameterName, parameterTypeName, node.Children |> getParameterAnnotation)
            | _ -> failwith "Unexpected parameter node."

        let rec parseParameters (accum: Parameter list) =
            function
            | [] -> accum
            | ({ Info = NodeInfo(libclang.CursorKind.ParmDecl, _) } as node) :: nodes ->
                nodes |> parseParameters ((node |> parseParamter) :: accum)
            | _ :: nodes -> nodes |> parseParameters accum

        match methodParent with
        | { Info = NodeInfo(libclang.CursorKind.CxxMethod, name); Children = parameters; ResultType = Some(NodeType(_, returnTypeName)) } 
        | { Info = NodeInfo(libclang.CursorKind.FunctionDecl, name); Children = parameters; ResultType = Some(NodeType(_, returnTypeName)) } ->
            Method(name, parameters |> parseParameters [] |> List.rev, returnTypeName)
        | _ -> failwith "Unexpected method node."

    let rec parseInterface (interfaceParent: Node) =
        let rec parseMethods (accum: Method list) =
            function
            | [] -> accum |> List.rev
            | ({ Info = NodeInfo(libclang.CursorKind.CxxMethod, _) } as node) :: nodes -> nodes |> parseMethods ((node |> parseMethod) :: accum)
            | _ :: nodes -> nodes |> parseMethods accum

        let rec parseInterfaceId =
            function
            | [] -> failwith "No inteface id found."
            | { Info = NodeInfo(libclang.CursorKind.AnnotateAttr, attributeName) } :: _ when GuidRegex().IsMatch(attributeName, 0) ->
                match System.Guid.TryParse(GuidRegex().Match(attributeName).Guid.Value) with
                | (true, guidValue) -> guidValue.ToString().ToUpper()
                | _ -> failwith "Failed to parse GUID value for interface."
            | _ :: nodes -> nodes |> parseInterfaceId

        let rec parseBaseInterface =
            function
            | [] -> None
            | { Info = NodeInfo(libclang.CursorKind.CxxBaseSpecifier, _); Type = Some(NodeType(_, baseName)) } :: _ -> Some(baseName)
            | _ :: nodes -> nodes |> parseBaseInterface

        let (NodeInfo(_, name)) = interfaceParent.Info
        Interface(name, interfaceParent.Children |> parseBaseInterface, interfaceParent.Children |> parseMethods [], interfaceParent.Children |> parseInterfaceId)

    let rec toHeaderTypeInfo (accum: HeaderTypeInfo) =
        function
        | [] -> accum
        | { SourceFile = nodeSourceFile } as node :: nodes when nodeSourceFile = sourceFile -> 
            match node.Info with
            | NodeInfo(libclang.CursorKind.EnumDecl, _) -> nodes |> toHeaderTypeInfo { accum with Enums = (parseEnum node) :: accum.Enums }
            | NodeInfo(libclang.CursorKind.StructDecl, _) -> nodes |> toHeaderTypeInfo { accum with Structs = (parseStruct node) :: accum.Structs }
            | NodeInfo(libclang.CursorKind.ClassDecl, _) -> nodes |> toHeaderTypeInfo { accum with Interfaces = (parseInterface node) :: accum.Interfaces }
            | NodeInfo(libclang.CursorKind.FunctionDecl, _) -> nodes |> toHeaderTypeInfo { accum with Functions = (parseMethod node) :: accum.Functions }
            | _ -> nodes |> toHeaderTypeInfo accum
        | _ :: nodes -> nodes |> toHeaderTypeInfo accum

    let result = headerRoot.Children |> toHeaderTypeInfo HeaderTypeInfo.Default
    { result with Enums = result.Enums |> List.rev; Structs = result.Structs |> List.rev; Interfaces = result.Interfaces |> List.rev; Functions = result.Functions |> List.rev }