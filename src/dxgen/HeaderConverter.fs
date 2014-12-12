module HeaderConverter

open HeaderInfo
open HeaderLoader

let toHeaderTypeInfo (headerRoot: Node): HeaderTypeInfo =
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

    let rec parseInterface (interfaceParent: Node) =
        ()

    let rec toHeaderTypeInfo (accum: HeaderTypeInfo) =
        function
        | [] -> accum
        | node :: nodes -> 
            match node.Info with
            | NodeInfo(libclang.CursorKind.EnumDecl, _) -> nodes |> toHeaderTypeInfo { accum with Enums = (parseEnum node) :: accum.Enums }
            | NodeInfo(libclang.CursorKind.StructDecl, _) -> nodes |> toHeaderTypeInfo { accum with Structs = (parseStruct node) :: accum.Structs }
            | _ -> nodes |> toHeaderTypeInfo accum

    headerRoot.Children |> toHeaderTypeInfo HeaderTypeInfo.Default