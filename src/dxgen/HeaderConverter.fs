module HeaderConverter

open HeaderInfo
open HeaderLoader

let toHeaderTypeInfo (headerRoot: Node): HeaderTypeInfo =
    let parseEnum (enumParent: Node) =
        let rec parseVariants (accum: EnumVariant list) (nodes: Node list) =
            let parseVariant (node: Node) =                
                match node with
                | { Info = NodeInfo(libclang.CursorKind.EnumConstantDecl, name); Value = Some(EnumValue(value)) } -> EnumVariant(name, value)
                | _ -> failwith "Unexpected enum variant."
                
            match nodes with
            | [] -> accum |> List.rev
            | node :: nodes -> nodes |> parseVariants ((node |> parseVariant) :: accum)

        let (NodeInfo(_, name)) = enumParent.Info
        Enum(name, enumParent.Children |> parseVariants [])

    let parseStruct (structParent: Node) =
        printfn "%A" structParent

        let rec parseFields (accum: StructField list) (nodes: Node list) =
            let parseField (node: Node) =
                match node with
                | { Info = NodeInfo(libclang.CursorKind.FieldDecl, name); Type = Some(NodeType(_, typeName)) } -> StructField(typeName, name, None)
                | _ -> failwith "Unexpected struct field."

            match nodes with
            | [] -> accum |> List.rev
            | node :: nodes -> nodes |> parseFields ((node |> parseField) :: accum)

        let (NodeInfo(_, name)) = structParent.Info
        Struct(name, structParent.Children |> parseFields [])

    let rec toHeaderTypeInfo (accum: HeaderTypeInfo) (nodes: Node list) =
        match nodes with
        | [] -> accum
        | node :: nodes -> 
            match node.Info with
            | NodeInfo(libclang.CursorKind.EnumDecl, _) -> nodes |> toHeaderTypeInfo { accum with Enums = (parseEnum node) :: accum.Enums }
            | NodeInfo(libclang.CursorKind.StructDecl, _) -> nodes |> toHeaderTypeInfo { accum with Structs = (parseStruct node) :: accum.Structs }
            | _ -> nodes |> toHeaderTypeInfo accum

    headerRoot.Children |> toHeaderTypeInfo HeaderTypeInfo.Default