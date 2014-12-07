module HeaderConverter

open CodeGenTree
open HeaderLoader

let toCodeGenTree (headerRoot: Node): CodeGenTree =
    let parseEnum (enumParent: Node) =
        let rec parseVariants (accum: EnumVariant list) (nodes: Node list) =
            let parseVariant (node: Node) =
                let (NodeInfo(_, name)) = node.Info
                
                match node.NodeValue with
                | Some(EnumValue(value)) -> EnumVariant(name, value)
                | _ -> failwith "Unexpected node value."
                
            match nodes with
            | [] -> accum |> List.rev
            | node :: nodes -> nodes |> parseVariants ((node |> parseVariant) :: accum)

        let (NodeInfo(_, name)) = enumParent.Info
        Enum(name, enumParent.Children |> parseVariants [])

    let rec toCodeGenTree (nodes: Node list) (accum: CodeGenTree) =
        match nodes with
        | [] -> accum
        | node :: nodes -> 
            match node.Info with
            | NodeInfo(libclang.CursorKind.EnumDecl, _) -> toCodeGenTree nodes { accum with Enums = (parseEnum node) :: accum.Enums }
            | _ -> toCodeGenTree nodes accum

    toCodeGenTree headerRoot.Children CodeGenTree.Default
