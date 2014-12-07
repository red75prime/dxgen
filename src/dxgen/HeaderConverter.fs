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

    let rec toCodeGenTree (accum: CodeGenTree) (nodes: Node list) =
        match nodes with
        | [] -> accum
        | node :: nodes -> 
            match node.Info with
            | NodeInfo(libclang.CursorKind.EnumDecl, _) -> nodes |> toCodeGenTree { accum with Enums = (parseEnum node) :: accum.Enums }
            | _ -> nodes |> toCodeGenTree accum

    headerRoot.Children |> toCodeGenTree CodeGenTree.Default