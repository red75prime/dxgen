module TypeInfoBuilder

open TypeInfo
open HeaderLoader
open FSharpx

type private GuidRegex = Regex< @"GUID\(""(?<Guid>([^""]+))""\)">

let toHeaderTypeInfo (sourceFile: string) (headerRoot: Node): TypeInfo =
    let parseEnum (enumParent: Node) =
        let parseVariant =                
            function
            | Kind(libclang.CursorKind.EnumConstantDecl, name) & Value(EnumValue(value)) -> EnumVariant(name, value)
            | _ -> failwith "Unexpected enum variant."
                
        let rec parseVariants (accum: EnumVariant list) =
            function
            | [] -> accum |> List.rev
            | node :: nodes -> nodes |> parseVariants ((node |> parseVariant) :: accum)

        let (Kind(_, name)) = enumParent
        Enum(name, enumParent.Children |> parseVariants [])

    let parseStruct (structParent: Node) =
        let parseArrayBounds =
            function
            | [] -> None
            | nodes -> nodes 
                       |> List.filter (fun (Kind(cursorKind, _)) -> cursorKind = libclang.CursorKind.IntegerLiteral)
                       |> List.map (function
                                    | Kind(libclang.CursorKind.IntegerLiteral, _) & Value(LiteralValue(value)) -> value |> System.Convert.ToUInt64
                                    | node -> failwith "Unexpected literal value.") 
                       |> Some

        let parseField =
            function
            | Kind(libclang.CursorKind.FieldDecl, name) & Type(_, typeName) as node -> 
                StructField(typeName, name, node.Children |> parseArrayBounds)
            | _ -> failwith "Unexpected struct field."

        let rec parseFields (accum: StructField list) =
            function
            | [] -> accum |> List.rev
            | Kind(libclang.CursorKind.FieldDecl, _) as node :: nodes -> nodes |> parseFields ((node |> parseField) :: accum)
            | UnknownNode :: nodes -> nodes |> parseFields accum

        let (Kind(_, name)) = structParent
        Struct(name, structParent.Children |> parseFields [])

    let parseMethod (methodParent: Node) =
        let rec getParameterAnnotation =
            function
            | [] -> In
            | Kind(libclang.CursorKind.AnnotateAttr, name) :: _ -> 
                match name with
                | "In" -> In
                | "In_Optional" -> InOptional
                | "Out" -> Out
                | "Out_Optional" -> OutOptional
                | "In_Out" -> InOut
                | annotation -> printfn "Warning: Unexpected parameter annotation (%s)." annotation
                                In

            | _ :: nodes -> nodes |> getParameterAnnotation

        let parseParamter =
            function
            | Kind(libclang.CursorKind.ParmDecl, parameterName) & Type(_, parameterTypeName) as node ->
                Parameter(parameterName, parameterTypeName, node.Children |> getParameterAnnotation)
            | _ -> failwith "Unexpected parameter node."

        let rec parseParameters (accum: Parameter list) =
            function
            | [] -> accum
            | (Kind(libclang.CursorKind.ParmDecl, _) as node) :: nodes ->
                nodes |> parseParameters ((node |> parseParamter) :: accum)
            | _ :: nodes -> nodes |> parseParameters accum

        match methodParent with
        | Kind(libclang.CursorKind.CxxMethod, name) & Children(parameters) & ResultType(_, returnTypeName) 
        | Kind(libclang.CursorKind.FunctionDecl, name) & Children(parameters) & ResultType(_, returnTypeName) ->
            Method(name, parameters |> parseParameters [] |> List.rev, returnTypeName)
        | _ -> failwith "Unexpected method node."

    let rec parseInterface (interfaceParent: Node) =
        let rec parseMethods (accum: Method list) =
            function
            | [] -> accum |> List.rev
            | (Kind(libclang.CursorKind.CxxMethod, _) as node) :: nodes -> nodes |> parseMethods ((node |> parseMethod) :: accum)
            | _ :: nodes -> nodes |> parseMethods accum

        let rec parseInterfaceId =
            function
            | [] -> failwith "No inteface id found."
            | Kind(libclang.CursorKind.AnnotateAttr, attributeName) :: _ when GuidRegex().IsMatch(attributeName, 0) ->
                match System.Guid.TryParse(GuidRegex().Match(attributeName).Guid.Value) with
                | (true, guidValue) -> guidValue.ToString().ToUpper()
                | _ -> failwith "Failed to parse GUID value for interface."
            | _ :: nodes -> nodes |> parseInterfaceId

        let rec parseBaseInterface =
            function
            | [] -> None
            | (Kind(libclang.CursorKind.CxxBaseSpecifier, _) & Type(_, baseName)) :: _ -> Some(baseName)
            | _ :: nodes -> nodes |> parseBaseInterface

        let (Kind(_, name)) = interfaceParent
        Interface(name, interfaceParent.Children |> parseBaseInterface, interfaceParent.Children |> parseMethods [], interfaceParent.Children |> parseInterfaceId)

    let rec buildTypeInfo (accum: TypeInfo) =
        function
        | [] -> accum
        | { SourceFile = nodeSourceFile } as node :: nodes when nodeSourceFile = sourceFile -> 
            match node with
            | Kind(libclang.CursorKind.EnumDecl, _) -> nodes |> buildTypeInfo { accum with Enums = (parseEnum node) :: accum.Enums }
            | Kind(libclang.CursorKind.StructDecl, _) -> nodes |> buildTypeInfo { accum with Structs = (parseStruct node) :: accum.Structs }
            | Kind(libclang.CursorKind.ClassDecl, _) -> nodes |> buildTypeInfo { accum with Interfaces = (parseInterface node) :: accum.Interfaces }
            | Kind(libclang.CursorKind.FunctionDecl, _) -> nodes |> buildTypeInfo { accum with Functions = (parseMethod node) :: accum.Functions }
            | Kind(libclang.CursorKind.UnexposedDecl, _) -> nodes |> buildTypeInfo (node.Children |> buildTypeInfo accum)
            | _ -> nodes |> buildTypeInfo accum
        | _ :: nodes -> nodes |> buildTypeInfo accum

    //TODO: Handle the case where things are nested?  Why is there an UnexposedDecl with all the DXGI in it?
    let result = headerRoot.Children |> buildTypeInfo TypeInfo.Default
    { result with Enums = result.Enums |> List.rev; Structs = result.Structs |> List.rev; Interfaces = result.Interfaces |> List.rev; Functions = result.Functions |> List.rev }