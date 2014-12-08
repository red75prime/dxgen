module HeaderLoader

open System.Runtime.InteropServices
open Microsoft.FSharp.NativeInterop
open FSharpx
open libclang

type NodeInfo = NodeInfo of kind: CursorKind * name: string 
type NodeType = NodeType of kind: TypeKind * name: string

type NodeLiteralValue =
| EnumValue of int64
| LiteralValue of string

//TODO: May need to extract the value of string and float literals; TBD.
//TODO: Extract macro definitions.
type Node = {
    Info: NodeInfo
    Type: NodeType option
    Value: NodeLiteralValue option
    SourceFile: string
    Children: Node list
}

#nowarn "9"
let loadHeader (headerLocation: System.IO.FileInfo) (pchLocation: System.IO.FileInfo option) =
    let options = [| "-x"; "c++"; "-std=c++11"; "-fms-extensions"; "-fms-compatiblity"; "-fmsc-version=1800" |]
    let index = createIndex(0, 0)

    let pchTempLocation = Option.maybe {
        let! pchLocation = pchLocation
        let translationUnit = parseTranslationUnit(index, pchLocation.FullName, options, options.Length, [||], 0u, TranslationUnitFlags.None)

        return 
            try
                let fileLocation = System.IO.Path.GetTempFileName()
                saveTranslationUnit(translationUnit, fileLocation, 0u)
                System.IO.FileInfo(fileLocation)
            finally
                translationUnit |> disposeTranslationUnit
    }

    let options = Array.concat [options
                                pchTempLocation |> Option.map (fun pch -> [| "-include-pch"; pch.FullName |]) |> Option.getOrElse [||]]

    let translationUnit = parseTranslationUnit(index, headerLocation.FullName, options, options.Length, [||], 0u, TranslationUnitFlags.None)

    let toString (str: String) =
        let result = System.Runtime.InteropServices.Marshal.PtrToStringAnsi(str.data)
        disposeString(str)
        result

    let getNodeInfo cursor =
        (cursor |> getCursorKind, cursor |> getCursorSpelling |> toString) |> NodeInfo

    let getNodeType cursor =
        let typeInfo = cursor |> getCursorType
        match typeInfo.kind with
        | TypeKind.Invalid -> None
        | _ -> (typeInfo.kind, typeInfo |> getTypeSpelling |> toString) |> NodeType |> Some

    //TODO: Handle string literals differently.
    let getNodeValue cursor =
        match cursor |> getCursorKind with
        | CursorKind.EnumConstantDecl -> cursor |> getEnumConstantDeclValue |> EnumValue |> Some
        | CursorKind.IntegerLiteral -> 
            let range = cursor |> getCursorExtent
            let mutable tokens: nativeptr<Token> = Unchecked.defaultof<_>
            let mutable tokenCount = 0u

            tokenize(translationUnit, range, &tokens, &tokenCount)
             
            try
                (translationUnit, NativePtr.get tokens 0) |> getTokenSpelling |> toString |> LiteralValue |> Some
            finally
                disposeTokens(translationUnit, tokens, tokenCount)
        | _ -> None

    let getNodeSourceFile cursor =
        let range = cursor |> getCursorExtent |> getRangeStart
        let mutable file: File = File.Zero
        let mutable line = 0ul
        let mutable column = 0ul
        let mutable offset = 0ul

        getExpansionLocation(range, &file, &line, &column, &offset)

        file |> getFileName |> toString |> System.IO.Path.GetFileName
   
    let buildNode cursor = 
        { Info = cursor |> getNodeInfo
          Type = cursor |> getNodeType
          Value = cursor |> getNodeValue
          SourceFile = cursor |> getNodeSourceFile
          Children = [] }

    let rec childVisitor (cursor: Cursor) (_: Cursor) (clientData: ClientData): ChildVisitResult =
        let mutable handle = clientData |> GCHandle.FromIntPtr
        let childrenHandle = ([]: Node list) |> GCHandle.Alloc
 
        try
            (cursor, new CursorVisitor(childVisitor), childrenHandle |> GCHandle.ToIntPtr) |> libclang.visitChildren |> ignore
            let result = { (cursor |> buildNode) with Children = (childrenHandle.Target :?> list<Node>) |> List.rev } 
    
            handle.Target <- match handle.Target with
                             | :? list<Node> as siblings -> result :: siblings :> obj
                             | :? Node as root -> { root with Children = result :: root.Children } :> obj
                             | _ -> failwith("Unexpected type in node traversal.")
    
            ChildVisitResult.Continue
        finally
            childrenHandle.Free()

    let cursor = getTranslationUnitCursor(translationUnit)
    let nodesHandle = (cursor |> buildNode) |> GCHandle.Alloc
    visitChildren(cursor, new CursorVisitor(childVisitor), nodesHandle |> GCHandle.ToIntPtr) |> ignore

    try
        nodesHandle.Target :?> Node
    finally
        nodesHandle.Free()
        disposeTranslationUnit(translationUnit)
        disposeIndex(index)
        
        Option.maybe {
            let! pch = pchTempLocation
            pch.Delete()
        } |> ignore