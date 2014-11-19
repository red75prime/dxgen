module HeaderParser

open System.Runtime.InteropServices
open libclang


type NodeInfo = NodeInfo of kind: CursorKind * name: string 
type NodeType = NodeType of kind: TypeKind * name: string

type NodeLiteralValue =
| EnumValue of int64
| LiteralValue of string

//TODO: Get the value of any literals (pack this with enum values?)
//TODO: Try to extract defines?
type ASTNode = {
    Info: NodeInfo
    Type: NodeType option
    NodeValue: NodeLiteralValue option
    SourceFile: string
    Children: ASTNode list
}

let private toString (str: String) =
    let result = System.Runtime.InteropServices.Marshal.PtrToStringAnsi(str.data)
    libclang.disposeString(str)
    result

let private getNodeInfo cursor =
    (cursor |> getCursorKind, cursor |> getCursorSpelling |> toString) |> NodeInfo

let private getNodeType cursor =
     cursor |> getCursorType 
            |> (fun ty -> if ty.kind = TypeKind.Invalid then None else Some(ty))
            |> Option.map (fun ti -> (ti.kind, ti |> getTypeSpelling |> toString) |> NodeType)

let private getNodeValue cursor =
//    EnumValue = if cursorKind = CursorKind.EnumConstantDecl then Some(cursor |> getEnumConstantDeclValue) else None
//    LiteralValue = None
    None

let private getNodeSourceFile cursor =
    let range = cursor |> getCursorExtent |> getRangeStart
    let mutable file: File = File.Zero
    let mutable line = 0ul
    let mutable column = 0ul
    let mutable offset = 0ul

    getExpansionLocation(range, &file, &line, &column, &offset)

    file |> getFileName |> toString |> System.IO.Path.GetFileName
   
let private buildNode cursor = 
    { Info = cursor |> getNodeInfo
      Type = cursor |> getNodeType
      NodeValue = cursor |> getNodeValue
      SourceFile = cursor |> getNodeSourceFile
      Children = [] }

let rec private childVisitor (cursor: Cursor) (parent: Cursor) (clientData: ClientData): ChildVisitResult =
    let mutable handle = clientData |> GCHandle.FromIntPtr
    let childrenHandle = ([]: ASTNode list) |> GCHandle.Alloc
 
    try
        (cursor, new CursorVisitor(childVisitor), childrenHandle |> GCHandle.ToIntPtr) |> libclang.visitChildren |> ignore
        let result = { (cursor |> buildNode) with Children = (childrenHandle.Target :?> list<ASTNode>) |> List.rev } 
    
        handle.Target <- match handle.Target with
                         | :? list<ASTNode> as siblings -> result :: siblings :> obj
                         | :? ASTNode as root -> { root with Children = result :: root.Children } :> obj
                         | _ -> failwith("Unexpected type in node traversal.")
    
        ChildVisitResult.Continue
    finally
        childrenHandle.Free()

//TODO: Compile and use the pre-compiled header file.
let buildAST pchLocation headerLocation =
    let options = [| "-x"; "c++"; "-std=c++11"; "-fms-extensions"; "-fms-compatiblity"; "-fmsc-version=1800" |]
    let index = createIndex(0, 0)
    let translationUnit = parseTranslationUnit(index, headerLocation, options, options.Length, [||], 0u, TranslationUnitFlags.None)
    let cursor = getTranslationUnitCursor(translationUnit)
    let nodesHandle = (cursor |> buildNode) |> GCHandle.Alloc

    visitChildren(cursor, new CursorVisitor(childVisitor), nodesHandle |> GCHandle.ToIntPtr) |> ignore

    try
        nodesHandle.Target :?> ASTNode
    finally
        nodesHandle.Free()
        disposeTranslationUnit(translationUnit)
        disposeIndex(index)