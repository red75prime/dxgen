module HeaderParser

open System.Runtime.InteropServices
open libclang

type ASTNode = {
    Kind: CursorKind
    Name: string
    TypeKind: TypeKind option
    TypeName: string option
    Children: ASTNode list
    EnumValue: int64 option
}

let private extractString (str: String) =
    let result = System.Runtime.InteropServices.Marshal.PtrToStringAnsi(str.data)
    libclang.disposeString(str)
    result

let private buildNode cursor = 
    let typeInfo = cursor 
                   |> getCursorType 
                   |> (fun ty -> if ty.kind = TypeKind.Invalid 
                                 then None 
                                 else Some(ty))

    let cursorKind = cursor |> getCursorKind

    { Kind = cursorKind
      Name = cursor |> getCursorSpelling |> extractString
      TypeKind = typeInfo |> Option.map (fun o -> o.kind)
      TypeName = typeInfo |> Option.map (getTypeSpelling >> extractString)
      EnumValue = if cursorKind = CursorKind.EnumConstantDecl then Some(cursor |> getEnumConstantDeclValue) else None
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