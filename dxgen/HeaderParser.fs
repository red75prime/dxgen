module HeaderParser

open System.Runtime.InteropServices
open libclang

type ASTNode = {
    Kind: CursorKind
    Name: string
    Children: ASTNode list
}

let private buildNode kind name children = {
        Kind = kind
        Name = name
        Children = children
    }

let private extractString (str: String) =
    let result = System.Runtime.InteropServices.Marshal.PtrToStringAnsi(str.data)
    libclang.disposeString(str)
    result

let rec private visitChildren (cursor: Cursor) (parent: Cursor) (clientData: ClientData): ChildVisitResult =
    let mutable handle = clientData |> GCHandle.FromIntPtr
    let childrenHandle = ([]: ASTNode list) |> GCHandle.Alloc

    (cursor, new CursorVisitor(visitChildren), childrenHandle |> GCHandle.ToIntPtr) |> libclang.visitChildren |> ignore
 
    let result = { 
        Kind = cursor |> getCursorKind
        Name = cursor |> getCursorSpelling |> extractString
        Children =  (childrenHandle.Target :?> list<ASTNode>)
    }

    handle.Target <- match handle.Target with
                     | :? list<ASTNode> as siblings ->
                        result :: siblings :> obj
                     | :? ASTNode as root ->
                        { root with Children = result :: root.Children } :> obj
                     | _ -> failwith("Unexpected type in node traversal.")
    
    childrenHandle.Free()

    libclang.ChildVisitResult.Continue