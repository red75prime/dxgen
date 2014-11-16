﻿module HeaderParser

open System.Runtime.InteropServices
open libclang

type ASTNode = {
    Kind: CursorKind
    Name: string
    TypeKind: TypeKind option
    TypeName: string option
    Children: ASTNode list
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
    { Kind = cursor |> getCursorKind
      Name = cursor |> getCursorSpelling |> extractString
      TypeKind = typeInfo |> Option.map (fun o -> o.kind)
      TypeName = typeInfo |> Option.map (getTypeSpelling >> extractString)
      Children = [] }

let rec private visitChildren (cursor: Cursor) (parent: Cursor) (clientData: ClientData): ChildVisitResult =
    let mutable handle = clientData |> GCHandle.FromIntPtr
    let childrenHandle = ([]: ASTNode list) |> GCHandle.Alloc

    (cursor, new CursorVisitor(visitChildren), childrenHandle |> GCHandle.ToIntPtr) |> libclang.visitChildren |> ignore
 
    let result = { (cursor |> buildNode) with Children = (childrenHandle.Target :?> list<ASTNode>) } 
    

    handle.Target <- match handle.Target with
                     | :? list<ASTNode> as siblings ->
                        result :: siblings :> obj
                     | :? ASTNode as root ->
                        { root with Children = result :: root.Children } :> obj
                     | _ -> failwith("Unexpected type in node traversal.")
    
    childrenHandle.Free()

    ChildVisitResult.Continue