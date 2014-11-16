// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.

open System.Runtime.InteropServices

type ASTNode = {
    Type: libclang.CursorKind
    Value: string
    Children: ASTNode list
}

let (|ExtractString|) (str: libclang.String) =
    let result = System.Runtime.InteropServices.Marshal.PtrToStringAnsi(str.data)
    libclang.disposeString(str)
    result

let rec visitChildren (cursor: libclang.Cursor) (parent: libclang.Cursor) (clientData: libclang.ClientData) =
    let (ExtractString name) = cursor |> libclang.getCursorSpelling
    let kind = cursor |> libclang.getCursorKind
    let mutable handle = GCHandle.FromIntPtr(clientData)

    let children: ASTNode list = []
    let childrenHandle = children |> GCHandle.Alloc

    libclang.visitChildren(cursor, new libclang.CursorVisitor(visitChildren), childrenHandle |> GCHandle.ToIntPtr) |> ignore
    let children = (childrenHandle.Target :?> list<ASTNode>)
    childrenHandle.Free()

    let result = { Type = kind; Value = name; Children =  children}
    let siblings = handle.Target :?> list<ASTNode>
    handle.Target <- result :: siblings

    libclang.ChildVisitResult.Continue

[<EntryPoint>]
let main argv = 
    let index = libclang.createIndex(0, 0)

    let options = [| "-x"; "c++"; "-std=c++11"; "-fms-extensions"; "-fms-compatiblity"; "-fmsc-version=1800" |]
    let translationUnit = libclang.parseTranslationUnit(index, @"C:\Program Files (x86)\Windows Kits\8.1\Include\shared\dxgi.h", options, options.Length, [||], 0u, libclang.TranslationUnitFlags.None)

    let cursor = libclang.getTranslationUnitCursor(translationUnit)

    let del: libclang.CursorVisitor = new libclang.CursorVisitor(visitChildren)
    let nodes: ASTNode list = []
    let nodesHandle = nodes |> GCHandle.Alloc
    libclang.visitChildren(cursor, del, nodesHandle |> GCHandle.ToIntPtr) |> ignore

    printfn "%A" (nodesHandle.Target :?> list<ASTNode>)
    nodesHandle.Free()

    libclang.disposeTranslationUnit(translationUnit)
    libclang.disposeIndex(index)

    0 // return an integer exit code
