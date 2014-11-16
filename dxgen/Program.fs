// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.

let (|ExtractString|) (str: libclang.String) =
    let result = System.Runtime.InteropServices.Marshal.PtrToStringAnsi(str.data)
    libclang.disposeString(str)
    result

let visitChildren (cursor: libclang.Cursor) (parent: libclang.Cursor) (clientData: libclang.ClientData) =
    let (ExtractString name) = cursor |> libclang.getCursorSpelling
    let kind = cursor |> libclang.getCursorKind

    printfn "%s - %A" name kind
    libclang.ChildVisitResult.Recurse

[<EntryPoint>]
let main argv = 
    let index = libclang.createIndex(0, 0)

    let options = [| "-x"; "c++"; "-std=c++11"; "-fms-extensions"; "-fms-compatiblity"; "-fmsc-version=1800" |]
    let translationUnit = libclang.parseTranslationUnit(index, "C:\\Program Files (x86)\\Windows Kits\\8.1\\Include\\shared\\dxgi.h", options, options.Length, [||], 0u, libclang.TranslationUnitFlags.None)

    let cursor = libclang.getTranslationUnitCursor(translationUnit)

    let del: libclang.CursorVisitor = new libclang.CursorVisitor(visitChildren)
    libclang.visitChildren(cursor, del, libclang.ClientData.Zero) |> ignore

    libclang.disposeTranslationUnit(translationUnit)
    libclang.disposeIndex(index)

    0 // return an integer exit code
