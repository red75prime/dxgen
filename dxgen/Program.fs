// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.

[<EntryPoint>]
let main argv = 
    let result = HeaderParser.buildAST "" @"C:\Program Files (x86)\Windows Kits\8.1\Include\shared\dxgi.h"

    printfn "%A" result

    0 // return an integer exit code
