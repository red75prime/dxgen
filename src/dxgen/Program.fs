// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.
module Program

open System.IO

[<EntryPoint>]
let main argv = 
    let result = HeaderLoader.loadHeader (FileInfo(@"C:\Program Files (x86)\Windows Kits\8.1\Include\shared\dxgi.h")) (Some(FileInfo(@".\PCH\DXGI_PCH.h")))

    use file = System.IO.File.CreateText("test.txt")
    fprintfn file "%A" result

    0 // return an integer exit code
