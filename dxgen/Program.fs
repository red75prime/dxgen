// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.

[<EntryPoint>]
let main argv = 
    let index = libclang.createIndex(0, 0)
    libclang.disposeIndex(index)

    0 // return an integer exit code
