// Learn more about F# at http://fsharp.org

open System
open Generate
[<EntryPoint>]
let main argv =
    // Generate
    copyFiles()
    buildDocumentation()
    0 // return an integer exit code
