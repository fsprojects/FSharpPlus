#nowarn "3186"
#r @"..\..\build\FsControl.Core.dll"
#r @"..\..\build\FSharpPlus.dll"
open FSharpPlus

let lst5n6  = map ((+) 4) [ 1;2 ]
let arr5n6  = map ((+) 4) [|1;2|]
let lst5n6' = (+) <!> [4] <*> [1;2]