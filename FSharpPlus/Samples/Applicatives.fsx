#r @"..\..\FSharpPlus\bin\Release\FsControl.Core.dll"
#load @"..\Operators.fs"
open FSharpPlus

let lst5n6  = map ((+) 4) [ 1;2 ]
let arr5n6  = map ((+) 4) [|1;2|]
let lst5n6' = (+) <!> [4] <*> [1;2]