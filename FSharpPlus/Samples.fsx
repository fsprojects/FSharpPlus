#r @"..\packages\FsControl.1.0.2\lib\net40\FsControl.Core.dll"
#load "Prelude.fs"

open FSharpPlus

let x = map ((+)4) [1;2]
let y = (+) <!> [3] <*> [45]