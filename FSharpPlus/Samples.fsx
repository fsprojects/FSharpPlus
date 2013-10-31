#r @"..\packages\FsControl.1.0.3\lib\net40\FsControl.Core.dll"
#load "Prelude.fs"
#load "Extensions.fs"
#load "NonEmptyList.fs"
#load "ZipList.fs"

open FSharpPlus
open FSharpPlus.Extensions

let r00 = map ((+)4) [1;2]
let r01 = (+) <!> [3] <*> [45]

let r02 = {Head = 1 ; Tail = [2;3;4;5]}
let r03 = extract   r02
let r04 = duplicate r02
let r05 = extend extract r02

let r06 = (+) <!> r02 <*> {Head = 100 ; Tail = [200]}

let r07 = monad {                
    let! x1 = {Head =  1; Tail = [2]}
    let! x2 = {Head = 10; Tail = [20]}
    return ((+) x1 x2)}

let r08 = (parse "12" + 11 |> toBytes |> toList).[0..1]