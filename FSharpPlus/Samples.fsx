#r @"..\packages\FsControl.1.0.3\lib\net40\FsControl.Core.dll"

#load "Operators.fs"
#load "Builders.fs"
#load "Extensions.fs"
#load "NonEmptyList.fs"
#load "ZipList.fs"
#load "ParallelArray.fs"

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

let r08 = r07.[1..2]
let r09 = (ZipList [1;2;3;4]).[2..3]

let r10 = (parse "12" + 11 |> toBytes |> toList).[0..1]

let r11 = [| 1..100000|]
let r12 = [|10..100000|]
let r13 = (+) <!> parray r11 <*> parray r12
let r14 = parray r11 |+| parray r12
let r15 = 10 *| parray r11 |+| parray r12 |- 5
let r16 = Const 1 </mappend/> parray [||]