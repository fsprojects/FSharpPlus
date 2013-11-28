#r @"..\..\FSharpPlus\bin\Release\FsControl.Core.dll"
#load @"..\Operators.fs"
#load @"..\ParallelArray.fs"

open FSharpPlus

let arr1 = [| 1..100000|]
let arr2 = [|10..100000|]

let arr1_plus_arr2  = (+) <!> parray arr1 <*> parray arr2
let arr1_plus_arr2' = parray arr1 |+| parray arr2
let arrCombined     = 10 *| parray arr1 |+| parray arr2 |- 5
let asMonoid        = Const "Hello " </mappend/> parray [|"City"; "World"; "Sun"|]