#r @"..\..\build\FsControl.Core.dll"
#r @"..\..\build\FSharpPlus.dll"

open FSharpPlus

let arr1 = [| 1..100000|]
let arr2 = [|10..100000|]

let arr1_plus_arr2  = (+) <!> parray arr1 <*> parray arr2

open ApplicativeMath

let arr1_plus_arr2' = parray arr1 |+| parray arr2
let arrCombined     = 10 *| parray arr1 |+| parray arr2 |- 5
let asMonoid        = Const "Hello " </mappend/> parray [|"City"; "World"; "Sun"|]