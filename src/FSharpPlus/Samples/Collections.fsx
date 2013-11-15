#r @"..\..\packages\FsControl.1.0.6\lib\net40\FsControl.Core.dll"
#load @"..\Operators.fs"
#load @"..\Builders.fs"
#load @"..\Extensions.fs"
#load @"..\NonEmptyList.fs"

open FSharpPlus

let inline print x = async { System.Console.WriteLine(toString x) }
let inline (++) a b = mappend a b
async {
    let arr = [|1;2;3;4|]
    let skip2 = skip 2 arr
    do! print skip2
    let nel = { NonEmptyList.Head = 1; Tail = [2;3;4] } ++ { NonEmptyList.Head = 5; Tail = [6] }
    do! print nel
} |> Async.RunSynchronously


let inline f x1 x2 = linq {
    for e1 in x1 do
    for e2 in x2 do
    where  (parse e1 + e2 < 23)
    select (e1,e2)   }

let (arr1, arr2) = [|"1";"2";"3"|], [|10;20;30;40|]
let (lst1, lst2) = [ "1";"2";"3" ], [ 10;20;30;40 ]

let (arr, lst) = f arr1 arr2, f lst1 lst2