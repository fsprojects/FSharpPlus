#r @"..\..\packages\FsControl.1.0.5\lib\net40\FsControl.Core.dll"
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


let arr1 = [|"1";"2";"3"|]
let arr2 = [|10;20;30;40|]

let arr = linq {
    for e1 in arr1 do
    for e2 in arr2 do
    where  (parse e1 + e2 < 23)
    select (e1,e2)   }


let lst1 = ["1";"2";"3"]
let lst2 = [10;20;30;40]

let lst = linq {
    for e1 in lst1 do
    for e2 in lst2 do
    where  (parse e1 + e2 < 23)
    select (e1,e2)   }