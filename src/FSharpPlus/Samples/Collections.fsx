#r @"..\..\build\FsControl.Core.dll"
#r @"..\..\build\FSharpPlus.dll"

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


let inline myQuery x1 x2 = linq {
    for e1 in x1 do
    for e2 in x2 do
    where   (parse e1 + e2 < 23)
    groupBy (parse e1 + e2 %  2) into g
    sortBy  (-(fst g))
    select  (toString (fst g), snd g )}

let (arr1, arr2) = [|"1";"2";"3"|], [|10;20;30;40|]
let (lst1, lst2) = [ "1";"2";"3" ], [ 10;20;30;40 ]
let (seq1, seq2) = seq ["1";"2";"3"], seq [10;20;30;40]

let arr = myQuery arr1 arr2
let lst = myQuery lst1 lst2
let sq  = myQuery seq1 seq2


let inline myQuery2 x = linq {
    for e in x do
    groupAdjBy (parse e %  2) into g
    select  (toString (fst g), snd g )} 

let lst3 = myQuery2 [ "1";"2";"4";"3" ]
let seq3 = myQuery2 (Seq.initInfinite string)