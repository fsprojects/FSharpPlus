#if INTERACTIVE
#r @"../../FSharpPlus/bin/Release/net8.0/FSharpPlus.dll"
#else
module Samples.Collections
#endif

open FSharpPlus
open FSharpPlus.Data

let inline print x = async { System.Console.WriteLine(string x) }

let nel = { NonEmptyList.Head = 1; Tail = [2;3;4] }
let dCnt = Seq.length nel
let sCnt = length nel

async {
    let arr = [|1;2;3;4|]
    let skip2 = skip 2 arr
    do! print skip2
    let nel = { NonEmptyList.Head = 1; Tail = [2;3;4] } ++ { NonEmptyList.Head = 5; Tail = [6] }
    do! print nel
} |> Async.RunSynchronously

let topTenResults : seq<_> = monad.plus {
    for i in 0..100 do
    for j in 0..100 do
    where (i = j)
    top 10}

let inline myQuery x1 x2 = monad {
    let! e1 = x1
    let! e2 = x2
    where   (parse e1 + e2 < 23)
    groupBy (parse e1 + e2 %  2) into g    
    orderBy  (-(fst g))
    select  (string (fst g), snd g )    
    }

let (arr1, arr2) = [|"1";"2";"3"|], [|10;20;30;40|]
let (lst1, lst2) = [ "1";"2";"3" ], [ 10;20;30;40 ]
let (seq1, seq2) = seq ["1";"2";"3"], seq [10;20;30;40]

let arr = myQuery arr1 arr2
let lst = myQuery lst1 lst2
let sq  = myQuery seq1 seq2

let inline myQuery2 x = monad {
    let! e = x
    chunkBy (parse e %  2) into g
    select  (string (fst g), snd g )}

let lst3 = myQuery2 [ "1";"2";"4";"3" ]
let seq3 = myQuery2 (Seq.initInfinite string)



// Test fallback for mfilter


type WrappedListB<'s> = WrappedListB of 's list with
    static member Return   (x) = WrappedListB [x]
    static member (+)  (WrappedListB l, WrappedListB x) = WrappedListB (l @ x)
    static member Zero   = WrappedListB List.empty
    static member ToSeq    (WrappedListB lst)     = List.toSeq lst
    static member FoldBack (WrappedListB x, f, z) = List.foldBack f x z


open FSharpPlus.Control

type WrappedListB'<'s> = WrappedListB' of 's list with // Same as B but without clean signatures
    static member Return   (_:WrappedListB'<'a>, _:Return ) = fun (x:'a)     -> WrappedListB' [x]
    static member (+)      (WrappedListB' l, WrappedListB' x) = WrappedListB' (l @ x)
    static member Zero     (_:WrappedListB'<'a>, _:Zero) = WrappedListB' List.empty
    static member ToSeq    (WrappedListB' lst)     = List.toSeq lst
    static member FoldBack (WrappedListB' x, f, z) = List.foldBack f x z


let five   = filter ((=) 5) (WrappedListB [1;2;3;4;5;6])   // <- Uses the default method for filter.

#nowarn 3873 // interrupt compilation

let five'   = filter ((=) 5) (WrappedListB' [1;2;3;4;5;6])   // <- Uses the default method for filter.
