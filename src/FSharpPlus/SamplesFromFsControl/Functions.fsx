#if INTERACTIVE
#r @"../bin/Release/net45/FSharpPlus.dll"
#else
module SamplesFromFsControl.Functions
#endif

open FSharpPlus.Operators

let inline flip f x y = f y x
let inline konst k _ = k
let inline (</) x = (|>) x
let inline (/>) x = flip x
let inline option n f = function None -> n | Some x -> f x


// Test Functors
let times2,minus3 = (*) 2, (-)/> 3
let resJust1      = map minus3 (Some 4)
let noValue       = map minus3 None
let lstTimes2     = map times2 [1;2;3;4]
let fTimes2minus3 = map minus3 times2
let res39         = fTimes2minus3 21
let res3n4        = result ((+) 2) <*> [1;2]



// Test numbers

let qr0  = divRem 7  3  //val qr0 : int * int = (2, 1)
let qr1  = divRem 7I 3I //val qr1 : System.Numerics.BigInteger * System.Numerics.BigInteger = (2, 1)
let qr2  = divRem 7. 3. //val qr2 : float * float = (2.333333333, 0.0) -> using default method.

let inline findMin (lst: 'a list) =
    let rec loop acc = function
        | [] -> acc
        | x::_ when x = minValue -> x
        | x::xs -> loop (if x < acc then x else acc) xs
    loop maxValue lst
    
let minInt  = findMin [1;0;12;2]
let minUInt = findMin [1u;0u;12u;2u]  // loops only twice




// Test Extension Methods
open FsControl

let mapp1 = [1..3] </plus/> [4..8]
let mapp2 = [1..3]  .Plus   [4..8]
let mcon1 = [|[|1..3|];[|4..5|]|] |> join
let mcon2 = [|[|1..3|];[|4..5|]|] .Join()  // Optional arguments work from F# 4.1. In C# you can write (new[] {new[] {1, 2, 3}, new[] {4, 5, 6}}).Join();

let arr = [|1..4|].Rev().Map((+) -1).Intersperse(10)
let lst =  [1..4] .Rev().Map((+) -1).Intersperse(10)

let opt  = [Some 1; Some 2].SequenceA()
let arr' = [|[|1|]; [|2|]|].SequenceA()