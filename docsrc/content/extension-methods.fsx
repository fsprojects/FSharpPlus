(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../../bin"

#r @"../../src/FSharpPlus/bin/Release/net45/FSharpPlus.dll"
open FSharpPlus

(**
Extension Methods
=================

*)

(**

Some methods are also exposed as extensions. This makes possible some uses from C#

Here are some examples:

*)

open FSharpPlus.Control

let mapp1 = [1..3] </plus/> [4..8]
let mapp2 = [1..3]  .Plus   [4..8]
let mcon1 = [|[|1..3|];[|4..5|]|] |> join
let mcon2 = [|[|1..3|];[|4..5|]|] .Join()  // Optional arguments work from F# 4.1. In C# you can write (new[] {new[] {1, 2, 3}, new[] {4, 5, 6}}).Join();

let arr = [|1..4|].Rev().Map((+) -1).Intersperse(10)
let lst =  [1..4] .Rev().Map((+) -1).Intersperse(10)

let opt  = [Some 1; Some 2].Sequence()
let arr' = [|[|1|]; [|2|]|].Sequence()