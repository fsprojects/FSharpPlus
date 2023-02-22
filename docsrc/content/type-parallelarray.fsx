(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#r @"../../src/FSharpPlus/bin/Release/netstandard2.0/FSharpPlus.dll"
(**
ParallelArray<'T>
=================

This type is basically a wrapper over Arrays, which:

 - Have a ZipList like applicative implementation.
 - Have parallel processing semantics by default.

Examples
--------
*)

(**
```f#
#r @"nuget: FSharpPlus"
```
*)
open FSharpPlus
open FSharpPlus.Data

let arr1 = [| 1..100000|]
let arr2 = [|10..100000|]

let arr1_plus_arr2  = (+) <!> parray arr1 <*> parray arr2

open FSharpPlus.Math.Applicative

let arr1_plus_arr2' = parray arr1 .+. parray arr2
let arrCombined     = 10 *. parray arr1 .+. parray arr2 .- 5
let asMonoid        = Infinite "Hello " </plus/> parray [|"City"; "World"; "Sun"|]