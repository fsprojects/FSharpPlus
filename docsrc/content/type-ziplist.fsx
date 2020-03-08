(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../../bin"

(**
ZipList<'T>
===========

This is a wrapper over seq<'T> which changes its applicative semantic to point-wise processing.

Examples
--------
*)


#r @"../../src/FSharpPlus/bin/Release/net45/FSharpPlus.dll"

open FSharpPlus
open FSharpPlus.Data

let seq1 = seq { 1..100000}
let seq2 = seq {10..100000}

let seq1_plus_seq2  = (+) <!> ZipList seq1 <*> ZipList seq2

open FSharpPlus.Math.Applicative

let seq1_plus_seq2' = ZipList seq1 .+. ZipList seq2
let arrCombined     = 10 *. ZipList seq1 .+. ZipList seq2 .- 5
let asMonoid        = result "Hello " </plus/> ZipList ["City"; "World"; "Sun"]

// try ZipList.run {the results}