(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../../bin"

(**
NonEmptyList<'T>
================

A type-safe list that contains at least one element.

Examples
--------
*)


#r @"../../src/FSharpPlus/bin/Release/net46/FSharpPlus.dll"

open FSharpPlus
open FSharpPlus.Data

(**
### Constructing NonEmptyList
*)
// you can construct a NonEmptyList by using ofSeq
let list123' = NonEmptyList.create 1 [ 2; 3 ]
// or more idiomatically
let list123 = nelist { 1 ; 2; 3 } // will work in F# version 4.7

let listOne = NonEmptyList.singleton 1
// cons
let list2 = NonEmptyList.cons 100 list123
// append two NonEmptyLists
let list3 = plus list2 (NonEmptyList.singleton 200)
// this can be written as (since list2 is a NonEmptyList):
let list3' = plus list2 (result 200)
// in order to get back to a regular list you can then use toList:
let list4 = toList list3'

(**
### Operations on NonEmptyList
*)

let lengthOfList3 = length list3

let headOf3 = list3.Head
let headOf3' = head list3

let tailOf3 = list3.Tail