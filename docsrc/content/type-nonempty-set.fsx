(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
(**
NonEmptySet<'T>
================

A type-safe set that contains at least one element.

Examples
--------
*)


#r @"../../src/FSharpPlus/bin/Release/net45/FSharpPlus.dll"

open FSharpPlus
open FSharpPlus.Data

(**
### Constructing NonEmptySet
*)
// you can construct a NonEmptySet by using NonEmptySet.Create
let set123 = NonEmptySet.Create(1, 2, 3)

let set4 = NonEmptySet.singleton 4
let set4' : NonEmptySet<int> = result 4

// union two NonEmptySets
let set1234 = NonEmptySet.union set123 set4

// in order to get back to a regular set you can then use NonEmptySet.toSet:
let set1234' = NonEmptySet.toSet set1234


(**
### Operations on NonEmptySet
*)

let set12345 = set1234 |> NonEmptySet.add 5

let set12345' = NonEmptySet.unionMany (NonEmptyList.create set123 [set4; result 5])

printfn "%b" (NonEmptySet.isSubset set1234 set12345)


