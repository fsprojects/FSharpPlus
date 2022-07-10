(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
(**
NonEmptyMap<'Key, 'Value>
================

A type-safe map that contains at least one element.

Examples
--------
*)


#r @"../../src/FSharpPlus/bin/Release/net45/FSharpPlus.dll"

open FSharpPlus
open FSharpPlus.Data

(**
### Constructing NonEmptyMap
*)
// you can construct a NonEmptyMap by using NonEmptyMap.Create
let map1 = NonEmptyMap.Create(("a", 1), ("b", 2))

let map2 = NonEmptyMap.singleton "c" 3

// in order to get back to a regular map you can then use NonEmptyMap.toMap:
let map1' = NonEmptyMap.toMap map1


(**
### Operations on NonEmptyMap
*)

let map3 = map1 |> NonEmptyMap.add "d" 4

let map4 = NonEmptyMap.union map2 map3

map4 |> NonEmptyMap.tryFind "c" |> printfn "%A"


