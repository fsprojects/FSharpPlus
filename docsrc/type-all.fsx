(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../bin"

(**
All
===

This is a wrapper type for booleans, with a specific set of monoidal operations.
The contained bools would become true only if both (all) operands are true.

Related Types
------------

 - [Any](type-any.html): Similar wrapper, but using the 'any' criteria.


Abstractions
------------

 -  [Semigroup](abstraction-semigroup.html)
 -  [Monoid](abstraction-monoid.html)

Examples
--------
*)


#r @"../src/FSharpPlus/bin/Release/net45/FSharpPlus.dll"

open FSharpPlus
open FSharpPlus.Data

let res1 = All true ++ zero ++ All false
// val res1 : All = All false

let even x = x % 2 = 0

let res2 = [2;4;6;7;8] |> map (even >> All) |> sum
// val res2 : All = All false
