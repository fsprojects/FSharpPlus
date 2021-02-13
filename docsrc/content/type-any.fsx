(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../../bin"

(**
Any
===

This is a wrapper type for booleans, with a specific set of monoidal operations.
The contained bools would become true only if one of (any) operands are true.

Related Types
------------

 - [All](type-all.html): Similar wrapper, but using the 'all' criteria.


Abstractions
------------

 -  [Semigroup](abstraction-semigroup.html)
 -  [Monoid](abstraction-monoid.html)

Examples
--------
*)


#r @"../../src/FSharpPlus/bin/Release/net46/FSharpPlus.dll"

open FSharpPlus
open FSharpPlus.Data

let res1 = Any true ++ zero ++ Any false
// val res1 : Any = Any true

let even x = x % 2 = 0

let res2 = [2;4;6;7;8] |> map (even >> Any) |> sum
// val res2 : Any = Any true