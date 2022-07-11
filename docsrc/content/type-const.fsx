(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
(**
Const<'T,'U>
============

The Const functor, defined as Const&lt;&#39;T, &#39;U&gt; where &#39;U is a phantom type. Useful for: Lens getters Its applicative instance plays a fundamental role in Lens.

Examples
--------
*)


#r @"../../src/FSharpPlus/bin/Release/net45/FSharpPlus.dll"

open FSharpPlus
open FSharpPlus.Lens
open FSharpPlus.Data
// note for instance the definition of view (from the Lens part of F#+):
let view (optic: ('a -> Const<_,'b>) -> _ -> Const<_,'t>) (source: 's) : 'a = Const.run (optic Const source)
