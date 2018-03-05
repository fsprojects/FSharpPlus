(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../../bin"

(**
Profunctor
==========

A bifunctor that is a contravariant in the first argument and covariant in the second.

___



Minimal complete definition
---------------------------


 * ``dimap f g x``
*)
(**
    static member Dimap (x:'Bifunctor<'T,'V>, f:'U->'T, g:'V->'W) :'Bifunctor<'U,'W>
*)
(**


Other operations
----------------

 * ``lmap f x``
*)
(**
    static member Contramap (x:Profunctor<'T,'V>, f:'U->'T) :'Bifunctor<'U,'V>
*)
(**

 * ``rmap g x``
*)
(**
    static member Map (x:Profunctor<'T,'V>, f:'V->'W) :'Bifunctor<'T,'W>
*)
(**




Rules
-----
*)
(**
    dimap id id = id
    dimap (h' << h) (f << f') = dimap h f << dimap h' f'
*)
(**


Related Abstractions
--------------------

 - [Functor](abstraction-functor.html): All profunctors are also functors over the second parameter.


Concrete implementations
------------------------

From .Net/F#
 
 -  ``('T -> 'U)``
 -  ``Func<'T,'U>``

From F#+

 -  ``Kleisli<'T, 'Monad<'U>>``

 [Suggest another](https://github.com/gusty/FSharpPlus/issues/new) concrete implementation

Examples
--------
*)



#r @"../../src/FSharpPlus/bin/Release/net45/FSharpPlus.dll"

open System
open FSharpPlus
open FSharpPlus.Data
open FSharpPlus.Math.Generic

module Predicate = let run (p: Predicate<_>) x = p.Invoke (x)

let isEven       = Predicate (fun x -> x % 2 = 0)

let resStrFalse  = dimap int string (Predicate.run isEven) 99.0


let lx x = Char.GetNumericValue x + 100.
let rx x = string (x + 100)
let kl = Kleisli (fun (y:float) -> [int y; int y * 2 ; int y * 3])

let resl = lmap lx kl
let r105n210n315 = Kleisli.run resl '5'
let resr = rmap rx kl
let r105n110n115 = Kleisli.run resr 5.0
let resd = dimap lx rx kl
let r205n310n415 = Kleisli.run resd '5'