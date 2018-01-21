(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../../bin"

(**
Contravariant
=============

A Contravariant Functor can be mapped over the input.

___



Minimal complete definition
---------------------------


 * ``contramap f x``
*)
(**
    static member Contramap (x:'Functor<'T>, f:'U->'T) :'Functor<'U>
*)
(**




Rules
-----
*)
(**
    contramap id = id
    contramap f << contramap g = contramap (g << f)
*)
(**


Related Abstractions
--------------------

 - [Functor](abstraction-functor.html): A Covariant Functor.
 
 - [Profunctor](abstraction-profunctor.html) : A profunctor is a bifunctor that is contravariant in the first argument and covariant in the second.


Concrete implementations
------------------------

From .Net/F#
 
 -  ``('T -> 'U)``
 -  ``Predicate<'T>``
 -  ``IComparer<'T>``
 -  ``IEqualityComparer<'T>``

 
From F#+

 -  ``Const<'C,'T>``

 [Suggest another](https://github.com/gusty/FSharpPlus/issues/new) concrete implementation

 Examples
--------
*)



#r @"../../src/FSharpPlus/bin/Release/net45/FSharpPlus.dll"

open System
open FSharpPlus


module Predicate = let run (p: Predicate<_>) x = p.Invoke (x)

let intToString (x:int) = string x
let resStr54 = contramap (fun (x:float) -> int x) intToString <| 54.
let isEven      = Predicate (fun x -> x % 2 = 0)
let fstIsEven   = contramap List.head isEven
let resBoolTrue = Predicate.run fstIsEven [0..10]

type Person = Person of string
let personEqComp = HashIdentity.Structural<Person>
let personList = [1, Person "me"; 2, Person "you"; 3, Person "you"]
let cnt3 = Seq.length <| Linq.Enumerable.Distinct (personList)
let cnt2 = Seq.length <| Linq.Enumerable.Distinct (personList, contramap snd personEqComp)