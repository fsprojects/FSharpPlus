(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../../bin"

(**
Bifoldable
=======

Intuitively a bifoldable is a type with 2 arguments, each of them being foldable.

___

A bifoldable (short for binary foldable) or foldable of two variables is a container of up to two elements whose components can be folded to a single value.

Unlike bimap which preserves the container type, bifoldable will extract and fold the value.


Minimal complete definition
---------------------------


 * ``bifoldMap f g x``
*)
(**
    static member BifoldMap (x:'Bifoldable<'T,'V>, f:'T->'U, g:'V->'U) :'U
*)
(**

* ``bifold f g z x``
*)
(**
    static member Bifold (x:'Bifoldable<'T,'V>, f:'T->'Monoid, g:'V->'Monoid, z: 'Monoid) :'Monoid
*)
(**

* ``bifoldBack f g x z``
*)
(**
    static member BifoldBack (x:'Bifoldable<'T,'V>, f:'T->'Monoid, g:'V->'Monoid, z: 'Monoid) :'Monoid
*)
(**


Other operations
----------------

 * ``bisum x``
*)
(**
    static member Bisum (x:Bifunctor<'T,'T>) :'T
*)

(**




Rules
-----
*)
(**
    bisum x = bifoldMap id id x
    bifoldMap f g x = bifoldBack (f >> (++)) (g >> (++)) x zero
    //TODO: bifoldr f g z t = appEndo (bifoldMap (Endo . f) (Endo . g) t) z
*)
(**


Related Abstractions
--------------------

 - [Foldable](abstraction-foldable.html): All bifoldable contain up to two elements that are foldable to a single common type.
 - [Monoid](abstraction-monoid.html): For containers where the two elements are not disjoint, the same relation that foldable has to monoid applies.

Concrete implementations
------------------------

From .Net/F#
 
 -  ``'T * 'U``
 -  ``Result<'T,'U>``
 -  ``Choice<'T,'U>``

 
From F#+

 -  ``Const<'C,'T>``
 -  ``Validation<'err,'a>``

 [Suggest another](https://github.com/fsprojects/FSharpPlus/issues/new) concrete implementation

Examples
--------
*)

#r @"../../src/FSharpPlus/bin/Release/net45/FSharpPlus.dll"

open FSharpPlus
open FSharpPlus.Control

let listMapSeqLength = List.map Seq.length
let listMapTimes2 = List.map ((*) 2)

let c1 : Choice<int list,string list> = Choice1Of2 [1..2]
let c2 : Choice<int list,string list> = Choice2Of2 ["a";"bbbb"]

bifoldBack (listMapTimes2 >> (++)) (listMapSeqLength >> (++)) c1 [0] // = [2;4;0]
bifoldBack (listMapTimes2 >> (++)) (listMapSeqLength >> (++)) c2 [0] // = [1;4;0]
bifoldMap listMapTimes2 listMapSeqLength c1 // = [2;4]
bifoldMap listMapTimes2 listMapSeqLength c2 // = [1;4]

let t = ("b","c")
bifoldBack (++) (++) t "a" // = "bca"
bifold (++) (++) "a" t // = "abc"

// implementing on custom type:
type MyEither<'a,'b> = 
    | MyLeft of 'a 
    | MyRight of 'b
    static member inline BifoldMap (x: MyEither<_,_>, f, g) =
      match x with
      | MyLeft a -> f a
      | MyRight a -> g a

    static member BifoldBack (x: MyEither<_,_>, f, g, z) =
        match x with
        | MyLeft a -> f a z
        | MyRight a -> g a z

bisum (MyEither.MyLeft "a") // = "a"
bisum (1,2) // = 3


let inline law1 x =
  bisum x = bifoldMap id id x

law1 (1,1) // = true
law1 (Ok [1;2;3]) // = true
law1 (Error [1;2;3]) // = true
law1 (Choice1Of2 [1;2;3]) // = true
law1 (Choice2Of2 [1;2;3]) // = true
law1 (MyLeft [1;2;3]) // = true
law1 (MyRight [1;2;3]) // = true


let inline law2 x f g =
  bifoldMap f g x = bifoldBack (f >> (++)) (g >> (++)) x zero

law2 (1,1) ((+) 1) ((+) 2) // = true
law2 (Ok [1;2;3]) ((++) [1]) ((++) [2]) // = true
law2 ("a","b") ((+) "bbbb") ((+) "aaaa") // = true