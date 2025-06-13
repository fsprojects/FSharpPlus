(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#r @"../../src/FSharpPlus/bin/Release/net8.0/FSharpPlus.dll"

(**
Bitraversable
=============

Bitraversable identifies bifunctorial data structures whose elements can be traversed in order, performing Applicative actions at each element, and collecting a result structure with the same shape.

As opposed to Traversable data structures, which have one variety of element on which an action can be performed, Bitraversable data structures have two such varieties of elements.

___



Minimal complete definition
---------------------------

 * ``bitraverse f g x`` | ``bisequence x`` and ``bimap f g x``
*)
(**
    static member Bitraverse (t: 'Bitraversable<'T1,'U1>, f: 'T1->'Functor<'T2>, g: 'U1->'Functor<'U2>) : 'Functor<'Bitraversable<'T2,'U2>>
    static member Bisequence (t: 'Bitraversable<'Functor<'T>,'Functor<'U>>) : 'Functor<'Bitraversable<'T,'U>>
*)
(**



Rules
-----
*)
(**
    t << bitraverse f g = bitraverse (t << f) (t << g)
    bitraverse Identity Identity = Identity
    bitraverse (Compose << map g1 << f1) = Compose << fmap (bitraverse g1 g21) << bitraverse f1 f2
*)
(**


Related Abstractions
--------------------

 - [Functor](abstraction-functor.html): A bitraversable is generic on the Bitraversable type parameter and the (Applicative) Functor inner type parameter.
 - [Applicative](abstraction-applicative.html): An applicative is a functor whose ``map`` operation can be splitted in ``return`` and ``(<*>)`` operations. 
 - [Bifoldable](abstraction-bifoldable.html) : All bitraversables are bifoldables.


Concrete implementations
------------------------

From .Net/F#

-  ``'T * 'U``
-  ``struct ('T * 'U)``
-  ``Result<'T, 'U>``
-  ``Choice<'T, 'U>``


From F#+

 -  [``Const<'C, 'T>``](type-const.html)
 -  [``Validation<'Error, 'T>``](type-validation.html)


 [Suggest another](https://github.com/fsprojects/FSharpPlus/issues/new) concrete implementation

Examples
--------
*)

(**
```f#
#r @"nuget: FSharpPlus"
```
*)

open FSharpPlus


let asyncSquareRoot x =
    if x < 0 then 
        Error   (async { printfn "Calc error message"; return "Negative Value"}) 
        else Ok (async { printfn "Calc sqrt of %A" x ; return sqrt x})

let res42 = asyncSquareRoot 1764 |> bisequence |> Async.RunSynchronously