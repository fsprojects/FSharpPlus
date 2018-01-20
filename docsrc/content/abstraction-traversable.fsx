(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../../bin"

(**
Traversable
===========

Data structures that can be traversed from left to right, performing an action on each element.

___



Minimal complete definition
---------------------------


 * ``traverse f x`` | ``sequence x``
*)
(**
    static member Traverse (t:'Traversable<'T>, f : 'T->'Functor<'U>) : 'Functor<'Traversable<'U>>
    static member Sequence (t:'Traversable<'Functor<'T>>) : 'Functor<'Traversable<'T>>
*)
(**



Rules
-----
*)
(**
    t << traverse f = traverse (t << f) 
    traverse Identity = Identity
    traverse (Compose << fmap g . f) = Compose << fmap (traverse g) << traverse f
*)
(**


Related Abstractions
--------------------

 - [Functor](abstraction-functor.html): An applicative is a functor whose ``map`` operation can be splitted in ``return`` and ``(<*>)`` operations,
 
 - [Foldable](abstraction-foldable.html) : All traversables are foldables.


Concrete implementations
------------------------

From .Net/F#
 
 -  ``seq<'T>``
 -  ``list<'T>``
 -  ``array<'T>``
 -  ``option<'T>`` 
 -  ``ResizeArray<'T>``

 
From F#+

 -  ``ZipList<'T>``
 -  ``NonEmptyList<'T>``


 [Suggest another](https://github.com/gusty/FSharpPlus/issues/new) concrete implementation
*)