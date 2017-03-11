(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../../bin"

(**
Foldable
========

Data structures that can be folded to a summary value.

___



Minimal complete definition
---------------------------


 * ``toSeq x``
*)
(**
    static member ToSeq (x:'Foldable<'T>) :seq<'T>
*)
(**


Other operations
----------------

 * ``foldMap``
*)
(**
    FoldMap (x:'Foldable<'T>, f:'T->'Monoid)
*)
(**


Rules
-----
*)
(**
    foldBack f t z = appEndo (foldMap (Endo << f) t ) z
    fold     f z t = appEndo (getDual (foldMap (Dual << Endo << flip f) t)) z
    fold = foldMap id
*)
(**


Related Abstractions
--------------------

 - [Monoid](abstraction-monoid.html)


Concrete implementations
------------------------

From .Net/F#
 
 -  ``seq<'T>``
 -  ``list<'T>``
 -  ``'T []``
 -  ``option<'T>`` 
 -  ``ResizeArray<'T>`` 

 
From F#+

 -  ``NonEmptyList<'T>``


 [Suggest another](https://github.com/gusty/FSharpPlus/issues/new) concrete implementation
*)