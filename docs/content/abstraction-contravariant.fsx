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

 [Suggest another](https://github.com/gmpl/FSharpPlus/issues/new) concrete implementation
*)