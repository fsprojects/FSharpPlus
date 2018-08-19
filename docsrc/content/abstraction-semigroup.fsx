(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../../bin"

(**
Semigroup
=========
In mathematics, a semigroup is an algebraic structure consisting of a set together with an associative binary operation. A semigroup generalizes a monoid in that there might not exist an identity element. It also (originally) generalized a group (a monoid with all inverses) to a type where every element did not have to have an inverse, thus the name semigroup.
___
Minimal complete definition
---------------------------
 * ``(+)``/``(++)``
*)
(**
    static member (+) (x:'Semigroup, y:'Semigroup) :'Semigroup
*)
(**
Rules
-----
*)
(**
    (x + y) + z = x + (y + z)
*)
(**
Related Abstractions
--------------------
 - [Monoid](abstraction-monoid.html): A monoid is a Semigroup with an additional ``zero`` operation
 
 - Alt/MonadPlus: Applicatives/Monads that are also Semigroups/Monoids

Concrete implementations
------------------------
From .Net/F#
 
 -  ``list<'T>``
 -  ``option<'T>``
 -  ``array<'T>``
 -  ``string``
 -  ``StringBuilder``
 -  ``unit``
 -  ``Set<'T>``
 -  ``Map<'T,'U>``
 -  ``TimeSpan`` 
 -  ``Tuple<*>``
 -  ``'T1* ... *'Tn``
 -  ``Task<'T>``
 -  ``'T->'Semigroup``
 -  ``Async<'T>``
 -  ``Expr<'T>``
 -  ``Lazy<'T>``
 -  ``Dictionary<'T,'U>``
 -  ``ResizeArray<'T>``
 -  ``seq<'T>``
 -  ``IEnumerator<'T>``
 
From F#+
 
 -  ``NonEmptyList<'S>``
 -  ``ZipList<'S>``
 -  ``Dual<'T>``
 -  ``Endo<'T>``
 -  ``All``
 -  ``Any``
 -  ``Const<'T,'U>``
 -  ``First<'T>``
 -  ``Last<'T>``
 -  ``DList<'T>``
 
 [Suggest another](https://github.com/fsprojects/FSharpPlus/issues/new) concrete implementation
*)