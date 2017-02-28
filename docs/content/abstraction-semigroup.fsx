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


 * ``append``/``(++)``
*)
(**
    static member Append (x:'Semigroup, y:'Semigroup) :'Semigroup
*)
(**





Rules
-----
*)
(**
    (x ++ y) ++ z = x ++ (y ++ z)
*)
(**


Related Abstractions
--------------------

 - [Monoid](abstraction-monoid.html): A monoid is a Semigroup with an additional ``Empty`` operation
 
 - MonadPlus: Monads that are also Monoids


Concrete implementations
------------------------

From .Net/F#
 
 -  ``list<'a>``
 -  ``option<'a>``
 -  ``array<'a>``
 -  ``string``
 -  ``StringBuilder``
 -  ``unit``
 -  ``Set<'a>``
 -  ``Map<'a,'b>``
 -  ``TimeSpan`` 
 -  ``'a*'b``
 -  ``'a*'b*'c``
 -  ``'a*'b*'c*'d``
 -  ``'a*'b*'c*'d*'e``
 -  ``Task<'a>``
 -  ``'T->'Semigroup``
 -  ``Async<'a>``
 -  ``Expr<'a>``
 -  ``Lazy<'a>``
 -  ``Dictionary<'a,'b>``
 -  ``ResizeArray<'a>``
 -  ``seq<'a>``
 
From F#+
 
 -  ``NonEmptyList<'s>``
 -  ``ZipList<'s>``
 -  ``Dual<'t>``
 -  ``Endo<'t>``
 -  ``All``
 -  ``Any``
 -  ``Const<'t,'u>``
 -  ``First<'t>``
 -  ``Last<'t>``
 
 [Suggest another](https://github.com/gmpl/FSharpPlus/issues/new) concrete implementation
*)