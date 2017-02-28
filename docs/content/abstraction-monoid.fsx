(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../../bin"

(**
Monoid
======

Types with an associative binary operation that has an identity.

___



Minimal complete definition
---------------------------


 * ``empty`` and ``append``/``(++)``
*)
(**
    static member Empty () :'Mo noid
    static member Append (x:'Monoid, y:'Monoid) :'Monoid
*)
(**

Other operations
----------------

 * ``concat``
*)
(**
    static member Concat (x:Seq<'Monoid>) :'Monoid
*)
(**




Rules
-----
*)
(**
	empty ++ x = x
    x ++ empty = x
    (x ++ y) ++ z = x ++ (y ++ z)
    concat = fold append empty
*)
(**


Related Abstractions
--------------------

 - [Semigroup](abstraction-semigroup.html): A monoid is a Semigroup with an additional ``Empty`` operation
 
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
 -  ``'T->'Monoid``
 -  ``Async<'a>``
 -  ``Expr<'a>``
 -  ``Lazy<'a>``
 -  ``Dictionary<'a,'b>``
 -  ``IDictionary<'a,'b>``
 -  ``ResizeArray<'a>``
 -  ``seq<'a>``
 
From F#+
 
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