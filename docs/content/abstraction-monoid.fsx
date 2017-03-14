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


 * ``mempty``
 * ``mappend x y``/``(++) x y``

*)
(**
    static member get_MEmpty () :'Monoid
    static member MAppend (x:'Monoid, y:'Monoid) :'Monoid
*)
(**

Other operations
----------------

 * ``mconcat``
*)
(**
    static member MConcat (x:Seq<'Monoid>) :'Monoid
*)
(**




Rules
-----
*)
(**
	mempty ++ x = x
    x ++ mempty = x
    (x ++ y) ++ z = x ++ (y ++ z)
    mconcat = fold mappend mempty
*)
(**


Related Abstractions
--------------------

 - [Semigroup](abstraction-semigroup.html): A monoid is a Semigroup with an additional ``mempty`` operation
 
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
 
 [Suggest another](https://github.com/gusty/FSharpPlus/issues/new) concrete implementation
*)