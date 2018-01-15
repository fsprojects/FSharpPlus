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
 * `zero``
 * ``(+) x y``/``(++) x y``
*)
(**
    static member get_Zero () :'Monoid
    static member (+) (x:'Monoid, y:'Monoid) :'Monoid
*)
(**
Other operations
----------------
 * ``Seq.sum``
*)
(**
    static member Sum (x:Seq<'Monoid>) :'Monoid
*)
(**
Rules
-----
*)
(**
	zero + x = x
    x + zero = x
    (x + y) + z = x + (y + z)
    Seq.sum = Seq.fold (+) zero
    sum = fold (+) zero (generic to all foldables)
*)
(**
Related Abstractions
--------------------
 - [Semigroup](abstraction-semigroup.html): A monoid is a Semigroup with an additional ``zero`` operation
 
 - MonadPlus: Monads that are also Monoids
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
 -  ``'T*'U``
 -  ``'T*'U*'V``
 -  ``'T*'U*'V*'W``
 -  ``'T*'U*'V*'W*'X``
 -  ``Task<'T>``
 -  ``'T->'Monoid``
 -  ``Async<'T>``
 -  ``Expr<'T>``
 -  ``Lazy<'T>``
 -  ``Dictionary<'T,'U>``
 -  ``IDictionary<'T,'U>``
 -  ``ResizeArray<'T>``
 -  ``seq<'T>``
 -  ``IEnumerator<'T>``
 
From F#+
 
 -  ``ZipList<'S>``
 -  ``Dual<'T>``
 -  ``Endo<'T>``
 -  ``All``
 -  ``Any``
 -  ``Const<'T,'U>``
 -  ``First<'T>``
 -  ``Last<'T>``
 -  ``DList<'T>``
 
 [Suggest another](https://github.com/gusty/FSharpPlus/issues/new) concrete implementation
*)