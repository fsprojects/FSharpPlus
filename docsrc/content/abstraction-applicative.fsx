(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../../bin"

(**
Applicative
===========

A functor with application, providing operations to embed pure expressions (``return``), and sequence computations and combine their results (``<*>``).

___



Minimal complete definition
---------------------------


 * ``return x``/``result x`` 

 * ``(<*>) f x``

*)
(**
    static member Return (x:'T) : 'Applicative<'T>
    static member (<*>) (f:'T->'U, x:Applicative<'T>) : Applicative<'U>
*)
(**

Note: ``return`` can't be used outside computation expressions, use ``result`` instead.


Rules
-----
*)
(**
    result id <*> v = v
    result (<<) <*> u <*> v <*> w = u <*> (v <*> w)
    result f <*> result x = result (f x)
    u <*> result y = result ((|>) y) <*> u
*)
(**


Related Abstractions
--------------------

 - [Functor](abstraction-functor.html): An applicative is a functor whose ``map`` operation can splitted in ``return`` and ``(<*>)`` operations,
 
 - [Monad](abstraction-monad.html) : Monads are functors with an additional ``Join`` operation,


Concrete implementations
------------------------

From .Net/F#
 
 -  ``seq<'T>``
 -  ``list<'T>``
 -  ``array<'T>``
 -  ``'T [,]``
 -  ``'T [,,]``
 -  ``'T [,,,]``
 -  ``option<'T>`` 
 -  ``IObservable<'T>``
 -  ``Lazy<'T>``
 -  ``Async<'T>``
 -  ``Result<'T,'U>`` 
 -  ``Choice<'T,'U>``
 -  ``KeyValuePair<'Key,'T>``
 -  ``Map<'Key,'T>``
 -  ``'Monoid * 'T``
 -  ``Task<'T>``
 -  ``'R->'T``
 -  ``Expr<'T>``
 -  ``Dictionary<'Key,'T>``
 -  ``ResizeArray<'T>``

 
From F#+

 -  ``Cont<'R,'T>`` 
 -  ``ContT<'R,'T>``
 -  ``Reader<'R,'T>`` 
 -  ``ReaderT<'R,'Monad<'T>>``
 -  ``Writer<'Monoid,'T>``
 -  ``WriterT<Monad<'T * 'Monoid>>``
 -  ``State<'S,'T * 'S>`` 
 -  ``StateT<'S,'Monad<'T * 'S>>``
 -  ``NonEmptyList<'T>``
 -  ``ZipList<'T>``
 -  ``ParallelArray<'T>``
 -  ``Const<'C,'T>``
 -  ``Compose<'F<'G<'T>>>``
 
Restricted:

 -  ``string``
 -  ``StringBuilder``
 -  ``Set<'T>`` 

 [Suggest another](https://github.com/gusty/FSharpPlus/issues/new) concrete implementation
*)
