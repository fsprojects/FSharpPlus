(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../../bin"

(**
Functor
=======

The Functor abstraction is used for types that can be mapped over.

___



Minimal complete definition
---------------------------


 * ``map f x``/``(|>>) x f``/``(<<|) f x``/``(<!>) f x``
*)
(**
    static member Map (x:'Functor<'T>, f:'T->'U) :'Functor<'U>
*)
(**


Other operations
----------------

 * ``unzip x``
*)
(**
    static member Unzip (x:Functor<'T * 'U>) :'Functor<'T> * 'Functor<'U>
*)
(**


Rules
-----
*)
(**
    map id  =  id
    map (f << g) = map f << fmap g
*)
(**


Related Abstractions
--------------------

 - [Applicative](abstraction-applicative.html): An applicative is a functor whose ``map`` operation can splitted in ``return`` and ``(<*>)`` operations,
 
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
 -  ``Choice<'T,'U>``
 -  ``KeyValuePair<'Key,'T>``
 -  ``Map<'Key,'T>``
 -  ``'Monoid * 'T``
 -  ``Task<'T>``
 -  ``'R->'T``
 -  ``Expr<'T>``
 -  ``Dictionary<'Key,'T>``
 -  ``IDictionary<'Key,'T>``
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
 -  ``Kleisli<'T, 'Monad<'U>>``
 -  ``Compose<'F<'G<'T>>>``
 
Restricted:

 -  ``string``
 -  ``StringBuilder``
 -  ``Set<'T>`` 

 [Suggest another](https://github.com/gusty/FSharpPlus/issues/new) concrete implementation
*)