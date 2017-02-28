(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../../bin"

(**
Monad
=====

Defines the basic operations over a monad, a concept from a branch of mathematics known as category theory. From the perspective of an F# programmer, however, it is best to think of a monad as an abstract datatype of actions. F#+ generic computation expressions provide a convenient syntax for writing monadic expressions.

___



Minimal comple te definition
---------------------------


 * ``return``/``result`` and ``(>>=)``
*)
(**
    static member Return (x:'T) : 'Applicative<'T>
    static member Bind (f:'T->'U, x:Applicative<'T>) : Applicative<'U>
*)
(**

Note: ``return`` can't be used outside computation expressions, use ``result`` instead.

Other operations
----------------

 * ``join``
*)
(**
    static member Join (x:'Monad<'Monad<'T>>) :'Monad<'T>
*)
(**



Rules
-----
*)
(**
    return a >>= k = k a
    m >>= return = m
    m >>= (fun x -> k x >>= h) = (m >>= k) >>= h
*)
(**


Related Abstractions
--------------------

 - [Functor](abstraction-functor.html): Monads are automatically functors.
 
 - [Applicative](abstraction-applicative.html) : Monads are automatically applicatives.


Concrete implementations
------------------------

From .Net/F#
 
 -  ``seq<'T>``
 -  ``list<'T>``
 -  ``array<'T>``
 -  ``option<'T>`` 
 -  ``Lazy<'T>``
 -  ``Async<'T>``
 -  ``Choice<'T,'U>``
 -  ``Map<'Key,'T>``
 -  ``'Monoid * 'T``
 -  ``Task<'T>``
 -  ``'R->'T``
 -  ``Dictionary<'Key,'T>``
 -  ``ResizeArray<'T>``

 
From F#+

 -  ``Identity<'T>`` 
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
 
 [Suggest another](https://github.com/gmpl/FSharpPlus/issues/new) concrete implementation
*)