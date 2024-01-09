(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#r @"../../src/FSharpPlus/bin/Release/netstandard2.0/FSharpPlus.dll"

(**
Par-Applicative
===============
A functor with application, providing operations to embed pure expressions (``preturn``), parallel computations and combine their results (``</>``).
___
Minimal complete definition
---------------------------
 * ``preturn x`` &nbsp; / &nbsp; ``result x``
 * ``(</>) f x``
*)
(**
    static member ParReturn (x: 'T) : 'Applicative<'T>
    static member (</>) (f: 'Applicative<'T -> 'U>, x: 'Applicative<'T>) : 'Applicative<'U>
*)
(**
Note: ``preturn`` can't be used outside computation expressions, use ``result`` instead.


Other operations
----------------

* ``plift2``
*)
(**
   static member ParLift2 (f: 'T1 -> 'T2 -> 'T, x1: 'Applicative<'T1>, x2: 'Applicative<'T2>) : 'Applicative<'T>
*)
(**


Rules
-----
*)
(**
    presult id </> v = v
    presult (<<) </> u </> v </> w = u </> (v </> w)
    presult f <*> presult x = presult (f x)
    u <*> presult y = presult ((|>) y) </> u
*)
(**
Related Abstractions
--------------------
 - [Functor](abstraction-functor.html): A parallel applicative is a functor whose ``map`` operation can be splitted in ``preturn`` and ``(</>)`` operations,
 
 - [Applicative](abstraction-applicative.html) : Parallel Applicatives are applicatives which usually don't form a [Monad](abstraction-monad.html).

Concrete implementations
------------------------
From F#
 
 -  ``seq<'T>``
 -  ``list<'T>``
 -  ``array<'T>``    *
 -  ``'T [,]``       *
 -  ``'T [,,]``      *
 -  ``'T [,,,]``     *
 -  ``option<'T>``   *
 -  ``voption<'T>``  *
 -  ``IObservable<'T>``
 -  ``Lazy<'T>``     *
 -  ``Async<'T>``
 -  ``Result<'T,'U>`` 
 -  ``Choice<'T,'U>``
 -  ``KeyValuePair<'Key,'T>`` *
 -  ``'Monoid * 'T`` *
 -  ``ValueTuple<'Monoid, 'T>`` *
 -  ``Task<'T>``
 -  ``ValueTask<'T>``
 -  ``'R->'T``          *
 -  ``Expr<'T>``        *
 -  ``ResizeArray<'T>`` *
 
From F#+

 -  [``Identity<'T>``](type-identity.html)
 -  [``Cont<'R,'T>``](type-cont.html)
 -  [``ContT<'R,'T>``](type-contt.html)
 -  [``Reader<'R,'T>``](type-reader.html)
 -  [``ReaderT<'R,'Monad<'T>>``](type-readert.html)
 -  [``Writer<'Monoid,'T>``](type-writer.html)
 -  [``WriterT<'Monad<'T * 'Monoid>>``](type-writert.html)
 -  [``State<'S,'T * 'S>``](type-state.html)
 -  [``StateT<'S,'Monad<'T * 'S>>``](type-statet.html)
 -  [``OptionT<'Monad<option<'T>>``](type-optiont.html)
 -  [``ValueOptionT<'Monad<voption<'T>>``](type-valueoptiont.html)
 -  [``SeqT<'Monad<seq<'T>>``](type-seqt.html)
 -  [``ListT<'Monad<list<'T>>``](type-listt.html)
 -  [``ResultT<'Monad<Result<'T,'TError>>``](type-resultt.html)
 -  [``ChoiceT<'Monad<Choice<'T,'TError>>``](type-choicet.html)
 -  [``Free<'Functor<'T>,'T>``](type-free.html)
 -  [``NonEmptyList<'T>``](type-nonempty.html)
 -  [``Validation<'Error,'T>``](type-validation.html)
 -  [``ZipList<'T>``](type-ziplist.html)
 -  [``ParallelArray<'T>``](type-parallelarray.html)
 -  [``Const<'C,'T>``](type-const.html)
 -  [``Compose<'Applicative1<'Applicative2<'T>>>``](type-compose.html)
 -  [``DList<'T>``](type-dlist.html)
 -  [``Vector<'T,'Dimension>``](type-vector.html)
 -  [``Matrix<'T,'Rows,'Columns>``](type-matrix.html)
 
Restricted:
 -  ``string``
 -  ``StringBuilder``
 -  ``Set<'T>``
 -  ``IEnumerator<'T>``

Only for <*> operation:
 -  ``Map<'Key, 'T>``
 -  ``Dictionary<'Key, 'T>``
 -  ``IDictionary<'Key, 'T>``
 -  ``IReadOnlyDictionary<'Key, 'T>``


 [Suggest another](https://github.com/fsprojects/FSharpPlus/issues/new) concrete implementation

Examples
--------
*)


(**
```f#
#r @"nuget: FSharpPlus"
```
*)

open FSharpPlus

// Validations

let validated = par2 {
    let! x = async { return Ok 1 }
    and! y = async { return Ok 2 }
    and! z = async { return Error ["Error"] }
    return x + y + z
}

validated |> Async.RunSynchronously
// val it: Result<int,string list> = Error ["Error"]
