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
* ``plift3``
*)
(**
   static member ParLift3 (f: 'T1 -> 'T2 -> 'T3 -> 'T, x1: 'Applicative<'T1>, x2: 'Applicative<'T2>, x3: 'Applicative<'T3>) : 'Applicative<'T>
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
 -  ``option<'T>``   *
 -  ``voption<'T>``  *
 -  ``Lazy<'T>``     *
 -  ``Async<'T>``
 -  ``Result<'T, 'U>``
 -  ``Choice<'T, 'U>``
 -  ``KeyValuePair<'Key, 'T>``  *
 -  ``'Monoid * 'T``            *
 -  ``ValueTuple<'Monoid, 'T>`` *
 -  ``Task<'T>``
 -  ``ValueTask<'T>``
 -  ``'R -> 'T``        *
 -  ``Expr<'T>``        *
 
 
From F#+

 -  [``NonEmptySeq<'T>``]
 -  [``NonEmptyList<'T>``](type-nonempty.html)
 -  [``Compose<'Applicative1<'Applicative2<'T>>>``](type-compose.html)

 (*) The operation is the same as that for the normal applicative
 

Only for <*> operation:
 -  ``array<'T>``
 -  ``ResizeArray<'T>``
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


// pointwise operations

let arr1 = (+) <!> [|1;2;3|] <*> [|10;20;30|]
let arr2 = (+) <!> [|1;2;3|] </> [|10;20;30|]

// val arr1: int array = [|11; 21; 31; 12; 22; 32; 13; 23; 33|]
// val arr2: int array = [|11; 22; 33|]


// Validations

let validated = par2 {
    let! x = async { return Ok 1 }
    and! y = async { return Ok 2 }
    and! z = async { return Error ["Error"] }
    return x + y + z
}

validated |> Async.RunSynchronously
// val it: Result<int,string list> = Error ["Error"]
