(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#r @"../../src/FSharpPlus/bin/Release/netstandard2.0/FSharpPlus.dll"

(**
Zip Applicative (aka Non-sequential Applicative)
================================================
A functor with application, providing operations to embed pure expressions (``pur``), run computations pointwise and/or paralell and combine their results (``<.>``).

___

Minimal complete definition
---------------------------
 * ``pur x`` &nbsp;
 * ``(<.>) f x``
*)
(**
    static member Pure (x: 'T) : 'ZipApplicative<'T>
    static member (<.>) (f: 'ZipApplicative<'T -> 'U>, x: 'ZipApplicative<'T>) : 'ZipApplicative<'U>
*)
(**


Other operations
----------------

* ``zip``
*)
(**
    static member Zip (x1: 'ZipApplicative<'T1>, x2: 'ZipApplicative<'T2>) : 'ZipApplicative<'T1 * 'T2>
*)
(**
* ``unzip``
*)
(**
    static member Unzip (x: 'ZipApplicative<'T1 * 'T2>) : 'ZipApplicative<'T1> * 'ZipApplicative<'T2>
*)
(**
* ``map2``
*)
(**
    static member Map2 (f: 'T1 -> 'T2 -> 'T, x1: 'ZipApplicative<'T1>, x2: 'ZipApplicative<'T2>) : 'ZipApplicative<'T>
*)

(**
* ``map3``
*)
(**
    static member Map3 (f: 'T1 -> 'T2 -> 'T3 -> 'T, x1: 'ZipApplicative<'T1>, x2: 'ZipApplicative<'T2>, x3: 'ZipApplicative<'T3>) : 'ZipApplicative<'T>
*)

(**


Rules
-----

*)
(**

Since ZipApplicatives are Applicatives they obey the same applicative rules:

    pur id <.> v = v
    pur (<<) <.> u <.> v <.> w = u <.> (v <.> w)
    pur f <*> pur x = pur (f x)
    u <*> pur y = pur ((|>) y) <.> u
*)
(**

But they have some additional rules:

    zip x y = tuple2 <!> x <.> y
    unzip = map fst &&& map snd
    id = unzip >> (<||) zip = (<||) zip >> unzip
*)

(**
Related Abstractions
--------------------
 - [Functor](abstraction-functor.html): A zipApplicative is a functor whose ``map`` operation can be splitted in ``pur`` and ``(<.>)`` operations,
 
 - [Applicative](abstraction-applicative.html) : ZipApplicatives are applicatives which usually don't form a [Monad](abstraction-monad.html), therefore the Applicative instance normally is not the same.

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
 -  [``Compose<'ZipApplicative1<'ZipApplicative2<'T>>>``](type-compose.html)

 (*) The operations are the same as those for the normal applicative
 

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

let (x, y) = zip (async { return 1 }) (async { return '2' }) |> Async.RunSynchronously
// val y: char = '2'


// crossproduct vs pointwise operations

let arr1 = (+) <!> [|1;2;3|] <*> [|10;20;30|]
let arr2 = (+) <!> [|1;2;3|] <.> [|10;20;30|]

// val arr1: int array = [|11; 21; 31; 12; 22; 32; 13; 23; 33|]
// val arr2: int array = [|11; 22; 33|]


// Validations

let validated = applicative2' {
    let! x = async { return Ok 1 }
    and! y = async { return Error ["Error1"] }
    and! z = async { return Error ["Error2"] }
    return x + y + z
}

validated |> Async.RunSynchronously
// val it: Result<int,string list> = Error ["Error1"; "Error2"]
