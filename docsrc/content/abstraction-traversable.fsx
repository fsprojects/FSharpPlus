(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.

#r @"../../src/FSharpPlus/bin/Release/netstandard2.0/FSharpPlus.dll"

(**
Traversable
===========

Data structures that can be traversed from left to right, performing an action on each element.

___



Minimal complete definition
---------------------------


 * ``traverse f x`` | ``sequence x``
*)
(**
    static member Traverse (t:'Traversable<'T>, f : 'T->'Functor<'U>) : 'Functor<'Traversable<'U>>
    static member Sequence (t:'Traversable<'Functor<'T>>) : 'Functor<'Traversable<'T>>
*)
(**



Rules
-----
*)
(**
    t << traverse f = traverse (t << f) 
    traverse Identity = Identity
    traverse (Compose << map g << f) = Compose << map (traverse g) << traverse f
*)
(**


Related Abstractions
--------------------

 - [Functor](abstraction-functor.html): A traversable is generic on the Traversable type parameter and the (Applicative) Functor inner type parameter.
 - [Applicative](abstraction-applicative.html): An applicative is a functor whose ``map`` operation can be splitted in ``return`` and ``(<*>)`` operations. 
 - [Foldable](abstraction-foldable.html) : All traversables are foldables.


Concrete implementations
------------------------

From .Net/F#
 
 -  ``seq<'T>``
 -  ``list<'T>``
 -  ``array<'T>``
 -  ``option<'T>`` 
 -  ``ResizeArray<'T>``
 -  ``Map<'K, 'T>``
 -  ``Result<'T, 'Error>``
 -  ``Choice<'T, 'Error>``

 
From F#+

 -  [``ZipList<'T>``](type-ziplist.html)
 -  [``NonEmptyList<'T>``](type-nonempty.html)
 -  [``NonEmptyMap<'Key, 'T>``](type-nonempty-map.html)
 -  [``Validation<'Error,'T>``](type-validation.html)


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


// Some functions
let getLine    = async { return System.Console.ReadLine() }
let f x = if x < 200 then [3 - x] else []
let g x = if x < 200 then Some (3 - x) else None

// traverse
let resSomeminus100 = traverse f (Some 103)
let resLstOfNull    = traverse f None 
let res210          = traverse f [1;2;3]  
let resSome210      = traverse g [1;2;3]  
let resEmptyList    = traverse f [1000;2000;3000] 
let resEListOfElist = traverse f []

// sequence
let resSome321  = sequence [Some 3;Some 2;Some 1]
let resNone     = sequence [Some 3;None  ;Some 1]
let res654      = (sequence [ (+) 3 ; (+) 2 ; (+) 1]) 3
let resCombined = sequence [ [1;2;3] ; [4;5;6]  ]
let resLstOfArr = sequence [|[1;2;3] ; [4;5;6] |]  // <- Uses the default method.
let resArrOfLst = sequence [[|1;2;3|];[|4;5;6 |]]

// This computation will ask for three user inputs
// try Async.RunSynchronously get3strings
let get3strings = sequence [getLine;getLine;getLine]
