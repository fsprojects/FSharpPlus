(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.

#r @"../../src/FSharpPlus/bin/Release/netstandard2.0/FSharpPlus.dll"
(**
Bifunctor
=======

Intuitively a bifunctor is a functor with 2 arguments which are covariant.

___

A bifunctor (short for binary functor) or functor of two variables is simply a functor whose domain is the product of two types.


Minimal complete definition
---------------------------


 * ``bimap f g x``
*)
(**
    static member Bimap (x:'Bifunctor<'T,'V>, f:'T->'U, g:'V->'W) :'Bifunctor<'U,'W>
*)
(**


Other operations
----------------

 * ``first f x``
*)
(**
    static member First (x:Bifunctor<'T,'V>, f:'T->'U) :'Bifunctor<'U,'V>
*)
(**

 * ``second g x``
*)
(**
    static member Map (x:Bifunctor<'T,'V>, f:'V->'W) :'Bifunctor<'T,'W>
*)
(**




Rules
-----
*)
(**
    bimap f g = first f << second g
*)
(**


Related Abstractions
--------------------

 - [Functor](abstraction-functor.html): All bifunctors are also functors over the second parameter.


Concrete implementations of Bifunctor<'T1,'T2>
-----------------------------------------------

From .Net/F#
 
 -  ``'T1 * 'T2``
 -  ``struct ('T1 * 'T2)``
 -  ``Result<'T2, 'T1>``
 -  ``Choice<'T2, 'T1>``
 -  ``KeyValuePair<'T1, 'T2>``

 
From F#+

 -  [``Const<'C, 'T>``](type-const.html)
 -  [``Validation<'Error, 'T>``](type-validation.html)

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

// convert (map) first element to an int and the second to a string
let rInt10Str10 = bimap  int string (10.0, 10)


let resOk11  = bimap  ((+) 1) string (Ok 10)
let rStrTrue = first  string (true, 10)
let rStr10   = second string (true, 10)