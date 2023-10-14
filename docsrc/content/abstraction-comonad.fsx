(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#r @"../../src/FSharpPlus/bin/Release/net45/FSharpPlus.dll"

(**
Comonad
=======

Comonads are the categorical dual of monads.

___



Minimal complete definition
---------------------------


 * ``extract s``

 * ``extend g s`` / ``(=>>) s g`` 

*)
(**
    static member Extract (s: 'Comonad<'T>) : 'T
    static member (=>>)   (s: 'Comonad<'T>, f: 'Comonad<'T> -> 'U) : 'Comonad<'U>
*)
(**

Other operations
----------------

 * ``duplicate x``
*)
(**
    static member Duplicate (x : 'Comonad<'T>) : 'Comonad<'Comonad<'T>>
*)
(**



Rules
-----
*)
(**
    extend extract       = id
    extract << extend f  = f
    extend f << extend g = extend (f << extend g)
*)
(**


Related Abstractions
--------------------

 - [Monad](abstraction-monad.html): Comonads are the categorical dual of monads.


Concrete implementations
------------------------

From .Net/F#
 
 -  ``Async<'T>``    
 -  ``Lazy<'T>``     
 -  ``Id<'T>``       
 -  ``('W * 'T)``
 -  ``struct ('W * 'T)``
 -  ``'Monoid -> 'T``
 -  ``ValueTask<'T>``
 
 
From F#+

 -  [``Reader<'R,'T>``](type-reader.html)
 -  [``Writer<'Monoid,'T>``](type-writer.html)

 
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
open FSharpPlus.Data

// A non-empty list
let lst   = {Head = 1; Tail = [2;3;4;5]}

// Get the head
let elem1 = extract   lst

// Get ALL tails
let tails = duplicate lst

// This should return the original list
let lst'  = extend extract lst



let ct1 = duplicate [1;2;3;4] // val it : List<List<int>> = [[1; 2; 3; 4]; [2; 3; 4]; [3; 4]; [4]]
let ct2 = duplicate ("a", 10) // val it : string * (string * int) = ("a", ("a", 10))
let ct3 = duplicate (fun (x:string) -> System.Int32.Parse x)
let r80100 = ct3 "80" "100"

let ct1' = extend id [1;2;3;4]
let ct2' = extend id ("a", 10)
let ct3' = extend id (fun (x:string) -> System.Int32.Parse x)

let ct1'' = (=>>) [1;2;3;4] id
let ct2'' = (=>>) ("a", 10) id
let ct3'' = (=>>) (fun (x:string) -> System.Int32.Parse x) id