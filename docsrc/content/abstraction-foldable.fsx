(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../../bin"

(**
Foldable
========

Data structures that can be folded to a summary value.

___



Minimal complete definition
---------------------------


 * ``toSeq x``
*)
(**
    static member ToSeq (x:'Foldable<'T>) :seq<'T>
*)
(**


Other operations
----------------

 * ``foldMap``
*)
(**
    FoldMap (x:'Foldable<'T>, f:'T->'Monoid)
*)
(**


Rules
-----
*)
(**
    foldBack f t z = appEndo (foldMap (Endo << f) t ) z
    fold     f z t = appEndo (getDual (foldMap (Dual << Endo << flip f) t)) z
    fold = foldMap id
*)
(**


Related Abstractions
--------------------

 - [Monoid](abstraction-monoid.html)


Concrete implementations
------------------------

From .Net/F#
 
 -  ``seq<'T>``
 -  ``list<'T>``
 -  ``'T []``
 -  ``option<'T>`` 
 -  ``ResizeArray<'T>`` 

 
From F#+

 -  ``ZipList<'T>``
 -  ``NonEmptyList<'T>``
 -  ``DList<'T>``


 [Suggest another](https://github.com/gusty/FSharpPlus/issues/new) concrete implementation

 Examples
--------
*)



#r @"../../src/FSharpPlus/bin/Release/net45/FSharpPlus.dll"

open FSharpPlus
open FSharpPlus.Data

open FsControl

let res1_Gt   = foldMap (compare 2) [1;2;3]
let resHelloW = foldMap (fun x -> Some ("hello " + x)) (Some "world")

module FoldableTree =
    type Tree<'a> =
        | Empty 
        | Leaf of 'a 
        | Node of (Tree<'a>) * 'a * (Tree<'a>)

        // add instance for Foldable class
        static member inline FoldMap (t:Tree<_>, f) =
            let rec loop x f =
                match x with
                | Empty          -> zero
                | Leaf  n        -> f n
                | Node (l, k, r) -> loop l f ++ f k ++ loop r f
            loop t f
        static member inline FoldBack (x:Tree<_>, f, z) = FoldBack.FromFoldMap f z x
        static member inline ToSeq    (x:Tree<_>) = Tree<_>.FoldBack (x, (fun x y -> seq {yield x; yield! y}), Seq.empty)
    
    let myTree = Node (Node (Leaf 1, 6, Leaf 3), 2 , Leaf 9)
    let resSum21      = foldMap id   myTree
    let resProduct324 = foldMap Mult myTree
    let res21         = foldBack   (+) myTree 0
    let res21'        = fold       (+) 0 myTree    // <- Fallback to the default method (ToSeq)