(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.

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
    foldMap (f >> g) = foldMap f >> g
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
 -  ``Set<'T>``
 -  ``HashSet<'T>``
 -  ``option<'T>`` 
 -  ``voption<'T>`` 
 -  ``ResizeArray<'T>`` 
 -  ``ReadOnlyCollection<'T>`` 
 -  ``IReadOnlyCollection<'T>``
 -  ``IReadOnlyList<'T>``

 
From F#+

 -  [``ZipList<'T>``](type-ziplist.html)
 -  [``NonEmptyList<'S>``](type-nonempty.html)
 -  [``DList<'T>``](type-dlist.html)


 [Suggest another](https://github.com/fsprojects/FSharpPlus/issues/new) concrete implementation

Examples
--------
*)



#r @"../../src/FSharpPlus/bin/Release/net8.0/FSharpPlus.dll"

open FSharpPlus
open FSharpPlus.Data
open FSharpPlus.Control

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
    let res21'        = fold       (+) 0 myTree    // <- Tree.Fold is not defined but it fallbacks to the default method (Tree.ToSeq)

module FoldableTree2 =
    type Tree<'a> =
        | Empty 
        | Leaf of 'a 
        | Node of (Tree<'a>) * 'a * (Tree<'a>)

        // add instance for Foldable abstraction (ToSeq is the minimal definition).
        static member ToSeq x =        
            let rec loop t = seq {
                match t with
                | Empty        -> ()
                | Leaf n       -> yield n
                | Node (l,k,r) -> yield k; yield! loop l; yield! loop r}
            loop x
       
        static member inline FoldBack (x, f, z) = 
            let rec _foldMap x f =
                match x with
                | Empty        -> getZero()
                | Leaf n       -> f n
                | Node (l,k,r) -> plus (_foldMap l f) (plus (f k) (_foldMap r f))
            Endo.run (_foldMap x (Endo << f )) z

    
    let tree = Node (Node (Leaf 1, 6, Leaf 3), 2 , Leaf 9)
    let res21  = foldBack   (+) tree 0

    // Following operations work by falling back to Tree.ToSeq which is the default
    let res21' = fold   (+) 0   tree      
    let resTr  = exists ((=) 3) tree
    let resS3  = tryPick (fun x -> if x = 3 then Some x else None) tree