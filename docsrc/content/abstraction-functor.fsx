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
    map (f << g) = map f << map g
*)
(**
Related Abstractions
--------------------
 - [Applicative](abstraction-applicative.html): An applicative is a functor whose ``map`` operation can be splitted in ``return`` and ``(<*>)`` operations,
 
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
 -  ``Result<'T,'U>``
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
 -  ``DList<'T>``
 
Restricted:
 -  ``string``
 -  ``StringBuilder``
 -  ``Set<'T>``
 -  ``IEnumerator<'T>``
 [Suggest another](https://github.com/gusty/FSharpPlus/issues/new) concrete implementation

Examples
--------
*)



#r @"../../src/FSharpPlus/bin/Release/net45/FSharpPlus.dll"

open FSharpPlus
open FSharpPlus.Math.Generic

let getLine    = async { return System.Console.ReadLine() }
let putStrLn x = async { printfn "%s" x}
let print    x = async { printfn "%A" x}

// Test IO
let action = monad {
    do! putStrLn  "What is your first name?"
    let! fn = getLine
    do! putStrLn  ("Thanks, " + fn) 
    do! putStrLn  ("What is your last name?")
    let! ln = getLine
    let  fullname = fn + " " + ln
    do! putStrLn  ("Your full name is: " + fullname)
    return fullname }


// Test Functors
let times2,minus3 = (*) 2, (-)/> 3
let resSome1      = map minus3 (Some 4G)
let noValue       = map minus3 None
let lstTimes2     = map times2 [1;2;3;4]
let fTimes2minus3 = map minus3 times2
let res39         = fTimes2minus3 21G
let getChars      = map (fun (x:string) -> x.ToCharArray() |> Seq.toList ) action
let quot7         = map ((+)2) <@ 5 @>


// try -> runIO getChars ;;

// Define a type Tree
type Tree<'a> =
    | Tree of 'a * Tree<'a> * Tree<'a>
    | Leaf of 'a
    static member map f (t:Tree<'a>  )  =
        match t with
        | Leaf x -> Leaf (f x)
        | Tree(x,t1,t2) -> Tree(f x, Tree.map f t1, Tree.map f t2)

// add ìnstance for Functor class
    static member Map (x:Tree<_>, f) = Tree.map f x

let myTree = Tree(6, Tree(2, Leaf 1, Leaf 3), Leaf 9)
let mappedTree = map fTimes2minus3 myTree



// An Applicative is automatically a Functor

type ZipList<'s> = ZipList of 's seq with
    static member Return (x:'a)     = ZipList (Seq.initInfinite (konst x))
    static member (<*>) (ZipList (f:seq<'a->'b>), ZipList x) = ZipList (Seq.zip f x |> Seq.map (fun (f, x) -> f x)) : ZipList<'b>

let mappedZipList = map string (ZipList [1;2;3])


// A Monad is automatically a Functor

type MyList<'s> = MyList of 's seq with
    static member Return (x:'a)     = MyList x
    static member (>>=)  (MyList x: MyList<'T>, f) = MyList (Seq.collect (f >> (fun (MyList x) -> x)) x)

let mappedMyList = map string (MyList [1;2;3])