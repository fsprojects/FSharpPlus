(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../../bin"

(**
Functors and Applicatives
=========================

*)

#r "../../bin/FSharpPlus/FSharpPlus.dll"
open FSharpPlus

(**

You may run this script step-by-step
The order of execution has to be respected since there are redefinitions of functions and operators


Functors
========

The intuitive definition is that a Functor is something you can map over.

So they all have a ``map`` operation which is their minimal definition.

Most containers are functors
*)

let r01 = List.map   (fun x -> string (x + 10)) [ 1;2;3 ]
let r02 = Array.map  (fun x -> string (x + 10)) [|1;2;3|]
let r03 = Option.map (fun x -> string (x + 10)) (Some 5)

(**
You can think of the Option functor as a particular case of a List that can be either empty or with just 1 element.

We could have used the generic function ``map`` from this library which works on any functor.

*)

let r01' = map (fun x -> string (x + 10)) [ 1;2;3 ]
let r02' = map (fun x -> string (x + 10)) [|1;2;3|]
let r03' = map (fun x -> string (x + 10)) (Some 5)

(** Now let's define a simple type and make it a functor by adding a ``Map`` static method *)

type Id<'t> = Id of 't with
    static member Map (Id x, f) = Id (f x)

let r04 = map (fun x -> string (x + 10)) (Id 5)

(**
Most computations are also functors

Here's an example with Async functions
*)

let async5 = async.Return 5
let r05  = map (fun x -> string (x + 10)) async5
let r05' = Async.RunSynchronously r05


(** But even plain functions are functors *)

let r06  = map (fun x -> string (x + 10)) ((+) 2)
let r06' = r06 3

(**
For functions ``map`` is equivalent to ``(<<)`` this means that mapping over a function is the same as composing the functions with the mapper

You can think of the List functor as a particular case of a function ``f: Naturals -> 't``

What about tuples?
*)

module TupleFst = let map f (a,b) = (f a, b)
module TupleSnd = let map f (a,b) = (a, f b)

let r07 = TupleFst.map (fun x -> string (x + 10)) (5, "something else")
let r08 = TupleSnd.map (fun x -> string (x + 10)) ("something else", 5)

(**
So there is more than one way to define a functor with tuples.
The same applies to the Discriminated Union of 2 types.
*)

// DUs
module ChoiceFst = let map f = function Choice1Of2 x -> Choice1Of2 (f x) | Choice2Of2 x -> Choice2Of2 x
module ChoiceSnd = let map f = function Choice2Of2 x -> Choice2Of2 (f x) | Choice1Of2 x -> Choice1Of2 x

let choiceValue1:Choice<int,string> = Choice1Of2 5
let choiceValue2:Choice<int,string> = Choice2Of2 "Can't divide by zero."

let r09  = ChoiceFst.map (fun x -> string (x + 10)) choiceValue1
let r09' = ChoiceFst.map (fun x -> string (x + 10)) choiceValue2

let r10  = ChoiceSnd.map (fun x -> "The error was: " + x) choiceValue1
let r10' = ChoiceSnd.map (fun x -> "The error was: " + x) choiceValue2


(** Tree as a functor *)

type Tree<'a> =
    | Tree of 'a * Tree<'a> * Tree<'a>
    | Leaf of 'a

module Tree = let rec map f = function 
                | Leaf x        -> Leaf (f x) 
                | Tree(x,t1,t2) -> Tree(f x, map f t1, map f t2)

let myTree = Tree(6, Tree(2, Leaf 1, Leaf 3), Leaf 9)

let r11 = Tree.map (fun x -> string (x + 10)) myTree

(**
Q: is String a Functor?
*)

let r12 = String.map (fun c -> System.Char.ToUpper(c)) "Hello world"

(**
A: Kind of, but we can't change the wrapped type. We're stick to ('a->'a) -> C<'a> -> C<'a> 
   if we assume 'a = char and C<'a> = String


Finally there are some laws:

 - ``map id = id``
 - ``map (f >> g) = map f >> map g``



Limitations:

We can define ``map2`` then ``map3``  then ..  ``mapN`` ?

*)

type Option<'T> with
    static member map2 f x y = 
        match x, y with
        | Some x, Some y -> Some (f x y)
        | _              -> None

    static member map3 f x y z = 
        match x, y, z with
        | Some x, Some y, Some z -> Some (f x y z)
        | _                      -> None

let r13 = Option.map2 (+) (Some 2) (Some 3)

let r14 = List.map2 (+) [1;2;3] [10;11;12]

let add3 a b c = a + b + c

let r15 = Option.map3 add3 (Some 2) (Some 2) (Some 1)

(**

Question: Is it possible to generalize to mapN?


Applicative Functors
====================

What if we split ``map`` in 2 steps?

*)

// map ('a -> 'b) -> C<'a> -> C<'b>
//     \--------/    \---/    \---/
//         (a)        (b)      (c)
//
// 1)    ('a -> 'b)        ->  C<'a -> 'b>
//       \--------/            \---------/
//           (a)                      
//               
// 2)  C<'a -> 'b> -> C<'a>  ->   C<'b>
//     \---------/    \---/       \---/
//                     (b)         (c)
//
//
// step1   ('a -> 'b)        ->  "C<'a -> 'b>"      Put the function into a context C
// step2 "C<'a -> 'b>" C<'a> ->   C<'b>             Apply the function in a context C to a value in a context C

(** Here's an example with Options *)

let step1 f = Some f
let step2 a b = 
    match a, b with
    | Some f, Some x -> Some (f x)
    | _              -> None

let r16 = step1 (fun x -> string (x + 10))
let r17 = step2 r16 (Some 5)

    
(** So now instead of writing: *)

let r18  = Option.map (fun x -> string (x + 10)) (Some 5)

(** we write *)

let r18' = step2 (step1 (fun x -> string (x + 10))) (Some 5)

    
(** and instead of ``map2`` like this: *)
let r19   = Option.map2 (+) (Some 2) (Some 3)

(** we write *)
let r19i  = step2 (step1 (+)) (Some 2)
(** .. and finally *)
let r19' = step2 r19i (Some 3)
(** by applying ``step2`` again. We can apply ``step2`` again if the result is still a function in a container, just like partial application.


lets give names to ``step1`` and ``step2``: ``pure`` and ``<*>`` *)

module OptionAsApplicative =
    let pure' x = Some x
    let (<*>) a b = 
        match a, b with
        | Some f, Some x -> Some (f x)
        | _              -> None

open OptionAsApplicative

let r18''  = Option.map (fun x -> string (x + 10)) (Some 5)

let r18''' = Some (fun x -> string (x + 10)) <*> Some 5
// analog to:
let r18'''' =     (fun x -> string (x + 10))          5


(** Now with ``map3`` (and further with mapN) *)

let r20 = Option.map3 add3 (Some 2) (Some 2) (Some 1)

let r20'  = Some add3 <*> Some 2 <*> Some 2 <*> Some 1
// analog to:
let r20''  =     add3          2          2          1


(** but even without ``add3`` we can write ``1 + 2 + 2`` which is ``1 + (2 + 2)`` and the same as: *)

let r20'''  = (+) 1 ((+) 2 2)

(** with options becomes: *)
let r20'''' = Some (+) <*> Some 1 <*> (Some (+) <*> Some 2 <*> Some 2)
(** constrast it with *)
let r20'''''  =    (+)          1     (     (+)          2          2)

(** we know ``apply`` is ``(<|)`` in F# *)

let r21     =      (+) <|       1 <|  (     (+) <|       2 <|       2)
let r21'    = Some (+) <*> Some 1 <*> (Some (+) <*> Some 2 <*> Some 2)

(**
So at this point the name "Applicative Functor" should make sense
    
Q: Isn't it easier to do just ``Some ( (+) 1 ((+) 2 2) )`` ?
   We get the same result in the end.
A: Yes, in this particular case it's the same but what if instead of ``Some 1`` we have ``None``
*)

let r22   = Some (+) <*> None <*> (Some (+) <*> Some 2 <*> Some 2)

(**   
That's because we're applying functions inside a context.

It looks the same as applying outside but in fact some effects occurs behind the scenes.

To have a better idea let's move out of Option:
*)

module Async =
    let pure' x = async.Return x
    let (<*>) f x = async.Bind(f, fun x1 -> async.Bind(x, fun x2 -> pure'(x1 x2)))

open Async

    
let r23   = async {return (+)} <*> async {return 2} <*> async {return 3}

let r23'  = pure' (+) <*> pure' 2 <*> pure' 3

(** try ``Async.RunSynchronously r23'`` *)

let getLine = async { 
        let x = System.Console.ReadLine() 
        return  System.Int32.Parse x
    }

let r24  = pure' (+) <*> getLine <*> getLine

(** try ``Async.RunSynchronously r24`` *)


module ListAsApplicative =
    let pure' x = [x]        
    let (<*>)  f x = List.collect (fun x1 -> List.collect (fun x2 -> [x1 x2]) x) f

    (* here are two other possible implementations of (<*>) for List
    let (<*>) f x = f |> List.map (fun f -> x |> List.map (fun x -> f x)) |> List.concat
    let (<*>) f x= 
        seq {
                for f in f do
                for x in x do
                yield f x} |> Seq.toList *)

open ListAsApplicative

let r25 =  List.map (fun x -> string (x + 10)) [1;2;3]

let r25'  =       [fun x -> string (x + 10)] <*> [1..3]
let r25'' = pure' (fun x -> string (x + 10)) <*> [1..3]


let r26 = [string; fun x -> string (x + 10)] <*> [1;2;3]

(** So, for lists ``map2`` is equivalent to write: *)

let r27 = [(+)] <*> [1;2] <*> [10;20;30]

let r28 = [(+);(-)] <*> [1;2] <*> [10;20;30]


    
module SeqAsApplicative =
    let pure' x = Seq.initInfinite (fun _ -> x)
    let (<*>) f x = Seq.zip f x |> Seq.map (fun (f,x) -> f x)

open SeqAsApplicative


let r29 =  Seq.map (fun x -> string (x + 10))    (seq [1;2;3])          |> Seq.toList
let r29' =   pure' (fun x -> string (x + 10)) <*> seq [1;2;3]           |> Seq.toList
    
let r30 = seq [(+);(-)] <*> seq [1;2] <*> seq [10;20;30]                |> Seq.toList  // compare it with r28


(** An exotic case where there is no ``pure``. *)

module MapAsApplicative = 
    let (<*>) (f:Map<'k,_>) x =
        Map (seq {
            for KeyValue(k, vf) in f do
                match Map.tryFind k x with
                | Some vx -> yield k, vf vx
                | _       -> () })


open MapAsApplicative

let r31 = Map ['a',(+);'b',(-)] <*> Map ['a',1;'b',2] <*> Map ['a',10;'b',20;'c',30] 

let r32 = Map ['c',(+);'b',(-)] <*> Map ['a',1;'b',2] <*> Map ['a',10;'b',20;'c',30] 

(**

Monads
======

*)

open OptionAsApplicative    
    
let a = Some 3
let b = Some 2
let c = Some 1

let half x = x / 2        

let f a b c =
    let x = a + b
    let y = half c
    x + y

let f' a b c =
    let x = Some (+)  <*> a <*> b
    let y = Some half <*> c
    Some (+) <*> x <*> y
    
let r33 = f' (Some 1) (Some 2) (Some 3)

let r33' = f' None (Some 2) (Some 3)
    
(** OK, but if I want to use a function like: *)
let exactHalf x =
    if x % 2 = 0 then Some (x / 2)
    else None

(** It doesn't fit *)

// let f'' a b c =
//     let x = Some (+) <*> a <*> b
//     let y = Some exactHalf <*> c   // y will be inferred as option<option<int>>
//     Some (+) <*> x <*> y           // so this will not compile


(**
The problem is, we were working with ordinary functions.
When we lift these function into C, we get functions wrapped in contexts.
With Applicatives we can use either a function in a context which is ready to use or an ordinary function, which we can lift easily with ``pure``.

But ``exactHalf`` is a different thing: its signature is ``int -> Option<int>``.
This function goes from a pure value to a value in a context, so either:

1) we use it directly but we first need to extract the argument from the context.

2) we use it in an Applicative, we will get a value in a context in another context, so we will need to flatten both contexts.

Monad provides solutions to both alternatives
*)

// bind : C<'a> -> ('a->C<'b>) -> C<'b>
// join : C<C<'a>> -> C<'a>

module OptionAsMonad =
    let join  x   = Option.bind id x
    let (>>=) x f = Option.bind f x
    // in monads pure' is called return, unit or result, but it's essentially the same function.
    let return' x = Some x
        
open OptionAsMonad        



let f'' a b c =
    let x = Some (+) <*> a <*> b
    let y = Some exactHalf <*> c |> join
    Some (+) <*> x <*> y
    

let f''' a b c =
    let x = Some (+) <*> a <*> b
    let y = c >>= exactHalf
    Some (+) <*> x <*> y

(** All monads are automatically applicatives, remember ``<*>`` for lists, it was:

``let (<*>)  f x = List.collect (fun x1 -> List.collect (fun x2 -> [x1 x2]) x) f`` *)

(** But ``List.collect`` is in fact ``bind``, and ``[x1 x2]`` is ``pure (x1 x2)`` *)

// let (<*>) f x = f >>= (fun x1 -> x >>= (fun x2 -> pure' (x1 x2)))

(**

And this definition of ``<*>`` applies to all monads.

Q: but we said all applicatives are functors, so monads should be functors as well, right?
A: Yes, they are, and this is the general definition of ``map`` based on ``bind`` and ``result`` (aka return or pure)
*)

let map f x = x >>= (pure' << f)

(**

Recommended links

Same explanation but with pictures
http://adit.io/posts/2013-04-17-functors,_applicatives,_and_monads_in_pictures.html

Haskell typeclasses
http://www.haskell.org/haskellwiki/Typeclassopedia *)