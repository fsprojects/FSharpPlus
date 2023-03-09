(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.

#r @"../../src/FSharpPlus/bin/Release/netstandard2.0/FSharpPlus.dll"
#nowarn "0058" // We need to cheat a bit with indentation here.

(**
Introducing FSharpPlus
======================

 - Download binaries from [Nuget](https://www.nuget.org/packages/FSharpPlus/), use the latest CI version.

 - Open an F# script file or the F# interactive, reference the library and open the namespace

*)

(**
```f#
#r @"nuget: FSharpPlus"
```
*)
open FSharpPlus

(**
 Ignore warnings about F# metadata if any.


Now we'll start with a quick overview of the features presented in F#+.

### Generic functions 

They are automatically available when opening the FSharpPlus namespace

here's an example with <code>map</code> ([fmap](https://wiki.haskell.org/Functor) for Haskellers, [Select](http://www.dotnetperls.com/select) for C-sharpers):

*)

map string [|2;3;4;5|]
// val it : string [] = [|"2"; "3"; "4"; "5"|]

map ((+) 9) (Some 3)
// val it : int option = Some 12

open FSharpPlus.Data

map string (NonEmptyList.create 2 [3;4;5])
// val it : NonEmptyList<string> = {Head = "2"; Tail = ["3"; "4"; "5"];}

(**
They're also available for your own types as long as they contain the appropriated method with the expected signature
*)


type Tree<'t> =
    | Tree of 't * Tree<'t> * Tree<'t>
    | Leaf of 't
    static member Map (x:Tree<'a>, f) = 
        let rec loop f = function
            | Leaf x -> Leaf (f x)
            | Tree (x, t1, t2) -> Tree (f x, loop f t1, loop f t2)
        loop f x

map ((*) 10) (Tree(6, Tree(2, Leaf 1, Leaf 3), Leaf 9))
// val it : Tree<int> = Tree (60,Tree (20,Leaf 10,Leaf 30),Leaf 90)

(**
Generic functions may be seen as an exotic thing in F# that only saves a few key strokes (<code>map</code> instead of <code>List.map</code> or <code>Array.map</code>) still they allow you to reach a higher abstraction level, using ad-hoc polymorphism.

But more interesting is the use of operators. You can't prefix them with the module they belong to, well you can but then it's no longer an operator. As an example many F# libraries define the bind operator <code>(>>=)</code> but it's not generic so if you use two different types which are both monads you will need to prefix it e.g. <code>State.(>>=)</code> and <code>Reader.(>>=)</code> which defeats the purpose of having an operator.

Here you have a ready-to-use generic bind operator: ``>>=``
*)

let x = ["hello";" ";"world"] >>= (fun x -> Seq.toList x)
// val x : char list = ['h'; 'e'; 'l'; 'l'; 'o'; ' '; 'w'; 'o'; 'r'; 'l'; 'd']


let tryParseInt : string -> int option = tryParse
let tryDivide x n = if n = 0 then None else Some (x / n)

let y = Some "20" >>= tryParseInt >>= tryDivide 100
// val y : int option = Some 5

(**
You have also the Kleisli composition (fish) operator:  ``>=>``

Which is becoming popular in F# after the [Railway Oriented Programming](https://www.google.ch/#q=railway+oriented+programming) tutorial series
*)

let parseAndDivide100By = tryParseInt >=> tryDivide 100

let parsedAndDivide100By20 = parseAndDivide100By "20"   // Some 5
let parsedAndDivide100By0' = parseAndDivide100By "zero" // None
let parsedAndDivide100By0  = parseAndDivide100By "0"    // None

let parseElement n = List.tryItem n >=> tryParseInt
let parsedElement  = parseElement 2 ["0"; "1";"2"]

(**
But don't forget the above used operators are generic, so we can change the type of our functions and we get a different functionality for free:
*)

(*** hide ***)
module E2 =

let tryParseInt x : Choice<int, string> = 
    match tryParse x with 
    | Some x -> Choice1Of2 x
    | None   -> Choice2Of2 ("Failed to parse " + x)
        

let tryDivide x n = 
    if n = 0 then Choice2Of2 "Can't divide by zero"
    else Choice1Of2 (x / n)

(**
The test code remains unchanged, but we get a more interesting functionality
*)

let parseAndDivide100By = tryParseInt >=> tryDivide 100

let parsedAndDivide100By20 = parseAndDivide100By "20"   // Choice1Of2 5
let parsedAndDivide100By0' = parseAndDivide100By "zero" // Choice2Of2 "Failed to parse zero"
let parsedAndDivide100By0  = parseAndDivide100By "0"    // Choice2Of2 "Can't divide by zero"


(**

Also when working with combinators, the generic applicative functor (space invaders) operator is very handy: ``<*>``
*)

let sumAllOptions = Some (+) <*> Some 2 <*> Some 10     // val sumAllOptions : int option = Some 12

let sumAllElemets = [(+)] <*> [10; 100] <*> [1; 2; 3]   // int list = [11; 12; 13; 101; 102; 103]

(**

For more details and features, see [generic operators and functions](generic-doc.html)

Here are all [generic operators and functions](reference\fsharpplus-operators.html)

And [here's a short explanation](applicative-functors.html) of Functor, Applicative and Monad abstractions with code samples.



### Lens



from https://github.com/ekmett/lens/wiki/Examples


First, open F#+ Lens
*)

open FSharpPlus.Lens

(** Now, you can read from lenses (``_2`` is a lens for the second component of a tuple) *)

let r1 = ("hello","world")^._2
// val it : string = "world"

(** and you can write to lenses. *)
let r2 = setl _2 42 ("hello","world")
// val it : string * int = ("hello", 42)

(**  Composing lenses for reading (or writing) goes in the order an imperative programmer would expect, and just uses ``(<<)``. *)
let r3 = ("hello",("world","!!!"))^.(_2 << _1)
// val it : string = "world"

let r4 = setl (_2 << _1) 42 ("hello",("world","!!!"))
// val it : string * (int * string) = ("hello", (42, "!!!"))

(**  You can make a Getter out of a pure function with ``to'``. *)
let r5 = "hello"^.to' length
// val it : int = 5

(**  You can easily compose a Getter with a Lens just using ``(<<)``. No explicit coercion is necessary. *)
let r6 = ("hello",("world","!!!"))^. (_2 << _2 << to' length)
// val it : int = 3

(**  As we saw above, you can write to lenses and these writes can change the type of the container. ``(.->)`` is an infix alias for ``set``. *)
let r7 = _1 .-> "hello" <| ((),"world")
// val it : string * string = ("hello", "world")

(**  It can be used in conjunction with ``(|>)`` for familiar von Neumann style assignment syntax: *)
let r8 = ((), "world") |> _1 .-> "hello"
// val it : string * string = ("hello", "world")

(**  Conversely view, can be used as an prefix alias for ``(^.)``. *)
let r9 = view _2 (10,20)
// val it : int = 20

(**

For more details:

Here's a full tour of [lens and all other optics](lens.html)

Have a look at all [lens functions](reference\fsharpplus-lens.html)
*)
