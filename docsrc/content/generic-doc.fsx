(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#r @"../../src/FSharpPlus/bin/Release/net45/FSharpPlus.dll"
open FSharpPlus

(**
Generic operators and functions
===============================

After reviewing [extension functions](extensions.html) it's natural to want to
use generic functions that can work across different types.

F#+ implements generic functions that efficiently call out to specific
implementations. This handles existing .Net and F# types, and you can use them
on your own and third-party types by implementing expected method names
and signatures.

Read about the specific operators:

 * Docs on [Operators - Common Combinators](operators-common.html)
 * Other docs exist for each [abstraction](abstractions.html)
 * API Doc for [Generic functions and operators](reference/operators.html)

They're particularly useful in that the specific function called will
depend on the input arguments and return type. However, this means you
sometimes need to explicitly specify the type if this information is
not available (actually it's a good debug technique to temporarily add
the types explicitly when the compiler tells you that the types are wrong).

For example:
*)

// Convert the number 42 to bytes... 
// ... here the type is known (42 is an int, the return value is byte[])
let a = 42 |> toBytes;;  
//val a : byte [] = [|42uy; 0uy; 0uy; 0uy|]

// However, this can't compile since the return type is not inferrable
// let b = [|42uy; 0uy; 0uy; 0uy|] |> ofBytes;;  

// The error will be something like:
// 
//  let b = [|42uy; 0uy; 0uy; 0uy|] |> ofBytes;;
//  -----------------------------------^^^^^^^
//
// error FS0071: Type constraint mismatch when applying the default type 'obj'
// for a type inference variable. No overloads match for method 'OfBytes'.
// The available overloads are shown below. Consider adding further type constraints

// [followed by many possible implementations...]

// So, in this case, we have to give the return type:
let b :int = [|42uy; 0uy; 0uy; 0uy|] |> ofBytes;;
// val b : int = 42

// ...or, the more usual case, you use in context where type can be inferred,
// like this example:
1 + ([|42uy; 0uy; 0uy; 0uy|] |> ofBytes);;
//val it : int = 43

(**
How do generic functions work?
==============================

F# does not support overloaded functions, but it does support overloaded
methods on types (classes) - including static methods. F#+ takes
advantage of this by definining generic functions that call out to
an internal class (referred to as an "Invokable") where various overloaded 
static methods are defined.

An Invokable is written such that the most specific, and hence, optimised
overload is resolved for existing .Net and F# types, and that a more general
implementation is used otherwise.

What does this all mean?

It means care is taken to use the most optimised implementation, and you can
implement your own instances of generic functions if you implement the required
methods.

Examples
========

Here are some examples of the generic ``map`` operation over existing .NET and F# types:

*)


map string [|2;3;4;5|]
// val it : string [] = [|"2"; "3"; "4"; "5"|]

map ((+) 9) (Some 3)
// val it : int option = Some 12

let res12 = map ((+) 9) (async {return 3})
// val it : Async<int> = Microsoft.FSharp.Control.FSharpAsync`1[System.Int32]
extract res12
// val it : int = 12

(**
Here are some examples with types defined in this library:
*)

open FSharpPlus.Data

map string (NonEmptyList.create 2 [3;4;5])
// val it : NonEmptyList<string> = {Head = "2"; Tail = ["3"; "4"; "5"];}

let stateFul42 = map string (State (fun x -> (42, x)))
State.run stateFul42 "state"
// val stateFul42 : State<string,string> = State <fun:map@12-9>
// val it : string * string = ("42", "state")

(**
Now let's define our own type with its own map definition
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

For a type defined in an external library it will work when it contains a static member matching the expected name and signature.

Here's an example of the generic function ``fromBigInt`` targeting a type defined in the MathNet library
*)
#r "../../packages/docs/MathNet.Numerics/lib/net40/MathNet.Numerics.dll"
#r "../../packages/docs/MathNet.Numerics.FSharp/lib/net45/MathNet.Numerics.FSharp.dll"

let x : MathNet.Numerics.BigRational = fromBigInt 10I