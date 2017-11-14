(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../../bin"

(**
Generic operators and functions
===============================

*)

#r "../../bin/net45/FSharpPlus.dll"
open FSharpPlus

(**

Generic operators, functions and constants are included in this library.

They work with many types, including:

 1) Existing .NET and F# types

 2) Other types included in this library

 3) Further user defined types

 4) Types defined in other libraries


Case 1 works by using overload resolution inside an internal class (referred to as the Invokable) used at the definition of the generic operation, while all the other cases work typically through Duck Typing, where an expected method name and signature must exists in the target Type or by using default implementations based on other operations.

Here are some examples of the generic ``map`` operation over existing .NET and F# types:

*)


map string [|2;3;4;5|]
// val it : string [] = [|"2"; "3"; "4"; "5"|]

map ((+) 9) (Some 3)
// val it : int option = Some 12

map ((+) 9) (async {return 3});;
// val it : Async<int> = Microsoft.FSharp.Control.FSharpAsync`1[System.Int32]
extract it;;
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

#r "../../packages/test/MathNet.Numerics/lib/net40/MathNet.Numerics.dll"
#r "../../packages/test/MathNet.Numerics.FSharp/lib/net40/MathNet.Numerics.FSharp.dll"

let x : MathNet.Numerics.BigRational = fromBigInt 10I