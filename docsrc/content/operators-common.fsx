(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../../bin"

#r @"../../src/FSharpPlus/bin/Release/net45/FSharpPlus.dll"
open FSharpPlus

(**
Operators - Common Combinators
===============================

These generic functions and operators are used commonly and are not part
of any other abstraction.

You can find these in the API docs: [Operators.fs](reference/operators.html)

flip
====

Creates a new function with first two arguments flipped.


konst
=====

Create a function that always returns the given argument.
This is known as a 'constant' function.

This is commonly useful where a function is required as a parameter
for flexibility, but isn't required in a specific instance.

example:
*)
    > [1;2;3] |> filter (konst true);; 
    val it : int list = [1; 2; 3]

(**
curry, uncurry, curryN, uncurryN
================================

Currying is the process of taking a function expecting a tuple, and returning a
function with the same number of arguments as the tuple size.

Uncurrying is the reverse process.

There is `curry` and `uncurry` that work on two arguments each, while `curryN`
and `uncurryN` work on any number.

example:
*)
    > let addThreeNums (x, y, z) = x + y + z;;
    val addThreeNums : x:int * y:int * z:int -> int

    > curryN addThreeNums 1 2 3;;
    val it : int = 6

(**
Functions as operators - </ />
==============================

A pair of operators `</` and `/>` are defined to allow any function to be used as
an operator. It will flip the args of your function so that it makes sense when
the first argument is coming from the left-hand-side.

example:
*)
> let biggerThan a b = a > b;;
val biggerThan : a:'a -> b:'a -> bool when 'a : comparison

> 10 </biggerThan/> 3;;
val it : bool = true

(**
tap
===

Tap executes a side-effect function, then returns the original input value.
Consider this as 'tapping into' a chain of functions.

example:
*)
    // a pipeline of functions, with a tap in the middle
    let names = ["John"; "Smith"]
    names |> map String.toUpper |> tap (printfn "%A") |> map String.toLower;;

    // prints this:
    ["JOHN"; "SMITH"]

    // but returns this:
    val it : string list = ["john"; "smith"]

(**
either
======

Extracts the value inside a Result from either side - whether Ok or Error.

It takes a pair of functions:

 * fOk - a function applied to the source if it contains an Ok value
 * fError - a function applied to the source if it contains an Error value

...and the source:

 * source - the source value containing an Ok or Error

*)
    > let myResult = Ok "I am ok!";;
    val myResult : Result<string,'a>

    > let myOther = Error -1;;
    val myOther : Result<'a,int>

    > either id id myResult;;
    val it : string = "I am ok!"

    > either id id myOther;;
    val it : int = -1

(**
Don't confuse the `either` function with `result` which lifts a value into a
Functor, just like `return` when in a computation expression.


option
======

Takes a function, a default value and a option value. If the option value is None, the function returns the default value.
Otherwise, it applies the function to the value inside Some and returns the result.
*)
    let inline option f n = function Some x -> f x | None -> n

(**

tuple2, tuple3, ...tuple8
=========================

Functions that generate a tuple. The number indicates the number of arguments
that are defined, and the corresponding size of tuple.

*)
    let inline tuple2 a b             = a,b
    let inline tuple3 a b c           = a,b,c
    let inline tuple4 a b c d         = a,b,c,d
    let inline tuple5 a b c d e       = a,b,c,d,e
    let inline tuple6 a b c d e f     = a,b,c,d,e,f
    let inline tuple7 a b c d e f g   = a,b,c,d,e,f,g
    let inline tuple8 a b c d e f g h = a,b,c,d,e,f,g,h
