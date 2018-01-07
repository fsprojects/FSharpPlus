(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../../bin"

(**
Computations Expressions
========================

This library allows to use some common computation expressions without writing any boiler plate code.

There is a single computation expression: ``monad`` but it comes in 4 flavours:

 - It could be delayed or strict
 - It can have embedded side-effects or act as a monad plus


Examples
========

You may run this script step-by-step.


*)

#r @"../../src/FSharpPlus/bin/Release/net45/FSharpPlus.dll"
open FSharpPlus

let lazyValue = monad {
    let! a = lazy (printfn "I'm lazy"; 2)
    let! b = lazy (printfn "I'm lazy too"; 10)
    return a + b}

// val lazyValue : System.Lazy<int> = Value is not created.

let res12 = lazyValue.Value


let maybeWithSideFx = monad.fx.strict { 
    let! a = Some 3
    let b = ref 0
    while !b < 10 do 
      let! n = Some ()
      incr b
    if a = 3 then printfn "got 3"
    else printfn "got something else (will never print this)"
    return a }

// val maybeWithSideFx : int option = Some 3



let lst = [None; None; Some 2; Some 4; Some 10; None]

let maybeManyTimes = monad.plus.strict {
    let defaultValue = 42
    let mutable i = 0
    return! None
    while i < 5 do
        printfn "looping %i" i
        i <- i + 1
        return! lst.[i]
    printfn "halfway"
    return! None
    printfn "near the end"
    return defaultValue }

// val maybeManyTimes : int option = Some 2