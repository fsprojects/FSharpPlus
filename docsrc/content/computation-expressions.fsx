(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../../bin"

(**
Computations Expressions
========================

This library allows to use some common computation expressions without writing any boiler plate code.

There is a single computation expression: ``monad`` but it comes in 4 flavours:

 - Delayed or strict

   Delayed computations require that the type implements a Delay method.
   F# comes with async and seq computation expressions, both are delayed.

 - It can have embedded side-effects or act as a monadplus

   A monadplus can return (or yield) many times, so for example all expressions in a loop can be returned, whereas in the other model those expressions are of type unit, since a side effect is expected.

   Async workflows is an example of a side-effect computation expression and seq expressions are an example of monadplus.

   Side effect workflows don't have any additional requirement over the type (apart from the monad operations), but monadplus requires the additional [get_Empty and (<|>)](abstraction-alternative.html) methods.

  The generic computation expression ``monad`` is a side-effect one, but it can be turned into a monadplus by accessing the ``.plus`` property.

  These computations are lazy by default, but they can be made strict by adding ``.strict`` or using a ``'``, ie ``monad.plus'``.


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


let maybeWithSideFx = monad' { 
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

let maybeManyTimes = monad.plus' {
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


let (asnNumber: Async<_>) = monad.fx {
    let mutable m = ResizeArray ()
    try
        for i = 1 to 10 do
            m.Add i
        return m.[-1]
    with e ->
        return -3 }


let (lstNumber: list<_>) = monad.plus' {
    try
        for i = 1 to 10 do
            return i
    with e ->
        return -3 }