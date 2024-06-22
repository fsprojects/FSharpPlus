(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.

#r @"../../src/FSharpPlus/bin/Release/netstandard2.0/FSharpPlus.dll"
open FSharpPlus
open FSharpPlus.Data

(**
Computations Expressions
========================

This library allows to use some common computation expressions without writing any boiler plate code.

For [Applicatives](abstraction-applicative.html) there is single computation expression: ``applicative { .. }``. Additionally ``applicative2 { .. }`` and ``applicative3 { .. }`` exists for composed (aka layered) applicatives.

For [ZipApplicatives](abstraction-zipapplicative.html) there is a counterpart set of computation expressions: ``applicative' { .. }``, ``applicative2' { .. }`` and ``applicative3' { .. }``.

For [monadic](abstraction-monad.html) code there is a single computation expression: ``monad { .. }`` but it comes in 4 flavours:

 - Delayed or strict

   Delayed computations require that the type implements a TryWith, TryFinally and optionally a Delay method.
   F# comes with async and seq computation expressions, both are delayed.

 - It can have embedded side-effects or act as a monadplus

   A monadplus can return (or yield) many times, so for example all expressions in a loop can be returned, whereas in the other model those expressions are of type unit, since a side effect is expected.

   Async workflows is an example of a side-effect computation expression and seq expressions are an example of monadplus.

   Side effect workflows don't have any additional requirement over the type (apart from the monad operations), but monadplus requires the additional [get_Empty and (<|>)](abstraction-alternative.html) methods.

  The generic computation expression ``monad`` is a side-effect one, but it can be turned into a monadplus by accessing the ``.plus`` property. 
  Note that ``monad.fx`` is an alias for ``monad``: fx is used as an abbreviation for side-effects.

  These computations are lazy by default, but they can be made strict by adding ``.strict`` or using a ``'``, ie ``monad.plus'``.

In other words:

 - ``monad.fx`` or simply ``monad``: Lazy monadic builder. Use when you want to use side-effects instead of the additive behavior of monad plus.
 - ``monad.fx.strict`` (or ``monad.fx'`` or simply ``monad.strict`` or ``monad'``) is the strict version of ``monad``.
 - ``monad.plus``: Lazy additive monadic builder. Use when you expect one or more results.
 - ``monad.plus'`` is the strict version of ``monad.plus``

Note that a type is either lazy or strict, but it could act as fx or plus at the same time (see below some examples). This means that we need to pay attention when using a CE over a type, if the type is lazy but with use a strict monad, we'll get strict semantics which probably would make no sense, but if we do the opposite we might run into runtime errors, fortunately a compile-time warning (or error) will prevent us.

A simple way to find out if a type is strict or lazy is to execute this in fsi: `let _ : MyType<'t> = monad { printfn "I'm strict" }`

For layered monads (monad transformers) the general rule is: the monad is strict unless at least one of its constituent types is lazy, in that case the whole monad becomes lazy.

*)

let _ : OptionT<list<unit option>> = monad { printfn "I'm strict" }
// will print I'm strict, because OptionT and list are strict

let _ : OptionT<seq<unit option>> = monad { printfn "I'm strict" }
// won't print anything, because seq is lazy

(**


Examples
========

You may run this script step-by-step.


*)

(**
```f#
#r @"nuget: FSharpPlus"
```
*)
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


(*
For more information about computation expressions you can read the paper : The F# Computation Expression Zoo
http://tomasp.net/academic/papers/computation-zoo/computation-zoo.pdf
*)
