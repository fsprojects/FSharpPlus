
#r @"nuget: FSharpPlus"
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
