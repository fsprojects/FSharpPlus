namespace FSharpPlus.Tests

open System
open FSharpPlus
open FSharpPlus.Builders
open FSharpPlus.Data
open NUnit.Framework

module Helpers =
    let areEqual (x:'t) (y:'t) = Assert.AreEqual (x, y)

open Helpers

module ComputationExpressions = 

    [<Test>]
    let monadFx() =
        let effects = ResizeArray()

        // This workflow perform side-effects before and after an async operation in a monad.fx
        let zerowf = monad {
            effects.Add(1)
            do! Async.Sleep 10
            effects.Add(2) }

        // Check side effects are not yet executed
        areEqual (toList effects) []

        // This workflow will always run the previous one
        let combinewf = monad { 
            if true then do! Async.Sleep 10
            return! zerowf }

        // The list should be empty, no workflow was run
        areEqual (toList effects) []

        Async.RunSynchronously combinewf

        // Since it's an FX workflow, the last line should have been executed
        areEqual (toList effects) [1;2]


    [<Test>]
    let monadPlus() =
        let effects = ResizeArray()

        // This is a plus workflow
        // Although we're not explicitely using a strict workflow list hasn't a proper delay mechanism
        let lst: _ list = monad.plus {
            effects.Add(3)
            return 5;
            return 6; }

        // Check if side effect was already performed
        areEqual (effects |> toList) [3]

        // Check 'plus' (<|>) operation was properly performed
        areEqual lst [5;6]

        let effects = ResizeArray()

        // Now let's a try with seq, which has a delay mechanism
        let seq3: seq<_> = monad.plus { 
            effects.Add "Start"
            try
                try
                    10 / 0 |> ignore
                finally
                    effects.Add "execute this"
            with
            | e -> 
                effects.Add (sprintf "Exception! %s" e.Message)
                return 42 }

        // Confirm the side effect wasn't performed
        areEqual (toList effects) []

        let seqValue = toList seq3

        // Now they should
        areEqual (toList effects) ["Start"; "execute this"; "Exception! Attempted to divide by zero."]

        // Check the result
        areEqual seqValue [42]