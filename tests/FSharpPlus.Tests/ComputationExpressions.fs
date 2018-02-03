namespace FSharpPlus.Tests

open System
open FSharpPlus
open FSharpPlus.Builders
open FSharpPlus.Data
open NUnit.Framework

open Helpers
open SideEffects

module ComputationExpressions = 

    [<Test>]
    let monadFx() =
        SideEffects.reset()

        // This workflow perform side-effects before and after an async operation in a monad.fx
        let zerowf = monad {
            SideEffects.add "1"
            do! Async.Sleep 10
            SideEffects.add "2" }

        // Check side effects are not yet executed
        areEqual (SideEffects.get()) []

        // This workflow will always run the previous one
        let combinewf = monad { 
            if true then do! Async.Sleep 10
            return! zerowf }

        // The list should be empty, no workflow was run
        areEqual (SideEffects.get()) []

        Async.RunSynchronously combinewf

        // Since it's an FX workflow, the last line should have been executed
        areEqual (SideEffects.get()) ["1"; "2"]


    [<Test>]
    let monadPlus() =
        SideEffects.reset()

        // This is a plus workflow
        // Although we're not explicitely using a strict workflow list hasn't a proper delay mechanism
        let lst: _ list = monad.plus {
            SideEffects.add "3"
            return 5;
            return 6; }

        // Check if side effect was already performed
        areEqual (SideEffects.get()) ["3"]

        // Check 'plus' (<|>) operation was properly performed
        areEqual lst [5;6]

        SideEffects.reset()

        // Now let's a try with seq, which has a delay mechanism
        let seq3: seq<_> = monad.plus { 
            SideEffects.add "Start"
            try
                try
                    10 / 0 |> ignore
                finally
                    SideEffects.add "execute this"
            with
            | e -> 
                SideEffects.add (sprintf "Exception! %s" e.Message)
                return 42 }

        // Confirm the side effect wasn't performed
        areEqual (SideEffects.get()) []

        let seqValue = toList seq3

        // Now they should
        areEqual (SideEffects.get()) ["Start"; "execute this"; "Exception! Attempted to divide by zero."]

        // Check the result
        areEqual seqValue [42]


    [<Test>]
    let delayedMonadTransformers() =

        SideEffects.reset()

        let threeElements : ReaderT<string, list<_>> = monad.plus {
            let! s = ask
            for i in 1 .. 3 do
                SideEffects.add (sprintf "processing %i" i)
                yield parse s + i }

        areEqual (SideEffects.get()) []
        
        // Following line would throw an exception (due to the for loop) if ReaderT had no Delay implementation
        let results = ReaderT.run threeElements "100"

        areEqual (SideEffects.get()) ["processing 1"; "processing 2"; "processing 3"]
        areEqual results [101; 102; 103]