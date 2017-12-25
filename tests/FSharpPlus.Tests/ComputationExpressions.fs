namespace FSharpPlus.Tests

open System
open FSharpPlus
open FSharpPlus.Builders
open FSharpPlus.Data
open NUnit.Framework

[<TestFixture>]
type Workflows() = 

    [<Test>]
    member x.MonadFx() = 
        let effects = ResizeArray()
        let zero = monad {
            effects.Add(1)
            do! Async.Sleep 10
            effects.Add(2) }        
        Assert.AreEqual(effects |> toList, [])

        let combine = monad { 
            if true then do! Async.Sleep 10
            return! zero }
        Assert.AreEqual(effects |> toList, [])
        Async.RunSynchronously combine
        Assert.AreEqual(effects |> toList, [1;2])

    member x.MonadPlus() = 
        let effects = ResizeArray()
        let lst: _ list = monad.plus {
            effects.Add(3)
            return 5;
            return 6; }
        Assert.AreEqual(effects |> toList, [1;2;3])
        Assert.AreEqual(lst, [5;6])

        let effects = ResizeArray()
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
        Assert.AreEqual(effects |> toList, [])
        let seqValue = seq3 |> Seq.toList
        Assert.AreEqual(effects |> toList, ["Start"; "execute this"; "Exception! Attempted to divide by zero."])
        Assert.AreEqual(seqValue, [42])