module FSharpPlus.Tests.SeqT

open System
open FSharpPlus
open FSharpPlus.Data
open NUnit.Framework
open FsCheck
open Helpers
open System.Collections.Generic
open System.Threading.Tasks

module BasicTests =
    [<Test>]
    let wrap_unwrap () =
        let c = SeqT (async.Return (seq ['a'..'g']))
        let res = c |> SeqT.run |> SeqT |> SeqT.run |> extract
        let exp = c |> SeqT.run |> extract
        CollectionAssert.AreEqual (res, exp)
  
    [<Test>]
    let infiniteLists () =
        let infinite = SeqT.unfold (fun x -> monad<Lazy<_>> { return (Some (x, x + 1) ) }) 0
        let finite = take 12 infinite
        let res = finite <|> infinite
        CollectionAssert.AreEqual (res |> take 13 |> SeqT.run |> extract, [0;1;2;3;4;5;6;7;8;9;10;11;0])
        
    // Compile tests
    let binds () =
        let res1 = SeqT [|seq [1..4] |] >>= fun x -> SeqT [|seq [x * 2] |]
        let res2 = SeqT (Task.FromResult (seq [1..4])) >>= (fun x -> SeqT (Task.FromResult (seq [x * 2])))
        let res3 = SeqT (ResizeArray [seq [1..4] ]) >>= (fun x -> SeqT (ResizeArray [seq [x * 2] ]))
        let res4 = SeqT (lazy (seq [1..4])) >>= (fun x -> SeqT (lazy (seq [x * 2])))
        let res5 = SeqT (seq [seq [1..4] ]) >>= (fun x -> SeqT (seq [seq [x * 2] ]))
        ()
        
    let bind_for_identity () =
        let res = SeqT (Identity (seq [1..4])) >>= fun x -> SeqT (Identity (seq [x * 2]))
        ()    
        
    let computation_expressions () =
        let oneTwoThree : SeqT<_, _> = monad.plus { 
            do! lift <| Async.Sleep 10
            yield 1
            do! lift <| Async.Sleep 50
            yield 2 
            yield 3}
        ()

    let applicatives () =
        let x = (+) <!> SeqT None <*> SeqT (Some (seq [1;2;3;4]))
        let y = (+) <!> SeqT (async.Return (seq [1])) <*> SeqT (async.Return (seq [2]))
        let z = (+) <!> SeqT (Task.FromResult (seq [1])) <*> SeqT (Task.FromResult (seq [2]))
        ()