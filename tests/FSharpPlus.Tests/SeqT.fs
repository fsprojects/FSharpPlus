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
        let infinite: SeqT<Lazy<_>, _> = SeqT.unfold (fun x -> monad { return (Some (x, x + 1) ) }) 0
        let finite = take 12 infinite
        let res = finite <|> infinite
        CollectionAssert.AreEqual (res |> take 13 |> SeqT.run |> extract, [0;1;2;3;4;5;6;7;8;9;10;11;0])

    [<Test>]
    let infiniteLists2 () =
        let infinite: SeqT<Async<_>, _> = SeqT.unfold (fun x -> monad { return (Some (x, x + 1) ) }) 0
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

    let monadTransOps () =
        let fn : SeqT<Reader<int, bool>, int> = 
            monad.plus {
                let! x1 = ask
                let! x2 = 
                    if x1 > 0 then result 1
                    else empty
                return x1 + x2
            }
        
        let x = (fn |> SeqT.run |> Reader.run) 10 |> Seq.toList
        areEqual [11] x
        let y = (fn |> SeqT.run |> Reader.run) -1 |> Seq.toList
        areEqual [] y


module ComputationExpressions =

    type __ = bool
    
    type Task = Task<__>
    type Async = Async<__>

    [<Test>]
    let tryWithBlocks () =
        
        let monadTransformer2layersTest5 () =
            let x: SeqT<Task, unit> = monad {
                try
                    failwith "Exception in try-with not handled"
                    ()
                with _ -> () }
            x
        let _ = monadTransformer2layersTest5 () |> SeqT.run |> Async.AwaitTask |> Async.RunSynchronously |> Seq.toList
        
        let monadTransformer2layersTest5_a () =
            let x: SeqT<Async, unit> = monad {
                try
                    failwith "Exception in try-with not handled"
                    ()
                with _ -> () }
            x
        let _ = monadTransformer2layersTest5_a () |> SeqT.run |> Async.RunSynchronously |> Seq.toList
        
        
        let monadTransformer3layersTest4 () =
            let x: SeqT<ReaderT<unit, Task>, unit> = monad {
                try
                    failwith "Exception in try-with not handled"
                    ()
                with _ -> () }
            x
        // let _ = (monadTransformer3layersTest4 () |> SeqT.run |> ReaderT.run) ()
        // type inference doesn't work with full piping
        let _ = let x = (monadTransformer3layersTest4 () |> SeqT.run) in (x |> ReaderT.run) ()
        
        let monadTransformer3layersTest5 () =
            let x: SeqT<ResultT<Task<Result<bool, unit>>>, unit> = monad {
              return // need to add this
                try
                    failwith "Exception in try-with not handled"
                    ()
                with _ -> () }
            x
        // type inference doesn't work with full piping
        let _ = let x = (monadTransformer3layersTest5 () |> SeqT.run) in  x |> ResultT.run |> extract |> Result.get
        
        
        let monadTransformer3layersTest1 () =
            let x: ReaderT<unit, ResultT<Task<Result<unit,unit>>>> = monad {
                try
                    failwith "Exception in try-with not handled"
                    ()
                with _ -> () }
            x
        let _ = (monadTransformer3layersTest1 () |> ReaderT.run) () |> ResultT.run |> extract |> Result.get

        ()