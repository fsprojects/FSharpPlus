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
        let (SeqT exp) = c
        CollectionAssert.AreEqual (res, extract exp)
  
    [<Test>]
    let infiniteLists1 () =
        let infinite: SeqT<Lazy<_>, _> = SeqT.unfold (fun x -> Some (x, x + 1)) 0
        let finite = take 12 infinite
        let res = finite <|> infinite
        CollectionAssert.AreEqual (res |> take 13 |> SeqT.run |> extract, [0;1;2;3;4;5;6;7;8;9;10;11;0])

    [<Test>]
    let infiniteLists2 () =
        let infinite: SeqT<Async<_>, _> = SeqT.unfold (fun x -> Some (x, x + 1)) 0
        let finite = take 12 infinite
        let res = finite <|> infinite
        CollectionAssert.AreEqual (res |> take 13 |> SeqT.run |> extract, [0;1;2;3;4;5;6;7;8;9;10;11;0])

    [<Test>]
    let zipLists1 () =
        let infinite: SeqT<Lazy<_>, int> = SeqT.unfoldM (fun x -> monad { return if x = 13 then failwith "Unlucky number" else (Some (x, x + 1) ) }) 0
        let finite:   SeqT<Lazy<_>, int> = SeqT.unfoldM (fun x -> monad { return if x =  7 then None else (Some (x, x + 1) ) }) 0
        let x1 = zip (map string infinite) finite
        let y1 = SeqT.run x1 |> extract |> toList
        CollectionAssert.AreEqual (y1, [("0", 0); ("1", 1); ("2", 2); ("3", 3); ("4", 4); ("5", 5); ("6", 6)])

        let x2 = lift2 tuple2 (map string finite) (take 2 infinite)
        let y2 = SeqT.run x2 |> extract |> toList
        CollectionAssert.AreEqual (y2, [("0", 0); ("0", 1); ("1", 0); ("1", 1); ("2", 0); ("2", 1); ("3", 0); ("3", 1); ("4", 0); ("4", 1); ("5", 0); ("5", 1); ("6", 0); ("6", 1)])

        let x3 = SeqT.zip3 (map string infinite) (map ((*) 10) infinite) (take 3 finite)
        let y3 = SeqT.run x3 |> extract |> toList
        CollectionAssert.AreEqual (y3, [("0", 0, 0); ("1", 10, 1); ("2", 20, 2)])

    [<Test>]
    let zipLists2 () =
        let infinite: SeqT<Async<_>, _> = SeqT.unfoldM (fun x -> monad { return if x = 13 then failwith "Unlucky number" else (Some (x, x + 1) ) }) 0
        let finite:   SeqT<Async<_>, _> = SeqT.unfoldM (fun x -> monad { return if x =  7 then None else (Some (x, x + 1) ) }) 0
        let x1 = zip (map string infinite) finite
        let y1 = SeqT.run x1 |> extract |> toList
        CollectionAssert.AreEqual (y1, [("0", 0); ("1", 1); ("2", 2); ("3", 3); ("4", 4); ("5", 5); ("6", 6)])
        
        let x2 = lift2 tuple2 (map string finite) (take 2 infinite)
        let y2 = SeqT.run x2 |> extract |> toList
        CollectionAssert.AreEqual (y2, [("0", 0); ("0", 1); ("1", 0); ("1", 1); ("2", 0); ("2", 1); ("3", 0); ("3", 1); ("4", 0); ("4", 1); ("5", 0); ("5", 1); ("6", 0); ("6", 1)])
        
        let x3 = SeqT.zip3 (map string infinite) (map ((*) 10) infinite) (take 3 finite)
        let y3 = SeqT.run x3 |> extract |> toList
        CollectionAssert.AreEqual (y3, [("0", 0, 0); ("1", 10, 1); ("2", 20, 2)])

    // Compile tests
    let binds () =
        let res1 = SeqT [|seq [1..4] |] >>= fun x -> SeqT [|seq [x * 2] |]
        let res2 = SeqT.hoist [1..4] >>= (fun x -> SeqT (Task.FromResult (seq [x * 2])))
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
        let x = (+) <!> SeqT None <*> SeqT.ofSeq [1;2;3;4]
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


    module AsyncSeq =

        type AsyncSeq<'t> = SeqT<Async<bool>, 't>
        let asyncSeq<'t> = monad<SeqT<Async<bool>, 't>>.plus

        /// Determines equality of two async sequences by convering them to lists, ignoring side-effects.
        let EQ (a: AsyncSeq<'a>) (b: AsyncSeq<'a>) =
            let exp = a |> SeqT.runAsList |> Async.RunSynchronously
            let act = b |> SeqT.runAsList |> Async.RunSynchronously
            if (exp = act) then true
            else
                printfn "expected=%A" exp
                printfn "actual=%A" act
                false

        
        [<Test>]
        let ``try finally works no exception``() =
            let mutable x = 0
            let s = asyncSeq {
                try yield 1
                finally x <- x + 3 }

            Assert.True ((x = 0))

            let s1 = s |> SeqT.runAsList |> Async.RunSynchronously
            Assert.True ((x = 3))

            let s2 = s |> SeqT.runAsList |> Async.RunSynchronously
            Assert.True ((x = 6))


        [<Test>]
        let ``try finally works exception``() =
            let mutable x = 0
            let s = asyncSeq {
                try
                    try yield 1
                        failwith "fffail"
                    finally x <- x + 1
                finally x <- x + 2 }

            Assert.True ((x = 0))

            let s1 = try (s |> SeqT.runAsList |> Async.RunSynchronously) with _ -> []
            Assert.True ((s1 = []))
            Assert.True ((x = 3))
            ()
            let s2 = try s |> SeqT.runAsList |> Async.RunSynchronously with _ -> []
            Assert.True ((s2 = []))
            Assert.True ((x = 6))


        [<Test>]
        let ``try with works exception``() =
            let mutable x = 0
            let s = asyncSeq {
                 try failwith "ffail"
                 with e -> x <- x + 3 }
            
            Assert.True ((x = 0))

            let s1 = try s |> SeqT.runAsList |> Async.RunSynchronously with _ -> []
            Assert.True ((s1 = []))
            Assert.True ((x = 3))

            let s2 = try s |> SeqT.runAsList |> Async.RunSynchronously with _ -> []
            Assert.True ((s2 = []))
            Assert.True ((x = 6))

        
        [<Test>]
        let ``try with works no exception``() =
            let mutable x = 0
            let s = asyncSeq {
                try yield 1
                with e -> x <- x + 3 }
            
            Assert.True ((x = 0))

            let s1 = try s |> SeqT.runAsList |> Async.RunSynchronously with _ -> []
            Assert.True ((s1 = [1]))
            Assert.True ((x = 0))

            let s2 = try s |> SeqT.runAsList |> Async.RunSynchronously with _ -> []
            Assert.True ((s2 = [1]))


        [<Test>]
        let ``AsyncSeq.zip``() =
            for la in [ []; [1]; [1;2;3;4;5] ] do
                for lb in [ []; [1]; [1;2;3;4;5] ] do
                    let a: SeqT<Async<_>, _> = la |> SeqT.ofSeq
                    let b: SeqT<Async<_>, _> = lb |> SeqT.ofSeq
                    let actual = SeqT.zip a b
                    let expected = Seq.zip la lb |> SeqT.ofSeq
                    Assert.True (EQ expected actual)
        
        
        [<Test>]
        let ``AsyncSeq.zipWithAsync``() =
            for la in [ []; [1]; [1;2;3;4;5] ] do
                for lb in [ []; [1]; [1;2;3;4;5] ] do
                    let a: SeqT<Async<_>, _> = la |> SeqT.ofSeq
                    let b: SeqT<Async<_>, _> = lb |> SeqT.ofSeq
                    let actual = SeqT.map2M (fun a b -> a + b |> async.Return) a b
                    let expected = Seq.zip la lb |> Seq.map ((<||) (+)) |> SeqT.ofSeq
                    Assert.True (EQ expected actual)
            
        