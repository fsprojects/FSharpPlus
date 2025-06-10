namespace FSharpPlus.Tests

#nowarn "686"

open System
open System.Collections.ObjectModel
open FSharpPlus
open FSharpPlus.Data
open FSharpPlus.Control
open NUnit.Framework
open Helpers
open FSharpPlus.Math.Applicative
open CSharpLib
open System.Threading.Tasks
#if TEST_TRACE
open FSharpPlus.Internals
#endif

module Traversable =

    type Either<'l,'r> = Left of 'l | Right of 'r with
        static member Return x = Right x
        static member inline get_Empty () = Left empty
        static member Map (x, f) = match x with Right a -> Right (f a) | Left a -> Left a
        static member (<*>) (f, x) =
            SideEffects.add ("f(x) <*> " + string x)
            match f, x with Right a, Right b -> Right (a b) | Left e, _ | _, Left e -> Left e
        static member IsLeftZero x = match x with Left _ -> true | _ -> false

    let traverseTest =
        let _None = sequence (seq [Some 3;None ;Some 1])
        let _None2 = sequence (TestNonEmptyCollection.Create (Some 42))
        ()

    [<Test>]
    let sequence_Default_Primitive () = 
        let testVal = sequence [|Some 1; Some 2|]
        Assert.AreEqual (Some [|1;2|], testVal)
        Assert.IsInstanceOf<Option<array<int>>> testVal

    [<Test>]
    let traverseDerivedFromSequence () = 
        let testVal = traverse (fun x -> [int16 x..int16 (x+2)]) (WrappedListH [1; 4])
        Assert.AreEqual (
            [
                WrappedListH [1s; 4s]; WrappedListH [1s; 5s]; WrappedListH [1s; 6s];
                WrappedListH [2s; 4s]; WrappedListH [2s; 5s]; WrappedListH [2s; 6s];
                WrappedListH [3s; 4s]; WrappedListH [3s; 5s]; WrappedListH [3s; 6s]
            ] , testVal)
        Assert.IsInstanceOf<list<WrappedListH<int16>>> testVal

    [<Test>]
    let sequence_Specialization () =
        
        let inline seqSeq (x:_ seq ) = sequence x
        let inline seqArr (x:_ []  ) = sequence x
        let inline seqLst (x:_ list) = sequence x

        let a : list<_> = seqSeq (seq [[1];[3]])
        CollectionAssert.AreEqual ([seq [1; 3]], a)
        Assert.IsInstanceOf<list<seq<int>>> a
        let b = seqArr ( [|[1];[3]|])
        CollectionAssert.AreEqual ([[|1; 3|]], b)
        Assert.IsInstanceOf<list<array<int>>> b
        let c = seqLst ( [ [1];[3] ])
        CollectionAssert.AreEqual ([[1; 3]], c)
        Assert.IsInstanceOf<list<list<int>>> c

    [<Test>]
    let traverse_Specialization () =
        let _ = Seq.traverse id [WrappedSeqD [1]; WrappedSeqD [2]]
        let _ = Seq.sequence    [WrappedSeqD [1]; WrappedSeqD [2]]
        let _ = Seq.traverse id [ZipList [1]; ZipList []; ZipList (seq {failwith "sholdn't get here"})] |> toList
        let _ = Seq.sequence    [ZipList [1]; ZipList []; ZipList (seq {failwith "sholdn't get here"})] |> toList
        ()

    [<Test>]
    let traverse_Order () =
        SideEffects.reset()
        let mapper v = SideEffects.add <| sprintf "mapping %d" v
        let _ = traverse (Option.map mapper) [Some 1; Some 2]
        SideEffects.are ["mapping 1"; "mapping 2"]


    [<Test>]
    let traversableForNonPrimitive () =
        let nel = nelist { Some 1 }
        let rs1 = traverse id nel
        Assert.IsInstanceOf<option<NonEmptyList<int>>> rs1
        let rs2 = sequence nel
        Assert.IsInstanceOf<option<NonEmptyList<int>>> rs2
        let nem = NonEmptyMap.Create (("a", Some 1), ("b", Some 2), ("c", Some 3))
        let rs3 = traverse id nem
        Assert.IsInstanceOf<option<NonEmptyMap<string, int>>> rs3
        let rs4 = sequence nem
        Assert.IsInstanceOf<option<NonEmptyMap<string, int>>> rs4
        let rs5 = traverse id (TestNonEmptyCollection.Create (Some 42))
        Assert.IsInstanceOf<option<NonEmptySeq<int>>> rs5
        let nes = neseq { Some 1 }
        let rs6 = traverse id nes
        Assert.IsInstanceOf<option<NonEmptySeq<int>>> rs6
        let rs7 = sequence nes
        Assert.IsInstanceOf<option<NonEmptySeq<int>>> rs7

    let toOptions x = if x <> 4 then Some x       else None
    let toChoices x = if x <> 4 then Choice1Of2 x else Choice2Of2 "This is a failure"
    let toLists   x = if x <> 4 then [x; x]       else []
    let toEithers x =
        if x > 4 then failwithf "Shouldn't be mapping for %i" x
        if x = 4 then Left ["This is a failure"] else Right x

    [<Test>]
    let traverseInfiniteApplicatives () =

        // It hangs if we try to share this value between tests
        let expectedEffects =
            [
                // map does this  -> """f(x) <*> Right 0"""
                """f(x) <*> Right 1"""
                """f(x) <*> Right 2"""
                """f(x) <*> Right 3"""
                """f(x) <*> Left ["This is a failure"]"""
            ]

        SideEffects.reset ()

        let a = sequence (Seq.initInfinite toOptions)
        let b = sequence (Seq.initInfinite toOptions)
        let c = sequence (Seq.initInfinite toChoices)
        let d = sequence (Seq.initInfinite toLists)
        let e = sequence (Seq.initInfinite toEithers)

        CollectionAssert.AreEqual (expectedEffects, SideEffects.get ())
        SideEffects.reset ()

        let _a = traverse toOptions (Seq.initInfinite id)
        let _b = traverse toOptions (Seq.initInfinite id)
        let _c = traverse toChoices (Seq.initInfinite id)
        let _d = traverse toLists   (Seq.initInfinite id)
        let _e = traverse toEithers (Seq.initInfinite id)

        CollectionAssert.AreEqual (expectedEffects, SideEffects.get ())
        Assert.AreEqual (None, a)
        Assert.AreEqual (None, b)
        Assert.AreEqual (Choice<seq<int>,string>.Choice2Of2 "This is a failure", c)
        Assert.AreEqual (List.empty<seq<int>>, d)
        Assert.AreEqual (Either<string list,seq<int>>.Left ["This is a failure"], e)

        SideEffects.reset ()

        let a = sequence (NonEmptySeq.initInfinite toOptions)
        let b = sequence (NonEmptySeq.initInfinite toOptions)
        let c = sequence (NonEmptySeq.initInfinite toChoices)
        let d = sequence (NonEmptySeq.initInfinite toLists)
        let e = sequence (NonEmptySeq.initInfinite toEithers)

        CollectionAssert.AreEqual (expectedEffects, SideEffects.get ())
        SideEffects.reset ()

        let _a = traverse toOptions (NonEmptySeq.initInfinite id)
        let _b = traverse toOptions (NonEmptySeq.initInfinite id)
        let _c = traverse toChoices (NonEmptySeq.initInfinite id)
        let _d = traverse toLists   (NonEmptySeq.initInfinite id)
        let _e = traverse toEithers (NonEmptySeq.initInfinite id)

        CollectionAssert.AreEqual (expectedEffects, SideEffects.get ())
        Assert.AreEqual (None, a)
        Assert.AreEqual (None, b)
        Assert.AreEqual (Choice<NonEmptySeq<int>,string>.Choice2Of2 "This is a failure", c)
        Assert.True ((d = []))
        Assert.AreEqual (Either<string list,NonEmptySeq<int>>.Left ["This is a failure"], e)
        

    let toEithersStrict x =
        if x = 4 then Left ["This is a failure"] else Right x

    [<Test>]
    let traverseFiniteApplicatives () =

        // It hangs if we try to share this value between tests
        let expectedEffects =
            [
                // map does this  -> """f(x) <*> Right 0"""
                """f(x) <*> Right 1"""
                """f(x) <*> Right 2"""
                """f(x) <*> Right 3"""
                """f(x) <*> Left ["This is a failure"]"""
            ]

        SideEffects.reset ()

        let a = sequence (Seq.initInfinite toOptions       |> Seq.take 20 |> Seq.toList)
        let b = sequence (Seq.initInfinite toOptions       |> Seq.take 20 |> Seq.toList)
        let c = sequence (Seq.initInfinite toChoices       |> Seq.take 20 |> Seq.toList)
        let d = sequence (Seq.initInfinite toLists         |> Seq.take 20 |> Seq.toList)
        let e = sequence (Seq.initInfinite toEithersStrict |> Seq.take 20 |> Seq.toList)

        CollectionAssert.AreEqual (expectedEffects, SideEffects.get ())
        SideEffects.reset ()

        let f = sequence (Seq.initInfinite toEithersStrict |> Seq.take 20 |> Seq.toArray)

        CollectionAssert.AreEqual (expectedEffects, SideEffects.get ())
        SideEffects.reset ()

        let _a = traverse toOptions       [1..20]
        let _b = traverse toOptions       [1..20]
        let _c = traverse toChoices       [1..20]
        let _d = traverse toLists         [1..20]
        let _e = traverse toEithersStrict [1..20]

        CollectionAssert.AreNotEqual (expectedEffects, SideEffects.get ())
        SideEffects.reset ()

        let _f = traverse toEithersStrict [|1..20|]

        CollectionAssert.AreNotEqual (expectedEffects, SideEffects.get ())
        Assert.AreEqual (None, a)
        Assert.AreEqual (None, b)
        Assert.AreEqual (Choice<list<int>,string>.Choice2Of2 "This is a failure", c)
        Assert.AreEqual (List.empty<list<int>>, d)
        Assert.AreEqual (Either<string list,list<int>>.Left ["This is a failure"], e)
        Assert.AreEqual (Either<string list,array<int>>.Left ["This is a failure"], f)
        ()

    [<Test>]
    let traverseAsyncSequences =
        SideEffects.reset ()

        let doSomething v =
            SideEffects.add (sprintf "doSomething: %A" v)
            sprintf "some: %A" v
            |> async.Return
        
        seq [1..10] 
        |> traverse doSomething
        |> map  (head >> printfn "%A")
        |> Async.RunSynchronously
        CollectionAssert.AreEqual (["doSomething: 1"], SideEffects.get ())

        SideEffects.reset ()
        NonEmptySeq.create 1 [2..10]
        |> traverse doSomething
        |> map  (head >> printfn "%A")
        |> Async.RunSynchronously
        CollectionAssert.AreEqual (["doSomething: 1"], SideEffects.get ())

    [<Test>]
    let traverseInfiniteAsyncSequences =
        let s = Seq.initInfinite async.Return
        let s' = sequence s
        let l = s' |> Async.RunSynchronously |> Seq.take 10 |> Seq.toList
        CollectionAssert.AreEqual ([0;1;2;3;4;5;6;7;8;9], l)

    [<Test>]
    let traverseInfiniteAsyncNonEmptySequences =
        let s = NonEmptySeq.initInfinite async.Return
        let s' = sequence s
        let l = s' |> Async.RunSynchronously |> Seq.take 10 |> Seq.toList
        CollectionAssert.AreEqual ([0;1;2;3;4;5;6;7;8;9], l)

    [<Test>]
    let traverseNonEmptySeqs () =
        #if TEST_TRACE
        Traces.reset()
        #endif

        let r1 = traverse async.Return (NonEmptySeq.unsafeOfSeq (Seq.initInfinite id))
        CollectionAssert.AreEqual ([0; 1], r1 |> map (NonEmptySeq.take 2) |> Async.RunSynchronously)
        Assert.IsInstanceOf<Option<Async<int NonEmptySeq>>> (Some r1)
        #if TEST_TRACE
        CollectionAssert.AreEqual (["Traverse ^a"; "Traverse NonEmptySeq: 'T NonEmptySeq, 'T -> Async<'U>"], Traces.get())
        #endif

        #if TEST_TRACE
        Traces.reset()
        #endif

        let r2 = NonEmptySeq<_>.Traverse (NonEmptySeq.unsafeOfSeq (Seq.initInfinite id), async.Return)
        CollectionAssert.AreEqual ([0; 1], r2 |> map (NonEmptySeq.take 2) |> Async.RunSynchronously)
        Assert.IsInstanceOf<Option<Async<int NonEmptySeq>>> (Some r2)
        #if TEST_TRACE
        CollectionAssert.AreEqual (["Traverse NonEmptySeq: 'T NonEmptySeq, 'T -> Async<'U>"], Traces.get())
        #endif

        #if TEST_TRACE
        Traces.reset()
        #endif

        let r3 = traverse (fun x -> if x <> 10 then Some x else None) (NonEmptySeq.initInfinite id)
        Assert.AreEqual(None, r3)
        Assert.IsInstanceOf<Option<NonEmptySeq<int> option>> (Some r3)
        #if TEST_TRACE
        CollectionAssert.AreEqual (["Traverse ^a"; "Traverse NonEmptySeq: NonEmptySeq, 'T -> Functor<'U>"], Traces.get())
        #endif

        #if TEST_TRACE
        Traces.reset()
        #endif

        let r4 = NonEmptySeq<_>.Traverse (NonEmptySeq.initInfinite id, fun x -> if x <> 10 then Some x else None)
        Assert.AreEqual(None, r4)
        Assert.IsInstanceOf<Option<NonEmptySeq<int> option>> (Some r4)
        #if TEST_TRACE
        CollectionAssert.AreEqual (["Traverse NonEmptySeq: NonEmptySeq, 'T -> Functor<'U>"], Traces.get())
        #endif


    [<Test>]
    let traverseTask () =
        #if TEST_TRACE
        Traces.reset()
        #endif
        let a = traverse Task.FromResult [1;2]
        CollectionAssert.AreEqual ([1;2], a.Result)
        Assert.IsInstanceOf<Option<list<int>>> (Some a.Result)
        #if TEST_TRACE
        CollectionAssert.AreEqual (["Traverse list"], Traces.get())
        #endif
        let b = map Task.FromResult [1;2] |> sequence
        CollectionAssert.AreEqual ([1;2], b.Result)
        Assert.IsInstanceOf<Option<list<int>>> (Some b.Result)
        let c = traverse Task.FromResult [|1;2|]
        CollectionAssert.AreEqual ([|1;2|], c.Result)
        Assert.IsInstanceOf<Option<array<int>>> (Some c.Result)
        let d = map Task.FromResult [|1;2|] |> sequence
        CollectionAssert.AreEqual ([|1;2|], d.Result)
        Assert.IsInstanceOf<Option<array<int>>> (Some d.Result)

    [<Test>]
    let traverseMap () =
        #if TEST_TRACE
        Traces.reset()
        #endif
        let m = Map.ofList [("a", 1); ("b", 2); ("c", 3)]
        let r1 = traverse (fun i -> if i = 2 then None else Some i) m
        let r2 = traverse Some m
        Assert.AreEqual(None, r1)
        CollectionAssert.AreEqual (r2.Value, m)

        let m1 = Map.ofList [(1, [1;1;1]); (2, [2;2;2])]
        let expected = [Map.ofList [(1, 1); (2, 2)]; Map.ofList [(1, 1); (2, 2)]; Map.ofList [(1, 1); (2, 2)];
                        Map.ofList [(1, 1); (2, 2)]; Map.ofList [(1, 1); (2, 2)]; Map.ofList [(1, 1); (2, 2)];
                        Map.ofList [(1, 1); (2, 2)]; Map.ofList [(1, 1); (2, 2)]; Map.ofList [(1, 1); (2, 2)]]
        let actual = sequence m1
        CollectionAssert.AreEqual (expected, actual)
        #if TEST_TRACE
        CollectionAssert.AreEqual (["Traverse Map";"Traverse Map"], Traces.get())
        #endif

    [<Test>]
    let traverseResults () =
        #if TEST_TRACE
        Traces.reset()
        #endif
        let a = sequence (if true then Ok [1] else Error "no")
        let b = traverse id (if true then Ok [1] else Error "no")
        let expected: Result<int, string> list = [Ok 1]
        CollectionAssert.AreEqual (expected, a)
        CollectionAssert.AreEqual (expected, b)
        #if TEST_TRACE
        CollectionAssert.AreEqual (["Traverse Result, 'T -> Functor<'U>"], Traces.get())
        #endif


module Bitraversable =

    type Either<'left,'right> = Left of 'left | Right of 'right with
        static member        Bimap (x, f, g) = match x with Right a -> Right (f a) | Left e -> Left (g e)
        static member inline Bisequence x = match x with Right a -> map Either<'Left,'Right>.Right a | Left e -> map Either<'Left,'Right>.Left e

    let _ErrorBad: Result<int,string> list = bitraverse id id (Error ["Bad"])
    let _FailureBad: Validation<string, int> list = bitraverse id id (Failure ["Bad"])
    let _Some42x = bitraverse (Option.map string) Some (Some 42, 'x')
    let _LeftBad: Either<string, int> list = bitraverse id id (Left ["Bad"])  // works through Bisequence and Bimap

    type Either2<'left,'right> = Left of 'left | Right of 'right with
        static member inline Bitraverse (x, f, g) = match x with | Right a -> Either2<'Error2,'T2>.Right <!> g a | Left e -> Either2<'Error2,'T2>.Left <!> f e

    let _Right42: Either2<string, int> list = bisequence (Right [42])  // works through Bitraverse

    let c: Const<int list, string list> = Const [1]
    let d: Const<_, bool> = Const 2
    let e: Const<_, bool> = Const 3

    let _Const1 = bisequence c    
    let _Const2 = bitraverse List.singleton List.singleton d    
    let _Const3 = bitraverse NonEmptyList.singleton NonEmptyList.singleton e

    ()

module ZipApplicatives =

    [<Test>]
    let transposeOptions () =
        let a1 = nelist { Some 1; Some 2; Some 3 }
        let a2 = transpose a1
        let a3 = transpose a2
        let b1 = [ Some 1; Some 2; Some 3 ]
        let b2 = transpose b1
        let b3 = transpose b2
        let c1 = [| Some 1; Some 2; Some 3 |]
        let c2 = transpose c1
        CollectionAssert.AreEqual (a1, a3)
        CollectionAssert.AreEqual (b1, b3)
        Assert.AreEqual (Some [|1; 2; 3|], c2)

    [<Test>]
    let transposeCollections () =
        let a1 = nelist { [1; 2]; [3; 4; 0]; [5; 6] }
        let a2 = transpose a1
        let a3 = transpose a2
        let a4 = transpose a3
        let b1 = [ [1; 2]; [3; 4; 0]; [5; 6] ]
        let b2 = transpose b1
        let b3 = transpose b2
        let b4 = transpose b3
        let c1 = [| Some 1; Some 2; Some 3 |]
        let c2 = transpose c1
        let d1 = List.empty<int list>
        let d2 = transpose d1
        let d3 = transpose d2
        CollectionAssert.AreEqual (a2, a4)
        CollectionAssert.AreEqual (b2, b4)
        Assert.AreEqual (Some [|1; 2; 3|], c2)
        CollectionAssert.AreEqual (d1, d3)