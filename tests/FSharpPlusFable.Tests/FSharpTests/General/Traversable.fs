module General.Traversable
open Testing
open General.Util
open FSharpPlus
open FSharpPlus.Data
#nowarn "686"
open System.Threading.Tasks


#if (!FABLE_COMPILER || FABLE_COMPILER_3) && !FABLE_COMPILER_4
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
#endif

let toOptions x = if x <> 4 then Some x       else None
let toChoices x = if x <> 4 then Choice1Of2 x else Choice2Of2 "This is a failure"
let toLists   x = if x <> 4 then [x; x]       else []
#if (!FABLE_COMPILER || FABLE_COMPILER_3) && !FABLE_COMPILER_4
let toEithers x = if x <> 4 then Right x else Left ["This is a failure"]
#endif

let traversable = testList "Traversable" [
    
    // Exception: TypeError: o[Symbol.iterator] is not a function
    #if !FABLE_COMPILER
    testCase "sequence_Default_Primitive" (fun () -> 
        let testVal = sequence [|Some 1; Some 2|]
        Assert.AreEqual (Some [|1;2|], testVal)
        Assert.IsInstanceOf<Option<array<int>>> testVal)
    #endif

    // .. Control/Functor.fs(..): (..) error FABLE: Cannot resolve trait call Map - Inline call from ./Traversable.fs [Wrong line number??]
    #if !FABLE_COMPILER
    testCase "traverseDerivedFromSequence" (fun () -> 
        let testVal = traverse (fun x -> [int16 x..int16 (x+2)]) (WrappedListH [1; 4])
        Assert.AreEqual (
            [
                WrappedListH [1s; 4s]; WrappedListH [1s; 5s]; WrappedListH [1s; 6s];
                WrappedListH [2s; 4s]; WrappedListH [2s; 5s]; WrappedListH [2s; 6s];
                WrappedListH [3s; 4s]; WrappedListH [3s; 5s]; WrappedListH [3s; 6s]
            ] , testVal)
        Assert.IsInstanceOf<list<WrappedListH<int16>>> testVal)
    #endif

    #if !FABLE_COMPILER
    testCase "sequence_Specialization" (fun () ->
        let inline seqSeq (x:_ seq ) = sequence x
        #if !DEBUG
        let inline seqArr (x:_ []  ) = sequence x
        let inline seqLst (x:_ list) = sequence x
        #endif

        let a : list<_> = seqSeq (seq [[1];[3]])
        equalSeq [seq [1; 3]] a
        Assert.IsInstanceOf<list<seq<int>>> a
        #if !DEBUG
        let b = seqArr ( [|[1];[3]|])
        equalSeq [[|1; 3|]] b
        Assert.IsInstanceOf<list<array<int>>> b
        let c = seqLst ( [ [1];[3] ])
        equalSeq [[1; 3]] c
        Assert.IsInstanceOf<list<list<int>>> c
        #endif
        ())
    #endif

    // Control/Applicative.fs(..): (..) error FABLE: Cannot resolve trait call IsLeftZero - Inline call from ./Traversable.fs(..) < ..
    #if !FABLE_COMPILER
    testCase "traverse_Specialization" (fun () ->
        let _ = Seq.traverse id [WrappedSeqD [1]; WrappedSeqD [2]]
        let _ = Seq.sequence    [WrappedSeqD [1]; WrappedSeqD [2]]
        let _ = Seq.traverse id [ZipList [1]; ZipList []; ZipList (seq {failwith "sholdn't get here"})] |> toList
        let _ = Seq.sequence    [ZipList [1]; ZipList []; ZipList (seq {failwith "sholdn't get here"})] |> toList
        ())
    #endif

    #if (!FABLE_COMPILER || FABLE_COMPILER_3) && !FABLE_COMPILER_4
    testList "traverse_Order" [
        testCase "nelist" (fun () ->
            SideEffects.reset()
            let mapper v = SideEffects.add <| sprintf "mapping %d" v
            let _ = traverse (Option.map mapper) [Some 1; Some 2]
            SideEffects.are ["mapping 1"; "mapping 2"]
        )]

    testList "traversableForNonPrimitive" [
        testCase "nelist" (fun () ->
            let nel = nelist { Some 1 }
            let rs1 = traverse id nel
            #if !FABLE_COMPILER
            Assert.IsInstanceOf<option<NonEmptyList<int>>> rs1
            #endif
            let rs2 = sequence nel
            #if !FABLE_COMPILER
            Assert.IsInstanceOf<option<NonEmptyList<int>>> rs2
            #endif
            ())
        #if !FABLE_COMPILER
        testCase "nemap" (fun () ->
            let nem = NonEmptyMap.Create (("a", Some 1), ("b", Some 2), ("c", Some 3))
            let rs3 = traverse id nem
            Assert.IsInstanceOf<option<NonEmptyMap<string, int>>> rs3
            let rs4 = sequence nem
            Assert.IsInstanceOf<option<NonEmptyMap<string, int>>> rs4)
        #endif
        #if !FABLE_COMPILER
        testCase "necol" (fun () ->
            let rs5 = traverse id (TestNonEmptyCollection.Create (Some 42))
            Assert.IsInstanceOf<option<NonEmptySeq<int>>> rs5
            ())
        testCase "neseq" (fun () ->
            let nes = neseq { Some 1 }
            let rs6 = traverse id nes
            Assert.IsInstanceOf<option<NonEmptySeq<int>>> rs6
            let rs7 = sequence nes
            Assert.IsInstanceOf<option<NonEmptySeq<int>>> rs7
            ())
        #endif
    ]
    #endif

    #if !FABLE_COMPILER
    testCase "traverseInfiniteApplicatives" (fun () ->

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

        SideEffects.are expectedEffects
        SideEffects.reset ()

        let _a = traverse toOptions (Seq.initInfinite id)
        let _b = traverse toOptions (Seq.initInfinite id)
        let _c = traverse toChoices (Seq.initInfinite id)
        let _d = traverse toLists   (Seq.initInfinite id)
        let _e = traverse toEithers (Seq.initInfinite id)

        SideEffects.are expectedEffects
        Assert.AreEqual (None, a)
        Assert.AreEqual (None, b)
        Assert.AreEqual (Choice<seq<int>,string>.Choice2Of2 "This is a failure", c)
        Assert.IsEmpty d
        Assert.AreEqual (Either<string list,seq<int>>.Left ["This is a failure"], e)

        SideEffects.reset ()

        let a = sequence (NonEmptySeq.initInfinite toOptions)
        let b = sequence (NonEmptySeq.initInfinite toOptions)
        let c = sequence (NonEmptySeq.initInfinite toChoices)
        let d = sequence (NonEmptySeq.initInfinite toLists)
        let e = sequence (NonEmptySeq.initInfinite toEithers)

        SideEffects.are expectedEffects
        SideEffects.reset ()

        let _a = traverse toOptions (NonEmptySeq.initInfinite id)
        let _b = traverse toOptions (NonEmptySeq.initInfinite id)
        let _c = traverse toChoices (NonEmptySeq.initInfinite id)
        let _d = traverse toLists   (NonEmptySeq.initInfinite id)
        let _e = traverse toEithers (NonEmptySeq.initInfinite id)

        SideEffects.are expectedEffects
        Assert.AreEqual (None, a)
        Assert.AreEqual (None, b)
        Assert.AreEqual (Choice<NonEmptySeq<int>,string>.Choice2Of2 "This is a failure", c)
        Assert.IsEmpty d
        Assert.AreEqual (Either<string list,NonEmptySeq<int>>.Left ["This is a failure"], e))
    #endif

    #if (!FABLE_COMPILER || FABLE_COMPILER_3) && !FABLE_COMPILER_4
    testList "traverseFiniteApplicatives" [ // TODO -> implement short-circuit without breaking anything else

        #if !FABLE_COMPILER
        testCase "a" (fun () ->
            SideEffects.reset ()

            let a = sequence (Seq.initInfinite toOptions |> Seq.take 20 |> Seq.toList)
            SideEffects.are []
            SideEffects.reset ()
            let _a = traverse toOptions [1..20]
            SideEffects.are []
            SideEffects.reset ()
            Assert.AreEqual (None, a)
            ())

        testCase "b" (fun () ->
            SideEffects.reset ()
            let b = sequence (Seq.initInfinite toOptions |> Seq.take 20 |> Seq.toList)
            SideEffects.are []
            SideEffects.reset ()
            let _b = traverse toOptions [1..20]
            SideEffects.are []
            SideEffects.reset ()
            Assert.AreEqual (None, b)
            ())

        testCase "c" (fun () ->
            SideEffects.reset ()
            let c = sequence (Seq.initInfinite toChoices |> Seq.take 20 |> Seq.toList)
            SideEffects.are []
            SideEffects.reset ()
            let _c = traverse toChoices [1..20]
            SideEffects.are []
            SideEffects.reset ()
            Assert.AreEqual (Choice<list<int>,string>.Choice2Of2 "This is a failure", c)
            ())

        testCase "d" (fun () ->
            SideEffects.reset ()
            let d = sequence (Seq.initInfinite toLists   |> Seq.take 20 |> Seq.toList)
            SideEffects.are []
            SideEffects.reset ()
            let _d = traverse toLists   [1..20]
            SideEffects.are []
            SideEffects.reset ()
            Assert.IsEmpty d
            ())

        testCase "e" (fun () ->
            let expectedEffects =
                [
                    // map does this  -> """f(x) <*> Right 0"""
                    """f(x) <*> Right 1"""
                    """f(x) <*> Right 2"""
                    """f(x) <*> Right 3"""
                    """f(x) <*> Left ["This is a failure"]"""
                ]
            SideEffects.reset ()
            let e = sequence (Seq.initInfinite toEithers |> Seq.take 20 |> Seq.toList)
            SideEffects.are expectedEffects
            SideEffects.reset ()
            let _e = traverse toEithers [1..20]
            //SideEffects.are expectedEffects
            SideEffects.reset ()
            Assert.AreEqual (Either<string list,list<int>>.Left ["This is a failure"], e)
            ())

        testCase "f" (fun () ->
            let expectedEffects =
                [
                    // map does this  -> """f(x) <*> Right 0"""
                    """f(x) <*> Right 1"""
                    """f(x) <*> Right 2"""
                    """f(x) <*> Right 3"""
                    """f(x) <*> Left ["This is a failure"]"""
                ]
            SideEffects.reset ()
            let f = sequence (Seq.initInfinite toEithers |> Seq.take 20 |> Seq.toArray)
            SideEffects.are expectedEffects
            SideEffects.reset ()
            let _f = traverse toEithers [|1..20|]
            //SideEffects.are expectedEffects
            SideEffects.reset ()
            Assert.AreEqual (Either<string list,array<int>>.Left ["This is a failure"], f)
            ())
        #endif
    ]
    #endif

    #if !FABLE_COMPILER
    testCase "traverseAsyncSequences" (fun () ->
        SideEffects.reset ()

        let doSomething v =
            SideEffects.add (sprintf "doSomething: %A" v)
            sprintf "some: %A" v
            |> async.Return
        
        seq [1..10] 
        |> traverse doSomething
        |> map  (head >> printfn "%A")
        |> Async.RunSynchronously
        equalSeq ["doSomething: 1"] (SideEffects.get ())

        SideEffects.reset ()
        NonEmptySeq.create 1 [2..10]
        |> traverse doSomething
        |> map  (head >> printfn "%A")
        |> Async.RunSynchronously
        equalSeq ["doSomething: 1"] (SideEffects.get ()))
    #endif

    #if !FABLE_COMPILER
    testCase "traverseInfiniteAsyncSequences" (fun () ->
        let s = Seq.initInfinite async.Return
        let s' = sequence s
        let l = s' |> Async.RunSynchronously |> Seq.take 10 |> Seq.toList
        equalSeq [0;1;2;3;4;5;6;7;8;9] l)
    #endif

    #if !FABLE_COMPILER && !DEBUG
    testCase "traverseTask" (fun () ->
        let a = traverse Task.FromResult [1;2]
        equalSeq [1;2] a.Result
        Assert.IsInstanceOf<Option<list<int>>> (Some a.Result)
        let b = map Task.FromResult [1;2] |> sequence
        equalSeq [1;2] b.Result
        Assert.IsInstanceOf<Option<list<int>>> (Some b.Result)
        let c = traverse Task.FromResult [|1;2|]
        equalSeq [|1;2|] c.Result
        Assert.IsInstanceOf<Option<array<int>>> (Some c.Result)
        let d = map Task.FromResult [|1;2|] |> sequence
        equalSeq [|1;2|] d.Result
        Assert.IsInstanceOf<Option<array<int>>> (Some d.Result))
    #endif

    #if !FABLE_COMPILER
    testCase "traverseMap" (fun () ->
        let m = Map.ofList [("a", 1); ("b", 2); ("c", 3)]
        let r1 = traverse (fun i -> if i = 2 then None else Some i) m
        let r2 = traverse Some m
        Assert.AreEqual(None, r1)
        equalSeq r2.Value m

        let m1 = Map.ofList [(1, [1;1;1]); (2, [2;2;2])]
        let r1 = m1 |> traversei (fun _ _ -> None)
        let r2 = m1 |> traversei (fun i v -> if List.forall ((=) i) v then Some (i :: v) else None)
        Assert.AreEqual(None, r1)
        equalSeq (Map.ofList [(1, [1;1;1;1]); (2, [2;2;2;2])]) r2.Value

        let expected = [Map.ofList [(1, 1); (2, 2)]; Map.ofList [(1, 1); (2, 2)]; Map.ofList [(1, 1); (2, 2)];
                        Map.ofList [(1, 1); (2, 2)]; Map.ofList [(1, 1); (2, 2)]; Map.ofList [(1, 1); (2, 2)];
                        Map.ofList [(1, 1); (2, 2)]; Map.ofList [(1, 1); (2, 2)]; Map.ofList [(1, 1); (2, 2)]]
        let actual = sequence m1
        equalSeq expected actual)
    #endif

    #if !FABLE_COMPILER
    testCase "traverseResults" (fun () ->
        let a = sequence (if true then Ok [1] else Error "no")
        let b = traverse id (if true then Ok [1] else Error "no")
        let expected: Result<int, string> list = [Ok 1]
        equalSeq expected a
        equalSeq expected b)
    #endif

    ]

module Bitraversable =
    #if !FABLE_COMPILER

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
    #endif
    ()
