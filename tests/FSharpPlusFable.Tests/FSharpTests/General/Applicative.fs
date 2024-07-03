module General.Applicative


open Testing
open General.Util
open FSharpPlus
#if !FABLE_COMPILER
open FSharpPlus.Math.Applicative
#endif
open FSharpPlus.Data
#nowarn "686"



type ZipList<'s> = ZipList of 's seq with
    static member Map    (ZipList x, f:'a->'b)               = ZipList (Seq.map f x)
    static member Return (x:'a)                              = ZipList (Seq.initInfinite (konst x))
    static member (<*>) (ZipList (f:seq<'a->'b>), ZipList x) = ZipList (Seq.zip f x |> Seq.map (fun (f, x) -> f x)) : ZipList<'b>
    
type ZipList'<'s> = ZipList' of 's seq with
    static member Return (x:'a)                                = ZipList' (Seq.initInfinite (konst x))
    static member (<*>) (ZipList' (f:seq<'a->'b>), ZipList' x) = ZipList' (Seq.zip f x |> Seq.map (fun (f, x) -> f x)) : ZipList'<'b>

let applicative = testList "Applicative" [ 
    #if !FABLE_COMPILER
    testCase "applicativeMath" (fun () -> 
        let inline (+) (a: 'T) (b: 'T) : 'T = a + b
        let inline ( .+  ) (x: 'Functor't)     (y: 't)             = map ((+)/> y) x : 'Functor't
        let inline (  +. ) (x: 't)             (y: 'Functor't)     = map ((+)   x) y : 'Functor't
        let inline ( .+. ) (x: 'Applicative't) (y: 'Applicative't) = (+) <!> x <*> y : 'Applicative't

        let testVal = [1;2] .+. [10;20] .+. [100;200] .+  2
        Assert.AreEqual ([113; 213; 123; 223; 114; 214; 124; 224], testVal)
        Assert.IsInstanceOf<Option<list<int>>> (Some testVal)

        let testVal2 = NonEmptySeq.create 1 [2] .+. NonEmptySeq.create 10 [20] .+. NonEmptySeq.create 100 [200] .+ 2
        Assert.AreEqual ([113; 213; 123; 223; 114; 214; 124; 224], Seq.toList testVal2)
        Assert.IsInstanceOf<Option<NonEmptySeq<int>>> (Some testVal2)

        let testLTE1 = Some 1 .<=. Some 2
        Assert.AreEqual (Some true, testLTE1)
        Assert.IsInstanceOf<Option<bool>> testLTE1

        let testLTE2 = Some 1 .<= 2
        Assert.AreEqual (Some true, testLTE2)
        Assert.IsInstanceOf<Option<bool>> testLTE2
        
        let testLTE3 = 3 <=. Some 1
        Assert.AreEqual (Some false, testLTE3)
        Assert.IsInstanceOf<Option<bool>> testLTE3

        let testLTE4 = Some 3 .<=. Some 3
        Assert.AreEqual (Some true, testLTE4)
        Assert.IsInstanceOf<Option<bool>> testLTE4

        let testGTE1 = Some 1 .>=. Some 2
        Assert.AreEqual (Some false, testGTE1)
        Assert.IsInstanceOf<Option<bool>> testGTE1

        let testGTE2 = Some 1 .>= 2
        Assert.AreEqual (Some false, testGTE2)
        Assert.IsInstanceOf<Option<bool>> testGTE2
        
        let testGTE3 = 3 >=. Some 1
        Assert.AreEqual (Some true, testGTE3)
        Assert.IsInstanceOf<Option<bool>> testGTE3

        let testGTE4 = Some 3 .>=. Some 3
        Assert.AreEqual (Some true, testGTE4)
        Assert.IsInstanceOf<Option<bool>> testGTE4)
    #endif

    #if !FABLE_COMPILER
    testCase "applicatives" (fun () -> 

        let run (ZipList x) = x
        let run' (ZipList' x) = x

        // Test Applicative (functions)
        let res607 = map (+) ( (*) 100 ) 6 7
        let res606 = ( (+) <*>  (*) 100 ) 6
        let res508 = (map (+) ((+) 3 ) <*> (*) 100) 5

        // Test Applicative (ZipList)
        let res9n5  = map ((+) 1) (ZipList [8;4])
        let _20n30  = result (+) <*> result 10 <*> ZipList [10;20]
        let _18n14  = result (+) <*> ZipList [8;4] <*> result 10
        let res9n5' = map ((+) 1) (ZipList' [8;4])

        Assert.AreEqual (607, res607)
        Assert.AreEqual (606, res606)
        Assert.AreEqual (508, res508)
        Assert.AreEqual (toList (run res9n5), toList (run' res9n5')))
    #endif

    #if !FABLE_COMPILER
    testCase "testLift2" (fun () -> 
        let expectedEffects = ["Using WrappedSeqD's Return"; "Using WrappedSeqD's Apply"; "Using WrappedSeqD's Apply"]
        SideEffects.reset ()
        let _ = (WrappedSeqD [1] , WrappedSeqD [2]) ||> lift2 (+)
        equalSeq expectedEffects (SideEffects.get ())

        let a1 = StateT <| fun x -> (async { return 1, "Here's the state " +  x})
        let b1 = StateT <| fun x -> (async { return 5, "Here's the other state " +  x})
        let r1 = lift2 (+) a1 b1
        Assert.AreEqual ((6, "Here's the other state Here's the state S"), (StateT.run r1 "S" |> Async.RunSynchronously))

        let a2 = WriterT <| (async { return 1, "Here's the state "})
        let b2 = WriterT <| (async { return 5, "Here's the other state "})
        let r2 = lift2 (+) a2 b2
        Assert.AreEqual ((6, "Here's the state Here's the other state "), (WriterT.run r2 |> Async.RunSynchronously))

        let a3 = ReaderT <| fun x -> (async { return "Here's the state " +  x})
        let b3 = ReaderT <| fun x -> (async { return "Here's the other state " +  x})
        let r3 = lift2 (+) a3 b3
        Assert.AreEqual ("Here's the state SHere's the other state S", (ReaderT.run r3 "S" |> Async.RunSynchronously)))
    #endif
    ]