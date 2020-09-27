namespace FSharpPlus.Tests

open System
open NUnit.Framework
open FSharpPlus
open FSharpPlus.Data
open Helpers

module ComputationExpressions = 

    [<Test>]
    let monadFx() =
        SideEffects.reset ()

        // This workflow perform side-effects before and after an async operation in a monad.fx
        let zerowf = monad {
            SideEffects.add "1"
            do! Async.Sleep 10
            SideEffects.add "2" }

        // Check side effects are not yet executed
        areEqual [] (SideEffects.get ())

        // This workflow will always run the previous one
        let combinewf = monad { 
            if true then do! Async.Sleep 10
            return! zerowf }

        // The list should be empty, no workflow was run
        areEqual [] (SideEffects.get ())

        Async.RunSynchronously combinewf

        // Since it's an FX workflow, the last line should have been executed
        areEqual ["1"; "2"] (SideEffects.get ())


    [<Test>]
    let monadPlus() =
        SideEffects.reset ()

        // This is a plus workflow
        // Although we're not explicitely using a strict workflow list hasn't a proper delay mechanism
        let lst: _ list = monad.plus {
            SideEffects.add "3"
            return 5;
            return 6; }

        // Check if side effect was already performed
        areEqual ["3"] (SideEffects.get ())

        // Check 'plus' (<|>) operation was properly performed
        areEqual [5; 6] lst

        SideEffects.reset ()

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
        areEqual [] (SideEffects.get ())

        let seqValue = toList seq3

        // Now they should
        areEqual ["Start"; "execute this"; "Exception! Attempted to divide by zero."] (SideEffects.get ())

        // Check the result
        areEqual [42] seqValue


    [<Test>]
    let delayedMonadTransformers() =

        SideEffects.reset ()

        let threeElements: ReaderT<string, list<_>> = monad.plus {
            let! s = ask
            for i in 1 .. 3 do
                SideEffects.add (sprintf "processing %i" i)
                yield parse s + i }

        areEqual [] (SideEffects.get ())
        
        // Following line would throw an exception (due to the for loop) if ReaderT had no Delay implementation
        let results = ReaderT.run threeElements "100"

        areEqual ["processing 1"; "processing 2"; "processing 3"] (SideEffects.get ())
        areEqual [101; 102; 103] results


    [<Test>]
    let usingInForLoops () =

        SideEffects.reset ()

        // desugared version
        let a1 s _ = StateT (fun s -> Seq.singleton (s, 0))

        let a2 =
            monad.plus.For ([("first1", "second1"); ("first2", "second2")], (fun str ->
                monad.plus.For ([1 + (fst str).Length ; 2 + (snd str).Length], (fun len ->
                    a1 "" (Some len) >>=  fun _ -> result str))))

        let _a3 = StateT.run a2 0|> Seq.map fst

        // sugared
        let b1 s _ = StateT (fun s -> Seq.singleton (s, 0))
        let b2 = monad.plus {
            for str in [("first1", "second1"); ("first2", "second2")] do
            for len in [1 + (fst str).Length ; 2 + (snd str).Length] do
            let!  _  = b1 "" (Some len)
            return str }
    
        let _b3 = StateT.run b2 0 |> Seq.map fst

        // external type, default definition of using
        let c1 s _ = WrappedSeqA (Seq.singleton (s, 0))
        let c2 = monad.plus {
            for str in [("first1", "second1"); ("first2", "second2")] do
            for len in [1 + (fst str).Length ; 2 + (snd str).Length] do
            let!  _  = c1 "" (Some len)
            return str }

        let _c3 = c2 |> Seq.toList

        // external type, custom definition of using
        let d1 s _ = WrappedSeqB (Seq.singleton (s, 0))
        let d2 = monad.plus {
            for str in [("first1", "second1"); ("first2", "second2")] do
            for len in [1 + (fst str).Length ; 2 + (snd str).Length] do
            let!  _  = d1 "" (Some len)
            return str }

        areEqual [] (SideEffects.get ())
        let _d3 = d2 |> Seq.toList
        areEqual ["Using WrappedSeqB's Using"; "Using WrappedSeqB's Using"; "Using WrappedSeqB's Using"] (SideEffects.get ())
        SideEffects.reset ()
        
        // external type, custom definition of TryFinally
        let e1 s _ = WrappedSeqC (Seq.singleton (s, 0))
        let e2 = monad.plus {
            for str in [("first1", "second1"); ("first2", "second2")] do
            for len in [1 + (fst str).Length ; 2 + (snd str).Length] do
            let!  _  = e1 "" (Some len)
            return str }

        areEqual [] (SideEffects.get ())
        let _e3 = e2 |> Seq.toList
        areEqual ["Using WrappedSeqC's TryFinally"; "Using WrappedSeqC's TryFinally"; "Using WrappedSeqC's TryFinally"] (SideEffects.get ())
        SideEffects.reset ()

        // plain seqs
        let f1 s _ = (Seq.singleton (s, 0))
        let f2 = monad.plus {
            for str in [("first1", "second1"); ("first2", "second2")] do
            for len in [1 + (fst str).Length ; 2 + (snd str).Length] do
            let!  _  = f1 "" (Some len)
            return str }  

        let _f3 = f2 |> Seq.toList

        // lazy (monad.fx)
        let g1 s _ = (lazy (s, 0))
        let g2 = monad.fx {
            for str in [("first1", "second1"); ("first2", "second2")] do
            for len in [1 + (fst str).Length ; 2 + (snd str).Length] do
            let!  _  = g1 "" (Some len)
            () }  

        let _g3 = g2.Value

        // async (monad.fx)
        let h1 s _ = (async.Return (s, 0))
        let h2 = monad.fx {
            for str in [("first1", "second1"); ("first2", "second2")] do
            for len in [1 + (fst str).Length ; 2 + (snd str).Length] do
            let!  _  = h1 "" (Some len)
            () }  

        let _h3 = h2 |> Async.RunSynchronously

        // external type, custom definition of using, Strict (Monadic Container)
        let i1 s _ = WrappedListG (List.singleton (s, 0))
        let _i2 = monad.plus.strict {
            for str in [("first1", "second1"); ("first2", "second2")] do
            for len in [1 + (fst str).Length ; 2 + (snd str).Length] do
            let!  _  = i1 "" (Some len)
            return str }

        areEqual ["Using WrappedListG's Using"; "Using WrappedListG's Using"; "Using WrappedListG's Using"] (SideEffects.get ())
        SideEffects.reset ()

        // same example but without explicitely telling that the monad is strict
        let j1 s _ = WrappedListG (List.singleton (s, 0))
        let j2 = monad.plus {
            for str in [("first1", "second1"); ("first2", "second2")] do
            for len in [1 + (fst str).Length ; 2 + (snd str).Length] do
            let!  _  = j1 "" (Some len)
            return str }

        areEqual ["Using WrappedListG's Using"; "Using WrappedListG's Using"; "Using WrappedListG's Using"] (SideEffects.get ())


    
    open System.Threading

    [<Test>]
    let usingInAsyncs() =

        // from https://github.com/Microsoft/visualfsharp/issues/1436        
        let flag = ref 0
        let cts = new System.Threading.CancellationTokenSource()
        let go = monad.fx {
            use disp =
                cts.Cancel()
                { new IDisposable with
                    override __.Dispose() = incr flag }
            while true do
                do! Async.Sleep 50
            }
        try
            Async.RunSynchronously (go, cancellationToken = cts.Token)
        with
            :? System.OperationCanceledException -> ()
        Assert.AreEqual(1, !flag)

    type AsyncOfOptionDisposable () =
        interface IDisposable with
            member __.Dispose() = SideEffects.add "I'm disposed"
        member __.AsyncSomeOption() : Async<int option> = async { 
            SideEffects.add "I'm doing something async"
            return Some 1 }
        member __.IdSomeOption() : Identity<int option> = monad { 
            SideEffects.add "I'm doing something id"
            return Some 1 }

    [<Test>]
    let usingInOptionT () =
        SideEffects.reset ()
        let reproducePrematureDisposal : Async<int option> =
            monad {
                use somethingDisposable = new AsyncOfOptionDisposable ()
                let! (res: int) = OptionT <| somethingDisposable.AsyncSomeOption ()
                SideEffects.add (sprintf "Unpacked async option: %A" res)
                return res
            } |> OptionT.run
        let _ = reproducePrematureDisposal |> Async.RunSynchronously
        areEqual ["I'm doing something async"; "Unpacked async option: 1"; "I'm disposed"] (SideEffects.get ())
   
    [<Test>]
    let testCompileUsingInOptionTStrict () = // wrong results, Async is not strict
        SideEffects.reset ()
        let reproducePrematureDisposal : Async<int option> =
            monad.strict {
                use somethingDisposable = new AsyncOfOptionDisposable ()
                let! (res: int) = OptionT <| somethingDisposable.AsyncSomeOption ()
                SideEffects.add (sprintf "Unpacked async option: %A" res)
                return res
            } |> OptionT.run
        let _ = reproducePrematureDisposal |> Async.RunSynchronously
        areEqual ["I'm disposed"; "I'm doing something async"; "Unpacked async option: 1"] (SideEffects.get ())
        
    [<Test>]
    let UsingInOptionTStrict () = // this is the way to use it with a strict monad
        SideEffects.reset ()
        let reproducePrematureDisposal : Identity<int option> =
            monad.strict {
                use somethingDisposable = new AsyncOfOptionDisposable ()
                let! (res: int) = OptionT <| somethingDisposable.IdSomeOption ()
                SideEffects.add (sprintf "Unpacked id option: %A" res)
                return res
            } |> OptionT.run
        let _ = reproducePrematureDisposal |> Identity.run
        areEqual ["I'm doing something id"; "Unpacked id option: 1"; "I'm disposed"] (SideEffects.get ())


    open System.Collections.Generic

    [<Test>]
    let usingInWhileLoops () =
        let effects = 
            [
                "using"
                "moving"
                "-move-next-"
                "-get-Current-"
                "--> 0"
                "moving"
                "-move-next-"
                "-get-Current-"
                "--> 1"
                "moving"
                "-move-next-"
                "about to finish"
                "-get-Current-"
                "--> 2"
                "moving"
                "-move-next-"
                "-dispose-"
            ]
        let strictEffects =
            [
                "using"
                "-get-Current-"
                "--> 0"
                "moving"
                "-move-next-"
                "moving"
                "-move-next-"
                "moving"
                "-move-next-"
                "about to finish"
                "moving"
                "-move-next-"
                "-dispose-"
            ]

        let toDebugEnum (x: IEnumerator<'t>) =
            {
                new IEnumerator<'t> with 
                    override __.get_Current() = SideEffects.add "-get-Current-"; x.Current
                    override __.get_Current() = SideEffects.add "-get-Current-(boxed)-"; box x.Current
                    override __.MoveNext ()   = SideEffects.add "-move-next-"; x.MoveNext ()
                    override __.Reset ()      = SideEffects.add "-reset-"; x.Reset ()
                    override __.Dispose ()    = SideEffects.add "-dispose-"; x.Dispose ()}

        let testSeq = (seq {yield 0; yield 1; SideEffects.add "about to finish"; yield 2})
        
        // Check lazy monads
        SideEffects.reset ()

        let funcM : unit -> unit = monad {
          use enum = toDebugEnum    ( SideEffects.add "using"; testSeq.GetEnumerator ())
          while (SideEffects.add "moving"; enum.MoveNext ()) do
             SideEffects.add (sprintf "--> %i" enum.Current) }

        areEqual [] (SideEffects.get ())
        funcM ()
        areEqual effects (SideEffects.get ())

        SideEffects.reset ()

        let readerM : Reader<unit,unit> = monad {
          use enum = toDebugEnum (SideEffects.add "using"; testSeq.GetEnumerator ())
          while (SideEffects.add "moving"; enum.MoveNext ()) do
             SideEffects.add (sprintf "--> %i" enum.Current) }

        areEqual [] (SideEffects.get ())
        Reader.run readerM ()
        areEqual effects (SideEffects.get ())

        SideEffects.reset ()

        let stateM : State<unit,unit> = monad {
          use enum = toDebugEnum (SideEffects.add "using"; testSeq.GetEnumerator ())
          while (SideEffects.add "moving"; enum.MoveNext ()) do
             SideEffects.add (sprintf "--> %i" enum.Current) }

        areEqual [] (SideEffects.get ())
        State.run stateM () |> ignore
        areEqual effects (SideEffects.get ())

        SideEffects.reset ()

        let contM: Cont<unit,unit> = monad {
          use enum = toDebugEnum (SideEffects.add "using"; testSeq.GetEnumerator ())
          while (SideEffects.add "moving"; enum.MoveNext ()) do
             SideEffects.add (sprintf "--> %i" enum.Current) }

        areEqual [] (SideEffects.get ())
        Cont.run contM id
        areEqual effects (SideEffects.get ())

        // Monad transformers are delayed if at least one of the layers is lazy.
        SideEffects.reset ()
        
        let readerToptionM : ReaderT<unit,unit option> = monad {
          use enum = toDebugEnum (SideEffects.add "using"; testSeq.GetEnumerator ())
          while (SideEffects.add "moving"; enum.MoveNext ()) do
             SideEffects.add (sprintf "--> %i" enum.Current) }

        areEqual [] (SideEffects.get ())
        ReaderT.run readerToptionM () |> ignore
        areEqual effects (SideEffects.get ())

        SideEffects.reset ()

        let readerTfuncM: ReaderT<unit,unit->unit> = monad {
          use enum = toDebugEnum (SideEffects.add "using"; testSeq.GetEnumerator ())
          while (SideEffects.add "moving"; enum.MoveNext ()) do
             SideEffects.add (sprintf "--> %i" enum.Current) }

        areEqual [] (SideEffects.get ())
        let a = ReaderT.run readerTfuncM ()
        areEqual [] (SideEffects.get ())
        let b = a ()
        areEqual effects (SideEffects.get ())

        SideEffects.reset ()

        let optionTreaderM: OptionT<Reader<unit,unit option>> = monad {
          use enum = toDebugEnum (SideEffects.add "using"; testSeq.GetEnumerator ())
          while (SideEffects.add "moving"; enum.MoveNext ()) do
             SideEffects.add (sprintf "--> %i" enum.Current) }

        areEqual [] (SideEffects.get ())
        let c = OptionT.run optionTreaderM
        areEqual [] (SideEffects.get ())
        let d = Reader.run c ()
        areEqual effects (SideEffects.get ())

        // Writer is strict
        SideEffects.reset ()

        let writerM: Writer<unit,unit> = monad {
          use enum = toDebugEnum (SideEffects.add "using"; testSeq.GetEnumerator ())
          while (SideEffects.add "moving"; enum.MoveNext ()) do
             SideEffects.add (sprintf "--> %i" enum.Current) }

        areEqual strictEffects (SideEffects.get ())
        Writer.run writerM |> ignore
        areEqual strictEffects (SideEffects.get ())

        // Writer combined with a strict monad is also strict
        SideEffects.reset ()

        let optionTwriterM: OptionT<Writer<unit,unit option>> = monad {
          use enum = toDebugEnum (SideEffects.add "using"; testSeq.GetEnumerator ())
          while (SideEffects.add "moving"; enum.MoveNext ()) do
             SideEffects.add (sprintf "--> %i" enum.Current) }

        areEqual strictEffects (SideEffects.get ())
        let e = OptionT.run optionTwriterM
        areEqual strictEffects (SideEffects.get ())
        let f = Writer.run e
        areEqual strictEffects (SideEffects.get ())


    [<Test>]
    let tryWithBlocks () =

        let lazyMonadTest () =
            let x : seq<unit> = monad {
                try
                    failwith "Exception in try-with not handled"
                    ()
                with _ -> () }
            x
        let _ = lazyMonadTest () |> Seq.toList
        
        let strictMonadTest () =
            let x : list<unit> = monad.strict {
                try
                    failwith "Exception in try-with not handled"
                    ()
                with _ -> () }
            x
        let _ = strictMonadTest ()  
    
        let monadTransformer3layersTest1 () =
            let x: StateT<string, ReaderT<int, seq<(unit * string)>>> = monad {
                try
                    failwith "Exception in try-with not handled"
                    ()
                with _ -> () }
            x
        let _ = ((monadTransformer3layersTest1 () |> StateT.run) "" |> ReaderT.run) 0 |> Seq.toList
        
        let monadTransformer3layersTest2 () =
            let x: StateT<string, ReaderT<int, list<(unit * string)>>> = monad {
                try
                    failwith "Exception in try-with not handled"
                    ()
                with _ -> () }
            x
        let _ = ((monadTransformer3layersTest2 () |> StateT.run) "" |> ReaderT.run) 0
        
        let monadTransformer3layersTest3 () =
            let x: WriterT<OptionT<seq<(unit * string) option>>> = monad {
                try
                    failwith "Exception in try-with not handled"
                    ()
                with _ -> () }
            x
        let _ = monadTransformer3layersTest3 () |> WriterT.run |> OptionT.run |> Seq.toList
        
        // Same test but with list instead of seq, which makes the whole monad strict
        // If .strict is not used it fails compilation with a nice error asking us to add it
        let monadTransformer3layersTest4 () =
            let x: WriterT<OptionT<list<(unit * string) option>>> = monad.strict {
                try
                    failwith "Exception in try-with not handled"
                    ()
                with _ -> () }
            x
        let _ = monadTransformer3layersTest4 () |> WriterT.run |> OptionT.run
        

        // ContT doesn't deal with the inner monad, so we don't need to do anything.
        let contTTest () =
            let x: ContT<list<unit>,unit> = monad {
                try
                    failwith "Exception in try-with not handled"
                    ()
                with _ -> () }
            x
        let _ = (contTTest () |> ContT.run) result        

        ()


    [<Test>]
    let tryFinallyBlocks () =

        let lazyMonadTest () =
            SideEffects.reset ()
            let x : seq<unit> = monad {
                use disp = { new IDisposable with override __.Dispose() = SideEffects.add "Disposing" }
                try
                    failwith "Exception in try-finally"
                    ()
                finally
                    SideEffects.add "Finally goes here" }
            x
        let _ = try (lazyMonadTest () |> Seq.toList) with _ -> Unchecked.defaultof<_>
        areEqual ["Finally goes here"; "Disposing"] (SideEffects.get ())
        
        let strictMonadTest () =
            SideEffects.reset ()
            let x : list<unit> = monad.strict {
                use disp = { new IDisposable with override __.Dispose() = SideEffects.add "Disposing" }
                try
                    failwith "Exception in try-finally"
                    ()
                finally
                    SideEffects.add "Finally goes here" }
            x
        let _ = try (strictMonadTest ()) with _ -> Unchecked.defaultof<_>
        areEqual ["Finally goes here"; "Disposing"] (SideEffects.get ())

        let monadTransformer3layersTest1 () =
            SideEffects.reset ()
            let x: StateT<string, ReaderT<int, seq<(unit * string)>>> = monad {
                use disp = { new IDisposable with override __.Dispose() = SideEffects.add "Disposing" }
                try
                    failwith "Exception in try-finally"
                    ()
                finally
                    SideEffects.add "Finally goes here" }
            x
        let _ = try (((monadTransformer3layersTest1 () |> StateT.run) "" |> ReaderT.run) 0 |> Seq.toList) with _ -> Unchecked.defaultof<_>
        areEqual ["Finally goes here"; "Disposing"] (SideEffects.get ())

        let monadTransformer3layersTest2 () =
            SideEffects.reset ()
            let x: StateT<string, ReaderT<int, list<(unit * string)>>> = monad {
                use disp = { new IDisposable with override __.Dispose() = SideEffects.add "Disposing" }
                try
                    failwith "Exception in try-finally"
                    ()
                finally
                    SideEffects.add "Finally goes here" }
            x
        let _ = try (((monadTransformer3layersTest2 () |> StateT.run) "" |> ReaderT.run) 0) with _ -> Unchecked.defaultof<_>
        areEqual ["Finally goes here"; "Disposing"] (SideEffects.get ())

        let monadTransformer3layersTest3 () =
            SideEffects.reset ()
            let x: WriterT<OptionT<seq<(unit * string) option>>> = monad {
                use disp = { new IDisposable with override __.Dispose() = SideEffects.add "Disposing" }
                try
                    failwith "Exception in try-finally"
                    ()
                finally
                    SideEffects.add "Finally goes here" }
            x
        let _ = try (monadTransformer3layersTest3 () |> WriterT.run |> OptionT.run |> Seq.toList) with _ -> Unchecked.defaultof<_>
        areEqual ["Finally goes here"; "Disposing"] (SideEffects.get ())

        // Same test but with list instead of seq, which makes the whole monad strict
        // If .strict is not used it fails compilation with a nice error asking us to add it
        let monadTransformer3layersTest4 () =
            SideEffects.reset ()
            let x: WriterT<OptionT<list<(unit * string) option>>> = monad.strict {
                use disp = { new IDisposable with override __.Dispose() = SideEffects.add "Disposing" }
                try
                    failwith "Exception in try-finally"
                    ()
                finally
                    SideEffects.add "Finally goes here" }
            x
        let _ = try (monadTransformer3layersTest4 () |> WriterT.run |> OptionT.run) with _ -> Unchecked.defaultof<_>
        areEqual ["Finally goes here"; "Disposing"] (SideEffects.get ())

        // ContT doesn't deal with the inner monad, so we don't need to do anything.
        let contTTest () =
            SideEffects.reset ()
            let x: ContT<list<unit>,unit> = monad {
                use disp = { new IDisposable with override __.Dispose() = SideEffects.add "Disposing" }
                try
                    failwith "Exception in try-finally"
                    ()
                finally
                    SideEffects.add "Finally goes here" }
            x
        let _ = try ((contTTest () |> ContT.run) result) with _ -> Unchecked.defaultof<_>
        areEqual ["Finally goes here"; "Disposing"] (SideEffects.get ())


        ()