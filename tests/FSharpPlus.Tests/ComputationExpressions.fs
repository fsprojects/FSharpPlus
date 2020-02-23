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


    [<Test>]
    let usingInForLoops() =

        SideEffects.reset()

        // desugared version
        let a1 s _ = StateT (fun s -> Seq.singleton (s, 0))

        let a2 =
            monad.plus.For ([("first1", "second1"); ("first2", "second2")], (fun str ->
                monad.plus.For ([1 + (fst str).Length ; 2 + (snd str).Length], (fun len ->
                    a1 "" (Some len) >>=  fun _ -> result str))))

        let a3 = StateT.run a2 0|> Seq.map fst

        // sugared
        let b1 s _ = StateT (fun s -> Seq.singleton (s, 0))
        let b2 = monad.plus {
            for str in [("first1", "second1"); ("first2", "second2")] do
            for len in [1 + (fst str).Length ; 2 + (snd str).Length] do
            let!  _  = b1 "" (Some len)
            return str }
    
        let b3 = StateT.run b2 0 |> Seq.map fst

        // external type, default definition of using
        let c1 s _ = WrappedSeqA (Seq.singleton (s, 0))
        let c2 = monad.plus {
            for str in [("first1", "second1"); ("first2", "second2")] do
            for len in [1 + (fst str).Length ; 2 + (snd str).Length] do
            let!  _  = c1 "" (Some len)
            return str }

        let c3 = c2 |> Seq.toList

        // external type, custom definition of using
        let d1 s _ = WrappedSeqB (Seq.singleton (s, 0))
        let d2 = monad.plus {
            for str in [("first1", "second1"); ("first2", "second2")] do
            for len in [1 + (fst str).Length ; 2 + (snd str).Length] do
            let!  _  = d1 "" (Some len)
            return str }

        areEqual (SideEffects.get()) []
        let d3 = d2 |> Seq.toList
        areEqual (SideEffects.get()) ["Using WrappedSeqB's Using"; "Using WrappedSeqB's Using"; "Using WrappedSeqB's Using"]
        SideEffects.reset()
        
        // external type, custom definition of TryFinally
        let e1 s _ = WrappedSeqC (Seq.singleton (s, 0))
        let e2 = monad.plus {
            for str in [("first1", "second1"); ("first2", "second2")] do
            for len in [1 + (fst str).Length ; 2 + (snd str).Length] do
            let!  _  = e1 "" (Some len)
            return str }

        areEqual (SideEffects.get()) []
        let e3 = e2 |> Seq.toList
        areEqual (SideEffects.get()) ["Using WrappedSeqC's TryFinally"; "Using WrappedSeqC's TryFinally"; "Using WrappedSeqC's TryFinally"]
        SideEffects.reset()

        // plain seqs
        let f1 s _ = (Seq.singleton (s, 0))
        let f2 = monad.plus {
            for str in [("first1", "second1"); ("first2", "second2")] do
            for len in [1 + (fst str).Length ; 2 + (snd str).Length] do
            let!  _  = f1 "" (Some len)
            return str }  

        let f3 = f2 |> Seq.toList

        // lazy (monad.fx)
        let g1 s _ = (lazy (s, 0))
        let g2 = monad.fx {
            for str in [("first1", "second1"); ("first2", "second2")] do
            for len in [1 + (fst str).Length ; 2 + (snd str).Length] do
            let!  _  = g1 "" (Some len)
            () }  

        let g3 = g2.Value

        // async (monad.fx)
        let h1 s _ = (async.Return (s, 0))
        let h2 = monad.fx {
            for str in [("first1", "second1"); ("first2", "second2")] do
            for len in [1 + (fst str).Length ; 2 + (snd str).Length] do
            let!  _  = h1 "" (Some len)
            () }  

        let h3 = h2 |> Async.RunSynchronously

        // external type, custom definition of using, Strict (Monadic Container)
        let i1 s _ = WrappedListG (List.singleton (s, 0))
        let i2 = monad.plus.strict {
            for str in [("first1", "second1"); ("first2", "second2")] do
            for len in [1 + (fst str).Length ; 2 + (snd str).Length] do
            let!  _  = i1 "" (Some len)
            return str }

        areEqual (SideEffects.get()) ["Using WrappedListG's Using"; "Using WrappedListG's Using"; "Using WrappedListG's Using"]
        SideEffects.reset()

        // same example but without explicitely telling that the monad is strict
        let j1 s _ = WrappedListG (List.singleton (s, 0))
        let j2 = monad.plus {
            for str in [("first1", "second1"); ("first2", "second2")] do
            for len in [1 + (fst str).Length ; 2 + (snd str).Length] do
            let!  _  = j1 "" (Some len)
            return str }

        areEqual (SideEffects.get()) ["Using WrappedListG's Using"; "Using WrappedListG's Using"; "Using WrappedListG's Using"]


    
    open System.Threading

    [<Test>]
    let usingInAsyncs() =

        // from https://github.com/Microsoft/visualfsharp/issues/1436        
        SideEffects.reset()
        do let source = new CancellationTokenSource ()
           let token = source.Token
           let example = monad.fx {
             SideEffects.add "A: Here we go"
             use! h = Async.OnCancel <| fun () -> SideEffects.add "A: We got cancelled"
             use x =
                source.Cancel ()
                SideEffects.add "A: Creating disposable"
                {new IDisposable with override __.Dispose () = SideEffects.add "A: Disposed properly"}
             do SideEffects.add "A: Never getting here" }
           Async.Start (example, token)

        do let source = new CancellationTokenSource ()
           let token = source.Token
           let example = monad.fx {
             SideEffects.add "B: Here we go"
             use! h = Async.OnCancel <| fun () -> SideEffects.add "B: We got cancelled"
             use x = 
                SideEffects.add "B: Creating disposable"
                {new IDisposable with override __.Dispose () = SideEffects.add "B: Disposed properly"}
             do source.Cancel ()
             do! async { return () }
             do SideEffects.add "B: Never getting here" }
           Async.Start (example, token)


        do Async.Sleep 1000 |> Async.RunSynchronously

        let effA, effB = List.partition (String.startsWith "A") (SideEffects.get())
        areEqual (List.last effA, List.last effB) ("A: Disposed properly", "B: Disposed properly")

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
        areEqual (SideEffects.get()) ["I'm doing something async"; "Unpacked async option: 1"; "I'm disposed"]
   
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
        areEqual (SideEffects.get()) ["I'm disposed"; "I'm doing something async"; "Unpacked async option: 1"]
        
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
        areEqual (SideEffects.get()) ["I'm doing something id"; "Unpacked id option: 1"; "I'm disposed"]


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

        let toDebugEnum (x: IEnumerator<'t>) =
            {
                new IEnumerator<'t> with 
                    override __.get_Current() = SideEffects.add "-get-Current-"; x.Current
                    override __.get_Current() = SideEffects.add "-get-Current-(boxed)-"; box x.Current
                    override __.MoveNext ()   = SideEffects.add "-move-next-"; x.MoveNext ()
                    override __.Reset ()      = SideEffects.add "-reset-"; x.Reset ()
                    override __.Dispose ()    = SideEffects.add "-dispose-"; x.Dispose ()}

        let testSeq = (seq {yield 0; yield 1; SideEffects.add "about to finish"; yield 2})
        
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

        let contM : Cont<unit,unit> = monad {
          use enum = toDebugEnum (SideEffects.add "using"; testSeq.GetEnumerator ())
          while (SideEffects.add "moving"; enum.MoveNext ()) do
             SideEffects.add (sprintf "--> %i" enum.Current) }

        areEqual [] (SideEffects.get ())
        Cont.run contM id
        areEqual effects (SideEffects.get ())


        SideEffects.reset ()

        let readertoptionM : ReaderT<unit,unit option> = monad {
          use enum = toDebugEnum (SideEffects.add "using"; testSeq.GetEnumerator ())
          while (SideEffects.add "moving"; enum.MoveNext ()) do
             SideEffects.add (sprintf "--> %i" enum.Current) }

        areEqual [] (SideEffects.get ())
        ReaderT.run readertoptionM () |> ignore
        areEqual effects (SideEffects.get ())

        SideEffects.reset ()

        let readertfuncM : ReaderT<unit,unit->unit> = monad {
          use enum = toDebugEnum (SideEffects.add "using"; testSeq.GetEnumerator ())
          while (SideEffects.add "moving"; enum.MoveNext ()) do
             SideEffects.add (sprintf "--> %i" enum.Current) }

        areEqual [] (SideEffects.get ())
        let a = ReaderT.run readertfuncM ()
        areEqual [] (SideEffects.get ())
        let b = a ()
        areEqual effects (SideEffects.get ())