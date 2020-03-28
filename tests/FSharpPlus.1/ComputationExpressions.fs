module FSharpPlus.One.ComputationExpressions
open FSharpPlus.One.Usage
open System
open FSharpPlus
open FSharpPlus.Data


let monadFx() =
    let sideEffects=SideEffects()
    // This workflow perform side-effects before and after an async operation in a monad.fx
    let zerowf = monad {
        sideEffects.Add "1"
        do! Async.Sleep 10
        sideEffects.Add "2" }

    // This workflow will always run the previous one
    let combinewf = monad { 
        if true then do! Async.Sleep 10
        return! zerowf }

    // The list should be empty, no workflow was run
    Async.RunSynchronously combinewf

    // Since it's an FX workflow, the last line should have been executed



let monadPlus() =
    let sideEffects=SideEffects()

    // This is a plus workflow
    // Although we're not explicitely using a strict workflow list hasn't a proper delay mechanism
    let lst: _ list = monad.plus {
        sideEffects.Add "3"
        return 5;
        return 6; }

    sideEffects.Reset ()

    // Now let's a try with seq, which has a delay mechanism
    let seq3: seq<_> = monad.plus { 
        sideEffects.Add "Start"
        try
            try
                10 / 0 |> ignore
            finally
                sideEffects.Add "execute this"
        with
        | e -> 
            sideEffects.Add (sprintf "Exception! %s" e.Message)
            return 42 }


    let seqValue = toList seq3
    ()

let delayedMonadTransformers() =
    let sideEffects=SideEffects()

    let threeElements: ReaderT<string, list<_>> = monad.plus {
        let! s = ask
        for i in 1 .. 3 do
            sideEffects.Add (sprintf "processing %i" i)
            yield parse s + i }

    // Following line would throw an exception (due to the for loop) if ReaderT had no Delay implementation
    let results = ReaderT.run threeElements "100"
    ()

let usingInForLoops () =

    let sideEffects=SideEffects()

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
    let c1 s _ = Seq.singleton (s, 0)
    let c2 = monad.plus {
        for str in [("first1", "second1"); ("first2", "second2")] do
        for len in [1 + (fst str).Length ; 2 + (snd str).Length] do
        let!  _  = c1 "" (Some len)
        return str }

    let _c3 = c2 |> Seq.toList

    // external type, custom definition of using
    let d1 s _ = Seq.singleton (s, 0)
    let d2 = monad.plus {
        for str in [("first1", "second1"); ("first2", "second2")] do
        for len in [1 + (fst str).Length ; 2 + (snd str).Length] do
        let!  _  = d1 "" (Some len)
        return str }

    let _d3 = d2 |> Seq.toList
    sideEffects.Reset ()
    
    // external type, custom definition of TryFinally
    let e1 s _ = Seq.singleton (s, 0)
    let e2 = monad.plus {
        for str in [("first1", "second1"); ("first2", "second2")] do
        for len in [1 + (fst str).Length ; 2 + (snd str).Length] do
        let!  _  = e1 "" (Some len)
        return str }

    let _e3 = e2 |> Seq.toList
    sideEffects.Reset ()

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
    let i1 s _ = List.singleton (s, 0)
    let _i2 = monad.plus.strict {
        for str in [("first1", "second1"); ("first2", "second2")] do
        for len in [1 + (fst str).Length ; 2 + (snd str).Length] do
        let!  _  = i1 "" (Some len)
        return str }

    sideEffects.Reset ()

    // same example but without explicitely telling that the monad is strict
    let j1 s _ = List.singleton (s, 0)
    let j2 = monad.plus {
        for str in [("first1", "second1"); ("first2", "second2")] do
        for len in [1 + (fst str).Length ; 2 + (snd str).Length] do
        let!  _  = j1 "" (Some len)
        return str }

    ()


open System.Threading

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

type AsyncOfOptionDisposable (sideEffects:SideEffects) =
    interface IDisposable with
        member __.Dispose() = sideEffects.Add "I'm disposed"
    member __.AsyncSomeOption() : Async<int option> = async { 
        sideEffects.Add "I'm doing something async"
        return Some 1 }
    member __.IdSomeOption() : Identity<int option> = monad { 
        sideEffects.Add "I'm doing something id"
        return Some 1 }

let usingInOptionT () =
    let sideEffects= SideEffects()
    let reproducePrematureDisposal : Async<int option> =
        monad {
            use somethingDisposable = new AsyncOfOptionDisposable (sideEffects)
            let! (res: int) = OptionT <| somethingDisposable.AsyncSomeOption ()
            sideEffects.Add (sprintf "Unpacked async option: %A" res)
            return res
        } |> OptionT.run
    let _ = reproducePrematureDisposal |> Async.RunSynchronously
    ()

open System.Collections.Generic

let usingInWhileLoops () =
    let sideEffects= SideEffects()
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
                override __.get_Current() = sideEffects.Add "-get-Current-"; x.Current
                override __.get_Current() = sideEffects.Add "-get-Current-(boxed)-"; box x.Current
                override __.MoveNext ()   = sideEffects.Add "-move-next-"; x.MoveNext ()
                override __.Reset ()      = sideEffects.Add "-reset-"; x.Reset ()
                override __.Dispose ()    = sideEffects.Add "-dispose-"; x.Dispose ()}

    let testSeq = (seq {yield 0; yield 1; sideEffects.Add "about to finish"; yield 2})
    
    // Check lazy monads

    let funcM : unit -> unit = monad {
      use enum = toDebugEnum    ( sideEffects.Add "using"; testSeq.GetEnumerator ())
      while (sideEffects.Add "moving"; enum.MoveNext ()) do
         sideEffects.Add (sprintf "--> %i" enum.Current) }

    funcM ()

    sideEffects.Reset ()

    let readerM : Reader<unit,unit> = monad {
      use enum = toDebugEnum (sideEffects.Add "using"; testSeq.GetEnumerator ())
      while (sideEffects.Add "moving"; enum.MoveNext ()) do
         sideEffects.Add (sprintf "--> %i" enum.Current) }

    Reader.run readerM ()

    sideEffects.Reset ()

    let stateM : State<unit,unit> = monad {
      use enum = toDebugEnum (sideEffects.Add "using"; testSeq.GetEnumerator ())
      while (sideEffects.Add "moving"; enum.MoveNext ()) do
         sideEffects.Add (sprintf "--> %i" enum.Current) }

    State.run stateM () |> ignore

    sideEffects.Reset ()

    let contM: Cont<unit,unit> = monad {
      use enum = toDebugEnum (sideEffects.Add "using"; testSeq.GetEnumerator ())
      while (sideEffects.Add "moving"; enum.MoveNext ()) do
         sideEffects.Add (sprintf "--> %i" enum.Current) }

    Cont.run contM id

    // Monad transformers are delayed if at least one of the layers is lazy.
    sideEffects.Reset ()
    
    let readerToptionM : ReaderT<unit,unit option> = monad {
      use enum = toDebugEnum (sideEffects.Add "using"; testSeq.GetEnumerator ())
      while (sideEffects.Add "moving"; enum.MoveNext ()) do
         sideEffects.Add (sprintf "--> %i" enum.Current) }

    ReaderT.run readerToptionM () |> ignore

    sideEffects.Reset ()

    let readerTfuncM: ReaderT<unit,unit->unit> = monad {
      use enum = toDebugEnum (sideEffects.Add "using"; testSeq.GetEnumerator ())
      while (sideEffects.Add "moving"; enum.MoveNext ()) do
         sideEffects.Add (sprintf "--> %i" enum.Current) }

    let a = ReaderT.run readerTfuncM ()
    let b = a ()

    sideEffects.Reset ()

    let optionTreaderM: OptionT<Reader<unit,unit option>> = monad {
      use enum = toDebugEnum (sideEffects.Add "using"; testSeq.GetEnumerator ())
      while (sideEffects.Add "moving"; enum.MoveNext ()) do
         sideEffects.Add (sprintf "--> %i" enum.Current) }

    let c = OptionT.run optionTreaderM
    let d = Reader.run c ()

    // Writer is strict
    sideEffects.Reset ()

    let writerM: Writer<unit,unit> = monad {
      use enum = toDebugEnum (sideEffects.Add "using"; testSeq.GetEnumerator ())
      while (sideEffects.Add "moving"; enum.MoveNext ()) do
         sideEffects.Add (sprintf "--> %i" enum.Current) }

    Writer.run writerM |> ignore

    // Writer combined with a strict monad is also strict
    sideEffects.Reset ()

    let optionTwriterM: OptionT<Writer<unit,unit option>> = monad {
      use enum = toDebugEnum (sideEffects.Add "using"; testSeq.GetEnumerator ())
      while (sideEffects.Add "moving"; enum.MoveNext ()) do
         sideEffects.Add (sprintf "--> %i" enum.Current) }

    let e = OptionT.run optionTwriterM
    let f = Writer.run e
    sideEffects.Get ()