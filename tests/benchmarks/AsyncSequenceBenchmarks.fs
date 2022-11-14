module AsyncSequences

open BenchmarkDotNet.Attributes
open System.Threading
open System.Threading.Tasks

type AsyncSeqTaskState<'t> =
    | Idle
    | Ok of 't
    | Error of exn
    | Cancelled

let sequence0 (t: seq<Async<_>>) : Async<seq<_>> =
    async {
        let! ct = Async.CancellationToken

        return
            seq {
                use enum = t.GetEnumerator()

                while enum.MoveNext() do
                    yield Async.RunSynchronously(enum.Current, cancellationToken = ct)
            }
    }

let sequence_StartImmediateAsTask (t: seq<Async<_>>) : Async<seq<_>> =
    async {
        let startImmediateAsTask ct a =
            Async.StartImmediateAsTask(a, ct).Result

        let! ct = Async.CancellationToken
        return t |> Seq.map (startImmediateAsTask ct)
    }

let sequence_ManualResetEventSlim (t: seq<Async<_>>) : Async<seq<_>> =
    async {
        let startImmediateAsTask ct (a: Async<'t>) : 't =
            let mutable state =
                AsyncSeqTaskState<'t>.Idle

            let mutex = new ManualResetEventSlim(false)

            let setState newState =
                try
                    state <- newState
                finally
                    mutex.Set()

            Async.StartWithContinuations(
                a,
                (fun k -> AsyncSeqTaskState<'t>.Ok k |> setState),
                (fun e -> AsyncSeqTaskState<'t>.Error e |> setState),
                (fun _ -> setState AsyncSeqTaskState<'t>.Cancelled),
                ct
            )

            mutex.Wait()

            match state with
            | Idle
            | Cancelled -> TaskCanceledException() |> raise
            | Ok res -> res
            | Error e -> raise e

        let! ct = Async.CancellationToken
        return t |> Seq.map (startImmediateAsTask ct)
    }

let SyncSum = async {
    return 1 + 1
}

let AsyncWith1SecSleep = async {
    do! Async.Sleep 5
    return 1 + 1
}

let SyncAsyncSleepOverAsync = async {
    Async.RunSynchronously (Async.Sleep 5)
    return 1 + 1
}

type Benchmarks() =
    [<Params(10, 100, 1000)>]
    member val public times = 0 with get, set
    
    [<Params(2, 3)>]
    member val public threads = 0 with get, set

    [<GlobalSetup>]
    member self.GlobalSetup() =
        ThreadPool.SetMinThreads (self.threads, self.threads) |> ignore
        ThreadPool.SetMaxThreads (self.threads, self.threads) |> ignore

    [<Benchmark(Baseline = true)>]
    member this.Base() =
        seq {
            for _ = 1 to this.times do
                yield SyncSum
                yield AsyncWith1SecSleep
                yield SyncAsyncSleepOverAsync }
        |> sequence0
        |> Async.RunSynchronously
        |> Seq.toArray

    [<Benchmark>]
    member this.StartImmediateAsTask() =        
        seq {
            for _ = 1 to this.times do
                yield SyncSum
                yield AsyncWith1SecSleep
                yield SyncAsyncSleepOverAsync }
        |> sequence_StartImmediateAsTask
        |> Async.RunSynchronously
        |> Seq.toArray

    [<Benchmark>]
    member this.ManualResetEventSlim() =
        seq {
            for _ = 1 to this.times do
                yield SyncSum
                yield AsyncWith1SecSleep
                yield SyncAsyncSleepOverAsync }
        |> sequence_ManualResetEventSlim
        |> Async.RunSynchronously
        |> Seq.toArray
