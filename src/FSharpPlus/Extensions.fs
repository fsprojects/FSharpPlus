namespace FSharpPlus

open System
open FSharpPlus.Operators

module Extensions =

    type Collections.Generic.IEnumerable<'T>  with
        member this.GetSlice = function
            | None  , None   -> this
            | Some a, None   -> this |> Seq.skip a
            | None  , Some b -> this |> Seq.take b
            | Some a, Some b -> this |> Seq.skip a |> Seq.take (b-a+1)


    type List<'T> with
        static member singleton x = [x]
        member this.GetSlice = function
            | None  , None   -> this
            | Some a, None   when a < 0 -> this |> skip (this.Length + a)
            | Some a, None              -> this |> skip                a 
            | None  , Some b when b < 0 -> this |> take (this.Length + b)
            | None  , Some b            -> this |> take                b
            | Some a, Some b when a >= 0 && b >= 0 -> this |> skip a |> take b
            | Some a, Some b -> 
                let l = this.Length
                let f i = if i < 0 then l + i else i
                let a = f a
                this |> skip a |> take (f b - a + 1)


    // http://msdn.microsoft.com/en-us/library/system.threading.tasks.task.whenall.aspx 

    open System.Threading
    open System.Threading.Tasks

    let private (|Canceled|Faulted|Completed|) (t: Task<'a>) =
        if t.IsCanceled then Canceled
        else if t.IsFaulted then Faulted(t.Exception)
        else Completed(t.Result)

    type Task<'t> with
        static member WhenAll(tasks : Task<'a>[], ?cancellationToken : CancellationToken) =
            let tcs = TaskCompletionSource<'a[]>()
            let cancellationToken = defaultArg cancellationToken CancellationToken.None
            cancellationToken.Register((fun () -> tcs.TrySetCanceled() |> ignore)) |> ignore
            let results = Array.zeroCreate<'a>(tasks.Length)
            let pending = ref results.Length
            tasks 
            |> Seq.iteri (fun i t ->
                let continuation = function
                | Canceled -> tcs.TrySetCanceled() |> ignore
                | Faulted(e) -> tcs.TrySetException(e) |> ignore
                | Completed(r) -> 
                    results.[i] <- r
                    if Interlocked.Decrement(pending) = 0 then 
                        tcs.SetResult(results)
                t.ContinueWith(continuation, cancellationToken,
                               TaskContinuationOptions.ExecuteSynchronously,
                               TaskScheduler.Default) |> ignore)
            tcs.Task