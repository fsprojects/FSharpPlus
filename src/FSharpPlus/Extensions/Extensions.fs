namespace FSharpPlus

open System

/// Module containing F#+ Extension Methods on existing types
module Extensions =

    type Collections.Generic.IEnumerable<'T> with
        member this.GetSlice = function
            | None  , None   -> this
            | Some a, None   -> this |> Seq.skip a
            | None  , Some b -> this |> Seq.take b
            | Some a, Some b -> this |> Seq.skip a |> Seq.take (b-a+1)


    type List<'T> with
        
        member this.GetSlice = function
            | None  , None   -> this
            | Some a, None   when a < 0 -> this |> List.skip (this.Length + a)
            | Some a, None              -> this |> List.skip                a 
            | None  , Some b when b < 0 -> this |> List.take (this.Length + b)
            | None  , Some b            -> this |> List.take                b
            | Some a, Some b when a >= 0 && b >= 0 -> this |> List.skip a |> List.take b
            | Some a, Some b -> 
                let l = this.Length
                let f i = if i < 0 then l + i else i
                let a = f a
                this |> List.skip a |> List.take (f b - a + 1)

         

    // http://msdn.microsoft.com/en-us/library/system.threading.tasks.task.whenall.aspx 
    #if !FABLE_COMPILER

    open System.Threading
    open System.Threading.Tasks

    let private (|Canceled|Faulted|Completed|) (t: Task<'a>) =
        if t.IsCanceled then Canceled
        else if t.IsFaulted then Faulted t.Exception
        else Completed t.Result

    type Task<'t> with
        static member WhenAll (tasks: Task<'a>[], ?cancellationToken: CancellationToken) =
            let tcs = TaskCompletionSource<'a[]> ()
            let cancellationToken = defaultArg cancellationToken CancellationToken.None
            cancellationToken.Register (fun () -> tcs.TrySetCanceled () |> ignore) |> ignore
            let results = Array.zeroCreate<'a> tasks.Length
            let pending = ref results.Length
            tasks 
            |> Seq.iteri (fun i t ->
                let continuation = function
                | Canceled    -> tcs.TrySetCanceled () |> ignore
                | Faulted e   -> tcs.TrySetException e |> ignore
                | Completed r -> 
                    results.[i] <- r
                    if Interlocked.Decrement pending = 0 then 
                        tcs.SetResult results
                t.ContinueWith (continuation, cancellationToken, TaskContinuationOptions.ExecuteSynchronously, TaskScheduler.Default) |> ignore)
            tcs.Task
    #endif

    type Async<'t> with

        /// Combine all asyncs in one, chaining them in sequence order.
        static member Sequence (t:seq<Async<_>>) : Async<seq<_>> = async {
            let! ct = Async.CancellationToken
            return seq {
                use enum = t.GetEnumerator ()
                while enum.MoveNext() do
                    yield Async.RunSynchronously (enum.Current, cancellationToken = ct) }}

        /// Combine all asyncs in one, chaining them in sequence order.
        static member Sequence (t: list<Async<_>>) : Async<list<_>> =
            let rec loop acc = function
                | []    -> async.Return (List.rev acc)
                | x::xs -> async.Bind (x, fun x -> loop (x::acc) xs)
            loop [] t

        /// Combine all asyncs in one, chaining them in sequence order.
        static member Sequence (t: array<Async<_>>) : Async<array<_>> = async {
            let siz = Array.length t
            let arr = Array.zeroCreate siz
            for i in 0 .. siz-1 do
                let! v = t.[i]
                arr.[i] <- v
            return arr }


    type Option<'t> with

        /// Returns None if it contains a None element, otherwise a list of all elements
        static member Sequence (t: seq<option<'T>>) =
            let mutable ok = true
            let res = Seq.toArray (seq {
                use e = t.GetEnumerator ()
                while e.MoveNext () && ok do
                    match e.Current with
                    | Some v -> yield v
                    | None   -> ok <- false })
            if ok then Some (Array.toSeq res) else None
