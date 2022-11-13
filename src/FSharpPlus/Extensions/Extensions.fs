namespace FSharpPlus

/// Module containing F#+ Extension Methods on existing types
module Extensions =

    open System

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
    open FSharp.Core.CompilerServices

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

        #if !FABLE_COMPILER
        /// Combine all asyncs in one, chaining them in sequence order.
        static member Sequence (t:seq<Async<_>>) : Async<seq<_>> = async {
            let! ct = Async.CancellationToken
            return seq {
                use enum = t.GetEnumerator ()
                while enum.MoveNext() do
                    yield Async.RunSynchronously (enum.Current, cancellationToken = ct) }}
        #endif

        /// Combine all asyncs in one, chaining them in sequence order.
        static member Sequence (t: list<Async<'T>>) : Async<list<'T>> =
        #if FABLE_COMPILER || NET45
            let rec loop acc = function
                | []    -> async.Return (List.rev acc)
                | x::xs -> async.Bind (x, fun x -> loop (x::acc) xs)
            loop [] t
        #else
            async {
                let mutable coll = ListCollector<'T> ()
                for e in t do
                    let! v = e
                    coll.Add v
                return coll.Close () }
        #endif

        /// Combine all asyncs in one, chaining them in sequence order.
        static member Sequence (t: array<Async<_>>) : Async<array<_>> = async {
            let siz = Array.length t
            let arr = Array.zeroCreate siz
            for i in 0 .. siz-1 do
                let! v = t.[i]
                arr.[i] <- v
            return arr }

        /// Creates an async Result from a Result where the Ok case is async.
        static member Sequence (t: Result<Async<'T>, 'Error>) : Async<Result<'T,'Error>> =
            match t with
            | Ok a    -> Async.map Ok a
            | Error e -> async.Return (Error e)

        /// Creates an async Choice from a Choice where the Choice1Of2 case is async.
        static member Sequence (t: Choice<Async<'T>, 'Choice2Of2>) : Async<Choice<'T,'Choice2Of2>> =
            match t with
            | Choice1Of2 a -> Async.map Choice1Of2 a
            | Choice2Of2 e -> async.Return (Choice2Of2 e)

        /// Creates an async Result from a Result where both cases are async.
        static member Bisequence (t: Result<Async<'T>, Async<'Error>>) : Async<Result<'T,'Error>> =
            match t with
            | Ok a    -> Async.map Ok a
            | Error e -> Async.map Error e

        /// Creates an async Choice from a Choice where both cases are async.
        static member Bisequence (t: Choice<Async<'T>, Async<'Choice2Of2>>) : Async<Choice<'T,'Choice2Of2>> =
            match t with
            | Choice1Of2 a -> Async.map Choice1Of2 a
            | Choice2Of2 e -> Async.map Choice2Of2 e

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
