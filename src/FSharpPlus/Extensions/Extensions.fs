namespace FSharpPlus

/// Module containing F#+ Extension Methods on existing types
module Extensions =

    open System
    #if NET9_0_OR_GREATER
    open System.Runtime.CompilerServices
    [<Extension>]
    type IEnumerableExtensions =
        [<Extension>]
        static member GetSlice (this) = function
    #else
    type Collections.Generic.IEnumerable<'T> with
        member this.GetSlice = function
    #endif
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


        /// Creates a task Result from a Result where the Ok case is a task.
        static member Sequential (t: Result<Task<'T>, 'Error>) : Task<Result<'T, 'Error>> = Result.either (Task.map Ok) (Task.result << Error) t

        /// Creates a task Result from a Result where the Ok case is a task.
        static member Sequential (t: Choice<Task<'T>, 'Error>) : Task<Choice<'T, 'Error>> = Choice.either (Task.map Choice1Of2) (Task.result << Choice2Of2) t

    #endif

    #if !FABLE_COMPILER

    type ValueTask<'t> with

        /// Creates a task Result from a Result where the Ok case is a task.
        static member Sequential (t: Result<ValueTask<'T>, 'Error>) : ValueTask<Result<'T, 'Error>> = Result.either (ValueTask.map Ok) (ValueTask.result << Error) t

        /// Creates a task Result from a Result where the Ok case is a task.
        static member Sequential (t: Choice<ValueTask<'T>, 'Error>) : ValueTask<Choice<'T, 'Error>> = Choice.either (ValueTask.map Choice1Of2) (ValueTask.result << Choice2Of2) t

    #endif

    type Async<'t> with

        static member internal Map f x = async.Bind (x, async.Return << f)

        #if !FABLE_COMPILER

        /// <summary>Runs an asynchronous computation, starting immediately on the current operating system
        /// thread, but also returns the execution as <see cref="T:System.Threading.Tasks.Task`1"/>
        /// This behaves exactly like Async.StartImmediateAsTask but without unexpected exceptions-wrapping.
        /// </summary>
        ///
        /// <remarks>If no cancellation token is provided then the default cancellation token is used.
        /// You may prefer using this method if you want to achive a similar behviour to async await in C# as 
        /// async computation starts on the current thread with an ability to return a result.
        /// </remarks>
        ///
        /// <param name="computation">The asynchronous computation to execute.</param>
        /// <param name="cancellationToken">The <c>CancellationToken</c> to associate with the computation.
        /// The default is used if this parameter is not provided.</param>
        ///
        /// <returns>A <see cref="T:System.Threading.Tasks.Task"/> that will be completed
        /// in the corresponding state once the computation terminates (produces the result, throws exception or gets canceled)</returns>
        ///
        /// <category index="0">FSharp.Core Extensions</category>
        ///
        /// <example id="as-task-1">
        /// <code lang="fsharp">
        /// printfn "A"
        ///
        /// let t =
        ///     async {
        ///         printfn "B"
        ///         do! Async.Sleep(1000)
        ///         printfn "C"
        ///     } |> Async.AsTask
        ///
        /// printfn "D"
        /// t.Wait()
        /// printfn "E"
        /// </code>
        /// Prints "A", "B", "D" immediately, then "C", "E" in 1 second.
        /// </example>
        static member AsTask (computation: Async<'T>, ?cancellationToken) : Task<'T> =
            let cancellationToken = defaultArg cancellationToken (new CancellationToken ())
            let ts = TaskCompletionSource<'T> ()
            Async.StartWithContinuations (
                computation,
                ts.SetResult,
                (function
                    | :? AggregateException as agg -> ts.SetException agg.InnerExceptions
                    | exn -> ts.SetException exn),
                (fun _ -> ts.SetCanceled ()),
                cancellationToken)
            ts.Task

        // See https://github.com/fsharp/fslang-suggestions/issues/840

        /// <summary>Return an asynchronous computation that will wait for the given task to complete and return
        /// its result.</summary>
        ///
        /// <param name="task">The task to await.</param>
        ///
        /// <remarks>Prefer this over <c>Async.AwaitTask</c>.
        ///
        /// If an exception occurs in the asynchronous computation then an exception is re-raised by this function.
        ///
        /// If the task is cancelled then <see cref="F:System.Threading.Tasks.TaskCanceledException"/> is raised. Note
        /// that the task may be governed by a different cancellation token to the overall async computation where the
        /// Await occurs. In practice you should normally start the task with the cancellation token returned by
        /// <c>let! ct = Async.CancellationToken</c>, and catch any <see cref="F:System.Threading.Tasks.TaskCanceledException"/>
        /// at the point where the overall async is started.
        /// </remarks>
        static member Await (task: Task<'T>) : Async<'T> =
            Async.FromContinuations (fun (sc, ec, cc) ->
                task.ContinueWith (fun (task: Task<'T>) ->
                    if task.IsFaulted then
                        let e = task.Exception
                        if e.InnerExceptions.Count = 1 then ec e.InnerExceptions[0]
                        else ec e
                    elif task.IsCanceled then cc (TaskCanceledException ())
                    else sc task.Result)
                |> ignore)
        
        
        /// <summary>Return an asynchronous computation that will wait for the given task to complete and return
        /// its result.</summary>
        ///
        /// <param name="task">The task to await.</param>
        ///
        /// <remarks>Prefer this over <c>Async.AwaitTask</c>.
        ///
        /// If an exception occurs in the asynchronous computation then an exception is re-raised by this function.
        ///
        /// If the task is cancelled then <see cref="F:System.Threading.Tasks.TaskCanceledException"/> is raised. Note
        /// that the task may be governed by a different cancellation token to the overall async computation where the
        /// Await occurs. In practice you should normally start the task with the cancellation token returned by
        /// <c>let! ct = Async.CancellationToken</c>, and catch any <see cref="F:System.Threading.Tasks.TaskCanceledException"/>
        /// at the point where the overall async is started.
        /// </remarks>
        static member Await (task: Task) : Async<unit> =
            Async.FromContinuations (fun (sc, ec, cc) ->
                task.ContinueWith (fun (task: Task) ->
                    if task.IsFaulted then
                        let e = task.Exception
                        if e.InnerExceptions.Count = 1 then ec e.InnerExceptions[0]
                        else ec e
                    elif task.IsCanceled then cc (TaskCanceledException ())
                    else sc ())
                |> ignore)
        

        /// Combine all asyncs in one, chaining them in sequence order.
        /// Similar to Async.Sequential but the returned Async contains a sequence, which is lazily evaluated.
        static member SequentialLazy (t: seq<Async<'T>>) : Async<seq<_>> = async {
            let! ct = Async.CancellationToken
            return Seq.map (fun t -> Async.AsTask(t, ct).Result) t }
        
        #endif

        /// Combine all asyncs in one, chaining them in sequence order.
        static member Sequential (t: list<Async<'T>>) : Async<list<'T>> =
        #if FABLE_COMPILER
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
        static member Sequential (t: array<Async<_>>) : Async<array<_>> = async {
            let siz = Array.length t
            let arr = Array.zeroCreate siz
            for i in 0 .. siz-1 do
                let! v = t.[i]
                arr.[i] <- v
            return arr }

        /// Creates an async Result from a Result where the Ok case is async.
        static member Sequential (t: Result<Async<'T>, 'Error>) : Async<Result<'T,'Error>> =
            match t with
            | Ok a    -> Async.Map Ok a
            | Error e -> async.Return (Error e)

        /// Creates an async Choice from a Choice where the Choice1Of2 case is async.
        static member Sequential (t: Choice<Async<'T>, 'Choice2Of2>) : Async<Choice<'T,'Choice2Of2>> =
            match t with
            | Choice1Of2 a -> Async.Map Choice1Of2 a
            | Choice2Of2 e -> async.Return (Choice2Of2 e)

        /// Creates an async Result from a Result where both cases are async.
        static member Bisequential (t: Result<Async<'T>, Async<'Error>>) : Async<Result<'T,'Error>> =
            match t with
            | Ok a    -> Async.Map Ok a
            | Error e -> Async.Map Error e

        /// Creates an async Choice from a Choice where both cases are async.
        static member Bisequential (t: Choice<Async<'T>, Async<'Choice2Of2>>) : Async<Choice<'T,'Choice2Of2>> =
            match t with
            | Choice1Of2 a -> Async.Map Choice1Of2 a
            | Choice2Of2 e -> Async.Map Choice2Of2 e


    type Option<'t> with

        /// Returns None if it contains a None element, otherwise a list of all elements.
        static member Sequential (t: seq<option<'t>>) =
        #if FABLE_COMPILER
            let mutable ok = true
            let res = Seq.toArray (seq {
                use e = t.GetEnumerator ()
                while e.MoveNext () && ok do
                    match e.Current with
                    | Some v -> yield v
                    | None   -> ok <- false })
            if ok then Some (Array.toSeq res) else None
        #else
            let mutable accumulator = ArrayCollector<'t> ()
            let mutable noneFound = false
            use e = t.GetEnumerator ()
            while e.MoveNext () && not noneFound do
                match e.Current with
                | Some v -> accumulator.Add v
                | None -> noneFound <-  true
                
            if noneFound
            then None
            else accumulator.Close () |> Array.toSeq |> Some
        #endif
            

    type ValueOption<'t> with

        /// Returns None if it contains a None element, otherwise a list of all elements.
        static member Sequential (t: seq<voption<'t>>) =
        #if FABLE_COMPILER
            let mutable ok = true
            let res = Seq.toArray (seq {
                use e = t.GetEnumerator ()
                while e.MoveNext () && ok do
                    match e.Current with
                    | ValueSome v -> yield v
                    | ValueNone   -> ok <- false })
            if ok then ValueSome (Array.toSeq res) else ValueNone
        #else
            let mutable accumulator = ArrayCollector<'t> ()
            let mutable noneFound = false
            use e = t.GetEnumerator ()
            while e.MoveNext () && not noneFound do
                match e.Current with
                | ValueSome v -> accumulator.Add v
                | ValueNone -> noneFound <-  true
                
            if noneFound
            then ValueNone
            else accumulator.Close () |> Array.toSeq |> ValueSome
        #endif


    type Choice<'T1, 'T2> with

        /// Returns the first Choice2Of2 if it contains a Choice2Of2 element, otherwise a list of all Choice1Of2 elements.
        static member Sequential (t: seq<Choice<'T1, 'T2>>) =
        #if FABLE_COMPILER
            let mutable error = ValueNone
            let res = Seq.toArray (seq {
                use e = t.GetEnumerator ()
                while e.MoveNext () && error.IsNone do
                    match e.Current with
                    | Choice1Of2 v -> yield v
                    | Choice2Of2 e -> error <- ValueSome e })

            match error with
            | ValueNone -> Choice1Of2 (Array.toSeq res)
            | ValueSome e -> Choice2Of2 e
        #else
            let mutable accumulator = ArrayCollector<'T1> ()
            let mutable error = ValueNone
            use e = t.GetEnumerator ()
            while e.MoveNext () && error.IsNone do
                match e.Current with
                | Choice1Of2 v -> accumulator.Add v
                | Choice2Of2 x -> error <- ValueSome x
            match error with
            | ValueNone -> Choice1Of2 (accumulator.Close () |> Array.toSeq)
            | ValueSome x -> Choice2Of2 x
        #endif


        /// Returns all Choice2Of2's combined, otherwise a sequence of all Choice1Of2 elements.
        static member Parallel (choice2Combiner, t: seq<Choice<'T1, 'T2>>) =
            let mutable error = ValueNone
            let res = Seq.toArray (seq {
                use e = t.GetEnumerator ()
                while e.MoveNext () do
                    match e.Current, error with
                    | Choice1Of2 v, ValueNone   -> yield v
                    | Choice2Of2 e, ValueNone   -> error <- ValueSome e
                    | Choice2Of2 e, ValueSome x -> error <- ValueSome (choice2Combiner x e)
                    | _                         -> () })

            match error with
            | ValueNone -> Choice1Of2 (Array.toSeq res)
            | ValueSome e -> Choice2Of2 e


    type Result<'T, 'Error> with

        /// Returns the first Error if it contains an Error element, otherwise a sequence of all elements.
        static member Sequential (t: seq<Result<'T, 'Error>>) =
        #if FABLE_COMPILER
            let mutable error = ValueNone
            let res = Seq.toArray (seq {
                use e = t.GetEnumerator ()
                while e.MoveNext () && error.IsNone do
                    match e.Current with
                    | Ok v -> yield v
                    | Error e -> error <- ValueSome e })

            match error with
            | ValueNone -> Ok (Array.toSeq res)
            | ValueSome e -> Error e
        #else
            let mutable accumulator = ArrayCollector<'T> ()
            let mutable error = ValueNone
            use e = t.GetEnumerator ()
            while e.MoveNext () && error.IsNone do
                match e.Current with
                | Ok v -> accumulator.Add v
                | Error x -> error <- ValueSome x
            match error with
            | ValueNone -> Ok (accumulator.Close () |> Array.toSeq)
            | ValueSome x -> Error x
        #endif

        /// Returns all Errors combined, otherwise a sequence of all elements.
        static member Parallel (errorCombiner, t: seq<Result<'T, 'Error>>) =
            let mutable error = ValueNone
            let res = Seq.toArray (seq {
                use e = t.GetEnumerator ()
                while e.MoveNext () do
                    match e.Current, error with
                    | Ok v   , ValueNone   -> yield v
                    | Error e, ValueNone   -> error <- ValueSome e
                    | Error e, ValueSome x -> error <- ValueSome (errorCombiner x e)
                    | _                    -> () })

            match error with
            | ValueNone -> Ok (Array.toSeq res)
            | ValueSome e -> Error e

        /// Returns the first Error if it contains an Error element, otherwise a list of all elements.
        static member Sequential (t: list<Result<'T, 'Error>>) =
        #if FABLE_COMPILER
            let mutable error = ValueNone
            let res = Seq.toList (seq {
                use e = (t :> seq<_>).GetEnumerator ()
                while e.MoveNext () && error.IsNone do
                    match e.Current with
                    | Ok v -> yield v
                    | Error e -> error <- ValueSome e })

            match error with
            | ValueNone -> Ok res
            | ValueSome e -> Error e
        #else
            let mutable accumulator = ListCollector<'T> ()
            let mutable error = ValueNone
            use e = (t :> seq<_>).GetEnumerator ()
            while e.MoveNext () && error.IsNone do
                match e.Current with
                | Ok v -> accumulator.Add v
                | Error x -> error <- ValueSome x
            match error with
            | ValueNone -> Ok (accumulator.Close ())
            | ValueSome x -> Error x
        #endif

        /// Returns the Error if it contains an Error element, otherwise the option inside an Ok.
        static member Sequential (t: option<Result<'T, 'Error>>) =
            match t with
            | Some (Ok x)    -> Ok (Some x)
            | None           -> Ok None
            | Some (Error x) -> Error x

        /// Returns the Error if it contains an Error element, otherwise the voption inside an Ok.
        static member Sequential (t: voption<Result<'T, 'Error>>) =
            match t with
            | ValueSome (Ok x)    -> Ok (ValueSome x)
            | ValueNone           -> Ok ValueNone
            | ValueSome (Error x) -> Error x