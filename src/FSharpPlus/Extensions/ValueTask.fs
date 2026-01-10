namespace FSharpPlus

#if !FABLE_COMPILER

/// Additional operations on ValueTask<'T>
[<RequireQualifiedAccess>]
module ValueTask =

    open System
    open System.Threading
    open System.Threading.Tasks
    open FSharpPlus.Internals.Errors

    /// Active pattern to match the state of a completed ValueTask
    let inline (|Succeeded|Canceled|Faulted|) (t: ValueTask<'T>) =
        if t.IsCompletedSuccessfully then Succeeded t.Result
        elif t.IsFaulted then Faulted (Unchecked.nonNull (t.AsTask().Exception))
        elif t.IsCanceled then Canceled
        else invalidOp "Internal error: The task is not yet in a final state."

    let inline internal continueTask (tcs: TaskCompletionSource<'Result>) (k: 't -> unit) (x: ValueTask<'t>) =
        let f = function
            | Succeeded r -> k r
            | Faulted axn -> tcs.SetException axn.InnerExceptions
            | Canceled    -> tcs.SetCanceled ()
        x.ConfigureAwait(false).GetAwaiter().UnsafeOnCompleted (fun () -> f x)

  #if NET5_0_OR_GREATER
    let [<Literal>] private tcsOptions = TaskCreationOptions.RunContinuationsAsynchronously
  #else
    let             private tcsOptions = ()
  #endif

    /// <summary>Creates a ValueTask that's completed successfully with the specified value.</summary>
    /// <param name="value"></param>
    /// <returns>A ValueTask that is completed successfully with the specified value.</returns>
    let result (value: 'T) : ValueTask<'T> =
    #if NET5_0_OR_GREATER
        ValueTask.FromResult value
    #else
        let tcs = TaskCompletionSource<'T> tcsOptions
        tcs.SetResult value
        ValueTask<'T> tcs.Task
    #endif

    /// <summary>Creates a ValueTask that's completed unsuccessfully with the specified exception.</summary>
    /// <param name="exn">The exception to be raised.</param>
    /// <returns>A ValueTask that is completed unsuccessfully with the specified exception.</returns>
    let raise<'T> (exn: exn) : ValueTask<'T> =
      #if NET5_0_OR_GREATER  
        ValueTask.FromException<'T> exn
      #else
        let tcs = TaskCompletionSource<'T> tcsOptions
        tcs.SetException exn
        ValueTask<'T> tcs.Task
      #endif
    
    /// <summary>Creates a Task that's completed unsuccessfully with the specified exceptions.</summary>
    /// <param name="aex">The AggregateException to be raised.</param>
    /// <returns>A Task that is completed unsuccessfully with the specified exceptions.</returns>
    /// <remarks>
    /// Prefer this function to handle AggregateExceptions over Task.FromException as it handles them correctly.
    /// </remarks>
    let inline internal FromExceptions<'T> (aex: AggregateException) : ValueTask<'T> =
      #if NET5_0_OR_GREATER
        match aex with
        | agg when agg.InnerExceptions.Count = 1 -> ValueTask.FromException<'T> agg.InnerExceptions[0]
        | _ ->
      #endif
            let tcs = TaskCompletionSource<'T> tcsOptions
            tcs.SetException aex.InnerExceptions
            ValueTask<'T> tcs.Task

    /// <summary>Creates a ValueTask that's canceled.</summary>
    /// <returns>A ValueTask that's canceled.</returns>
    let canceled<'T> : ValueTask<'T> =
      #if NET5_0_OR_GREATER
        ValueTask.FromCanceled<'T> (CancellationToken true)
      #else
        let tcs = TaskCompletionSource<'T> tcsOptions
        tcs.SetCanceled ()
        ValueTask<'T> tcs.Task
      #endif

    /// <summary>Creates a ValueTask workflow from 'source' workflow, mapping its result with 'mapper'.</summary>
    /// <param name="mapper">The mapping function.</param>
    /// <param name="source">The source ValueTask workflow.</param>
    /// <returns>The resulting ValueTask workflow.</returns>
    let map (mapper: 'T -> 'U) (source: ValueTask<'T>) : ValueTask<'U> =
        if source.IsCompleted then
            match source with
            | Succeeded r -> try result (mapper r) with e -> raise e
            | Faulted exn -> FromExceptions exn
            | Canceled    -> canceled
        else
            let tcs = TaskCompletionSource<'U> tcsOptions
            source |> continueTask tcs (fun r -> try tcs.SetResult (mapper r) with e -> tcs.SetException e)
            tcs.Task |> ValueTask<'U>


    /// <summary>Creates a ValueTask workflow from two workflows 'x' and 'y', mapping its results with 'f'.</summary>
    /// <remarks>Workflows are run in sequence.</remarks>
    /// <param name="mapper">The mapping function.</param>
    /// <param name="task1">First ValueTask workflow.</param>
    /// <param name="task2">Second ValueTask workflow.</param>
    let lift2 (mapper: 'T1 -> 'T2 -> 'U) (task1: ValueTask<'T1>) (task2: ValueTask<'T2>) : ValueTask<'U> =
        if task1.IsCompleted && task2.IsCompleted then
            match task1, task2 with
            | Succeeded r1, Succeeded r2 -> try result (mapper r1 r2) with e -> raise e
            | Succeeded _ , Faulted exn  -> FromExceptions exn
            | Succeeded _ , Canceled     -> canceled
            | Faulted exn , _            -> FromExceptions exn
            | Canceled    , _            -> canceled
        else
            let tcs = TaskCompletionSource<'U> tcsOptions
            if   task1.IsCanceled then tcs.SetCanceled ()
            elif task1.IsFaulted  then tcs.SetException (Unchecked.nonNull (task1.AsTask().Exception)).InnerExceptions
            elif task2.IsCanceled then tcs.SetCanceled ()
            elif task2.IsFaulted  then tcs.SetException (Unchecked.nonNull (task2.AsTask().Exception)).InnerExceptions
            elif task1.IsCompletedSuccessfully then  task2 |> continueTask tcs (fun y -> try tcs.SetResult (mapper task1.Result y) with e -> tcs.SetException e)
            elif task2.IsCompletedSuccessfully then  task1 |> continueTask tcs (fun x -> try tcs.SetResult (mapper x task2.Result) with e -> tcs.SetException e)
            else task1 |> continueTask tcs (fun x -> task2 |> continueTask tcs (fun y -> try tcs.SetResult (mapper x y) with e -> tcs.SetException e))
            ValueTask<'U> tcs.Task

    /// <summary>Creates a ValueTask workflow from three workflows 'x', 'y' and z, mapping its results with 'f'.</summary>
    /// <remarks>Workflows are run in sequence.</remarks>
    /// <param name="mapper">The mapping function.</param>
    /// <param name="task1">First ValueTask workflow.</param>
    /// <param name="task2">Second ValueTask workflow.</param>
    /// <param name="task3">Third ValueTask workflow.</param>
    let lift3 (mapper: 'T1 -> 'T2 -> 'T3 -> 'U) (task1: ValueTask<'T1>) (task2: ValueTask<'T2>) (task3: ValueTask<'T3>) : ValueTask<'U> =
        if task1.IsCompleted && task2.IsCompleted && task3.IsCompleted then
            match task1, task2, task3 with
            | Succeeded r1, Succeeded r2, Succeeded r3 -> try result (mapper r1 r2 r3) with e -> raise e
            | Faulted exn , _            , _           -> FromExceptions exn
            | Canceled    , _            , _           -> canceled
            | _           , Faulted exn  , _           -> FromExceptions exn
            | _           , Canceled     , _           -> canceled
            | _           , _           , Faulted exn  -> FromExceptions exn
            | _           , _           , Canceled     -> canceled
        else
            let tcs = TaskCompletionSource<'U> tcsOptions
            if   task1.IsCanceled then tcs.SetCanceled ()
            elif task1.IsFaulted  then tcs.SetException (Unchecked.nonNull (task1.AsTask().Exception)).InnerExceptions
            elif task2.IsCanceled then tcs.SetCanceled ()
            elif task2.IsFaulted  then tcs.SetException (Unchecked.nonNull (task2.AsTask().Exception)).InnerExceptions
            elif task3.IsCanceled then tcs.SetCanceled ()
            elif task3.IsFaulted  then tcs.SetException (Unchecked.nonNull (task3.AsTask().Exception)).InnerExceptions
            else
                task1 |> continueTask tcs (fun r1 ->
                    task2 |> continueTask tcs (fun r2 ->
                        task3 |> continueTask tcs (fun r3 ->
                            try tcs.SetResult (mapper r1 r2 r3) with e -> tcs.SetException e)))
            ValueTask<'U> tcs.Task

    /// <summary>Creates a ValueTask workflow from two workflows, mapping its results with a specified function.</summary>
    /// <remarks>Similar to lift2 but although workflows are started in sequence they might end independently in different order
    /// and all errors are collected.
    /// </remarks>
    /// <param name="mapper">The mapping function.</param>
    /// <param name="task1">First ValueTask workflow.</param>
    /// <param name="task2">Second ValueTask workflow.</param>
    let map2 mapper (task1: ValueTask<'T1>) (task2: ValueTask<'T2>) : ValueTask<'U> =
        if task1.IsCompletedSuccessfully && task2.IsCompletedSuccessfully then
            try result (mapper task1.Result task2.Result)
            with e -> raise e
        else
            let tcs = TaskCompletionSource<_> tcsOptions
            let r1 = ref Unchecked.defaultof<_>
            let r2 = ref Unchecked.defaultof<_>
            let mutable cancelled = false
            let failures = [|IReadOnlyCollection.empty; IReadOnlyCollection.empty|]
            let pending = ref 2

            let trySet () =
                if Interlocked.Decrement pending = 0 then
                    let noFailures = Array.forall IReadOnlyCollection.isEmpty failures
                    if noFailures && not cancelled then
                        try tcs.TrySetResult (mapper r1.Value r2.Value) |> ignore
                        with e -> tcs.TrySetException e |> ignore
                    elif noFailures then tcs.TrySetCanceled () |> ignore
                    else tcs.TrySetException (failures |> Seq.map AggregateException |> Seq.reduce Exception.add).InnerExceptions |> ignore

            let k (v: ref<_>) i t =
                match t with
                | Succeeded r -> v.Value <- r
                | Faulted aex -> failures[i] <- aex.InnerExceptions
                | Canceled    -> cancelled <- true
                trySet ()

            if task1.IsCompleted && task2.IsCompleted then
                task1 |> k r1 0
                task2 |> k r2 1
            else
                task1.ConfigureAwait(false).GetAwaiter().UnsafeOnCompleted (fun () -> (k r1 0) task1)
                task2.ConfigureAwait(false).GetAwaiter().UnsafeOnCompleted (fun () -> (k r2 1) task2)
            ValueTask<'U> tcs.Task

    /// <summary>Creates a ValueTask workflow from three workflows, mapping its results with a specified function.</summary>
    /// <remarks>Similar to lift3 but although workflows are started in sequence they might end independently in different order
    /// and all errors are collected.
    /// </remarks>
    /// <param name="mapper">The mapping function.</param>
    /// <param name="task1">First ValueTask workflow.</param>
    /// <param name="task2">Second ValueTask workflow.</param>
    /// <param name="task3">Third ValueTask workflow.</param>
    let map3 mapper (task1: ValueTask<'T1>) (task2: ValueTask<'T2>) (task3: ValueTask<'T3>) : ValueTask<'U> =
        if task1.IsCompletedSuccessfully && task2.IsCompletedSuccessfully && task3.IsCompletedSuccessfully then
            try result (mapper task1.Result task2.Result task3.Result)
            with e -> raise e
        else
            let tcs = TaskCompletionSource<_> tcsOptions
            let r1 = ref Unchecked.defaultof<_>
            let r2 = ref Unchecked.defaultof<_>
            let r3 = ref Unchecked.defaultof<_>
            let mutable cancelled = false
            let failures = [|IReadOnlyCollection.empty; IReadOnlyCollection.empty; IReadOnlyCollection.empty|]
            let pending = ref 3

            let trySet () =
                if Interlocked.Decrement pending = 0 then
                    let noFailures = Array.forall IReadOnlyCollection.isEmpty failures
                    if noFailures && not cancelled then
                        try tcs.TrySetResult (mapper r1.Value r2.Value r3.Value) |> ignore
                        with e -> tcs.TrySetException e |> ignore
                    elif noFailures then tcs.TrySetCanceled () |> ignore
                    else tcs.TrySetException (failures |> Seq.map AggregateException |> Seq.reduce Exception.add).InnerExceptions |> ignore            

            let k (v: ref<_>) i t =
                match t with
                | Succeeded r -> v.Value <- r
                | Faulted aex -> failures[i] <- aex.InnerExceptions
                | Canceled    -> cancelled <- true
                trySet ()

            if task1.IsCompleted && task2.IsCompleted && task3.IsCompleted then
                task1 |> k r1 0
                task2 |> k r2 1
                task3 |> k r3 2
            else
                task1.ConfigureAwait(false).GetAwaiter().UnsafeOnCompleted (fun () -> (k r1 0) task1)
                task2.ConfigureAwait(false).GetAwaiter().UnsafeOnCompleted (fun () -> (k r2 1) task2)
                task3.ConfigureAwait(false).GetAwaiter().UnsafeOnCompleted (fun () -> (k r3 2) task3)
            ValueTask<'U> tcs.Task

    /// <summary>Creates a ValueTask workflow that is the result of applying the resulting function of a ValueTask workflow
    /// to the resulting value of another ValueTask workflow</summary>
    /// <param name="f">ValueTask workflow returning a function</param>
    /// <param name="x">ValueTask workflow returning a value</param>
    let apply (f: ValueTask<'T -> 'U>) (x: ValueTask<'T>) : ValueTask<'U> =
        if f.IsCompleted && x.IsCompleted then
            match f, x with
            | Succeeded r1, Succeeded r2 -> try result (r1 r2) with e -> raise e
            | Succeeded _ , Faulted exn  -> FromExceptions exn
            | Succeeded _ , Canceled     -> canceled
            | Faulted exn , _            -> FromExceptions exn
            | Canceled    , _            -> canceled
        else
            let tcs = TaskCompletionSource<'U> tcsOptions
            if   f.IsCanceled then tcs.SetCanceled ()
            elif f.IsFaulted  then tcs.SetException (Unchecked.nonNull (f.AsTask().Exception)).InnerExceptions
            elif x.IsCanceled then tcs.SetCanceled ()
            elif x.IsFaulted  then tcs.SetException (Unchecked.nonNull (x.AsTask().Exception)).InnerExceptions
            elif f.IsCompletedSuccessfully then x |> continueTask tcs (fun r -> try tcs.SetResult (f.Result r) with e -> tcs.SetException e)
            elif x.IsCompletedSuccessfully then f |> continueTask tcs (fun r -> try tcs.SetResult (r x.Result) with e -> tcs.SetException e)
            else f |> continueTask tcs (fun r -> x |> continueTask tcs (fun r' -> try tcs.SetResult (r r') with e -> tcs.SetException e))
            ValueTask<'U> tcs.Task

    /// <summary>Creates a ValueTask workflow from two workflows 'task1' and 'task2', tupling its results.</summary>
    let zipSequentially (task1: ValueTask<'T1>) (task2: ValueTask<'T2>) : ValueTask<'T1 * 'T2> =
        if task1.IsCompleted && task2.IsCompleted then
            match task1, task2 with
            | Succeeded r1, Succeeded r2 -> result (r1, r2)
            | Succeeded _ , Faulted exn  -> FromExceptions exn
            | Succeeded _ , Canceled     -> canceled
            | Faulted exn , _            -> FromExceptions exn
            | Canceled    , _            -> canceled
        else
            let tcs = TaskCompletionSource<'T1 * 'T2> tcsOptions
            if   task1.IsCanceled then tcs.SetCanceled ()
            elif task1.IsFaulted  then tcs.SetException (Unchecked.nonNull (task1.AsTask().Exception)).InnerExceptions
            elif task2.IsCanceled then tcs.SetCanceled ()
            elif task2.IsFaulted  then tcs.SetException (Unchecked.nonNull (task2.AsTask().Exception)).InnerExceptions
            elif task1.IsCompletedSuccessfully then  task2 |> continueTask tcs (fun y -> tcs.SetResult (task1.Result, y))
            elif task2.IsCompletedSuccessfully then  task1 |> continueTask tcs (fun x -> tcs.SetResult (x, task2.Result))
            else task1 |> continueTask tcs (fun x -> task2 |> continueTask tcs (fun y -> tcs.SetResult (x, y)))
            ValueTask<'T1 * 'T2> tcs.Task

    /// <summary>Creates a ValueTask workflow from two workflows, tupling its results.</summary>
    /// <remarks>Similar to zipSequentially but although workflows are started in sequence they might end independently in different order
    /// and all errors are collected.
    /// </remarks>
    let zip (task1: ValueTask<'T1>) (task2: ValueTask<'T2>) = map2 (fun x y -> x, y) task1 task2

    /// <summary>Creates a ValueTask workflow from three workflows, tupling its results.</summary>
    /// <remarks>Similar to zipSequentially but although workflows are started in sequence they might end independently in different order
    /// and all errors are collected.
    /// </remarks>
    let zip3 (task1: ValueTask<'T1>) (task2: ValueTask<'T2>) (task3: ValueTask<'T3>) = map3 (fun x y z -> x, y, z) task1 task2 task3
    
    /// Flattens two nested ValueTask into one.
    let join (source: ValueTask<ValueTask<'T>>) : ValueTask<'T> =
        (source |> map (fun x -> x.AsTask())).AsTask().Unwrap () |> ValueTask<'T>
    
    /// <summary>Creates a ValueTask workflow from 'source' workflow, mapping and flattening its result with 'f'.</summary>
    let bind (f: 'T -> ValueTask<'U>) (source: ValueTask<'T>) : ValueTask<'U> =
        source |> map f |> join
            
    /// <summary>Creates a ValueTask that ignores the result of the source ValueTask.</summary>
    /// <param name="source">The source ValueTask.</param>
    /// <returns>A ValueTask that completes when the source completes.</returns>
    /// <remarks>It can be used to convert non-generic ValueTask to unit ValueTask.</remarks>
    let ignore (source: ValueTask) : ValueTask<unit> =
        if source.IsCompleted  then Unchecked.defaultof<_>
        elif source.IsFaulted  then FromExceptions (Unchecked.nonNull (source.AsTask().Exception))
        elif source.IsCanceled then canceled
        else
            let tcs = TaskCompletionSource<unit> tcsOptions
            let k (t: ValueTask) : unit =
                if t.IsCanceled  then tcs.SetCanceled ()
                elif t.IsFaulted then tcs.SetException (Unchecked.nonNull (source.AsTask().Exception)).InnerExceptions
                else tcs.SetResult ()
            source.ConfigureAwait(false).GetAwaiter().UnsafeOnCompleted (fun () -> k source)
            ValueTask<unit> tcs.Task

    /// Used to de-sugar try .. with .. blocks in Computation Expressions.
    let inline tryWith ([<InlineIfLambda>]compensation: exn -> ValueTask<'T>) ([<InlineIfLambda>]body: unit -> ValueTask<'T>) : ValueTask<'T> =
        let runCompensation exn =
            try compensation exn
            with e -> raise e
        let unwrapException (agg: AggregateException) =
            if agg.InnerExceptions.Count = 1 then agg.InnerExceptions.[0]
            else agg :> Exception
        try Ok (body ()) with e -> Error e
        |> function
        | Ok task ->
            if task.IsCompleted then
                match task with
                | Succeeded _ -> task
                | Faulted aex -> runCompensation (unwrapException aex)
                | Canceled    -> canceled
            else
                task.AsTask().ContinueWith(fun (x: Task<'T>) -> Task.tryWith (compensation >> fun x -> x.AsTask()) (fun () -> x)).Unwrap () |> ValueTask<'T>
        | Error exn -> runCompensation exn
        
    
    /// Used to de-sugar try .. finally .. blocks in Computation Expressions.
    let inline tryFinally ([<InlineIfLambda>]compensation : unit -> unit) ([<InlineIfLambda>]body: unit -> ValueTask<'T>) : ValueTask<'T> =
        let task =
            try body ()
            with _ ->
                try
                    compensation ()
                    reraise ()
                with e -> raise e
        if task.IsCompleted then
            try
                compensation ()
                task
            with e -> raise e
        else
            task.AsTask().ContinueWith(fun (x: Task<'T>) -> Task.tryFinally compensation (fun () -> x)).Unwrap () |> ValueTask<'T>

    /// Used to de-sugar use .. blocks in Computation Expressions.
    let inline using (disp: 'T when 'T :> IDisposable) ([<InlineIfLambda>]body: 'T -> ValueTask<'U>) =
        tryFinally
            (fun () -> if not (isNull (box disp)) then disp.Dispose ())
            (fun () -> body disp)

    /// <summary>Returns <paramref name="source"/> if it is not faulted, otherwise evaluates <paramref name="fallbackThunk"/> and returns the result.</summary>
    ///
    /// <param name="fallbackThunk">A thunk that provides an alternate task computation when evaluated.</param>
    /// <param name="source">The input task.</param>
    ///
    /// <returns>The task if it is not faulted, else the result of evaluating <paramref name="fallbackThunk"/>.</returns>
    /// <remarks><paramref name="fallbackThunk"/> is not evaluated unless <paramref name="source"/> is faulted.</remarks>
    ///
    let inline orElseWith ([<InlineIfLambda>]fallbackThunk: exn -> ValueTask<'T>) (source: ValueTask<'T>) : ValueTask<'T> = tryWith fallbackThunk (fun () -> source)

    /// <summary>Returns <paramref name="source"/> if it is not faulted, otherwise e<paramref name="fallbackValueTask"/>.</summary>
    ///
    /// <param name="fallbackValueTask">The alternative ValueTask to use if <paramref name="source"/> is faulted.</param>
    /// <param name="source">The input task.</param>
    ///
    /// <returns>The option if the option is Some, else the alternate option.</returns>
    let orElse (fallbackValueTask: ValueTask<'T>) (source: ValueTask<'T>) : ValueTask<'T> = orElseWith (fun _ -> fallbackValueTask) source

    /// <summary>Attempts to recover from a potentially failed task by mapping the exception to a successful result.</summary>
    /// <param name="mapper">Mapping function from exception to result.</param>
    /// <param name="source">The source task.</param>
    /// <returns>A successful resulting task.</returns>
    /// <remarks>The result is always a successful task, unless the mapping function itself throws an exception.</remarks>
    let inline recover ([<InlineIfLambda>]mapper: exn -> 'T) (source: ValueTask<'T>) : ValueTask<'T> =
        tryWith (mapper >> result) (fun () -> source)

    /// <summary>Maps the exception of a faulted task to another exception.</summary>
    /// <param name="mapper">Mapping function from exception to exception.</param>
    /// <param name="source">The source task.</param>
    /// <returns>The resulting task.</returns>
    let inline mapError ([<InlineIfLambda>]mapper: exn -> exn) (source: ValueTask<'T>) : ValueTask<'T> =
        if source.IsCompleted then
            match source with
            | Faulted exn -> FromExceptions (AggregateException (mapper exn))
            | _           -> source
        else
            let tcs = TaskCompletionSource<'T> tcsOptions
            let k = function
                | Succeeded r -> tcs.SetResult r
                | Faulted aex -> tcs.SetException (AggregateException (mapper aex)).InnerExceptions
                | Canceled    -> tcs.SetCanceled ()
            source.ConfigureAwait(false).GetAwaiter().UnsafeOnCompleted (fun () -> k source)
            ValueTask<'T> tcs.Task

    /// Creates a Task from a Result value.
    /// If the Result is Ok, the Task will complete successfully with the value.
    /// If the Result is Error, the Task will complete unsuccessfully with the exception.
    /// <param name="source">The source Result.</param>
    /// <returns>The resulting Task.</returns>
    let ofResult (source: Result<'T, exn>) : ValueTask<'T> =
        match source with
        | Ok x -> result x
        | Error exn -> raise exn

#endif
