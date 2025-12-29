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
        else invalidOp "Internal error: The task is not yet completed."
    
    let inline continueTask (tcs: TaskCompletionSource<'Result>) (x: ValueTask<'t>) (k: 't -> unit) =
        let f = function
        | Succeeded r -> k r
        | Canceled  -> tcs.SetCanceled ()
        | Faulted e -> tcs.SetException e.InnerExceptions
        if x.IsCompleted then f x
        else x.ConfigureAwait(false).GetAwaiter().UnsafeOnCompleted (fun () -> f x)

    let inline continueWith f (x: ValueTask<'t>) =
        if x.IsCompleted then f x
        else x.ConfigureAwait(false).GetAwaiter().UnsafeOnCompleted (fun () -> f x)


    /// <summary>Creates a ValueTask that's completed successfully with the specified value.</summary>
    /// <param name="value"></param>
    /// <returns>A ValueTask that is completed successfully with the specified value.</returns>
    let result (value: 'T) : ValueTask<'T> =
    #if NET5_0_OR_GREATER
        ValueTask.FromResult value
    #else
        let tcs = TaskCompletionSource<'T> ()
        tcs.SetResult value
        ValueTask<'T> tcs.Task
    #endif
    
    /// <summary>Creates a ValueTask that's completed unsuccessfully with the specified exception.</summary>
    /// <param name="exn">The exception to be raised.</param>
    /// <returns>A ValueTask that is completed unsuccessfully with the specified exception.</returns>
    /// <remarks>
    /// If the exception is not an AggregateException it is wrapped into one.
    /// Prefer this function over ValueTask.FromException as it handles AggregateExceptions correctly.
    /// </remarks>
    let raise<'T> (exn: exn) : ValueTask<'T> =
        match exn with
        | :? AggregateException as agg when agg.InnerExceptions.Count > 1 ->
            let tcs = TaskCompletionSource<'T> ()
            tcs.SetException agg.InnerExceptions
            ValueTask<'T> tcs.Task
        | :? AggregateException as agg -> ValueTask.FromException<'T> agg.InnerExceptions[0]
        | exn                          -> ValueTask.FromException<'T> exn

    let private cancellationTokenSingleton = CancellationToken true

    /// <summary>Creates a ValueTask that's canceled.</summary>
    /// <returns>A ValueTask that's canceled.</returns>
    let canceled<'T> : ValueTask<'T> = ValueTask.FromCanceled<'T> cancellationTokenSingleton

    /// <summary>Creates a ValueTask workflow from 'source' workflow, mapping its result with 'mapper'.</summary>
    /// <param name="mapper">The mapping function.</param>
    /// <param name="source">The source ValueTask workflow.</param>
    /// <returns>The resulting ValueTask workflow.</returns>
    let map (mapper: 'T -> 'U) (source: ValueTask<'T>) : ValueTask<'U> =
        if source.IsCompleted then
            match source with
            | Succeeded r -> try result (mapper r) with e -> raise e
            | Faulted exn -> raise exn
            | Canceled    -> canceled
        else
            let tcs = TaskCompletionSource<'U> ()
            let k = function
                | Succeeded r -> try tcs.SetResult (mapper r) with e -> tcs.SetException e
                | Faulted exn -> tcs.SetException exn.InnerExceptions
                | Canceled    -> tcs.SetCanceled ()
            continueWith k source
            ValueTask<'U> tcs.Task


    /// <summary>Creates a ValueTask workflow from two workflows 'x' and 'y', mapping its results with 'f'.</summary>
    /// <remarks>Workflows are run in sequence.</remarks>
    /// <param name="f">The mapping function.</param>
    /// <param name="x">First ValueTask workflow.</param>
    /// <param name="y">Second ValueTask workflow.</param>
    let lift2 (f: 'T1 -> 'T2 -> 'U) (task1: ValueTask<'T1>) (task2: ValueTask<'T2>) : ValueTask<'U> =
        let tcs = TaskCompletionSource<'U> ()
        continueTask tcs task1 (fun x ->
            continueTask tcs task2 (fun y -> try tcs.SetResult (f x y) with e -> tcs.SetException e))
        ValueTask<'U> tcs.Task

    /// <summary>Creates a ValueTask workflow from three workflows 'x', 'y' and z, mapping its results with 'f'.</summary>
    /// <remarks>Workflows are run in sequence.</remarks>
    /// <param name="f">The mapping function.</param>
    /// <param name="x">First ValueTask workflow.</param>
    /// <param name="y">Second ValueTask workflow.</param>
    /// <param name="z">Third ValueTask workflow.</param>
    let lift3 (f: 'T1 -> 'T2 -> 'T3 -> 'U) (x: ValueTask<'T1>) (y: ValueTask<'T2>) (z: ValueTask<'T3>) : ValueTask<'U> =
        let tcs = TaskCompletionSource<'U> ()
        continueTask tcs x (fun x ->
            continueTask tcs y (fun y ->
                continueTask tcs z (fun z -> try tcs.SetResult (f x y z) with e -> tcs.SetException e)))
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
            with e ->
                let tcs = TaskCompletionSource<_> ()
                tcs.SetException e
                tcs.Task |> ValueTask<'U>
        else
        let tcs = TaskCompletionSource<_> ()
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
            | Canceled    -> cancelled <- true
            | Faulted e   -> failures[i] <- e.InnerExceptions
            trySet ()

        if task1.IsCompleted && task2.IsCompleted then
            task1 |> k r1 0
            task2 |> k r2 1
        else
            continueWith (k r1 0) task1
            continueWith (k r2 1) task2
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
            with e ->
                let tcs = TaskCompletionSource<_> ()
                tcs.SetException e
                ValueTask<'U> tcs.Task
        else
        let tcs = TaskCompletionSource<_> ()
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
            | Canceled    -> cancelled <- true
            | Faulted e   -> failures[i] <- e.InnerExceptions
            trySet ()

        if task1.IsCompleted && task2.IsCompleted && task3.IsCompleted then
            task1 |> k r1 0
            task2 |> k r2 1
            task3 |> k r3 2
        else
            continueWith (k r1 0) task1
            continueWith (k r2 1) task2
            continueWith (k r3 2) task3
        ValueTask<'U> tcs.Task

    /// <summary>Creates a ValueTask workflow that is the result of applying the resulting function of a ValueTask workflow
    /// to the resulting value of another ValueTask workflow</summary>
    /// <param name="f">ValueTask workflow returning a function</param>
    /// <param name="x">ValueTask workflow returning a value</param>
    let apply (f: ValueTask<'T->'U>) (x: ValueTask<'T>) : ValueTask<'U> =
        let tcs = TaskCompletionSource<'U> ()
        continueTask tcs f (fun f ->
            continueTask tcs x (fun x -> try tcs.SetResult (f x) with e -> tcs.SetException e))
        ValueTask<'U> tcs.Task

    /// <summary>Creates a ValueTask workflow from two workflows 'x' and 'y', tupling its results.</summary>
    let zipSequentially (task1: ValueTask<'T1>) (task2: ValueTask<'T2>) : ValueTask<'T1 * 'T2> =
        let tcs = TaskCompletionSource<'T1 * 'T2> ()
        continueTask tcs task1 (fun x ->
            continueTask tcs task2 (fun y -> tcs.SetResult (x, y)))
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
        if source.IsCompleted then
            match source with
            | Succeeded inner -> inner
            | Faulted aex -> raise aex
            | Canceled -> canceled
        else
            let tcs = TaskCompletionSource<'T> ()
            continueTask tcs source (fun inner ->
                continueTask tcs inner tcs.SetResult)
            ValueTask<'T> tcs.Task
    
    
    /// <summary>Creates a ValueTask workflow from 'source' workflow, mapping and flattening its result with 'f'.</summary>
    let bind (f: 'T -> ValueTask<'U>) (source: ValueTask<'T>) : ValueTask<'U> = source |> Unchecked.nonNull |> map f |> join
            
    /// <summary>Creates a ValueTask that ignores the result of the source ValueTask.</summary>
    /// <remarks>It can be used to convert non-generic ValueTask to unit ValueTask.</remarks>
    let ignore (source: ValueTask) : ValueTask<unit> =
        if source.IsCompleted  then Unchecked.defaultof<_>
        elif source.IsFaulted  then raise (Unchecked.nonNull (source.AsTask().Exception))
        elif source.IsCanceled then canceled
        else
            let tcs = TaskCompletionSource<unit> ()
            if source.IsFaulted then tcs.SetException (Unchecked.nonNull (source.AsTask().Exception)).InnerExceptions
            elif source.IsCanceled then tcs.SetCanceled ()
            else
                let k (t: ValueTask) : unit =
                    if t.IsCanceled  then tcs.SetCanceled ()
                    elif t.IsFaulted then tcs.SetException (Unchecked.nonNull (source.AsTask().Exception)).InnerExceptions
                    else tcs.SetResult ()
                if source.IsCompleted then k source
                else source.ConfigureAwait(false).GetAwaiter().UnsafeOnCompleted (fun () -> k source)
            ValueTask<unit> tcs.Task

    /// Used to de-sugar try .. with .. blocks in Computation Expressions.
    let inline tryWith ([<InlineIfLambda>]compensation: exn -> ValueTask<'T>) ([<InlineIfLambda>]body: unit -> ValueTask<'T>) : ValueTask<'T> =
        let unwrapException (agg: AggregateException) =
            if agg.InnerExceptions.Count = 1 then agg.InnerExceptions.[0]
            else agg :> Exception
        
        let mutable ran = false
        
        let compensation exn =
            if not ran then ran <- true
            try compensation exn
            with e -> raise e

        let task = 
            try body ()
            with
            | :? AggregateException as aex -> compensation (unwrapException aex)
            | exn                          -> compensation exn
        if ran then task
        elif task.IsCompleted then
            match task with
            | Succeeded _ -> task
            | Faulted aex -> compensation (unwrapException aex)
            | Canceled    -> compensation (TaskCanceledException ())
        else
            let tcs = TaskCompletionSource<'T> ()
            let f = function
                | Succeeded r -> tcs.SetResult r
                | Faulted aex -> continueTask tcs (compensation (unwrapException aex))      (fun r -> try tcs.SetResult r with e -> tcs.SetException e)
                | Canceled    -> continueTask tcs (compensation (TaskCanceledException ())) (fun r -> try tcs.SetResult r with e -> tcs.SetException e)
            task.ConfigureAwait(false).GetAwaiter().UnsafeOnCompleted (fun () -> f task)
            ValueTask<'T> tcs.Task
    
    /// Used to de-sugar try .. finally .. blocks in Computation Expressions.
    let inline tryFinally ([<InlineIfLambda>]compensation : unit -> unit) ([<InlineIfLambda>]body: unit -> ValueTask<'T>) : ValueTask<'T> =
        let mutable ran = false
        let compensation () =
            if not ran then
                ran <- true
                compensation ()
        try
            let task = body ()
            if task.IsCompleted then compensation (); task
            else
                let t = task.AsTask()
                t.ContinueWith(fun (_: Task<'T>) -> compensation (); t).Unwrap () |> ValueTask<'T>
        with _ ->
            compensation ()
            reraise ()

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

#endif