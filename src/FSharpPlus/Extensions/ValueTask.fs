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

    let inline continueWith (x: ValueTask<'t>) f =
        if x.IsCompleted then f x
        else x.ConfigureAwait(false).GetAwaiter().UnsafeOnCompleted (fun () -> f x)

    /// Creates a ValueTask from a value
    let result (value: 'T) : ValueTask<'T> =
    #if NET5_0_OR_GREATER
        ValueTask.FromResult value
    #else
        let tcs = TaskCompletionSource<'T> ()
        tcs.SetResult value
        tcs.Task |> ValueTask<'T>
    #endif

    /// <summary>Creates a ValueTask workflow from 'source' another, mapping its result with 'f'.</summary>
    /// <param name="f">The mapping function.</param>
    /// <param name="source">ValueTask workflow.</param>
    let map (f: 'T -> 'U) (source: ValueTask<'T>) : ValueTask<'U> =
        let tcs = TaskCompletionSource<'U> ()
        continueTask tcs source (fun x ->
            try tcs.SetResult (f x)
            with e -> tcs.SetException e)
        tcs.Task |> ValueTask<'U>


    /// <summary>Creates a ValueTask workflow from two workflows 'x' and 'y', mapping its results with 'f'.</summary>
    /// <remarks>Workflows are run in sequence.</remarks>
    /// <param name="f">The mapping function.</param>
    /// <param name="x">First ValueTask workflow.</param>
    /// <param name="y">Second ValueTask workflow.</param>
    let lift2 (f: 'T -> 'U -> 'V) (x: ValueTask<'T>) (y: ValueTask<'U>) : ValueTask<'V> =
        let tcs = TaskCompletionSource<'V> ()
        continueTask tcs x (fun x ->
            continueTask tcs y (fun y ->
                try tcs.SetResult (f x y)
                with e -> tcs.SetException e))
        tcs.Task |> ValueTask<'V>
        
    /// <summary>Creates a ValueTask workflow from three workflows 'x', 'y' and z, mapping its results with 'f'.</summary>
    /// <remarks>Workflows are run in sequence.</remarks>
    /// <param name="f">The mapping function.</param>
    /// <param name="x">First ValueTask workflow.</param>
    /// <param name="y">Second ValueTask workflow.</param>
    /// <param name="z">Third ValueTask workflow.</param>
    let lift3 (f: 'T -> 'U -> 'V -> 'W) (x: ValueTask<'T>) (y: ValueTask<'U>) (z: ValueTask<'V>) : ValueTask<'W> =
        let tcs = TaskCompletionSource<'W> ()
        continueTask tcs x (fun x ->
            continueTask tcs y (fun y ->
                continueTask tcs z (fun z ->
                    try tcs.SetResult (f x y z)
                    with e -> tcs.SetException e)))
        tcs.Task |> ValueTask<'W>

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
            continueWith task1 (k r1 0)
            continueWith task2 (k r2 1)
        tcs.Task |> ValueTask<'U>

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
                tcs.Task |> ValueTask<'U>
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
            continueWith task1 (k r1 0)
            continueWith task2 (k r2 1)
            continueWith task3 (k r3 2)
        tcs.Task |> ValueTask<'U>

    /// <summary>Creates a ValueTask workflow that is the result of applying the resulting function of a ValueTask workflow
    /// to the resulting value of another ValueTask workflow</summary>
    /// <param name="f">ValueTask workflow returning a function</param>
    /// <param name="x">ValueTask workflow returning a value</param>
    let apply (f: ValueTask<'T->'U>) (x: ValueTask<'T>) : ValueTask<'U> =
        let tcs = TaskCompletionSource<'U> ()
        continueTask tcs f (fun f ->
            continueTask tcs x (fun x ->
                try tcs.SetResult (f x)
                with e -> tcs.SetException e))
        tcs.Task |> ValueTask<'U>

    /// <summary>Creates a ValueTask workflow from two workflows 'x' and 'y', tupling its results.</summary>
    let zipSequentially (x: ValueTask<'T>) (y: ValueTask<'U>) : ValueTask<'T * 'U> =
        let tcs = TaskCompletionSource<'T * 'U> ()
        continueTask tcs x (fun x ->
            continueTask tcs y (fun y ->
                tcs.SetResult (x, y)))
        tcs.Task |> ValueTask<'T * 'U>

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
        let tcs = TaskCompletionSource<'T> ()
        continueTask tcs source (fun x ->
            continueTask tcs x (fun x ->
                tcs.SetResult x))
        tcs.Task |> ValueTask<'T>
    
    
    /// <summary>Creates a ValueTask workflow from 'source' workflow, mapping and flattening its result with 'f'.</summary>
    let bind (f: 'T -> ValueTask<'U>) (source: ValueTask<'T>) : ValueTask<'U> =
        let tcs = TaskCompletionSource<'U> ()
        continueTask tcs source (fun x ->
            try 
                continueTask tcs (f x) (fun fx ->
                    tcs.SetResult fx)
            with e -> tcs.SetException e)
        tcs.Task |> ValueTask<'U>
            
    /// <summary>Creates a ValueTask that ignores the result of the source ValueTask.</summary>
    /// <remarks>It can be used to convert non-generic ValueTask to unit ValueTask.</remarks>
    let ignore (source: ValueTask) : ValueTask<unit> =
        if source.IsCompletedSuccessfully then Unchecked.defaultof<_>
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
            tcs.Task |> ValueTask<unit>

    /// Used to de-sugar try .. with .. blocks in Computation Expressions.
    let inline tryWith ([<InlineIfLambda>]compensation: exn -> ValueTask<'T>) ([<InlineIfLambda>]body: unit -> ValueTask<'T>) : ValueTask<'T> =
        let unwrapException (agg: AggregateException) =
            if agg.InnerExceptions.Count = 1 then agg.InnerExceptions.[0]
            else agg :> Exception
        try
            let task = body ()
            if task.IsCompleted then
                match task with
                | Succeeded _ -> task
                | Faulted exn -> compensation (unwrapException exn)
                | Canceled    -> compensation (TaskCanceledException ())
            else
                let tcs = TaskCompletionSource<'T> ()
                let f = function
                    | Succeeded r -> tcs.SetResult r
                    | Faulted exn -> continueTask tcs (compensation (unwrapException exn))      (fun r -> try tcs.SetResult r with e -> tcs.SetException e)
                    | Canceled    -> continueTask tcs (compensation (TaskCanceledException ())) (fun r -> try tcs.SetResult r with e -> tcs.SetException e)
                task.ConfigureAwait(false).GetAwaiter().UnsafeOnCompleted (fun () -> f task)
                ValueTask<'T> tcs.Task
        with
        | :? AggregateException as exn -> compensation (unwrapException exn)
        | exn                          -> compensation exn
    
    /// Used to de-sugar try .. finally .. blocks in Computation Expressions.
    let inline tryFinally ([<InlineIfLambda>]compensation : unit -> unit) ([<InlineIfLambda>]body: unit -> ValueTask<'T>) : ValueTask<'T> =
        let mutable ran = false
        let compensation () =
            if not ran then
                compensation ()
                ran <- true
        try
            let task = body ()
            if task.IsCompleted then compensation (); task
            else
                let tcs = TaskCompletionSource<'T> ()
                let f = function
                | Succeeded r -> tcs.SetResult r
                | Faulted exn -> tcs.SetException exn.InnerExceptions
                | Canceled    -> tcs.SetCanceled ()
                task.ConfigureAwait(false).GetAwaiter().UnsafeOnCompleted (fun () -> compensation (); f task)
                ValueTask<'T> tcs.Task
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

    /// Raises an exception in the ValueTask
    let raise<'TResult> (``exception``: exn) = ValueTask<'TResult> (Task.FromException<'TResult> ``exception``)

#endif