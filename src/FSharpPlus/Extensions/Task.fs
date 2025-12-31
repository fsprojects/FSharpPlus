namespace FSharpPlus

#nowarn "44" // Suppress obsolete warning for tryWith and tryFinally
#if !FABLE_COMPILER

/// Additional operations on Task<'T>
[<RequireQualifiedAccess>]
module Task =

    open System
    open System.Threading
    open System.Threading.Tasks
    open FSharpPlus.Internals.Errors
    
    /// Active pattern to match the state of a completed Task
    let inline internal (|Succeeded|Canceled|Faulted|) (t: Task<'a>) =
        if t.IsCompletedSuccessfully then Succeeded t.Result
        elif t.IsFaulted then Faulted (Unchecked.nonNull (t.Exception))
        elif t.IsCanceled then Canceled
        else invalidOp "Internal error: The task is not yet completed."

    let inline private continueWith ([<InlineIfLambda>]f) (x: Task<'t>) =
        if x.IsCompleted then f x
        else x.ConfigureAwait(false).GetAwaiter().UnsafeOnCompleted (fun () -> f x)


    /// <summary>Creates a Task that's completed successfully with the specified value.</summary>
    /// <param name="value"></param>
    /// <returns>A Task that is completed successfully with the specified value.</returns>
    let result (value: 'T) : Task<'T> = Task.FromResult value
    
    /// <summary>Creates a Task that's completed unsuccessfully with the specified exceptions.</summary>
    /// <param name="exn">The AggregateException to be raised.</param>
    /// <returns>A Task that is completed unsuccessfully with the specified exceptions.</returns>
    /// <remarks>
    /// Prefer this function to handle AggregateExceptions over Task.FromException as it handles them correctly.
    /// </remarks>
    let internal FromExceptions<'T> (aex: AggregateException) : Task<'T> =
        match aex with
        | agg when agg.InnerExceptions.Count = 1 -> Task.FromException<'T> agg.InnerExceptions[0]
        | agg ->
            let tcs = TaskCompletionSource<'T> ()
            tcs.SetException agg.InnerExceptions
            tcs.Task

    let private cancellationTokenSingleton = CancellationToken true

    /// <summary>Creates a Task that's canceled.</summary>
    /// <returns>A Task that's canceled.</returns>
    let canceled<'T> : Task<'T> = Task.FromCanceled<'T> cancellationTokenSingleton
    
    
    /// <summary>Creates a task workflow from 'source' workflow, mapping its result with 'mapper'.</summary>
    /// <param name="mapper">The mapping function.</param>
    /// <param name="source">The source task workflow.</param>
    /// <returns>The resulting task workflow.</returns>
    let map (mapper: 'T -> 'U) (source: Task<'T>) : Task<'U> =
        let source = nullArgCheck (nameof source) source

        backgroundTask {
            let! r = source
            return mapper r
        }

    /// <summary>Creates a task workflow from two workflows 'task1' and 'task2', mapping its results with 'mapper'.</summary>
    /// <remarks>Workflows are run in sequence.</remarks>
    /// <param name="mapper">The mapping function.</param>
    /// <param name="task1">First task workflow.</param>
    /// <param name="task2">Second task workflow.</param>
    let lift2 (mapper: 'T1 -> 'T2 -> 'U) (task1: Task<'T1>) (task2: Task<'T2>) : Task<'U> =
        let x = nullArgCheck (nameof task1) task1
        let y = nullArgCheck (nameof task2) task2

        if x.IsCompleted && y.IsCompleted then
            match x, y with
            | Succeeded r1, Succeeded r2 -> try result (mapper r1 r2) with e -> Task.FromException<_> e
            | Succeeded _ , Faulted exn  -> FromExceptions exn
            | Succeeded _ , Canceled     -> canceled
            | Faulted exn  , _           -> FromExceptions exn
            | Canceled    , _            -> canceled
        else
            let tcs = TaskCompletionSource<'U> ()

            match x.Status, y.Status with
            | TaskStatus.Canceled, _ -> tcs.SetCanceled ()
            | TaskStatus.Faulted, _  -> tcs.SetException (Unchecked.nonNull x.Exception).InnerExceptions
            | _, TaskStatus.Canceled -> tcs.SetCanceled ()
            | _, TaskStatus.Faulted  -> tcs.SetException (Unchecked.nonNull y.Exception).InnerExceptions
            | TaskStatus.RanToCompletion, _ ->
                y |> continueWith (function
                    | Canceled    -> tcs.SetCanceled ()
                    | Faulted e   -> tcs.SetException e.InnerExceptions
                    | Succeeded r -> try tcs.SetResult (mapper x.Result r) with e -> tcs.SetException e)
            | _, TaskStatus.RanToCompletion ->
                x |> continueWith (function
                    | Canceled    -> tcs.SetCanceled ()
                    | Faulted e   -> tcs.SetException e.InnerExceptions
                    | Succeeded r -> try tcs.SetResult (mapper r y.Result) with e -> tcs.SetException e)
            | _, _ ->
                x |> continueWith (function
                    | Canceled    -> tcs.SetCanceled ()
                    | Faulted e   -> tcs.SetException e.InnerExceptions
                    | Succeeded r ->
                        y |> continueWith (function
                            | Canceled     -> tcs.SetCanceled ()
                            | Faulted e    -> tcs.SetException e.InnerExceptions
                            | Succeeded r' -> try tcs.SetResult (mapper r r') with e -> tcs.SetException e
                        ))
            tcs.Task

    /// <summary>Creates a task workflow from three workflows 'x', 'y' and z, mapping its results with 'f'.</summary>
    /// <remarks>Workflows are run in sequence.</remarks>
    /// <param name="f">The mapping function.</param>
    /// <param name="x">First task workflow.</param>
    /// <param name="y">Second task workflow.</param>
    /// <param name="z">Third task workflow.</param>
    let lift3 (f : 'T1 -> 'T2 -> 'T3 -> 'U) (x : Task<'T1>) (y : Task<'T2>) (z: Task<'T3>) : Task<'U> =
        let x = nullArgCheck (nameof x) x
        let y = nullArgCheck (nameof y) y
        let z = nullArgCheck (nameof z) z

        if x.Status = TaskStatus.RanToCompletion && y.Status = TaskStatus.RanToCompletion && z.Status = TaskStatus.RanToCompletion then
            try result (f x.Result y.Result z.Result)
            with e ->
                let tcs = TaskCompletionSource<'U> ()
                tcs.SetException e
                tcs.Task
        else
            let tcs = TaskCompletionSource<'U> ()
            match x.Status, y.Status, z.Status with
            | TaskStatus.Canceled, _                  , _                   -> tcs.SetCanceled ()
            | TaskStatus.Faulted , _                  , _                   -> tcs.SetException (Unchecked.nonNull x.Exception).InnerExceptions
            | _                  , TaskStatus.Canceled, _                   -> tcs.SetCanceled ()
            | _                  , TaskStatus.Faulted , _                   -> tcs.SetException (Unchecked.nonNull y.Exception).InnerExceptions
            | _                  , _                  , TaskStatus.Canceled -> tcs.SetCanceled ()
            | _                  , _                  , TaskStatus.Faulted  -> tcs.SetException (Unchecked.nonNull z.Exception).InnerExceptions
            | _                  , _                  , _                   ->
                x |> continueWith (
                    function
                    | Canceled    -> tcs.SetCanceled ()
                    | Faulted e   -> tcs.SetException e.InnerExceptions
                    | Succeeded r ->
                        y |> continueWith (
                            function
                            | Canceled     -> tcs.SetCanceled ()
                            | Faulted e    -> tcs.SetException e.InnerExceptions
                            | Succeeded r' ->
                                z |> continueWith (
                                    function
                                    | Canceled      -> tcs.SetCanceled ()
                                    | Faulted e     -> tcs.SetException e.InnerExceptions
                                    | Succeeded r'' ->
                                        try tcs.SetResult (f r r' r'')
                                        with e -> tcs.SetException e
                                )))
            tcs.Task

    /// <summary>Creates a Task workflow from two workflows, mapping its results with a specified function.</summary>
    /// <remarks>Similar to lift2 but although workflows are started in sequence they might end independently in different order
    /// and all errors are collected.
    /// </remarks>
    /// <param name="mapper">The mapping function.</param>
    /// <param name="task1">First Task workflow.</param>
    /// <param name="task2">Second Task workflow.</param>
    let map2 mapper (task1: Task<'T1>) (task2: Task<'T2>) : Task<'U> =
        let task1 = nullArgCheck (nameof task1) task1
        let task2 = nullArgCheck (nameof task2) task2

        if task1.Status = TaskStatus.RanToCompletion && task2.Status = TaskStatus.RanToCompletion then
            try result (mapper task1.Result task2.Result)
            with e ->
                let tcs = TaskCompletionSource<_> ()
                tcs.SetException e
                tcs.Task
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
            | Faulted aex -> failures[i] <- aex.InnerExceptions
            | Canceled    -> cancelled <- true
            trySet ()

        if task1.IsCompleted && task2.IsCompleted then
            k r1 0 task1
            k r2 1 task2
        else
            continueWith (k r1 0) task1
            continueWith (k r2 1) task2
        tcs.Task

    /// <summary>Creates a Task workflow from three workflows, mapping its results with a specified function.</summary>
    /// <remarks>Similar to lift3 but although workflows are started in sequence they might end independently in different order
    /// and all errors are collected.
    /// </remarks>
    /// <param name="mapper">The mapping function.</param>
    /// <param name="task1">First Task workflow.</param>
    /// <param name="task2">Second Task workflow.</param>
    /// <param name="task3">Third Task workflow.</param>
    let map3 mapper (task1: Task<'T1>) (task2: Task<'T2>) (task3: Task<'T3>) : Task<'U> =
        let task1 = nullArgCheck (nameof task1) task1
        let task2 = nullArgCheck (nameof task2) task2
        let task3 = nullArgCheck (nameof task3) task3

        if task1.Status = TaskStatus.RanToCompletion && task2.Status = TaskStatus.RanToCompletion && task3.Status = TaskStatus.RanToCompletion then
            try result (mapper task1.Result task2.Result task3.Result)
            with e ->
                let tcs = TaskCompletionSource<_> ()
                tcs.SetException e
                tcs.Task
        else
        let tcs = TaskCompletionSource<_> ()
        let r1 = ref Unchecked.defaultof<_>
        let r2 = ref Unchecked.defaultof<_>
        let r3 = ref Unchecked.defaultof<_>
        let mutable cancelled = false
        let failures = [|IReadOnlyCollection.empty<exn>; IReadOnlyCollection.empty; IReadOnlyCollection.empty|]
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
            | Faulted axn -> failures[i] <- axn.InnerExceptions
            | Canceled    -> cancelled <- true
            trySet ()

        if task1.IsCompleted && task2.IsCompleted && task3.IsCompleted then
            k r1 0 task1
            k r2 1 task2
            k r3 2 task3
        else
            continueWith (k r1 0) task1
            continueWith (k r2 1) task2
            continueWith (k r3 2) task3
        tcs.Task

    /// <summary>Creates a task workflow that is the result of applying the resulting function of a task workflow
    /// to the resulting value of another task workflow</summary>
    /// <param name="f">Task workflow returning a function</param>
    /// <param name="x">Task workflow returning a value</param>
    let apply (f: Task<'T->'U>) (x: Task<'T>) : Task<'U> =
        let f = nullArgCheck (nameof f) f
        let x = nullArgCheck (nameof x) x

        if f.Status = TaskStatus.RanToCompletion && x.Status = TaskStatus.RanToCompletion then
            try result (f.Result x.Result)
            with e ->
                let tcs = TaskCompletionSource<'U> ()
                tcs.SetException e
                tcs.Task
        else
            let tcs = TaskCompletionSource<'U> ()
            match f.Status, x.Status with
            | TaskStatus.Canceled, _ -> tcs.SetCanceled ()
            | TaskStatus.Faulted, _  -> tcs.SetException (Unchecked.nonNull f.Exception).InnerExceptions
            | _, TaskStatus.Canceled -> tcs.SetCanceled ()
            | _, TaskStatus.Faulted  -> tcs.SetException (Unchecked.nonNull x.Exception).InnerExceptions
            | TaskStatus.RanToCompletion, _ ->
                let k = function
                    | Canceled    -> tcs.SetCanceled ()
                    | Faulted e   -> tcs.SetException e.InnerExceptions
                    | Succeeded r ->
                        try tcs.SetResult (f.Result r)
                        with e -> tcs.SetException e
                continueWith k x
            | _, TaskStatus.RanToCompletion ->
                let k = function
                    | Canceled    -> tcs.SetCanceled ()
                    | Faulted e   -> tcs.SetException e.InnerExceptions
                    | Succeeded r ->
                        try tcs.SetResult (r x.Result)
                        with e -> tcs.SetException e
                continueWith k f
            | _, _ ->
                f |> continueWith (
                    function
                    | Canceled    -> tcs.SetCanceled ()
                    | Faulted e   -> tcs.SetException e.InnerExceptions
                    | Succeeded r ->
                        x |> continueWith (
                            function
                            | Canceled     -> tcs.SetCanceled ()
                            | Faulted e    -> tcs.SetException e.InnerExceptions
                            | Succeeded r' ->
                                try tcs.SetResult (r r')
                                with e -> tcs.SetException e
                        ))
            tcs.Task

    /// <summary>Creates a task workflow from two workflows 'x' and 'y', tupling its results.</summary>
    let zipSequentially (x: Task<'T>) (y: Task<'U>) : Task<'T * 'U> =
        let x = nullArgCheck (nameof x) x
        let y = nullArgCheck (nameof y) y

        if x.Status = TaskStatus.RanToCompletion && y.Status = TaskStatus.RanToCompletion then
            result (x.Result, y.Result)
        else
            let tcs = TaskCompletionSource<'T * 'U> ()
            match x.Status, y.Status with
            | TaskStatus.Canceled, _ -> tcs.SetCanceled ()
            | TaskStatus.Faulted, _  -> tcs.SetException (Unchecked.nonNull x.Exception).InnerExceptions
            | _, TaskStatus.Canceled -> tcs.SetCanceled ()
            | _, TaskStatus.Faulted  -> tcs.SetException (Unchecked.nonNull y.Exception).InnerExceptions
            | TaskStatus.RanToCompletion, _ ->
                let k = function
                    | Canceled    -> tcs.SetCanceled ()
                    | Faulted e   -> tcs.SetException e.InnerExceptions
                    | Succeeded r -> tcs.SetResult (x.Result, r)
                continueWith k y
            | _, TaskStatus.RanToCompletion ->
                let k = function
                    | Canceled    -> tcs.SetCanceled ()
                    | Faulted e   -> tcs.SetException e.InnerExceptions
                    | Succeeded r -> tcs.SetResult (r, y.Result)
                continueWith k x
            | _, _ ->
                x |> continueWith (
                    function
                    | Canceled    -> tcs.SetCanceled ()
                    | Faulted e   -> tcs.SetException e.InnerExceptions
                    | Succeeded r ->
                        y |> continueWith (function
                            | Canceled     -> tcs.SetCanceled ()
                            | Faulted e    -> tcs.SetException e.InnerExceptions
                            | Succeeded r' -> tcs.SetResult (r, r')))
            tcs.Task

    /// <summary>Creates a task workflow from two workflows 'task1' and 'task2', tupling its results.</summary>
    /// <remarks>Similar to zipSequentially but although workflows are started in sequence they might end independently in different order
    /// and all errors are collected.
    /// </remarks>
    let zip (task1: Task<'T1>) (task2: Task<'T2>) = map2 (fun x y -> x, y) task1 task2

    /// <summary>Creates a task workflow from two workflows 'task1', 'task2' and 'task3', tupling its results.</summary>
    /// <remarks>Similar to zipSequentially but although workflows are started in sequence they might end independently in different order
    /// and all errors are collected.
    /// </remarks>
    let zip3 (task1: Task<'T1>) (task2: Task<'T2>) (task3: Task<'T3>) = map3 (fun x y z -> x, y, z) task1 task2 task3

    /// Flattens two nested tasks into one.
    let join (source: Task<Task<'T>>) : Task<'T> =
        let source = nullArgCheck (nameof source) source

        backgroundTask {
            let! inner = source
            return! inner
        }
    
    /// <summary>Creates a task workflow from 'source' workflow, mapping and flattening its result with 'f'.</summary>
    let bind (f: 'T -> Task<'U>) (source: Task<'T>) : Task<'U> =
        let source = nullArgCheck (nameof source) source

        backgroundTask {
            let! r = source
            return! f r
        }
    
    /// <summary>Creates a task that ignores the result of the source task.</summary>
    /// <param name="source">The source Task.</param>
    /// <returns>A Task that completes when the source completes.</returns>
    /// <remarks>It can be used to convert non-generic Task to unit Task.</remarks>
    let ignore (source: Task) =
        let source = nullArgCheck (nameof source) source

        if source.IsCompletedSuccessfully then result ()
        elif source.IsFaulted  then FromExceptions (Unchecked.nonNull source.Exception)
        elif source.IsCanceled then canceled
        else
            let tcs = TaskCompletionSource<unit> ()
            let k (t: Task) : unit =
                if t.IsCanceled  then tcs.SetCanceled ()
                elif t.IsFaulted then tcs.SetException (Unchecked.nonNull source.Exception).InnerExceptions
                else tcs.SetResult ()
            source.ContinueWith k |> ignore
            tcs.Task

    [<ObsoleteAttribute("Swap parameters")>]
    let tryWith (body: unit -> Task<'T>) (compensation: exn -> Task<'T>) : Task<'T> = backgroundTask {
        try return! body ()
        with e -> return! compensation e }
    
    [<ObsoleteAttribute("Swap parameters")>]
    let tryFinally (body: unit -> Task<'T>) (compensation : unit -> unit) : Task<'T> = backgroundTask {
        try return! body ()
        finally compensation () }

    /// Used to de-sugar use .. blocks in Computation Expressions.
    let using (disp: 'T when 'T :> IDisposable) (body: 'T -> Task<'U>) =
        tryFinally
            (fun () -> body disp)
            (fun () -> if not (isNull (box disp)) then disp.Dispose ())

    /// <summary>Returns <paramref name="source"/> if it is not faulted, otherwise evaluates <paramref name="fallbackThunk"/> and returns the result.</summary>
    ///
    /// <param name="fallbackThunk">A thunk that provides an alternate task computation when evaluated.</param>
    /// <param name="source">The input task.</param>
    ///
    /// <returns>The task if it is not faulted, else the result of evaluating <paramref name="fallbackThunk"/>.</returns>
    /// <remarks><paramref name="fallbackThunk"/> is not evaluated unless <paramref name="source"/> is faulted.</remarks>
    ///
    let inline orElseWith ([<InlineIfLambda>]fallbackThunk: exn -> Task<'T>) (source: Task<'T>) : Task<'T> =
        let source = nullArgCheck (nameof source) source

        tryWith (fun () -> source) fallbackThunk

    /// <summary>Returns <paramref name="source"/> if it is not faulted, otherwise e<paramref name="fallbackTask"/>.</summary>
    ///
    /// <param name="fallbackTask">The alternative Task to use if <paramref name="source"/> is faulted.</param>
    /// <param name="source">The input task.</param>
    ///
    /// <returns>The option if the option is Some, else the alternate option.</returns>
    let orElse (fallbackTask: Task<'T>) (source: Task<'T>) : Task<'T> =
        let fallbackTask = nullArgCheck (nameof fallbackTask) fallbackTask
        let source       = nullArgCheck (nameof source)       source

        orElseWith (fun _ -> fallbackTask) source
    
    /// <summary>Creates a Task that's completed unsuccessfully with the specified exception.</summary>
    /// <param name="exn">The exception to be raised.</param>
    /// <returns>A Task that is completed unsuccessfully with the specified exception.</returns>
    let raise<'T> (exn: exn) : Task<'T> = Task.FromException<'T> exn


/// Workaround to fix signatures without breaking binary compatibility.
[<AutoOpen>]
module Task_v2 =
    open System.Threading.Tasks
    module Task =

        /// <summary>Runs a if the body throws an exception, if the returned task faults or if the returned task is canceled.</summary>
        /// <param name="compensation">The compensation function to run on exception.</param>
        /// <param name="body">The body function to run.</param>
        /// <returns>The resulting task.</returns>
        /// <remarks>This function is used to de-sugar try .. with .. blocks in Computation Expressions.</remarks>
        let inline tryWith ([<InlineIfLambda>] compensation: exn -> Task<'T>) ([<InlineIfLambda>]body: unit -> Task<'T>) = Task.tryWith body compensation

        /// <summary>Runs a compensation function after the body completes, regardless of whether the body completed successfully, faulted, or was canceled.</summary>
        /// <param name="compensation">The compensation function to run after the body completes.</param>
        /// <param name="body">The body function to run.</param>
        /// <returns>The resulting task.</returns>
        /// <remarks>This function is used to de-sugar try .. finally .. blocks in Computation Expressions.</remarks>
        let inline tryFinally ([<InlineIfLambda>] compensation: unit -> unit) ([<InlineIfLambda>]body: unit -> Task<'T>) = Task.tryFinally body compensation

#endif
