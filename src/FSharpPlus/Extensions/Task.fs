namespace FSharpPlus

#if !FABLE_COMPILER

/// Additional operations on Task<'T>
[<RequireQualifiedAccess>]
module Task =

    open System
    open System.Threading
    open System.Threading.Tasks
    
    let private (|Canceled|Faulted|Completed|) (t: Task<'a>) =
        if t.IsCanceled then Canceled
        else if t.IsFaulted then Faulted t.Exception
        else Completed t.Result

    /// <summary>Creates a task workflow from 'source' another, mapping its result with 'f'.</summary>
    let map (f: 'T -> 'U) (source: Task<'T>) : Task<'U> =
        if source.Status = TaskStatus.RanToCompletion then
            try Task.FromResult (f source.Result)
            with e ->
                let tcs = TaskCompletionSource<'U> ()
                tcs.SetException e
                tcs.Task
        else
            let tcs = TaskCompletionSource<'U> ()
            if source.Status = TaskStatus.Faulted then
                tcs.SetException source.Exception.InnerExceptions
                tcs.Task
            elif source.Status = TaskStatus.Canceled then
                tcs.SetCanceled ()
                tcs.Task
            else
                let k = function
                    | Canceled    -> tcs.SetCanceled ()
                    | Faulted e   -> tcs.SetException e.InnerExceptions
                    | Completed r ->
                        try tcs.SetResult (f r)
                        with e -> tcs.SetException e
                source.ContinueWith k |> ignore
                tcs.Task

    /// <summary>Creates a task workflow from two workflows 'x' and 'y', mapping its results with 'f'.</summary>
    /// <remarks>Workflows are run in sequence.</remarks>
    /// <param name="f">The mapping function.</param>
    /// <param name="x">First task workflow.</param>
    /// <param name="y">Second task workflow.</param>
    let lift2 (f: 'T -> 'U -> 'V) (x: Task<'T>) (y: Task<'U>) : Task<'V> =
        if x.Status = TaskStatus.RanToCompletion && y.Status = TaskStatus.RanToCompletion then
            try Task.FromResult (f x.Result y.Result)
            with e ->
                let tcs = TaskCompletionSource<'V> ()
                tcs.SetException e
                tcs.Task
        else
            let tcs = TaskCompletionSource<'V> ()
            match x.Status, y.Status with
            | TaskStatus.Canceled, _ -> tcs.SetCanceled ()
            | TaskStatus.Faulted, _  -> tcs.SetException x.Exception.InnerExceptions
            | _, TaskStatus.Canceled -> tcs.SetCanceled ()
            | _, TaskStatus.Faulted  -> tcs.SetException y.Exception.InnerExceptions
            | TaskStatus.RanToCompletion, _ ->
                let k = function
                    | Canceled    -> tcs.SetCanceled ()
                    | Faulted e   -> tcs.SetException e.InnerExceptions
                    | Completed r ->
                        try tcs.SetResult (f x.Result r)
                        with e -> tcs.SetException e
                y.ContinueWith k |> ignore
            | _, TaskStatus.RanToCompletion ->
                let k = function
                    | Canceled    -> tcs.SetCanceled ()
                    | Faulted e   -> tcs.SetException e.InnerExceptions
                    | Completed r ->
                        try tcs.SetResult (f r y.Result)
                        with e -> tcs.SetException e
                x.ContinueWith k |> ignore
            | _, _ ->
                x.ContinueWith (
                    function
                    | Canceled    -> tcs.SetCanceled ()
                    | Faulted e   -> tcs.SetException e.InnerExceptions
                    | Completed r ->
                        y.ContinueWith (
                            function
                            | Canceled     -> tcs.SetCanceled ()
                            | Faulted e    -> tcs.SetException e.InnerExceptions
                            | Completed r' ->
                                try tcs.SetResult (f r r')
                                with e -> tcs.SetException e
                        ) |> ignore) |> ignore
            tcs.Task

    /// <summary>Creates a task workflow from three workflows 'x', 'y' and z, mapping its results with 'f'.</summary>
    /// <remarks>Workflows are run in sequence.</remarks>
    /// <param name="f">The mapping function.</param>
    /// <param name="x">First task workflow.</param>
    /// <param name="y">Second task workflow.</param>
    /// <param name="z">Third task workflow.</param>
    let lift3 (f : 'T -> 'U -> 'V -> 'W) (x : Task<'T>) (y : Task<'U>) (z: Task<'V>) : Task<'W> =
        if x.Status = TaskStatus.RanToCompletion && y.Status = TaskStatus.RanToCompletion && z.Status = TaskStatus.RanToCompletion then
            try Task.FromResult (f x.Result y.Result z.Result)
            with e ->
                let tcs = TaskCompletionSource<'W> ()
                tcs.SetException e
                tcs.Task
        else
            let tcs = TaskCompletionSource<'W> ()
            match x.Status, y.Status, z.Status with
            | TaskStatus.Canceled, _                  , _                   -> tcs.SetCanceled ()
            | TaskStatus.Faulted , _                  , _                   -> tcs.SetException x.Exception.InnerExceptions
            | _                  , TaskStatus.Canceled, _                   -> tcs.SetCanceled ()
            | _                  , TaskStatus.Faulted , _                   -> tcs.SetException y.Exception.InnerExceptions
            | _                  , _                  , TaskStatus.Canceled -> tcs.SetCanceled ()
            | _                  , _                  , TaskStatus.Faulted  -> tcs.SetException z.Exception.InnerExceptions
            | _                  , _                  , _                   ->
                x.ContinueWith (
                    function
                    | Canceled    -> tcs.SetCanceled ()
                    | Faulted e   -> tcs.SetException e.InnerExceptions
                    | Completed r ->
                        y.ContinueWith (
                            function
                            | Canceled     -> tcs.SetCanceled ()
                            | Faulted e    -> tcs.SetException e.InnerExceptions
                            | Completed r' ->
                                z.ContinueWith (
                                    function
                                    | Canceled      -> tcs.SetCanceled ()
                                    | Faulted e     -> tcs.SetException e.InnerExceptions
                                    | Completed r'' ->
                                        try tcs.SetResult (f r r' r'')
                                        with e -> tcs.SetException e
                                ) |> ignore) |> ignore) |> ignore
            tcs.Task

    /// <summary>Creates a Task workflow from two workflows, mapping its results with a specified function.</summary>
    /// <remarks>Similar to lift2 but although workflows are started in sequence they might end independently in different order
    /// and all errors are collected.
    /// </remarks>
    /// <param name="mapper">The mapping function.</param>
    /// <param name="task1">First Task workflow.</param>
    /// <param name="task2">Second Task workflow.</param>
    let map2 mapper (task1: Task<'T1>) (task2: Task<'T2>) : Task<'U> =
        if task1.Status = TaskStatus.RanToCompletion && task2.Status = TaskStatus.RanToCompletion then
            try Task.FromResult (mapper task1.Result task2.Result)
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
            | Canceled    -> cancelled <- true
            | Faulted e   -> failures[i] <- e.InnerExceptions
            | Completed r -> v.Value <- r
            trySet ()

        if task1.IsCompleted && task2.IsCompleted then
            task1 |> k r1 0
            task2 |> k r2 1
        else
            task1.ContinueWith (k r1 0) |> ignore
            task2.ContinueWith (k r2 1) |> ignore
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
        if task1.Status = TaskStatus.RanToCompletion && task2.Status = TaskStatus.RanToCompletion && task3.Status = TaskStatus.RanToCompletion then
            try Task.FromResult (mapper task1.Result task2.Result task3.Result)
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
            | Canceled    -> cancelled <- true
            | Faulted e   -> failures[i] <- e.InnerExceptions
            | Completed r -> v.Value <- r
            trySet ()

        if task1.IsCompleted && task2.IsCompleted && task3.IsCompleted then
            task1 |> k r1 0
            task2 |> k r2 1
            task3 |> k r3 2
        else
            task1.ContinueWith (k r1 0) |> ignore
            task2.ContinueWith (k r2 1) |> ignore
            task3.ContinueWith (k r3 2) |> ignore
        tcs.Task

    /// <summary>Creates a task workflow that is the result of applying the resulting function of a task workflow
    /// to the resulting value of another task workflow</summary>
    /// <param name="f">Task workflow returning a function</param>
    /// <param name="x">Task workflow returning a value</param>
    let apply (f: Task<'T->'U>) (x: Task<'T>) : Task<'U> =
        if f.Status = TaskStatus.RanToCompletion && x.Status = TaskStatus.RanToCompletion then
            try Task.FromResult (f.Result x.Result)
            with e ->
                let tcs = TaskCompletionSource<'U> ()
                tcs.SetException e
                tcs.Task
        else
            let tcs = TaskCompletionSource<'U> ()
            match f.Status, x.Status with
            | TaskStatus.Canceled, _ -> tcs.SetCanceled ()
            | TaskStatus.Faulted, _  -> tcs.SetException f.Exception.InnerExceptions
            | _, TaskStatus.Canceled -> tcs.SetCanceled ()
            | _, TaskStatus.Faulted  -> tcs.SetException x.Exception.InnerExceptions
            | TaskStatus.RanToCompletion, _ ->
                let k = function
                    | Canceled    -> tcs.SetCanceled ()
                    | Faulted e   -> tcs.SetException e.InnerExceptions
                    | Completed r ->
                        try tcs.SetResult (f.Result r)
                        with e -> tcs.SetException e
                x.ContinueWith k |> ignore
            | _, TaskStatus.RanToCompletion ->
                let k = function
                    | Canceled    -> tcs.SetCanceled ()
                    | Faulted e   -> tcs.SetException e.InnerExceptions
                    | Completed r ->
                        try tcs.SetResult (r x.Result)
                        with e -> tcs.SetException e
                f.ContinueWith k |> ignore
            | _, _ ->
                f.ContinueWith (
                    function
                    | Canceled    -> tcs.SetCanceled ()
                    | Faulted e   -> tcs.SetException e.InnerExceptions
                    | Completed r ->
                        x.ContinueWith (
                            function
                            | Canceled     -> tcs.SetCanceled ()
                            | Faulted e    -> tcs.SetException e.InnerExceptions
                            | Completed r' ->
                                try tcs.SetResult (r r')
                                with e -> tcs.SetException e
                        ) |> ignore) |> ignore
            tcs.Task

    /// <summary>Creates a task workflow from two workflows 'x' and 'y', tupling its results.</summary>
    let zipSequentially (x: Task<'T>) (y: Task<'U>) : Task<'T * 'U> =
        if x.Status = TaskStatus.RanToCompletion && y.Status = TaskStatus.RanToCompletion then
            Task.FromResult (x.Result, y.Result)
        else
            let tcs = TaskCompletionSource<'T * 'U> ()
            match x.Status, y.Status with
            | TaskStatus.Canceled, _ -> tcs.SetCanceled ()
            | TaskStatus.Faulted, _  -> tcs.SetException x.Exception.InnerExceptions
            | _, TaskStatus.Canceled -> tcs.SetCanceled ()
            | _, TaskStatus.Faulted  -> tcs.SetException y.Exception.InnerExceptions
            | TaskStatus.RanToCompletion, _ ->
                let k = function
                    | Canceled    -> tcs.SetCanceled ()
                    | Faulted e   -> tcs.SetException e.InnerExceptions
                    | Completed r -> tcs.SetResult (x.Result, r)
                y.ContinueWith k |> ignore
            | _, TaskStatus.RanToCompletion ->
                let k = function
                    | Canceled    -> tcs.SetCanceled ()
                    | Faulted e   -> tcs.SetException e.InnerExceptions
                    | Completed r -> tcs.SetResult (r, y.Result)
                x.ContinueWith k |> ignore
            | _, _ ->
                x.ContinueWith (
                    function
                    | Canceled    -> tcs.SetCanceled ()
                    | Faulted e   -> tcs.SetException e.InnerExceptions
                    | Completed r ->
                        y.ContinueWith (function
                            | Canceled     -> tcs.SetCanceled ()
                            | Faulted e    -> tcs.SetException e.InnerExceptions
                            | Completed r' -> tcs.SetResult (r, r')) |> ignore) |> ignore
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
    let join (source: Task<Task<'T>>) : Task<'T> = source.Unwrap()
    
    /// <summary>Creates a task workflow from 'source' workflow, mapping and flattening its result with 'f'.</summary>
    let bind (f: 'T -> Task<'U>) (source: Task<'T>) : Task<'U> = source |> map f |> join
    
    /// <summary>Creates a task that ignores the result of the source task.</summary>
    /// <remarks>It can be used to convert non-generic Task to unit Task.</remarks>
    let ignore (task: Task) =
        if task.Status = TaskStatus.RanToCompletion then Task.FromResult ()
        else
            let tcs = TaskCompletionSource<unit> ()
            if task.Status = TaskStatus.Faulted then
                tcs.SetException task.Exception.InnerExceptions
            elif task.Status = TaskStatus.Canceled then
                tcs.SetCanceled ()
            else
                let k (t: Task) : unit =
                    if t.IsCanceled  then tcs.SetCanceled ()
                    elif t.IsFaulted then tcs.SetException t.Exception
                    else tcs.SetResult ()
                task.ContinueWith k |> ignore
            tcs.Task

    /// Used to de-sugar try .. with .. blocks in Computation Expressions.
    let rec tryWith (body: unit -> Task<'T>) (compensation: exn -> Task<'T>) : Task<'T> =
        let unwrapException (agg: AggregateException) =
            if agg.InnerExceptions.Count = 1 then agg.InnerExceptions.[0]
            else agg :> Exception
        try
            let task = body ()
            match task.Status with 
            | TaskStatus.RanToCompletion -> task
            | TaskStatus.Faulted         -> task.ContinueWith((fun (x:Task<'T>) -> compensation (unwrapException x.Exception))).Unwrap ()
            | TaskStatus.Canceled        -> task
            | _                          -> task.ContinueWith((fun (x:Task<'T>) -> tryWith (fun () -> x) compensation) ).Unwrap ()
        with
        | :? AggregateException as exn -> compensation (unwrapException exn)
        | exn                          -> compensation exn
    
    /// Used to de-sugar try .. finally .. blocks in Computation Expressions.
    let tryFinally (body: unit -> Task<'T>) (compensation : unit -> unit) : Task<'T> =
        let mutable ran = false
        let compensation () =
            if not ran then
                compensation ()
                ran <- true
        try
            let task = body ()
            let rec loop (task: Task<'T>) (compensation : unit -> unit) =
                match task.Status with
                | TaskStatus.RanToCompletion -> compensation (); task
                | TaskStatus.Faulted         -> task.ContinueWith((fun (x:Task<'T>) -> compensation (); x)).Unwrap ()
                | TaskStatus.Canceled        -> task
                | _                          -> task.ContinueWith((fun (x:Task<'T>) -> (loop x compensation: Task<_>))).Unwrap ()
            loop task compensation
        with _ ->
            compensation ()
            reraise ()

    /// Used to de-sugar use .. blocks in Computation Expressions.
    let using (disp: #IDisposable) (body: _ -> Task<'a>) =
        tryFinally
            (fun () -> body disp)
            (fun () -> if not (isNull (box disp)) then disp.Dispose ())

    /// Creates a Task from a value
    let result value = Task.FromResult value
    
    /// Raises an exception in the Task
    let raise<'T> (e: exn) =
        let tcs = TaskCompletionSource<'T> ()
        tcs.SetException e
        tcs.Task
#endif
