namespace FSharpPlus



#if NETSTANDARD2_1 && !FABLE_COMPILER

/// Additional operations on ValueTask<'T>
[<RequireQualifiedAccess>]
module ValueTasks =

    open System.Threading
    open System.Threading.Tasks
    
    let private (|Canceled|Faulted|Completed|) (t: ValueTask<'a>) =
        if t.IsCanceled then Canceled
        else if t.IsFaulted then Faulted (t.AsTask().Exception)
        else Completed t.Result
        
    let private (|TaskCanceled|TaskFaulted|TaskCompleted|) (t: Task<'a>) =
        if t.IsCanceled then TaskCanceled
        else if t.IsFaulted then TaskFaulted t.Exception
        else TaskCompleted t.Result
        
    let fromResult<'T> (result : 'T) =
        ValueTask<'T>(result)
        
    let fromException<'T> (e : exn) =
        ValueTask<'T>(Task.FromException<'T>(e))
    
    let fromCanceled<'T> (ct : CancellationToken) =
        ValueTask<'T>(Task.FromCanceled<'T>(ct))
        
    let fromTask<'T> (t : Task<'T>) =
        ValueTask<'T>(t)

    /// <summary>Creates a ValueTask workflow from 'source' another, mapping its result with 'f'.</summary>
    let map (f: 'T -> 'U) (source: ValueTask<'T>) : ValueTask<'U> =
        if source.IsCompletedSuccessfully then
            try fromResult (f source.Result)
            with exn ->
                fromException exn
        else
            if source.IsFaulted then
                fromException (source.AsTask().Exception) 
            elif source.IsCanceled then
                fromCanceled (CancellationToken(true))
            else
                let tcs = TaskCompletionSource<'U> ()
                let k = function
                    | TaskCanceled    -> tcs.SetCanceled ()
                    | TaskFaulted e   -> tcs.SetException e.InnerExceptions
                    | TaskCompleted r ->
                        try tcs.SetResult (f r)
                        with e -> tcs.SetException e
                let task = source.AsTask()
                task.ContinueWith k |> ignore
                fromTask task

    /// <summary>Creates a ValueTask workflow from two workflows 'x' and 'y', mapping its results with 'f'.</summary>
    /// <remarks>Workflows are run in sequence.</remarks>
    /// <param name="f">The mapping function.</param>
    /// <param name="x">First ValueTask workflow.</param>
    /// <param name="y">Second ValueTask workflow.</param>
    let map2 (f: 'T -> 'U -> 'V) (x: ValueTask<'T>) (y: ValueTask<'U>) : ValueTask<'V> =
        if x.IsCompletedSuccessfully && y.IsCompletedSuccessfully then
            try fromResult (f x.Result y.Result)
            with e ->
                fromException e
        else
            if x.IsCanceled then fromCanceled (CancellationToken(true))
            elif x.IsFaulted then fromException (x.AsTask().Exception)
            elif y.IsCanceled then fromCanceled (CancellationToken(true))
            elif y.IsFaulted then fromException (y.AsTask().Exception)
            elif x.IsCompletedSuccessfully then
                let tcs = TaskCompletionSource<'U> ()
                let k = function
                        | TaskCanceled    -> tcs.SetCanceled ()
                        | TaskFaulted e   -> tcs.SetException e.InnerExceptions
                        | TaskCompleted r ->
                           try tcs.SetResult (f x.Result r)
                           with e -> tcs.SetException e
                y.AsTask().ContinueWith k |> ignore
                fromTask tcs.Task
            elif y.IsCompletedSuccessfully then
                let tcs = TaskCompletionSource<'U> ()
                let k = function
                        | TaskCanceled    -> tcs.SetCanceled ()
                        | TaskFaulted e   -> tcs.SetException e.InnerExceptions
                        | TaskCompleted r ->
                           try tcs.SetResult (f y.Result r)
                           with e -> tcs.SetException e
                x.AsTask().ContinueWith k |> ignore
                fromTask tcs.Task
            else
                let tcs = TaskCompletionSource<'U> ()
                x.AsTask().ContinueWith (
                    function
                    | TaskCanceled    -> tcs.SetCanceled ()
                    | TaskFaulted e   -> tcs.SetException e.InnerExceptions
                    | TaskCompleted r ->
                        y.AsTask().ContinueWith (
                            function
                            | TaskCanceled     -> tcs.SetCanceled ()
                            | TaskFaulted e    -> tcs.SetException e.InnerExceptions
                            | TaskCompleted r' ->
                                try tcs.SetResult (f r r')
                                with e -> tcs.SetException e
                        ) |> ignore) |> ignore
                fromTask tcs.Task

    /// <summary>Creates a ValueTask workflow that is the result of applying the resulting function of a task workflow
    /// to the resulting value of another task workflow</summary>
    /// <param name="f">ValueTask workflow returning a function</param>
    /// <param name="x">ValueTask workflow returning a value</param>
    let apply (f: ValueTask<'T->'U>) (x: ValueTask<'T>) : ValueTask<'U> =
        if f.IsCompletedSuccessfully && x.IsCompletedSuccessfully then
            try fromResult (f.Result x.Result)
            with e ->
                fromException e
        else
            if f.IsCanceled then fromCanceled (CancellationToken(true))
            elif f.IsFaulted then fromException (f.AsTask().Exception)
            elif x.IsCanceled then fromCanceled (CancellationToken(true))
            elif x.IsFaulted then fromException (x.AsTask().Exception)
            elif f.IsCompletedSuccessfully then
                let tcs = TaskCompletionSource<'U> ()
                let k = function
                        | TaskCanceled    -> tcs.SetCanceled ()
                        | TaskFaulted e   -> tcs.SetException e.InnerExceptions
                        | TaskCompleted r ->
                            try tcs.SetResult (f.Result r)
                            with e -> tcs.SetException e
                x.AsTask().ContinueWith k |> ignore
                fromTask tcs.Task
            elif x.IsCompletedSuccessfully then
                let tcs = TaskCompletionSource<'U> ()
                let k = function
                        | TaskCanceled    -> tcs.SetCanceled ()
                        | TaskFaulted e   -> tcs.SetException e.InnerExceptions
                        | TaskCompleted r ->
                            try tcs.SetResult (r x.Result)
                            with e -> tcs.SetException e
                f.AsTask().ContinueWith k |> ignore
                fromTask tcs.Task
            else
                let tcs = TaskCompletionSource<'U> ()
                f.AsTask().ContinueWith (
                     function
                     | TaskCanceled    -> tcs.SetCanceled ()
                     | TaskFaulted e   -> tcs.SetException e.InnerExceptions
                     | TaskCompleted r ->
                         x.AsTask().ContinueWith (
                             function
                             | TaskCanceled     -> tcs.SetCanceled ()
                             | TaskFaulted e    -> tcs.SetException e.InnerExceptions
                             | TaskCompleted r' ->
                                 try tcs.SetResult (r r')
                                 with e -> tcs.SetException e
                         ) |> ignore) |> ignore
                fromTask tcs.Task

    /// <summary>Creates a ValueTask workflow from two workflows 'x' and 'y', tupling its results.</summary>
    let zip (x: ValueTask<'T>) (y: ValueTask<'U>) : ValueTask<'T * 'U> =
        if x.IsCompletedSuccessfully && y.IsCompletedSuccessfully then
            fromResult (x.Result, y.Result)
        else
            if x.IsCanceled then fromCanceled (CancellationToken(true))
            elif x.IsFaulted then fromException (x.AsTask().Exception)
            elif y.IsCanceled then fromCanceled (CancellationToken(true))
            elif y.IsFaulted then fromException (y.AsTask().Exception)
            elif x.IsCompletedSuccessfully then
                let tcs = TaskCompletionSource<'T * 'U> ()
                let k = function
                    | TaskCanceled    -> tcs.SetCanceled ()
                    | TaskFaulted e   -> tcs.SetException e.InnerExceptions
                    | TaskCompleted r -> tcs.SetResult (x.Result, r)
                y.AsTask().ContinueWith k |> ignore
                fromTask tcs.Task
            elif x.IsCompletedSuccessfully then
                let tcs = TaskCompletionSource<'T * 'U> ()
                let k = function
                        | TaskCanceled    -> tcs.SetCanceled ()
                        | TaskFaulted e   -> tcs.SetException e.InnerExceptions
                        | TaskCompleted r -> tcs.SetResult (r, y.Result)
                x.AsTask().ContinueWith k |> ignore
                fromTask tcs.Task
            else
                let tcs = TaskCompletionSource<'T * 'U> ()
                x.AsTask().ContinueWith (
                    function
                    | TaskCanceled    -> tcs.SetCanceled ()
                    | TaskFaulted e   -> tcs.SetException e.InnerExceptions
                    | TaskCompleted r ->
                        let k =
                            function
                            | Task2Canceled     -> tcs.SetCanceled ()
                            | TaskFaulted e    -> tcs.SetException e.InnerExceptions
                            | TaskCompleted r' -> tcs.SetResult (r, r')
                        y.AsTask().ContinueWith k |> ignore) |> ignore
                fromTask tcs.Task
            
    /// <summary>Creates a ValueTask that ignores the result of the source task.</summary>
    /// <remarks>It can be used to convert non-generic ValueTask to unit ValueTask.</remarks>
    let ignore (task: ValueTask) =
        if task.IsCompletedSuccessfully then fromResult ()
        else
            
            if task.IsFaulted then
                fromException (task.AsTask().Exception.InnerException)
            elif task.IsCanceled then
                fromCanceled(CancellationToken(true))
            else
                let tcs = TaskCompletionSource<unit> ()
                let k (t: Task) : unit =
                    if t.IsCanceled  then tcs.SetCanceled ()
                    elif t.IsFaulted then tcs.SetException t.Exception
                    else tcs.SetResult ()
                task.AsTask().ContinueWith k |> ignore
                fromTask tcs.Task

    /// Raises an exception in the ValueTask
    let raise (e: exn) =
        fromException e
#endif