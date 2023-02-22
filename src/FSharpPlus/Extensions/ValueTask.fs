namespace FSharpPlus

#if NETSTANDARD2_1 && !FABLE_COMPILER

/// Additional operations on ValueTask<'T>
[<RequireQualifiedAccess>]
module ValueTask =

    open System.Threading.Tasks

    let inline internal (|Succeeded|Canceled|Faulted|) (t: ValueTask<'T>) =
        if t.IsCompletedSuccessfully then Succeeded t.Result
        elif t.IsCanceled then Canceled
        else Faulted (t.AsTask().Exception.InnerExceptions)    
    
    let inline internal continueTask (tcs: TaskCompletionSource<'Result>) (x: ValueTask<'t>) (k: 't -> unit) =
        let f = function
        | Succeeded r -> k r
        | Canceled  -> tcs.SetCanceled ()
        | Faulted e -> tcs.SetException e
        if x.IsCompleted then f x
        else
            let aw = x.GetAwaiter ()
            aw.OnCompleted (fun () -> f x)

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
    let map2 (f: 'T -> 'U -> 'V) (x: ValueTask<'T>) (y: ValueTask<'U>) : ValueTask<'V> =
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
    let map3 (f: 'T -> 'U -> 'V -> 'W) (x: ValueTask<'T>) (y: ValueTask<'U>) (z: ValueTask<'V>) : ValueTask<'W> =
        let tcs = TaskCompletionSource<'W> ()
        continueTask tcs x (fun x ->
            continueTask tcs y (fun y ->
                continueTask tcs z (fun z ->
                    try tcs.SetResult (f x y z)
                    with e -> tcs.SetException e)))
        tcs.Task |> ValueTask<'W>

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
    let zip (x: ValueTask<'T>) (y: ValueTask<'U>) : ValueTask<'T * 'U> =
        let tcs = TaskCompletionSource<'T * 'U> ()
        continueTask tcs x (fun x ->
            continueTask tcs y (fun y ->
                tcs.SetResult (x, y)))
        tcs.Task |> ValueTask<'T * 'U>
    
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
    let ignore (source: ValueTask<'T>) =
        if source.IsCompletedSuccessfully then
            source.GetAwaiter().GetResult() |> ignore
            Unchecked.defaultof<_>
        else
            new ValueTask (source.AsTask ())
        

    /// Raises an exception in the ValueTask
    let raise (``exception``: exn) = ValueTask<'TResult> (Task.FromException<'TResult> ``exception``)
        
#endif