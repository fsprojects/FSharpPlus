namespace FSharpPlus

#if NETSTANDARD2_1 && !FABLE_COMPILER

/// Additional operations on ValueTask<'T>
[<RequireQualifiedAccess>]
module ValueTask =
    
    open System.Threading
    open System.Threading.Tasks
    
    let FromResult<'T> (result : 'T) =
        ValueTask<'T>(result)
        
    let FromException<'T> (e : exn) =
        ValueTask<'T>(Task.FromException<'T>(e))
    
    let FromCanceled<'T> (ct : CancellationToken) =
        ValueTask<'T>(Task.FromCanceled<'T>(ct))
        
    let FromTask<'T> (t : Task<'T>) =
        ValueTask<'T>(t)

    /// <summary>Creates a ValueTask workflow from 'source' another, mapping its result with 'f'.</summary>
    let map (f: 'T -> 'U) (source: ValueTask<'T>) : ValueTask<'U> =
        backgroundTask {
            let! r = source
            return f r
        } |> ValueTask<'U>

    /// <summary>Creates a ValueTask workflow from two workflows 'x' and 'y', mapping its results with 'f'.</summary>
    /// <remarks>Workflows are run in sequence.</remarks>
    /// <param name="f">The mapping function.</param>
    /// <param name="x">First ValueTask workflow.</param>
    /// <param name="y">Second ValueTask workflow.</param>
    let map2 (f: 'T -> 'U -> 'V) (x: ValueTask<'T>) (y: ValueTask<'U>) : ValueTask<'V> =
        backgroundTask {
            let! rX = x
            let! rY = y
            return f rX rY
        } |> ValueTask<'V>
        
    /// <summary>Creates a ValueTask workflow from three workflows 'x', 'y' and z, mapping its results with 'f'.</summary>
    /// <remarks>Workflows are run in sequence.</remarks>
    /// <param name="f">The mapping function.</param>
    /// <param name="x">First ValueTask workflow.</param>
    /// <param name="y">Second ValueTask workflow.</param>
    /// <param name="z">Third ValueTask workflow.</param>
    let map3 (f : 'T -> 'U -> 'V -> 'W) (x : ValueTask<'T>) (y : ValueTask<'U>) (z: ValueTask<'V>) : ValueTask<'W> =
        backgroundTask {
            let! rX = x
            let! rY = y
            let! rZ = z
            return f rX rY rZ
        } |> ValueTask<'W>

    /// <summary>Creates a ValueTask workflow that is the result of applying the resulting function of a ValueTask workflow
    /// to the resulting value of another ValueTask workflow</summary>
    /// <param name="f">ValueTask workflow returning a function</param>
    /// <param name="x">ValueTask workflow returning a value</param>
    let apply (f: ValueTask<'T->'U>) (x: ValueTask<'T>) : ValueTask<'U> =
        backgroundTask {
            let! r = x
            let! fn = f
            return (fn r)
        } |> ValueTask<'U>

    /// <summary>Creates a ValueTask workflow from two workflows 'x' and 'y', tupling its results.</summary>
    let zip (x: ValueTask<'T>) (y: ValueTask<'U>) : ValueTask<'T * 'U> =
        backgroundTask {
            let! rX = x
            let! rY = y
            return (rX, rY)
        } |> ValueTask<'T * 'U>
    
    /// Flattens two nested ValueTask into one.
    let join (source: ValueTask<ValueTask<'T>>) : ValueTask<'T> =
        backgroundTask {
            let! s = source
            return! s
        } |> ValueTask<'T>
    
    
    /// <summary>Creates a ValueTask workflow from 'source' workflow, mapping and flattening its result with 'f'.</summary>
    let bind (f: 'T -> ValueTask<'U>) (source: ValueTask<'T>) : ValueTask<'U> =
        source
        |> map f
        |> join
            
    /// <summary>Creates a ValueTask that ignores the result of the source ValueTask.</summary>
    /// <remarks>It can be used to convert non-generic ValueTask to unit ValueTask.</remarks>
    let ignore (source: ValueTask<'T>) =
        backgroundTask {
            let! _ = source
            return ()
        } |> ValueTask
        

    /// Raises an exception in the ValueTask
    let raise (e: exn) =
        FromException e
        
#endif