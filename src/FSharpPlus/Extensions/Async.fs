namespace FSharpPlus

/// Additional operations on Async
[<RequireQualifiedAccess>]
module Async =

    open System
    open System.Threading.Tasks
    
    #if !FABLE_COMPILER
    // Proper Async.StartImmediateAsTask implementation, without aggregate exception wrapping.
    let private startImmediateAsTask (computation: Async<'T>, cancellationToken) : Task<'T> =        
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
    #endif
    open FSharpPlus.Extensions

    /// <summary>Creates an async workflow from another workflow 'x', mapping its result with 'f'.</summary>
    let map f x = async.Bind (x, async.Return << f)

    /// <summary>Creates an async workflow from two workflows 'x' and 'y', mapping its results with 'f'.</summary>
    /// <remarks>Workflows are run in sequence.</remarks>
    /// <param name="f">The mapping function.</param>
    /// <param name="x">First async workflow.</param>
    /// <param name="y">Second async workflow.</param>
    let lift2 f x y = async {
        let! a = x
        let! b = y
        return f a b}
    
    /// <summary>Creates an async workflow from three workflows 'x', 'y' and 'z', mapping its results with 'f'.</summary>
    /// <remarks>Workflows are run in sequence.</remarks>
    /// <param name="f">The mapping function.</param>
    /// <param name="x">First async workflow.</param>
    /// <param name="y">Second async workflow.</param>
    /// <param name="z">third async workflow.</param>
    let lift3 f x y z = async {
        let! a = x
        let! b = y
        let! c = z
        return f a b c}

    /// <summary>Creates an async workflow from two workflows, mapping its results with a specified function.</summary>
    /// <remarks>Similar to lift2 but although workflows are started in sequence they might end independently in different order
    /// and all errors are collected.
    /// </remarks>
    /// <param name="mapper">The mapping function.</param>
    /// <param name="async1">First async workflow.</param>
    /// <param name="async2">Second async workflow.</param>
    #if FABLE_COMPILER
    let map2 mapper (async1: Async<'T1>) (async2: Async<'T2>) : Async<'U> =
        lift2 mapper async1 async2
    #else
    let map2 mapper (async1: Async<'T1>) (async2: Async<'T2>) : Async<'U> = async {
        let! ct = Async.CancellationToken
        let t1 = startImmediateAsTask (async1, ct)
        let t2 = startImmediateAsTask (async2, ct)
        return! Async.Await (Task.map2 mapper t1 t2) }
    #endif

    /// <summary>Creates an async workflow from three workflows, mapping its results with a specified function.</summary>
    /// <remarks>Similar to lift3 but although workflows are started in sequence they might end independently in different order
    /// and all errors are collected.
    /// </remarks>
    /// <param name="mapper">The mapping function.</param>
    /// <param name="async1">First async workflow.</param>
    /// <param name="async2">Second async workflow.</param>
    /// <param name="async3">third async workflow.</param>
    #if FABLE_COMPILER
    let map3 mapper (async1: Async<'T1>) (async2: Async<'T2>) (async3: Async<'T3>) : Async<'U> =
        lift3 mapper async1 async2 async3
    #else
    let map3 mapper (async1: Async<'T1>) (async2: Async<'T2>) (async3: Async<'T3>) : Async<'U> = async {
        let! ct = Async.CancellationToken
        let t1 = startImmediateAsTask (async1, ct)
        let t2 = startImmediateAsTask (async2, ct)
        let t3 = startImmediateAsTask (async3, ct)
        return! Async.Await (Task.map3 mapper t1 t2 t3) }
    #endif

    /// <summary>Creates an async workflow from two workflows 'x' and 'y', tupling its results.</summary>
    let zipSequentially x y = async {
        let! a = x
        let! b = y
        return a, b }

    /// <summary>Creates an async workflow from two workflows, tupling its results.</summary>
    /// <remarks>Similar to zipSequentially but although workflows are started in sequence they might end independently in different order
    /// and all errors are collected.
    /// </remarks>
    let zip (async1: Async<'T1>) (async2: Async<'T2>) = map2 (fun x y -> x, y) async1 async2

    /// <summary>Creates an async workflow from three workflows, tupling its results.</summary>
    /// <remarks>Similar to zipSequentially but although workflows are started in sequence they might end independently in different order
    /// and all errors are collected.
    /// </remarks>
    let zip3 (async1: Async<'T1>) (async2: Async<'T2>) (async3: Async<'T3>) = map3 (fun x y z -> x, y, z) async1 async2 async3

    /// Flatten two nested asyncs into one.
    let join x = async.Bind (x, id)

    /// <summary>Creates an async workflow that is the result of applying the resulting function of
    /// an async workflow to the resulting value of another async workflow.</summary>
    /// <param name="f">Async workflow returning a function.</param>
    /// <param name="x">Async workflow returning a value.</param>
    let apply f x = async.Bind (f, fun x1 -> async.Bind (x, fun x2 -> async {return x1 x2}))

    /// Raises an exception in the async workflow
    let raise<'T> (ex: exn) : Async<'T> = Async.FromContinuations (fun (_, errK, _) -> errK ex)
