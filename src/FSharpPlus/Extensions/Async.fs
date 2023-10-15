namespace FSharpPlus

/// Additional operations on Async
[<RequireQualifiedAccess>]
module Async =

    open System

    /// <summary>Creates an async workflow from another workflow 'x', mapping its result with 'f'.</summary>
    let map f x = async.Bind (x, async.Return << f)

    /// <summary>Creates an async workflow from two workflows 'x' and 'y', mapping its results with 'f'.</summary>
    /// <remarks>Workflows are run in sequence. For parallel use pmap2</remarks>
    /// <param name="f">The mapping function.</param>
    /// <param name="x">First async workflow.</param>
    /// <param name="y">Second async workflow.</param>
    let map2 f x y = async {
        let! a = x
        let! b = y
        return f a b }

    /// <summary>Creates an async workflow from two workflows 'x' and 'y', mapping its results with 'f'.</summary>
    /// <remarks>Similar to map2 but workflows are run in parallel.</remarks>
    /// <param name="f">The mapping function.</param>
    /// <param name="x">First async workflow.</param>
    /// <param name="y">Second async workflow.</param>
    #if FABLE_COMPILER
    let pmap2 f x y = map2 f x y
    #else
    let pmap2 f x y = async {
        let! ct = Async.CancellationToken
        let x = Async.StartImmediateAsTask (x, ct)
        let y = Async.StartImmediateAsTask (y, ct)
        let! x' = Async.AwaitTask x
        let! y' = Async.AwaitTask y
        return f x' y' }
    #endif

    /// <summary>Creates an async workflow from three workflows 'x', 'y' and 'z', mapping its results with 'f'.</summary>
    /// <remarks>Workflows are run in sequence. For parallel use pmap3</remarks>
    /// <param name="f">The mapping function.</param>
    /// <param name="x">First async workflow.</param>
    /// <param name="y">Second async workflow.</param>
    /// <param name="z">third async workflow.</param>
    let map3 f x y z = async {
        let! a = x
        let! b = y
        let! c = z
        return f a b c }

    /// <summary>Creates an async workflow from three workflows 'x', 'y' and 'z', mapping its results with 'f'.</summary>
    /// <remarks>Similar to map3 but workflows are run in parallel.</remarks>
    /// <param name="f">The mapping function.</param>
    /// <param name="x">First async workflow.</param>
    /// <param name="y">Second async workflow.</param>
    /// <param name="z">third async workflow.</param>
    #if FABLE_COMPILER
    let pmap3 f x y z = map3 f x y z
    #else
    let pmap3 f x y z = async {
        let! ct = Async.CancellationToken
        let x = Async.StartImmediateAsTask (x, ct)
        let y = Async.StartImmediateAsTask (y, ct)
        let z = Async.StartImmediateAsTask (z, ct)
        let! x' = Async.AwaitTask x
        let! y' = Async.AwaitTask y
        let! z' = Async.AwaitTask z
        return f x' y' z' }
    #endif

    /// <summary>Creates an async workflow from two workflows 'x' and 'y', tupling its results.</summary>
    let zip x y = async {
        let! a = x
        let! b = y
        return a, b }

    /// <summary>Creates an async-parallel workflow from two workflows 'x' and 'y', tupling its results.</summary>
    #if FABLE_COMPILER
    let pzip x y = zip x y
    #else
    let pzip x y = async {
        let! ct = Async.CancellationToken
        let x = Async.StartImmediateAsTask (x, ct)
        let y = Async.StartImmediateAsTask (y, ct)
        let! x' = Async.AwaitTask x
        let! y' = Async.AwaitTask y
        return x', y' }
    #endif

    /// Flatten two nested asyncs into one.
    let join x = async.Bind (x, id)

    /// <summary>Creates an async workflow that is the result of applying the resulting function of
    /// an async workflow to the resulting value of another async workflow.</summary>
    /// <param name="f">Async workflow returning a function.</param>
    /// <param name="x">Async workflow returning a value.</param>
    let apply f x = async.Bind (f, fun x1 -> async.Bind (x, fun x2 -> async {return x1 x2}))

    /// Raises an exception in the async workflow
    let raise<'T> (ex: exn) : Async<'T> = Async.FromContinuations (fun (_, errK, _) -> errK ex)
