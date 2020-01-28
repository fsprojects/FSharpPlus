namespace FSharpPlus

open System

/// Additional operations on Async
[<RequireQualifiedAccess>]
module Async =

    /// <summary>Creates an async workflow from another workflow 'x', mapping its result with 'f'.</summary>
    let map f x = async.Bind (x, async.Return << f)

    /// <summary>Creates an async workflow from two workflows 'x' and 'y', mapping it results with 'f'.</summary>
    /// <remarks>Workflows are run in sequence.</remarks>
    /// <param name="f">The mapping function.</param>
    /// <param name="x">First async workflow.</param>
    /// <param name="y">Second async workflow.</param>
    let map2 f x y = async {
        let! a = x
        let! b = y
        return f a b}

    /// <summary>Creates an async workflow from two workflows 'x' and 'y', tupling it results.</summary>
    let zip x y = async {
        let! a = x
        let! b = y
        return a, b}

    /// Flatten two nested asyncs into one.
    let join x = async.Bind (x, id)

    /// <summary>Create an async workflow that is the result of applying the resulting function of an async workflow to the resulting value of another async workflow.</summary>
    /// <param name="f">Async workflow returning a function.</param>
    /// <param name="x">Async workflow returning a value.</param>
    let apply f x = async.Bind (f, fun x1 -> async.Bind (x, fun x2 -> async {return x1 x2}))

    /// Raise an exception in the async workflow
    let raise<'T> (ex: exn) : Async<'T> = Async.FromContinuations (fun (_, errK, _) -> errK ex)