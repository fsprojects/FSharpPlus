namespace FSharpPlus

#if !FABLE_COMPILER

open System

/// Additional operations on Nullable
[<RequireQualifiedAccess>]
module Nullable =
    /// <summary>Monadic Bind; Transforms the value inside a Nullable to a Nullable using a specified binding function.</summary>
    /// <param name="f">The value binding function.</param>
    /// <param name="n">The Nullable value.</param>
    let bind f (n: Nullable<_>) =
        if n.HasValue then f n.Value
        else Nullable()

    /// <summary>Transforms the value inside a Nullable by using a specified mapping function.</summary>
    /// <param name="f">The value mapping function.</param>
    /// <param name="n">The Nullable value.</param>
    let map f n = bind (f >> Nullable) n

    /// <summary>Invokes a side-effect function to the value of a Nullable if present and ignores the result.</summary>
    /// <param name="f">The function to apply to the value of a Nullable.</param>
    /// <param name="n">The Nullable value.</param>
    let iter f (n: Nullable<_>) = if n.HasValue then f n.Value |> ignore

#endif
