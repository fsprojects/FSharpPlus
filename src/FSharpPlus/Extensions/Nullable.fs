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

    /// <summary>Returns whether a Nullable has a value.</summary>
    /// <param name="n">The Nullable value.</param>
    /// <returns>True if the Nullable has a value, false otherwise.</returns>
    let hasValue (n: Nullable<_>) = n.HasValue

    /// <summary>Returns whether a Nullable is empty.</summary>
    /// <param name="n">The Nullable value.</param>
    /// <returns>True if the Nullable does not have a value, false otherwise.</returns>
    let isNull (n: Nullable<_>) = not n.HasValue

    /// <summary>Returns the number of values in the Nullable (0 or 1).</summary>
    /// <param name="n">The Nullable value.</param>
    /// <returns>1 if the Nullable has a value, 0 otherwise.</returns>
    let count (n: Nullable<_>) = if n.HasValue then 1 else 0

    /// <summary>Returns the value inside a Nullable if it has one, otherwise returns defValue.</summary>
    /// <param name="defValue">The value to use if the Nullable does not have a value.</param>
    /// <param name="n">The Nullable value.</param>
    /// <returns>The value inside the Nullable or defValue.</returns>
    let defaultValue defValue (n: Nullable<_>) = if n.HasValue then n.Value else defValue

    /// <summary>Returns the value inside a Nullable if it has one, otherwise returns the result of evaluating defThunk.</summary>
    /// <param name="defThunk">The function that returns a default value.</param>
    /// <param name="n">The Nullable value.</param>
    /// <returns>The value inside the Nullable or the result of defThunk.</returns>
    let defaultWith defThunk (n: Nullable<_>) = if n.HasValue then n.Value else defThunk ()

    /// <summary>Returns whether a Nullable contains a value for which the given predicate returns true.</summary>
    /// <param name="pred">Function to test against the inner value.</param>
    /// <param name="n">The Nullable value.</param>
    /// <returns>True if the Nullable contains a value that matches the predicate, false otherwise.</returns>
    let exists pred (n: Nullable<_>) = if n.HasValue then pred n.Value else false

    /// <summary>Filters the value in a Nullable with the given predicate.</summary>
    /// <param name="pred">Function that returns whether the value in the Nullable should remain.</param>
    /// <param name="n">The Nullable value.</param>
    /// <returns>The original Nullable value if the predicate matched the value, empty Nullable otherwise.</returns>
    let filter pred (n: Nullable<_>) = if exists pred n then n else Nullable ()

    /// <summary>Updates state data with an update function and the value from a Nullable if it has one.</summary>
    /// <param name="f">A function to update the state data when given a value from the Nullable.</param>
    /// <param name="state">The initial state.</param>
    /// <param name="n">The Nullable value.</param>
    /// <returns>The new state if the Nullable contained a value, the original state otherwise.</returns>
    let fold f state (n: Nullable<_>) = if n.HasValue then f state n.Value else state

    /// <summary>Fold, but the update function has reversed arguments.</summary>
    /// <param name="f">A function to update the state data when given a value from the Nullable.</param>
    /// <param name="state">The initial state.</param>
    /// <param name="n">The Nullable value.</param>
    /// <returns>The new state if the Nullable contained a value, the original state otherwise.</returns>
    let foldBack f state (n: Nullable<_>) = if n.HasValue then f n.Value state else state

    /// <summary>Returns whether a Nullable is empty or its value passes the given predicate. Like exists, but returns true when there is no value.</summary>
    /// <param name="pred">Function to test against the inner value.</param>
    /// <param name="n">The Nullable value.</param>
    /// <returns>True if the Nullable is empty or the value matches the predicate, false otherwise.</returns>
    let forall pred (n: Nullable<_>) = if n.HasValue then pred n.Value else true

    /// <summary>Converts a Nullable to an array with 0 or 1 items.</summary>
    /// <param name="n">The Nullable value.</param>
    /// <returns>An array that contains the value in the Nullable if there is one, or an empty array.</returns>
    let toArray (n: Nullable<_>) = if n.HasValue then [|n.Value|] else [||]

    /// <summary>Converts a Nullable to a list with 0 or 1 items.</summary>
    /// <param name="n">The Nullable value.</param>
    /// <returns>A list that contains the value in the Nullable if there is one, or an empty list.</returns>
    let toList (n: Nullable<_>) = if n.HasValue then [n.Value] else []

#endif
