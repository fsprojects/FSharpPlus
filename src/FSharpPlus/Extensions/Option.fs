namespace FSharpPlus

/// Additional operations on Option
[<RequireQualifiedAccess>]
module Option =
    /// <summary>Applies an option value to an option function.</summary>
    /// <param name="f">The option function.</param>
    /// <param name="x">The option value.</param>
    /// <returns>An option of the function applied to the value, or <c>None</c> if either the function or the value is <c>None</c>.</returns>
    let apply f (x: option<'T>) : option<'U> =
        match f, x with
        | Some f, Some x -> Some (f x)
        | _              -> None

    /// <summary>If value is Some, returns both of them tupled. Otherwise it returns None tupled.</summary>
    /// <param name="v">The value.</param>
    /// <returns>The resulting tuple.</returns>
    let unzip (v: option<'T * 'U>) =
        match v with
        | Some (x, y) -> Some x, Some y
        | _           -> None  , None

    /// <summary>If both value are Some, returns both of them tupled. Otherwise it returns None.</summary>
    /// <param name="x">The first value.</param>
    /// <param name="y">The second value.</param>
    /// <returns>The resulting option.</returns>
    let zip x y : option<'T * 'U> =
        match x, y with
        | Some x, Some y -> Some (x, y)
        | _              -> None

    /// <summary>Converts an option to a Result.</summary>
    /// <param name="source">The option value.</param>
    /// <returns>The resulting Result value.</returns>
    let toResult (source: option<'T>) = match source with Some x -> Ok x | None -> Error ()

    /// <summary>Converts an option to a Result.</summary>
    /// <param name="errorValue">The error value to be used in case of None.</param>
    /// <param name="source">The option value.</param>
    /// <returns>The resulting Result value.</returns>
    let toResultWith (errorValue: 'Error) (source: 'T option) = match source with Some x -> Ok x | None -> Error errorValue

    /// <summary>Converts a Result to an option.</summary>
    /// <remarks>The error value (if any) is lost.</remarks>
    /// <param name="source">The Result value.</param>
    /// <returns>The resulting option value.</returns>
    let ofResult (source: Result<'T,'Error>) = match source with Ok x -> Some x | Error _ -> None

    /// Creates a safe version of the supplied function, which returns an option<'U> instead of throwing exceptions.
    let protect (f: 'T->'U) x =
        try
            Some (f x)
        with _ -> None