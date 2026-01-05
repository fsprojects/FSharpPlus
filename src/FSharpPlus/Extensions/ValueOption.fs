namespace FSharpPlus

/// Additional operations on ValueOption
[<RequireQualifiedAccess>]
module ValueOption =
    /// <summary>Applies an option value to an option function.</summary>
    /// <param name="f">The option function.</param>
    /// <param name="x">The option value.</param>
    /// <returns>An option of the function applied to the value, or <c>ValueNone</c> if either the function or the value is <c>ValueNone</c>.</returns>
    let apply f (x: voption<'T>) : voption<'U> =
        match f, x with
        | ValueSome f, ValueSome x -> ValueSome (f x)
        | _                        -> ValueNone

    /// <summary>If value is ValueSome, returns both of them tupled. Otherwise it returns ValueNone tupled.</summary>
    /// <param name="v">The value.</param>
    /// <returns>The resulting tuple.</returns>
    let unzip (v: voption<'T * 'U>) =
        match v with
        | ValueSome (x, y) -> ValueSome x, ValueSome y
        | _           -> ValueNone  , ValueNone

    /// <summary>If both value are ValueSome, returns both of them tupled. Otherwise it returns ValueNone.</summary>
    /// <param name="x">The first value.</param>
    /// <param name="y">The second value.</param>
    /// <returns>The resulting option.</returns>
    let zip x y : voption<'T * 'U> =
        match x, y with
        | ValueSome x, ValueSome y -> ValueSome (x, y)
        | _                        -> ValueNone

    /// <summary>If all 3 value are ValueSome, returns them tupled. Otherwise it returns ValueNone.</summary>
    /// <param name="x">The first value.</param>
    /// <param name="y">The second value.</param>
    /// <param name="z">The third value.</param>
    /// <returns>The resulting option.</returns>
    let zip3 x y z : voption<'T * 'U * 'V> =
        match x, y, z with
        | ValueSome x, ValueSome y, ValueSome z -> ValueSome (x, y, z)
        | _                      -> ValueNone

    /// <summary>Converts an option to a Result.</summary>
    /// <param name="source">The option value.</param>
    /// <returns>The resulting Result value.</returns>
    let toResult (source: voption<'T>) = match source with ValueSome x -> Ok x | ValueNone -> Error ()

    /// <summary>Converts an option to a Result.</summary>
    /// <param name="errorValue">The error value to be used in case of ValueNone.</param>
    /// <param name="source">The option value.</param>
    /// <returns>The resulting Result value.</returns>
    let toResultWith (errorValue: 'Error) (source: 'T voption) = match source with ValueSome x -> Ok x | ValueNone -> Error errorValue

    /// <summary>Converts a Result to an option.</summary>
    /// <remarks>The error value (if any) is lost.</remarks>
    /// <param name="source">The Result value.</param>
    /// <returns>The resulting option value.</returns>
    let ofResult (source: Result<'T,'Error>) = match source with Ok x -> ValueSome x | Error _ -> ValueNone

    /// Creates a safe version of the supplied function, which returns an voption<'U> instead of throwing exceptions.
    let protect (f: 'T->'U) x =
        try
            ValueSome (f x)
        with _ -> ValueNone

    /// <summary>Converts pair of bool and value to ValueOption.</summary>
    /// <remarks>Useful for handling C# try pattern with `out` parameter. E.g. `Int.TryParse` or `Dictionary.TryGetValue`.</remarks>
    /// <param name="pair">Pair of bool and value.</param>
    /// <returns><c>ValueSome</c> if bool is `true`, <c>ValueNone</c> otherwise.</returns>
    let ofPair (pair: (bool * 'T)) =
        match pair with
        | true,  x -> ValueSome x
        | false, _ -> ValueNone

    /// <summary>Converts a ValueOption to an Option.</summary>
    let toOption (source: ValueOption<'T>) =
        match source with
        | ValueSome x -> Some x
        | ValueNone   -> None
    
    /// <summary>Converts an Option to a ValueOption.</summary>
    let ofOption (source: option<'T>) =
        match source with
        | Some x -> ValueSome x
        | None   -> ValueNone

    /// <summary>Ignores the value inside the option, if any.</summary>
    /// <param name="source">The option value.</param>
    /// <returns><c>ValueSome ()</c> if the option is <c>ValueSome</c>, <c>ValueNone</c> otherwise.</returns>
    let ignore (source: ValueOption<'T>) =
        match source with
        | ValueSome _ -> ValueSome ()
        | ValueNone   -> ValueNone

    /// <summary>
    /// Extracts a value from either side of a ValueOption.
    /// </summary>
    /// <param name="fSome">The function to apply if the option is ValueSome.</param>
    /// <param name="fNone">The function to apply if the option is ValueNone.</param>
    /// <param name="source">The option to extract the value from.</param>
    let inline either ([<InlineIfLambda>]fSome: 'T -> 'U) ([<InlineIfLambda>]fNone: unit -> 'U) (source: ValueOption<'T>) : 'U =
        match source with
        | ValueSome v -> fSome v
        | ValueNone -> fNone ()
