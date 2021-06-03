namespace FSharpPlus

/// Additional operations on Result<'T,'Error>
[<RequireQualifiedAccess>]
module Result =
    
    /// Creates an Ok with the supplied value.
    let result value : Result<'T,'Error> = Ok value

    /// Creates an Error With the supplied value.
    let throw value : Result<'T,'Error> = Error value

    /// Applies the wrapped value to the wrapped function when both are Ok and returns a wrapped result or the first Error.
    /// <param name="f">The function wrapped in an Ok or an Error.</param>
    /// <param name="x">The value wrapped in an Ok or an Error.</param>
    /// <returns>An Ok of the function applied to the value, or the first <c>Error</c> if either the function or the value is <c>Error</c>.</returns>
    let apply f (x: Result<'T,'Error>) : Result<'U,'Error> = match f, x with Ok a, Ok b -> Ok (a b) | Error e, _ | _, Error e -> Error e

    
    /// <summary>Creates a Result value from a pair of Result values, using a function to combine them.</summary>
    /// <param name="f">The mapping function.</param>
    /// <param name="x">The first Result value.</param>
    /// <param name="y">The second Result value.</param>
    ///
    /// <returns>The combined value, or the first Error.</returns>
    let map2 f (x: Result<'T,'Error>) (y: Result<'U,'Error>) : Result<'V,'Error> = match x, y with Ok a, Ok b -> Ok (f a b) | Error e, _ | _, Error e -> Error e

    /// <summary>Creates a Result value from three Result values, using a function to combine them.</summary>
    /// <param name="f">The mapping function.</param>
    /// <param name="x">The first Result value.</param>
    /// <param name="y">The second Result value.</param>
    /// <param name="z">The third Result value.</param>
    ///
    /// <returns>The combined value, or the first Error.</returns>
    let map3 f (x: Result<'T, 'Error>) (y: Result<'U, 'Error>) (z: Result<'V, 'Error>): Result<'V, 'Error> =
        match x, y, z with
        | Ok a, Ok b, Ok c -> Ok(f a b c)
        | Error e, _, _
        | _, Error e, _
        | _, _, Error e -> Error e
    
    /// <summary>Flattens two nested Results.</summary>
    /// <param name="source">The nested Results.</param>
    /// <returns>A single Ok of the value when it was nested with OKs, or the Error.</returns>
    /// <remarks><c>flatten</c> is equivalent to <c>bind id</c>.</remarks>
    let flatten source : Result<'T,'Error> = match source with Ok (Ok v) -> Ok v | Ok (Error e) | Error e -> Error e
    
    [<System.Obsolete("Use Result.bindError instead.")>]
    let inline catch f = function Ok v -> Ok v | Error e -> (f: 't->_) e : Result<'v,'e>

    /// <summary>If the input value is an Ok leaves it unchanged, otherwise maps the Error value and flattens the resulting nested Result.</summary>
    /// <param name="binder">A function that takes the error and transforms it into a result.</param>
    /// <param name="source">The source input value.</param>
    /// <returns>A result of the output type of the binder.</returns>
    let inline bindError (binder: 'Error->Result<'T,'Error2>) (source: Result<'T,'Error>) = match source with Ok v -> Ok v | Error e -> binder e

    /// <summary>Extracts a value from either side of a Result.</summary>
    /// <param name="fOk">Function to be applied to source, if it contains an Ok value.</param>
    /// <param name="fError">Function to be applied to source, if it contains an Error value.</param>
    /// <param name="source">The source value, containing an Ok or an Error.</param>
    /// <returns>The result of applying either functions.</returns>
    let inline either (fOk: 'T->'U) (fError: 'Error->'U) source = match source with Ok v -> fOk v | Error e -> fError e

    /// Creates a safe version of the supplied function, which returns a Result<'U,exn> instead of throwing exceptions.
    let protect (f: 'T->'U) x =
        try
            Ok (f x)
        with e -> Error e

    /// Gets the 'Ok' value. If it's an 'Error' this function will throw an exception.
    let get (source: Result<'T,'Error>) =
        match source with
        | Ok x -> x
        | Error e ->
            match box e with
            | :? exn as e -> raise <| System.ArgumentException ("Result value was Error", "source", e)
            | e           -> invalidArg "source" ("Result value was Error: " + string e)

    /// Extracts the Ok value or use the supplied default value when it's an Error.
    let defaultValue (value:'T) (source: Result<'T,'Error>) : 'T = match source with Ok v -> v | _ -> value

    /// Extracts the Ok value or applies the compensation function over the Error.
    let defaultWith (compensation: 'Error->'T) (source: Result<'T,'Error>) : 'T = match source with Ok v -> v | Error e -> compensation e

    /// Converts a Result<'T,'Error> to a Choice<'T,'Error>.
    let toChoice (source: Result<'T,'U>) = match source with Ok x-> Choice1Of2 x | Error x -> Choice2Of2 x

    /// Creates a Result<'T,'Error> from a Choice<'T,'Error>.
    let ofChoice (source: Choice<'T,'U>) = match source with Choice1Of2 x-> Ok x | Choice2Of2 x -> Error x
    
    /// <summary>
    /// Creates two lists by classifying the values depending on whether they were wrapped with Ok or Error.
    /// </summary>
    /// <returns>
    /// A tuple with both resulting lists, Oks are in the first list.
    /// </returns>
    let partition (source: list<Result<'T,'Error>>) =
        let rec loop ((acc1, acc2) as acc) = function
            | [] -> acc
            | x::xs ->
                match x with
                | Ok x -> loop (x::acc1, acc2) xs
                | Error x -> loop (acc1, x::acc2) xs
        loop ([], []) (List.rev source)
