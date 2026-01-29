namespace FSharpPlus

/// Additional operations on Result<'T,'Error>
[<RequireQualifiedAccess>]
module Result =
    open FSharp.Core.CompilerServices
    
    /// Applies the wrapped value to the wrapped function when both are Ok and returns a wrapped result or the first Error.
    /// <param name="f">The function wrapped in an Ok or an Error.</param>
    /// <param name="x">The value wrapped in an Ok or an Error.</param>
    /// <returns>An Ok of the function applied to the value, or the first <c>Error</c> if either the function or the value is <c>Error</c>.</returns>
    let apply f (x: Result<'T,'Error>) : Result<'U,'Error> = match f, x with Ok a, Ok b -> Ok (a b) | Error e, _ | _, Error e -> Error e

    /// <summary>If value is Ok, returns both of them tupled. Otherwise it returns the Error value twice in a tuple.</summary>
    /// <param name="source">The value.</param>
    /// <returns>The resulting tuple.</returns>
    let unzip (source: Result<'T1 * 'T2, 'Error>) : Result<'T1, 'Error> * Result<'T2, 'Error> =
        match source with Ok (x, y) -> Ok x, Ok y | Error e -> Error e, Error e

    /// <summary>If value is Ok, returns all three of them tupled. Otherwise it returns the Error value three times in a tuple.</summary>
    /// <param name="source">The value.</param>
    /// <returns>The resulting tuple.</returns>
    let unzip3 (source: Result<'T1 * 'T2 * 'T3, 'Error>) : Result<'T1, 'Error> * Result<'T2, 'Error> * Result<'T3, 'Error> =
        match source with
        | Ok (x, y, z) -> Ok x, Ok y, Ok z
        | Error e -> Error e, Error e, Error e
    
    /// <summary>Creates a Result value from a pair of Result values.</summary>
    /// <param name="source1">The first Result value.</param>
    /// <param name="source2">The second Result value.</param>
    /// <returns>The tupled value, or the first Error.</returns>
    let zip (source1: Result<'T1, 'Error>) (source2: Result<'T2, 'Error>) : Result<'T1 * 'T2, 'Error> =
        match source1, source2 with
        | Ok a, Ok b -> Ok (a, b) | Error e, _ | _, Error e -> Error e

    /// <summary>Creates a Result value from a three Result values.</summary>
    /// <param name="source1">The first Result value.</param>
    /// <param name="source2">The second Result value.</param>
    /// <param name="source3">The third Result value.</param>
    /// <returns>The tupled value, or the first Error.</returns>
    let zip3 (source1: Result<'T1, 'Error>) (source2: Result<'T2, 'Error>) (source3: Result<'T3, 'Error>) : Result<'T1 * 'T2 * 'T3, 'Error> =
        match source1, source2, source3 with
        | Ok a, Ok b, Ok c -> Ok (a, b, c)
        | Error e, _, _ | _, Error e, _ | _, _, Error e -> Error e

    /// <summary>Creates a Result value from a pair of Result values, using a function to combine them.</summary>
    /// <param name="mapper">The mapping function.</param>
    /// <param name="source1">The first Result value.</param>
    /// <param name="source2">The second Result value.</param>
    /// <returns>The combined value, or the first Error.</returns>
    let map2 mapper (source1: Result<'T1, 'Error>) (source2: Result<'T2, 'Error>) : Result<'T3, 'Error> =
        match source1, source2 with
        | Ok a, Ok b -> Ok (mapper a b)
        | Error e, _ | _, Error e -> Error e

    /// <summary>Creates a Result value from three Result values, using a function to combine them.</summary>
    /// <param name="f">The mapping function.</param>
    /// <param name="x">The first Result value.</param>
    /// <param name="y">The second Result value.</param>
    /// <param name="z">The third Result value.</param>
    /// <returns>The combined value, or the first Error.</returns>
    let map3 mapper (source1: Result<'T1, 'Error>) (source2: Result<'T2, 'Error>) (source3: Result<'T3, 'Error>) : Result<'T4, 'Error> =
        match source1, source2, source3 with
        | Ok a, Ok b, Ok c -> Ok (mapper a b c)
        | Error e, _, _ | _, Error e, _ | _, _, Error e -> Error e

    /// <summary>Maps both Ok and Error of a Result.</summary>
    /// <param name="errorMapper">Function to be applied to source, if it contains an Error value.</param>
    /// <param name="okMapper">Function to be applied to source, if it contains an Ok value.</param>
    /// <param name="source">The source value, containing an Ok or an Error.</param>
    /// <returns>The result of applying the corresponding mapping function.</returns>
    let bimap (errorMapper: 'TError -> 'UError) (okMapper: 'T -> 'U) source =
        match source with
        | Error e -> Error (errorMapper e)
        | Ok a -> Ok (okMapper a)
    
    /// <summary>Flattens two nested Results.</summary>
    /// <param name="source">The nested Results.</param>
    /// <returns>A single Ok of the value when it was nested with OKs, or the Error.</returns>
    /// <remarks><c>flatten</c> is equivalent to <c>bind id</c>.</remarks>
    let flatten source : Result<'T,'Error> = match source with Ok (Ok v) -> Ok v | Ok (Error e) | Error e -> Error e
    
    /// Like Result.bindError but with flipped arguments.
    /// <summary>If the input value is an Ok leaves it unchanged, otherwise maps the Error value.</summary>
    /// <param name="f">A function that takes the error and transforms it into another error.</param>
    /// <param name="source">The source input value.</param>
    /// <returns>A result of the same type as the input.</returns>
    let inline catch (source: Result<'T, 'TError>) (f: 'TError -> Result<'T, 'UError>) : Result<'T, 'UError> =
        match source with
        | Ok v -> Ok v
        | Error e -> f e

    /// <summary>If the input value is an Ok leaves it unchanged, otherwise maps the Error value and flattens the resulting nested Result.</summary>
    /// <param name="binder">A function that takes the error and transforms it into a result.</param>
    /// <param name="source">The source input value.</param>
    /// <returns>A result of the output type of the binder.</returns>
    let inline bindError (binder: 'TError -> Result<'T, 'UError>) (source: Result<'T, 'TError>) : Result<'T, 'UError> =
        match source with Ok v -> Ok v | Error e -> binder e

    /// <summary><c>iterError f inp</c> executes <c>match inp with Ok _ -> () | Error x -> f x</c>.</summary>
    ///
    /// <param name="action">A function to apply to the error part of the source value.</param>
    /// <param name="source">The input result.</param>
    ///
    /// <example id="iter-1">
    /// <code lang="fsharp">
    /// Ok "Hello world" |> Result.iter (printfn "%s") // does nothing
    /// Error "Hello world" |> Result.iter (printfn "%s") // prints "Hello world"
    /// </code>
    /// </example>
    let inline iterError ([<InlineIfLambda>]action: 'Error -> unit) (source: Result<'T, 'Error>) = match source with Ok _ -> () | Error x -> action x

    /// <summary>Extracts a value from either side of a Result.</summary>
    /// <param name="fOk">Function to be applied to source, if it contains an Ok value.</param>
    /// <param name="fError">Function to be applied to source, if it contains an Error value.</param>
    /// <param name="source">The source value, containing an Ok or an Error.</param>
    /// <returns>The result of applying either functions.</returns>
    let inline either (fOk: 'T -> 'U) (fError: 'Error -> 'U) source = match source with Ok v -> fOk v | Error e -> fError e

    /// <summary>Creates a safe version of the supplied function, which wraps the exception into a Result instead of throwing it.</summary>
    /// <param name="unsafeFunction">The function that may throw exceptions.</param>
    /// <param name="source">The input value for the function.</param>
    /// <returns>An Ok of the function result, or an Error of the caught exception.</returns>
    let protect (unsafeFunction: 'T -> 'U) source : Result<'U, exn> =
        try
            Ok (unsafeFunction source)
        with e -> Error e

    /// <summary>Gets the 'Ok' value. If it's an 'Error' this function will throw an exception.</summary>
    /// <param name="source">The input result.</param>
    /// <returns>The 'Ok' value.</returns>
    let get (source: Result<'T, 'Error>) =
        match source with
        | Ok x -> x
        | Error e ->
            match box e with
            | :? exn as e -> raise <| System.ArgumentException ("Result value was Error", "source", e)
            | e           -> invalidArg "source" ("Result value was Error: " + string e)

    /// <summary>Gets the value of the result if the result is <c>Ok</c>, otherwise returns the specified default value.</summary>
    ///
    /// <param name="value">The specified default value.</param>
    /// <param name="result">The input result.</param>
    ///
    /// <returns>The result if the result is Ok, else the default value.</returns>
    /// <remarks>
    /// Note: this function has since been added to FSharp.Core.
    /// It will be removed in next major release of FSharpPlus.
    /// </remarks>
    let defaultValue (value:'T) (result: Result<'T, 'Error>) : 'T = match result with Ok v -> v | _ -> value

    /// <summary>Gets the value of the result if the result is <c>Ok</c>, otherwise evaluates <paramref name="defThunk"/> and returns the result.</summary>
    ///
    /// <param name="defThunk">A thunk that provides a default value when evaluated.</param>
    /// <param name="result">The input result.</param>
    /// <remarks>
    /// Note: this function has since been added to FSharp.Core.
    /// It will be removed in next major release of FSharpPlus.
    /// </remarks>
    let defaultWith (defThunk: 'Error -> 'T) (result: Result<'T, 'Error>) : 'T = match result with Ok v -> v | Error e -> defThunk e

    /// Converts a Result to a Choice.
    /// <param name="source">The input Result value.</param>
    /// <returns>A Choice value representing the same data.</returns>
    let toChoice (source: Result<'T, 'Error>) : Choice<'T, 'Error> = match source with Ok x -> Choice1Of2 x | Error x -> Choice2Of2 x

    /// Converts a Choice to a Result.
    /// <param name="source">The input Choice value.</param>
    /// <returns>A Result value representing the same data.</returns>
    let ofChoice (source: Choice<'T, 'Error>) : Result<'T, 'Error> = match source with Choice1Of2 x -> Ok x | Choice2Of2 x -> Error x

    /// <summary>
    /// Creates two lists by classifying the values depending on whether they were wrapped with Ok or Error.
    /// </summary>
    /// <returns>
    /// A tuple with both resulting lists, Oks are in the first list.
    /// </returns>
    let partition (source: list<Result<'T, 'Error>>) =
    #if FABLE_COMPILER
        let rec loop ((acc1, acc2) as acc) = function
            | [] -> acc
            | x::xs ->
                match x with
                | Ok x -> loop (x::acc1, acc2) xs
                | Error x -> loop (acc1, x::acc2) xs
        loop ([], []) (List.rev source)
    #else
        let mutable coll1 = new ListCollector<'T> ()
        let mutable coll2 = new ListCollector<'Error> ()
        List.iter (function Ok e -> coll1.Add e | Error e -> coll2.Add e) source
        coll1.Close (), coll2.Close ()
    #endif

    /// <summary>Creates a Result value from a pair of Result values, using a function to combine errors if both are Error.</summary>
    /// <param name="combiner">Function to combine error values.</param>
    /// <param name="mapper">The mapping function.</param>
    /// <param name="source1">The first Result value.</param>
    /// <param name="source2">The second Result value.</param>
    /// <returns>The combined value, or the combined Error.</returns>
    let apply2With combiner (mapper: 'T1 -> 'T2 -> 'U) (source1: Result<'T1, 'Error>) (source2: Result<'T2, 'Error>) : Result<'U, 'Error> =
        match source1, source2 with
        | Ok a, Ok b -> Ok (mapper a b)
        | Error e, Ok _ | Ok _, Error e -> Error e
        | Error e1, Error e2 -> Error (combiner e1 e2)

    /// <summary>Creates a Result value from three Result values, using a function to combine errors if more than one are Error.</summary>
    /// <param name="combiner">Function to combine error values.</param>
    /// <param name="mapper">The mapping function.</param>
    /// <param name="source1">The first Result value.</param>
    /// <param name="source2">The second Result value.</param>
    /// <param name="source3">The third Result value.</param>
    /// <returns>The combined value, or the combined Error.</returns>
    let apply3With combiner (mapper: 'T1 -> 'T2 -> 'T3 -> 'U) (source1: Result<'T1, 'Error>) (source2: Result<'T2, 'Error>) (source3: Result<'T3, 'Error>) : Result<'U, 'Error> =
        match source1, source2, source3 with
        | Ok a, Ok b, Ok c -> Ok (mapper a b c)
        | Error e, Ok _, Ok _ | Ok _, Error e, Ok _ | Ok _, Ok _, Error e -> Error e
        | Ok _, Error e1, Error e2 | Error e1, Ok _, Error e2 | Error e1, Error e2, Ok _ -> Error (combiner e1 e2)
        | Error e1, Error e2, Error e3 -> Error (combiner (combiner e1 e2) e3)

    /// <summary>Creates a Result value from a pair of Result values, using a function to combine errors if both are Error.</summary>
    /// <param name="combiner">Function to combine error values.</param>
    /// <param name="source1">The first Result value.</param>
    /// <param name="source2">The second Result value.</param>
    /// <returns>The tupled value, or the combined Error.</returns>
    let zipWith combiner (source1: Result<'T1, 'Error>) (source2: Result<'T2, 'Error>) : Result<'T1 * 'T2, 'Error> =
        match source1, source2 with
        | Ok a, Ok b -> Ok (a, b)
        | Error e, Ok _ | Ok _, Error e -> Error e
        | Error e1, Error e2 -> Error (combiner e1 e2)

    /// <summary>Creates a Result value from three Result values, using a function to combine errors if more than one are Error.</summary>
    /// <param name="combiner">Function to combine error values.</param>
    /// <param name="source1">The first Result value.</param>
    /// <param name="source2">The second Result value.</param>
    /// <param name="source3">The third Result value.</param>
    /// <returns>The tupled value, or the combined Error.</returns>
    let zip3With combiner (source1: Result<'T1, 'Error>) (source2: Result<'T2, 'Error>) (source3: Result<'T3, 'Error>) : Result<'T1 * 'T2 * 'T3, 'Error> =
        match source1, source2, source3 with
        | Ok a, Ok b, Ok c -> Ok (a, b, c)
        | Error e, Ok _, Ok _ | Ok _, Error e, Ok _ | Ok _, Ok _, Error e -> Error e
        | Ok _, Error e1, Error e2 | Error e1, Ok _, Error e2 | Error e1, Error e2, Ok _ -> Error (combiner e1 e2)
        | Error e1, Error e2, Error e3 -> Error (combiner (combiner e1 e2) e3)

    /// <summary>Creates a Result value from a pair of Result values, using a function to combine errors if both are Error.</summary>
    /// <param name="combiner">Function to combine error values.</param>
    /// <param name="mapper">The mapping function.</param>
    /// <param name="source1">The first Result value.</param>
    /// <param name="source2">The second Result value.</param>
    /// <returns>The combined value, or the combined Error.</returns>
    let map2With combiner mapper (source1: Result<'T1, 'Error>) (source2: Result<'T2, 'Error>) : Result<'U, 'Error> =
        match source1, source2 with
        | Ok a, Ok b -> Ok (mapper a b)
        | Error e, Ok _ | Ok _, Error e -> Error e
        | Error e1, Error e2 -> Error (combiner e1 e2)

    /// <summary>Creates a Result value from three Result values, using a function to combine errors if more than one are Error.</summary>
    /// <param name="combiner">Function to combine error values.</param>
    /// <param name="mapper">The mapping function.</param>
    /// <param name="source1">The first Result value.</param>
    /// <param name="source2">The second Result value.</param>
    /// <param name="source3">The third Result value.</param>
    /// <returns>The combined value, or the combined Error.</returns>
    let map3With combiner mapper (source1: Result<'T1, 'Error>) (source2: Result<'T2, 'Error>) (source3: Result<'T3, 'Error>) : Result<'U, 'Error> =
        match source1, source2, source3 with
        | Ok a, Ok b, Ok c -> Ok (mapper a b c)
        | Error e, Ok _, Ok _ | Ok _, Error e, Ok _ | Ok _, Ok _, Error e -> Error e
        | Ok _, Error e1, Error e2 | Error e1, Ok _, Error e2 | Error e1, Error e2, Ok _ -> Error (combiner e1 e2)
        | Error e1, Error e2, Error e3 -> Error (combiner (combiner e1 e2) e3)

    /// <summary>Ignores the value inside the result, if any.</summary>
    /// <param name="source">The result value.</param>
    /// <returns><c>Ok ()</c> if the result is <c>Ok</c>, <c>Error e</c> otherwise.</returns>
    let ignore (source: Result<'T, 'Error>) =
        match source with
        | Ok _ -> Ok ()
        | Error e -> Error e
