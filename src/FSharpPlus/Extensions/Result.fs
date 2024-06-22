namespace FSharpPlus

/// Additional operations on Result<'T,'Error>
[<RequireQualifiedAccess>]
module Result =
    open FSharp.Core.CompilerServices
    open System
    
    /// Creates an Ok with the supplied value.
    [<Obsolete("Prefer Result.Ok")>]
    let result value : Result<'T,'Error> = Ok value

    /// Creates an Error With the supplied value.
    [<Obsolete("Prefer Result.Error")>]
    let throw value : Result<'T,'Error> = Error value

    /// Applies the wrapped value to the wrapped function when both are Ok and returns a wrapped result or the first Error.
    /// <param name="f">The function wrapped in an Ok or an Error.</param>
    /// <param name="x">The value wrapped in an Ok or an Error.</param>
    /// <returns>An Ok of the function applied to the value, or the first <c>Error</c> if either the function or the value is <c>Error</c>.</returns>
    let apply f (x: Result<'T,'Error>) : Result<'U,'Error> = match f, x with Ok a, Ok b -> Ok (a b) | Error e, _ | _, Error e -> Error e

    /// <summary>If value is Ok, returns both of them tupled. Otherwise it returns the Error value twice in a tuple.</summary>
    /// <param name="source">The value.</param>
    /// <returns>The resulting tuple.</returns>
    let unzip (source: Result<'T * 'U, 'Error>) : Result<'T, 'Error> * Result<'U, 'Error> = match source with Ok (x, y) -> Ok x, Ok y | Error e -> Error e, Error e
    
    /// <summary>Creates a Result value from a pair of Result values.</summary>
    /// <param name="x">The first Result value.</param>
    /// <param name="y">The second Result value.</param>
    ///
    /// <returns>The tupled value, or the first Error.</returns>
    let zip (x: Result<'T, 'Error>) (y: Result<'U, 'Error>) : Result<'T * 'U, 'Error> = match x, y with Ok a, Ok b -> Ok (a, b) | Error e, _ | _, Error e -> Error e

    /// <summary>Creates a Result value from a three Result values.</summary>
    /// <param name="x">The first Result value.</param>
    /// <param name="y">The second Result value.</param>
    /// <param name="z">The third Result value.</param>
    ///
    /// <returns>The tupled value, or the first Error.</returns>
    let zip3 (x: Result<'T, 'Error>) (y: Result<'U, 'Error>) (z: Result<'V, 'Error>) : Result<'T * 'U * 'V, 'Error> = match x, y, z with Ok a, Ok b, Ok c -> Ok (a, b, c) | Error e, _, _ | _, Error e, _ | _, _, Error e -> Error e

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
    let map3 f (x: Result<'T, 'Error>) (y: Result<'U, 'Error>) (z: Result<'V, 'Error>) : Result<'W, 'Error> =
        match x, y, z with
        | Ok a, Ok b, Ok c -> Ok(f a b c)
        | Error e, _, _
        | _, Error e, _
        | _, _, Error e -> Error e

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
    
    // Note: To be fixed in F#+ 2. Arguments should be flipped in order to match the generic catch.
    [<System.Obsolete("Use Result.bindError instead.")>]
    let inline catch f = function Ok v -> Ok v | Error e -> (f: 't->_) e : Result<'v,'e>

    /// <summary>If the input value is an Ok leaves it unchanged, otherwise maps the Error value and flattens the resulting nested Result.</summary>
    /// <param name="binder">A function that takes the error and transforms it into a result.</param>
    /// <param name="source">The source input value.</param>
    /// <returns>A result of the output type of the binder.</returns>
    let inline bindError (binder: 'Error->Result<'T,'Error2>) (source: Result<'T,'Error>) = match source with Ok v -> Ok v | Error e -> binder e

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
    let defaultValue (value:'T) (result: Result<'T,'Error>) : 'T = match result with Ok v -> v | _ -> value

    /// <summary>Gets the value of the result if the result is <c>Ok</c>, otherwise evaluates <paramref name="defThunk"/> and returns the result.</summary>
    ///
    /// <param name="defThunk">A thunk that provides a default value when evaluated.</param>
    /// <param name="result">The input result.</param>
    /// <remarks>
    /// Note: this function has since been added to FSharp.Core.
    /// It will be removed in next major release of FSharpPlus.
    /// </remarks>
    let defaultWith (defThunk: 'Error->'T) (result: Result<'T,'Error>) : 'T = match result with Ok v -> v | Error e -> defThunk e

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

    let apply2With combiner f (x: Result<'T,'Error>) (y: Result<'U,'Error>) : Result<'V,'Error> =
        match x, y with
        | Ok a, Ok b -> Ok (f a b)
        | Error e, Ok _ | Ok _, Error e -> Error e
        | Error e1, Error e2 -> Error (combiner e1 e2)

    let apply3With combiner f (x: Result<'T,'Error>) (y: Result<'U,'Error>) (z: Result<'V,'Error>) : Result<'W,'Error> =
        match x, y, z with
        | Ok a, Ok b, Ok c -> Ok (f a b c)
        | Error e, Ok _, Ok _ | Ok _, Error e, Ok _ | Ok _, Ok _, Error e -> Error e
        | Ok _, Error e1, Error e2 | Error e1, Ok _, Error e2 | Error e1, Error e2, Ok _ -> Error (combiner e1 e2)
        | Error e1, Error e2, Error e3 -> Error (combiner (combiner e1 e2) e3)
