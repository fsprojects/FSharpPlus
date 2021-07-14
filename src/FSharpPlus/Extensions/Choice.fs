namespace FSharpPlus

/// Additional operations on Choice
[<RequireQualifiedAccess>]
module Choice =
    
    /// Creates a Choice1Of2 with the supplied value.
    let result x = Choice1Of2 x

    /// Creates a Choice2Of2 with the supplied value.
    let throw x = Choice2Of2 x

    /// Applies the wrapped value to the wrapped function when both are Choice1Of2 and returns a wrapped result or the first Choice2Of2.
    /// This is as if Choice1Of2 respresents a Success value and Choice2Of2 a Failure.
    /// <param name="f">The function wrapped in a Choice1Of2 or a Choice2Of2.</param>
    /// <param name="x">The value wrapped in a Choice1Of2 or a Choice2Of2.</param>
    /// <returns>A Choice1Of2 of the function applied to the value, or the first <c>Choice2Of2</c> if either the function or the value is <c>Choice2Of2</c>.</returns>
    let apply f (x: Choice<'T,'Error>) : Choice<'U,'Error> = match f, x with Choice1Of2 a, Choice1Of2 b -> Choice1Of2 (a b) | Choice2Of2 e, _ | _, Choice2Of2 e -> Choice2Of2 e

    /// <summary>Maps the value on the Choice1Of2 if any.</summary>
    /// <param name="mapping">A function to apply to the Choice1Of2 value.</param>
    /// <param name="source">The source input value.</param>
    /// <returns>A Choice1Of2 of the input value after applying the mapping function, or the original Choice2Of2 value if the input is Choice2Of2.</returns>
    let map (mapping: 'T->'U) (source: Choice<'T,'T2>) = match source with Choice1Of2 v -> Choice1Of2 (mapping v) | Choice2Of2 e -> Choice2Of2 e

    /// <summary>Creates a Choice value from a pair of Choice values, using a function to combine the Choice1Of2 values.</summary>
    /// <param name="mapping">A function to apply to the Choice1Of2 values.</param>
    /// <param name="x">The first Choice value.</param>
    /// <param name="y">The second Choice value.</param>
    ///
    /// <returns>The combined value, or the first Choice2Of2.</returns>
    let map2 mapping (x: Choice<'T,'Error>) (y: Choice<'U,'Error>) : Choice<'V,'Error> =
        match x, y with
        | Choice1Of2 a, Choice1Of2 b -> Choice1Of2 (mapping a b) 
        | Choice2Of2 e, _ 
        | _, Choice2Of2 e -> Choice2Of2 e

    /// <summary>Creates a Choice value from three of Choice values, using a function to combine the Choice1Of2 values.</summary>
    /// <param name="mapping">A function to apply to the Choice1Of2 values.</param>
    /// <param name="x">The first Choice value.</param>
    /// <param name="y">The second Choice value.</param>
    /// <param name="z">The third Choice value.</param>
    ///
    /// <returns>The combined value, or the first Choice2Of2.</returns>
    let map3 mapping (x: Choice<'T,'Error>) (y: Choice<'U,'Error>) (z: Choice<'V, 'Error>) : Choice<'W,'Error> =
        match x, y, z with
        | Choice1Of2 a, Choice1Of2 b, Choice1Of2 c -> Choice1Of2 (mapping a b c)
        | Choice2Of2 e, _           , _
        | _           , Choice2Of2 e, _
        | _           , _           , Choice2Of2 e -> Choice2Of2 e

    /// <summary>Flattens two nested Choice.</summary>
    /// <param name="source">The nested Choice.</param>
    /// <returns>A single Choice1Of2 of the value when it was nested with Choice1Of2s, or the Choice2Of2.</returns>
    /// <remarks><c>flatten</c> is equivalent to <c>bind id</c>.</remarks>
    let flatten source : Choice<'T1,'T2> = match source with Choice1Of2 (Choice1Of2 v) -> Choice1Of2 v | Choice1Of2 (Choice2Of2 e) | Choice2Of2 e -> Choice2Of2 e

    /// <summary>If the input value is a Choice2Of2 leaves it unchanged, otherwise maps the value on the Choice1Of2 and flattens the resulting nested Choice.</summary>
    /// <param name="binder">A function that takes the value of type T and transforms it into a Choice containing (potentially) a value of type U.</param>
    /// <param name="source">The source input value.</param>
    /// <returns>A result of the output type of the binder.</returns>
    let bind (binder: 'T->Choice<'U,'T2>) (source: Choice<'T,'T2>) = match source with Choice1Of2 v -> binder v | Choice2Of2 e -> Choice2Of2 e
    
    [<System.Obsolete("Use Choice.bindChoice2Of2")>]
    let inline catch (f: 't -> _) = function Choice1Of2 v -> Choice1Of2 v | Choice2Of2 e -> f e : Choice<'v,'e>
    
    /// <summary>If the input value is a Choice1Of2 leaves it unchanged, otherwise maps the value on the Choice2Of2 and flattens the resulting nested Choice.</summary>
    /// <param name="binder">A function that takes the value of type T and transforms it into a Choice containing (potentially) a value of type U.</param>
    /// <param name="source">The source input value.</param>
    /// <returns>A result of the output type of the binder.</returns>
    let bindChoice2Of2 (binder: 'T2->Choice<'T,'U2>) (source: Choice<'T,'T2>) = match source with Choice1Of2 v -> Choice1Of2 v | Choice2Of2 e -> binder e

    /// <summary>Extracts a value from either side of a Choice.</summary>
    /// <param name="fChoice1Of2">Function to be applied to source, if it contains a Choice1Of2 value.</param>
    /// <param name="fChoice2Of2">Function to be applied to source, if it contains a Choice2Of2 value.</param>
    /// <param name="source">The source value, containing a Choice1Of2 or a Choice2Of2.</param>
    /// <returns>The result of applying either functions.</returns>
    let inline either fChoice1Of2 fChoice2Of2 source = match source with Choice1Of2 v -> fChoice1Of2 v | Choice2Of2 e -> fChoice2Of2 e

    /// Creates a safe version of the supplied function, which returns a Choice<'U,exn> instead of throwing exceptions.
    let protect (f: 'T->'U) x =
        try
            Choice1Of2 (f x)
        with e -> Choice2Of2 e
