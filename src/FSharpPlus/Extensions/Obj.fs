namespace FSharpPlus

/// Additional operations on (reference) objects
[<RequireQualifiedAccess>]
module Obj =

    /// <summary>Returns the source if it is not null; otherwise, returns the specified default value.</summary>
    /// <param name="value">The default value to return if the source is null.</param>
    /// <param name="source">The source object.</param>
    /// <returns>The source if it is not null; otherwise, the specified default value.</returns>
    /// <remarks>The default value is evaluated eagerly.</remarks>
#if NET9_0_OR_GREATER && !FABLE_COMPILER
    let defaultValue value (source: 'T | null) : 'T =
#else
    let defaultValue value (source: 'T) : 'T =
#endif
        match source with
        | null  -> value
        | value -> value

    /// <summary>Returns the source if it is not null; otherwise, invokes the specified function to obtain a default value.</summary>
    /// <param name="fNull">The function to invoke to obtain a default value if the source is null.</param>
    /// <param name="source">The source object.</param>
    /// <returns>The source if it is not null; otherwise, the result of invoking the specified function.</returns>
    /// <remarks>The fNull function is only invoked if the source is null.</remarks>
#if NET9_0_OR_GREATER && !FABLE_COMPILER
    let inline defaultWith ([<InlineIfLambda>]fNull: unit -> 'T) (source: 'T | null) : 'T =
#else
    let inline defaultWith ([<InlineIfLambda>]fNull: unit -> 'T) (source: 'T) : 'T =
#endif
        match source with
        | null -> fNull ()
        | value -> value

    /// <summary>Applies one of two functions to the source object based on whether it is null or not.</summary>
    /// <param name="fValue">The function to apply if the source is not null.</param>
    /// <param name="fNull">The function to apply if the source is null.</param>
    /// <param name="source">The source object.</param>
    /// <returns>The result of applying fValue to the source if it is not null; otherwise, the result of applying fNull.</returns>
    /// <remarks>Only one of the functions is invoked based on the nullity of the source.</remarks>
#if NET9_0_OR_GREATER && !FABLE_COMPILER
    let inline either ([<InlineIfLambda>]fValue: 'T -> 'U) ([<InlineIfLambda>]fNull: unit -> 'U) (source: 'T | null) : 'U =
#else
    let inline either ([<InlineIfLambda>]fValue: 'T -> 'U) ([<InlineIfLambda>]fNull: unit -> 'U) (source: 'T) : 'U =
#endif
        match source with
        | null  -> fNull ()
        | value -> fValue value
