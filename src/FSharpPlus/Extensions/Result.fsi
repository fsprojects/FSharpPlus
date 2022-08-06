namespace FSharpPlus
    
    /// Additional operations on Result<'T,'Error>
    module Result =
        
        /// Creates an Ok with the supplied value.
        val result: value: 'T -> Result<'T,'Error>
        
        /// Creates an Error With the supplied value.
        val throw: value: 'Error -> Result<'T,'Error>
        
        /// Applies the wrapped value to the wrapped function when both are Ok and returns a wrapped result or the first Error.
        /// <param name="f">The function wrapped in an Ok or an Error.</param>
        /// <param name="x">The value wrapped in an Ok or an Error.</param>
        /// <returns>An Ok of the function applied to the value, or the first <c>Error</c> if either the function or the value is <c>Error</c>.</returns>
        val apply:
          f: Result<('T -> 'U),'Error> -> x: Result<'T,'Error>
            -> Result<'U,'Error>
        
        /// <summary>Creates a Result value from a pair of Result values, using a function to combine them.</summary>
        /// <param name="f">The mapping function.</param>
        /// <param name="x">The first Result value.</param>
        /// <param name="y">The second Result value.</param>
        ///
        /// <returns>The combined value, or the first Error.</returns>
        val map2:
          f: ('T -> 'U -> 'V) -> x: Result<'T,'Error> -> y: Result<'U,'Error>
            -> Result<'V,'Error>
        
        /// <summary>Creates a Result value from three Result values, using a function to combine them.</summary>
        /// <param name="f">The mapping function.</param>
        /// <param name="x">The first Result value.</param>
        /// <param name="y">The second Result value.</param>
        /// <param name="z">The third Result value.</param>
        ///
        /// <returns>The combined value, or the first Error.</returns>
        val map3:
          f: ('T -> 'U -> 'V -> 'W) -> x: Result<'T,'Error>
          -> y: Result<'U,'Error> -> z: Result<'V,'Error> -> Result<'W,'Error>
        
        /// <summary>Flattens two nested Results.</summary>
        /// <param name="source">The nested Results.</param>
        /// <returns>A single Ok of the value when it was nested with OKs, or the Error.</returns>
        /// <remarks><c>flatten</c> is equivalent to <c>bind id</c>.</remarks>
        val flatten:
          source: Result<Result<'T,'Error>,'Error> -> Result<'T,'Error>
        
        [<System.Obsolete ("Use Result.bindError instead.")>]
        val inline catch:
          f: ('t -> Result<'v,'e>) -> _arg1: Result<'v,'t> -> Result<'v,'e>
        
        /// <summary>If the input value is an Ok leaves it unchanged, otherwise maps the Error value and flattens the resulting nested Result.</summary>
        /// <param name="binder">A function that takes the error and transforms it into a result.</param>
        /// <param name="source">The source input value.</param>
        /// <returns>A result of the output type of the binder.</returns>
        val inline bindError:
          binder: ('Error -> Result<'T,'Error2>) -> source: Result<'T,'Error>
            -> Result<'T,'Error2>
        
        /// <summary>Extracts a value from either side of a Result.</summary>
        /// <param name="fOk">Function to be applied to source, if it contains an Ok value.</param>
        /// <param name="fError">Function to be applied to source, if it contains an Error value.</param>
        /// <param name="source">The source value, containing an Ok or an Error.</param>
        /// <returns>The result of applying either functions.</returns>
        val inline either:
          fOk: ('T -> 'U) -> fError: ('Error -> 'U) -> source: Result<'T,'Error>
            -> 'U
        
        /// Creates a safe version of the supplied function, which returns a Result<'U,exn> instead of throwing exceptions.
        val protect: f: ('T -> 'U) -> x: 'T -> Result<'U,exn>
        
        /// Gets the 'Ok' value. If it's an 'Error' this function will throw an exception.
        val get: source: Result<'T,'Error> -> 'T
        
        /// Extracts the Ok value or use the supplied default value when it's an Error.
        val defaultValue: value: 'T -> source: Result<'T,'Error> -> 'T
        
        /// Extracts the Ok value or applies the compensation function over the Error.
        val defaultWith:
          compensation: ('Error -> 'T) -> source: Result<'T,'Error> -> 'T
        
        /// Converts a Result<'T,'Error> to a Choice<'T,'Error>.
        val toChoice: source: Result<'T,'U> -> Choice<'T,'U>
        
        /// Creates a Result<'T,'Error> from a Choice<'T,'Error>.
        val ofChoice: source: Choice<'T,'U> -> Result<'T,'U>
        
        /// <summary>
        /// Creates two lists by classifying the values depending on whether they were wrapped with Ok or Error.
        /// </summary>
        /// <returns>
        /// A tuple with both resulting lists, Oks are in the first list.
        /// </returns>
        val partition: source: Result<'T,'Error> list -> 'T list * 'Error list

