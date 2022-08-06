namespace FSharpPlus
    
    /// Additional operations on Option
    module Option =
        
        /// <summary>Applies an option value to an option function.</summary>
        /// <param name="f">The option function.</param>
        /// <param name="x">The option value.</param>
        /// <returns>An option of the function applied to the value, or <c>None</c> if either the function or the value is <c>None</c>.</returns>
        val apply: f: ('T -> 'U) option -> x: 'T option -> 'U option
        
        /// <summary>If value is Some, returns both of them tupled. Otherwise it returns None tupled.</summary>
        /// <param name="v">The value.</param>
        /// <returns>The resulting tuple.</returns>
        val unzip: v: ('T * 'U) option -> 'T option * 'U option
        
        /// <summary>If both value are Some, returns both of them tupled. Otherwise it returns None.</summary>
        /// <param name="x">The first value.</param>
        /// <param name="y">The second value.</param>
        /// <returns>The resulting option.</returns>
        val zip: x: 'T option -> y: 'U option -> ('T * 'U) option
        
        /// <summary>Converts an option to a Result.</summary>
        /// <param name="source">The option value.</param>
        /// <returns>The resulting Result value.</returns>
        val toResult: source: 'T option -> Result<'T,unit>
        
        /// <summary>Converts an option to a Result.</summary>
        /// <param name="errorValue">The error value to be used in case of None.</param>
        /// <param name="source">The option value.</param>
        /// <returns>The resulting Result value.</returns>
        val toResultWith:
          errorValue: 'Error -> source: 'T option -> Result<'T,'Error>
        
        /// <summary>Converts a Result to an option.</summary>
        /// <remarks>The error value (if any) is lost.</remarks>
        /// <param name="source">The Result value.</param>
        /// <returns>The resulting option value.</returns>
        val ofResult: source: Result<'T,'Error> -> 'T option
        
        /// Creates a safe version of the supplied function, which returns an option<'U> instead of throwing exceptions.
        val protect: f: ('T -> 'U) -> x: 'T -> 'U option
        
        /// <summary>Converts pair of bool and value to Option.</summary>
        /// <remarks>Useful for handling C# try pattern with `out` parameter. E.g. `Int.TryParse` or `Dictionary.TryGetValue`.</remarks>
        /// <param name="pair">Pair of bool and value.</param>
        /// <returns><c>Some</c> if bool is `true`, <c>None</c> otherwise.</returns>
        val ofPair: bool * 'T -> 'T option

