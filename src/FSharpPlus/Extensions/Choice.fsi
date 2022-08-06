namespace FSharpPlus
    
    /// Additional operations on Choice
    module Choice =
        
        /// Creates a Choice1Of2 with the supplied value.
        val result: x: 'a -> Choice<'a,'b>
        
        /// Creates a Choice2Of2 with the supplied value.
        val throw: x: 'a -> Choice<'b,'a>
        
        /// Applies the wrapped value to the wrapped function when both are Choice1Of2 and returns a wrapped result or the first Choice2Of2.
        /// This is as if Choice1Of2 respresents a Success value and Choice2Of2 a Failure.
        /// <param name="f">The function wrapped in a Choice1Of2 or a Choice2Of2.</param>
        /// <param name="x">The value wrapped in a Choice1Of2 or a Choice2Of2.</param>
        /// <returns>A Choice1Of2 of the function applied to the value, or the first <c>Choice2Of2</c> if either the function or the value is <c>Choice2Of2</c>.</returns>
        val apply:
          f: Choice<('T -> 'U),'Error> -> x: Choice<'T,'Error>
            -> Choice<'U,'Error>
        
        /// <summary>Maps the value on the Choice1Of2 if any.</summary>
        /// <param name="mapping">A function to apply to the Choice1Of2 value.</param>
        /// <param name="source">The source input value.</param>
        /// <returns>A Choice1Of2 of the input value after applying the mapping function, or the original Choice2Of2 value if the input is Choice2Of2.</returns>
        val map: mapping: ('T -> 'U) -> source: Choice<'T,'T2> -> Choice<'U,'T2>
        
        /// <summary>Creates a Choice value from a pair of Choice values, using a function to combine the Choice1Of2 values.</summary>
        /// <param name="mapping">A function to apply to the Choice1Of2 values.</param>
        /// <param name="x">The first Choice value.</param>
        /// <param name="y">The second Choice value.</param>
        ///
        /// <returns>The combined value, or the first Choice2Of2.</returns>
        val map2:
          mapping: ('T -> 'U -> 'V) -> x: Choice<'T,'Error>
          -> y: Choice<'U,'Error> -> Choice<'V,'Error>
        
        /// <summary>Creates a Choice value from three of Choice values, using a function to combine the Choice1Of2 values.</summary>
        /// <param name="mapping">A function to apply to the Choice1Of2 values.</param>
        /// <param name="x">The first Choice value.</param>
        /// <param name="y">The second Choice value.</param>
        /// <param name="z">The third Choice value.</param>
        ///
        /// <returns>The combined value, or the first Choice2Of2.</returns>
        val map3:
          mapping: ('T -> 'U -> 'V -> 'W) -> x: Choice<'T,'Error>
          -> y: Choice<'U,'Error> -> z: Choice<'V,'Error> -> Choice<'W,'Error>
        
        /// <summary>Flattens two nested Choice.</summary>
        /// <param name="source">The nested Choice.</param>
        /// <returns>A single Choice1Of2 of the value when it was nested with Choice1Of2s, or the Choice2Of2.</returns>
        /// <remarks><c>flatten</c> is equivalent to <c>bind id</c>.</remarks>
        val flatten: source: Choice<Choice<'T1,'T2>,'T2> -> Choice<'T1,'T2>
        
        /// <summary>If the input value is a Choice2Of2 leaves it unchanged, otherwise maps the value on the Choice1Of2 and flattens the resulting nested Choice.</summary>
        /// <param name="binder">A function that takes the value of type T and transforms it into a Choice containing (potentially) a value of type U.</param>
        /// <param name="source">The source input value.</param>
        /// <returns>A result of the output type of the binder.</returns>
        val bind:
          binder: ('T -> Choice<'U,'T2>) -> source: Choice<'T,'T2>
            -> Choice<'U,'T2>
        
        [<System.Obsolete ("Use Choice.bindChoice2Of2")>]
        val inline catch:
          f: ('t -> Choice<'v,'e>) -> _arg1: Choice<'v,'t> -> Choice<'v,'e>
        
        /// <summary>If the input value is a Choice1Of2 leaves it unchanged, otherwise maps the value on the Choice2Of2 and flattens the resulting nested Choice.</summary>
        /// <param name="binder">A function that takes the value of type T and transforms it into a Choice containing (potentially) a value of type U.</param>
        /// <param name="source">The source input value.</param>
        /// <returns>A result of the output type of the binder.</returns>
        val bindChoice2Of2:
          binder: ('T2 -> Choice<'T,'U2>) -> source: Choice<'T,'T2>
            -> Choice<'T,'U2>
        
        /// <summary>Extracts a value from either side of a Choice.</summary>
        /// <param name="fChoice1Of2">Function to be applied to source, if it contains a Choice1Of2 value.</param>
        /// <param name="fChoice2Of2">Function to be applied to source, if it contains a Choice2Of2 value.</param>
        /// <param name="source">The source value, containing a Choice1Of2 or a Choice2Of2.</param>
        /// <returns>The result of applying either functions.</returns>
        val inline either:
          fChoice1Of2: ('a -> 'b) -> fChoice2Of2: ('c -> 'b)
          -> source: Choice<'a,'c> -> 'b
        
        /// Creates a safe version of the supplied function, which returns a Choice<'U,exn> instead of throwing exceptions.
        val protect: f: ('T -> 'U) -> x: 'T -> Choice<'U,exn>

