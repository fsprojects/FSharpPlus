namespace FSharpPlus
    
    /// Additional operations on Lazy - delayed computations
    module Lazy =
        
        /// <summary>Creates a Lazy value from another Lazy value, mapping through a function.</summary>
        /// <param name="mapping">The mapping function.</param>
        /// <param name="x">The Lazy value.</param>
        ///
        /// <returns>The mappeed value.</returns>
        val map: mapping: ('T -> 'U) -> x: Lazy<'T> -> Lazy<'U>
        
        /// <summary>Creates a Lazy value from a pair of Lazy values, using a mapping function to combine them.</summary>
        /// <param name="mapping">The mapping function.</param>
        /// <param name="x">The first Lazy value.</param>
        /// <param name="y">The second Lazy value.</param>
        ///
        /// <returns>The combined value.</returns>
        val map2:
          mapping: ('T -> 'U -> 'V) -> x: Lazy<'T> -> y: Lazy<'U>
            -> System.Lazy<'V>
        
        /// <summary>Creates a Lazy value from three Lazy values, using a function to combine them.</summary>
        /// <param name="mapping">The mapping function.</param>
        /// <param name="x">The first Lazy value.</param>
        /// <param name="y">The second Lazy value.</param>
        /// <param name="z">The third Lazy value.</param>
        ///
        /// <returns>The combined value.</returns>
        val map3:
          mapping: ('T -> 'U -> 'V -> 'W) -> x: Lazy<'T> -> y: Lazy<'U>
          -> z: Lazy<'V> -> System.Lazy<'W>
        
        /// <summary>Applies a Lazy value to a Lazy function.</summary>
        /// <param name="f">The Lazy function.</param>
        /// <param name="x">The Lazy value.</param>
        /// <returns>A Lazy value of the function applied to the value.</returns>
        val apply: f: Lazy<('T -> 'U)> -> x: Lazy<'T> -> Lazy<'U>

