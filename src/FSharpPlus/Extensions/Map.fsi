namespace FSharpPlus
    
    /// Additional operations on Map<'Key, 'Value>
    module Map =
        
        /// <summary>Returns the keys of the given map.</summary>
        /// <param name="source">The input map.</param>
        ///
        /// <returns>A seq of the keys in the map.</returns>
        val keys: source: Map<'Key,'T> -> seq<'Key> when 'Key: comparison
        
        /// <summary>Returns the values of the given map.</summary>
        /// <param name="source">The input map.</param>
        ///
        /// <returns>A seq of the values in the map.</returns>
        val values: source: Map<'Key,'T> -> seq<'T> when 'Key: comparison
        
        /// <summary>Maps the values of the original Map.</summary>
        /// <remarks>
        /// The core `Map.map` function maps over values too, but it passes both
        /// key and value to the mapping function.
        /// </remarks>
        /// <param name="f">The mapping function - takes only the value, and returns the mapped value.</param>
        /// <param name="x">The input Map.</param>
        ///
        /// <returns>The mapped Map.</returns>
        val mapValues:
          f: ('T -> 'a) -> x: Map<'Key,'T> -> Map<'Key,'a> when 'Key: comparison
        
        /// <summary>Maps values of two Maps.</summary>
        /// <remarks>Keys that are not present on both Maps are dropped.</remarks>
        /// <param name="f">The mapping function.</param>
        /// <param name="x">The first input Map.</param>
        /// <param name="y">The second input Map.</param>
        ///
        /// <returns>The mapped Map.</returns>
        val mapValues2:
          f: ('T1 -> 'T2 -> 'a) -> x: Map<'Key,'T1> -> y: Map<'Key,'T2>
            -> Map<'Key,'a> when 'Key: comparison
        
        /// <summary>Combines values from three maps using mapping function.</summary>
        /// <remarks>Keys that are not present on every Map are dropped.</remarks>
        /// <param name="mapping">The mapping function.</param>
        /// <param name="x">First input Map.</param>
        /// <param name="y">Second input Map.</param>
        /// <param name="z">Third input Map.</param>
        ///
        /// <returns>The mapped Map.</returns>
        val mapValues3:
          mapping: ('T1 -> 'T2 -> 'T3 -> 'a) -> x: Map<'Key,'T1>
          -> y: Map<'Key,'T2> -> z: Map<'Key,'T3> -> Map<'Key,'a>
            when 'Key: comparison
        
        /// <summary>Applies given function to each value of the given Map.</summary>
        /// <param name="mapping">The mapping function.</param>
        /// <param name="source">The input Map.</param>
        ///
        /// <returns>Returns Map with values x for each Map value where the function returns Some(x).</returns>
        val chooseValues:
          mapping: ('T -> 'a option) -> source: Map<'Key,'T> -> Map<'Key,'a>
            when 'Key: comparison
        
        /// <summary>Tuples values of two Maps.</summary>
        /// <remarks>Keys that are not present on both Maps are dropped.</remarks>
        /// <param name="x">The first input Map.</param>
        /// <param name="y">The second input Map.</param>
        ///
        /// <returns>The tupled Map.</returns>
        val zip:
          x: Map<'Key,'T1> -> y: Map<'Key,'T2> -> Map<'Key,('T1 * 'T2)>
            when 'Key: comparison
        
        /// <summary>Splits a Map with tuple pair values to two separate Maps.</summary>
        /// <param name="source">The source Map.</param>
        ///
        /// <returns>A tuple of each untupled Map.</returns>
        val unzip:
          source: Map<'Key,('T1 * 'T2)> -> Map<'Key,'T1> * Map<'Key,'T2>
            when 'Key: comparison
        
        /// Returns the union of two maps, using the combiner function for duplicate keys.
        val unionWith:
          combiner: ('Value -> 'Value -> 'Value) -> source1: Map<'Key,'Value>
          -> source2: Map<'Key,'Value> -> Map<'Key,'Value> when 'Key: comparison
        
        /// Returns the union of two maps, preferring values from the first in case of duplicate keys.
        val union:
          source: Map<'Key,'T> -> altSource: Map<'Key,'T> -> Map<'Key,'T>
            when 'Key: comparison
        
        /// Returns the intersection of two maps, using the combiner function for duplicate keys.
        val intersectWith:
          combiner: ('T -> 'T -> 'T) -> source1: Map<'Key,'T>
          -> source2: Map<'Key,'T> -> Map<'Key,'T> when 'Key: comparison
        
        ///Returns the intersection of two maps, preferring values from the first in case of duplicate keys.
        val intersect:
          source1: Map<'Key,'T> -> source2: Map<'Key,'T> -> Map<'Key,'T>
            when 'Key: comparison
        
        /// <summary>Same as chooseValues but with access to the key.</summary>
        /// <param name="f">The mapping function, taking key and element as parameters.</param>
        /// <param name="x">The input map.</param>
        ///
        /// <returns>Returns Map with values (k, v) for each Map value where the function returns Some(v).</returns>
        val choosei:
          f: ('Key -> 'T -> 'U option) -> x: Map<'Key,'T> -> Map<'Key,'U>
            when 'Key: comparison

