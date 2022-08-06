namespace FSharpPlus
    
    /// Additional operations on Dictionary<'Key, 'Value>
    module Dictionary =
        
        /// Converts a Dictionary to an IReadOnlyDictionary
        val toIReadOnlyDictionary:
          source: System.Collections.Generic.Dictionary<'Key,'Value>
            -> System.Collections.Generic.IReadOnlyDictionary<'Key,'Value>
        
        /// <summary>Tries to get the value of the given key.</summary>
        /// <remarks>Note: this is a function wrapper for the Dictionary.TryGetValue method,
        /// which also represents the result as an Option&lt;value&gt; instead of a bool
        /// and an out-value.
        /// </remarks>
        /// <param name="k">The key to find.</param>
        /// <param name="dct">The input dictionary.</param>
        ///
        /// <returns>An option wrapped value</returns>
        val tryGetValue:
          k: 'Key -> dct: System.Collections.Generic.Dictionary<'Key,'Value>
            -> 'Value option
        
        /// <summary>Does the dictionary contain the given key?</summary>
        /// <remarks>Note: this is a function wrapper for the Dictionary.ContainsKey method.</remarks>
        /// <param name="k">The key to find.</param>
        /// <param name="dct">The input dictionary.</param>
        ///
        /// <returns>A bool indicating if the key was found</returns>
        val containsKey:
          k: 'Key -> dct: System.Collections.Generic.Dictionary<'Key,'Value>
            -> bool
        
        /// <summary>Returns the keys of the given dictionary.</summary>
        /// <param name="source">The input dictionary.</param>
        ///
        /// <returns>A seq of the keys in the dictionary.</returns>
        val keys:
          source: System.Collections.Generic.Dictionary<'a,'b> -> seq<'a>
        
        /// <summary>Returns the values of the given dictionary.</summary>
        /// <param name="source">The input dictionary.</param>
        ///
        /// <returns>A seq of the values in the dictionary.</returns>
        val values:
          source: System.Collections.Generic.Dictionary<'a,'b> -> seq<'b>
        
        /// <summary>Maps the given function over each value in the dictionary.</summary>
        /// <param name="mapping">The mapping function.</param>
        /// <param name="x">The input dictionary.</param>
        ///
        /// <returns>The mapped dictionary.</returns>
        val map:
          mapping: ('T -> 'U)
          -> x: System.Collections.Generic.Dictionary<'Key,'T>
            -> System.Collections.Generic.Dictionary<'Key,'U>
            when 'Key: equality
        
        /// <summary>Creates a Dictionary value from a pair of Dictionaries, using a function to combine them.</summary>
        /// <remarks>Keys that are not present on both dictionaries are dropped.</remarks>
        /// <param name="mapping">The mapping function.</param>
        /// <param name="x">The first input dictionary.</param>
        /// <param name="y">The second input dictionary.</param>
        ///
        /// <returns>The combined dictionary.</returns>
        val map2:
          mapping: ('T1 -> 'T2 -> 'U)
          -> x: System.Collections.Generic.Dictionary<'Key,'T1>
          -> y: System.Collections.Generic.Dictionary<'Key,'T2>
            -> System.Collections.Generic.Dictionary<'Key,'U>
            when 'Key: equality
        
        /// <summary>Combines values from three Dictionaries using mapping function.</summary>
        /// <remarks>Keys that are not present on every Dictionary are dropped.</remarks>
        /// <param name="mapping">The mapping function.</param>
        /// <param name="x">First input Dictionary.</param>
        /// <param name="y">Second input Dictionary.</param>
        /// <param name="z">Third input Dictionary.</param>
        ///
        /// <returns>The mapped Dictionary.</returns>
        val map3:
          mapping: ('T1 -> 'T2 -> 'T3 -> 'U)
          -> x: System.Collections.Generic.Dictionary<'Key,'T1>
          -> y: System.Collections.Generic.Dictionary<'Key,'T2>
          -> z: System.Collections.Generic.Dictionary<'Key,'T3>
            -> System.Collections.Generic.Dictionary<'Key,'U>
            when 'Key: equality
        
        /// <summary>Applies given function to each value of the given dictionary.</summary>
        /// <param name="f">The mapping function.</param>
        /// <param name="x">The input dictionary.</param>
        ///
        /// <returns>Returns dictionary with values x for each dictionary value where the function returns Some(x).</returns>
        val chooseValues:
          f: ('T -> 'U option)
          -> x: System.Collections.Generic.IDictionary<'Key,'T>
            -> System.Collections.Generic.Dictionary<'Key,'U>
            when 'Key: equality
        
        /// <summary>Tuples values of two dictionaries.</summary>
        /// <remarks>Keys that are not present on both dictionaries are dropped.</remarks>
        /// <param name="x">The first input dictionary.</param>
        /// <param name="y">The second input dictionary.</param>
        ///
        /// <returns>The tupled dictionary.</returns>
        val zip:
          x: System.Collections.Generic.Dictionary<'Key,'T1>
          -> y: System.Collections.Generic.Dictionary<'Key,'T2>
            -> System.Collections.Generic.Dictionary<'Key,('T1 * 'T2)>
            when 'Key: equality
        
        /// <summary>Splits a dictionary with tuple pair values to two separate dictionaries.</summary>
        /// <param name="source">The source dictionary.</param>
        ///
        /// <returns>A tuple of each untupled dictionary.</returns>
        val unzip:
          source: System.Collections.Generic.Dictionary<'Key,('T1 * 'T2)>
            -> System.Collections.Generic.Dictionary<'Key,'T1> *
               System.Collections.Generic.Dictionary<'Key,'T2>
            when 'Key: equality
        
        /// Returns the union of two dictionaries, using the combiner function for duplicate keys.
        val unionWith:
          combiner: ('Value -> 'Value -> 'Value)
          -> source1: System.Collections.Generic.Dictionary<'Key,'Value>
          -> source2: System.Collections.Generic.Dictionary<'Key,'Value>
            -> System.Collections.Generic.Dictionary<'Key,'Value>
            when 'Key: equality
        
        ///Returns the union of two maps, preferring values from the first in case of duplicate keys.
        val union:
          source: System.Collections.Generic.Dictionary<'Key,'T>
          -> altSource: System.Collections.Generic.Dictionary<'Key,'T>
            -> System.Collections.Generic.Dictionary<'Key,'T>
            when 'Key: equality
        
        /// Returns the intersection of two Dicts, using the combiner function for duplicate keys.
        val intersectWith:
          combiner: ('T -> 'T -> 'T)
          -> source1: System.Collections.Generic.Dictionary<'Key,'T>
          -> source2: System.Collections.Generic.Dictionary<'Key,'T>
            -> System.Collections.Generic.Dictionary<'Key,'T>
        
        ///Returns the intersection of two maps, preferring values from the first in case of duplicate keys.
        val intersect:
          source1: System.Collections.Generic.Dictionary<'Key,'T>
          -> source2: System.Collections.Generic.Dictionary<'Key,'T>
            -> System.Collections.Generic.Dictionary<'Key,'T>
        
        /// <summary>Same as chooseValues but with access to the key.</summary>
        /// <param name="f">The mapping function, taking key and element as parameters.</param>
        /// <param name="x">The input dictionary.</param>
        ///
        /// <returns>Dictionary with values (k, x) for each dictionary value where the function returns Some(x).</returns>
        val choosei:
          f: ('Key -> 'T -> 'U option)
          -> x: System.Collections.Generic.IDictionary<'Key,'T>
            -> System.Collections.Generic.Dictionary<'Key,'U>
            when 'Key: equality

