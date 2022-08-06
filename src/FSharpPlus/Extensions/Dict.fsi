namespace FSharpPlus
    
    /// Additional operations on IDictionary<'Key, 'Value>
    module Dict =
        
        /// Converts an IDictionary to an IReadOnlyDictionary.
        val toIReadOnlyDictionary:
          source: System.Collections.Generic.IDictionary<'a,'b>
            -> System.Collections.Generic.IReadOnlyDictionary<'a,'b>
        
        /// <summary>Tries to get the value of the given key.</summary>
        /// <remarks>This is a function wrapper for the IDictionary.TryGetValue method,
        /// representing the result as an Option&lt;value&gt; instead of a bool plus an out-value.
        /// </remarks>
        /// <param name="k">The key whose value you wish to find.</param>
        /// <param name="dct">The input dictionary.</param>
        ///
        /// <returns>An option wrapped value</returns>
        val tryGetValue:
          k: 'Key -> dct: System.Collections.Generic.IDictionary<'Key,'Value>
            -> 'Value option
        
        /// <summary>Does the dictionary contain the given key?</summary>
        /// <remarks>Note: this is a function wrapper for the IDictionary.ContainsKey method</remarks>
        /// <param name="k">The key to find.</param>
        /// <param name="dct">The input dictionary.</param>
        ///
        /// <returns>A bool indicating if the key was found</returns>
        val containsKey:
          k: 'Key -> dct: System.Collections.Generic.IDictionary<'Key,'Value>
            -> bool
        
        /// <summary>Returns the keys of the given dictionary.</summary>
        /// <param name="source">The input dictionary.</param>
        ///
        /// <returns>A seq of the keys in the dictionary.</returns>
        val keys:
          source: System.Collections.Generic.IDictionary<'a,'b> -> seq<'a>
        
        /// <summary>Returns the values of the given dictionary.</summary>
        /// <param name="source">The input dictionary.</param>
        ///
        /// <returns>A seq of the values in the dictionary.</returns>
        val values:
          source: System.Collections.Generic.IDictionary<'a,'b> -> seq<'b>
        
        /// <summary>Maps the given function over each value in the dictionary.</summary>
        /// <param name="f">The mapping function.</param>
        /// <param name="x">The input dictionary.</param>
        ///
        /// <returns>The mapped dictionary.</returns>
        val map:
          f: ('T -> 'U) -> x: System.Collections.Generic.IDictionary<'Key,'T>
            -> System.Collections.Generic.IDictionary<'Key,'U>
            when 'Key: equality
        
        /// <summary>Creates a Dictionary value from a pair of Dictionaries, using a function to combine them.</summary>
        /// <remarks>Keys that are not present on both dictionaries are dropped.</remarks>
        /// <param name="f">The mapping function.</param>
        /// <param name="x">The first input dictionary.</param>
        /// <param name="y">The second input dictionary.</param>
        ///
        /// <returns>The combined dictionary.</returns>
        val map2:
          f: ('T1 -> 'T2 -> 'U)
          -> x: System.Collections.Generic.IDictionary<'Key,'T1>
          -> y: System.Collections.Generic.IDictionary<'Key,'T2>
            -> System.Collections.Generic.IDictionary<'Key,'U>
            when 'Key: equality
        
        /// <summary>Applies given function to each value of the given dictionary.</summary>
        /// <param name="f">The mapping function.</param>
        /// <param name="x">The input dictionary.</param>
        ///
        /// <returns>Returns dictionary with values x for each dictionary value where the function returns Some(x).</returns>
        val chooseValues:
          f: ('T -> 'U option)
          -> x: System.Collections.Generic.IDictionary<'Key,'T>
            -> System.Collections.Generic.IDictionary<'Key,'U>
            when 'Key: equality
        
        /// <summary>Tuples values of two dictionaries.</summary>
        /// <remarks>Keys that are not present on both dictionaries are dropped.</remarks>
        /// <param name="x">The first input dictionary.</param>
        /// <param name="y">The second input dictionary.</param>
        ///
        /// <returns>The tupled dictionary.</returns>
        val zip:
          x: System.Collections.Generic.IDictionary<'Key,'T1>
          -> y: System.Collections.Generic.IDictionary<'Key,'T2>
            -> System.Collections.Generic.IDictionary<'Key,('T1 * 'T2)>
            when 'Key: equality
        
        /// <summary>Splits a dictionary with tuple pair values to two separate dictionaries.</summary>
        /// <param name="source">The source dictionary.</param>
        ///
        /// <returns>A tuple of each untupled dictionary.</returns>
        val unzip:
          source: System.Collections.Generic.IDictionary<'Key,('T1 * 'T2)>
            -> System.Collections.Generic.IDictionary<'Key,'T1> *
               System.Collections.Generic.IDictionary<'Key,'T2>
            when 'Key: equality
        
        /// Returns the union of two dictionaries, using the combiner function for duplicate keys.
        val unionWith:
          combiner: ('Value -> 'Value -> 'Value)
          -> source1: System.Collections.Generic.IDictionary<'Key,'Value>
          -> source2: System.Collections.Generic.IDictionary<'Key,'Value>
            -> System.Collections.Generic.IDictionary<'Key,'Value>
            when 'Key: equality
        
        ///Returns the union of two maps, preferring values from the first in case of duplicate keys.
        val union:
          source: System.Collections.Generic.IDictionary<'Key,'T>
          -> altSource: System.Collections.Generic.IDictionary<'Key,'T>
            -> System.Collections.Generic.IDictionary<'Key,'T>
            when 'Key: equality
        
        /// Returns the intersection of two Dicts, using the combiner function for duplicate keys.
        val intersectWith:
          combiner: ('T -> 'T -> 'T)
          -> source1: System.Collections.Generic.IDictionary<'Key,'T>
          -> source2: System.Collections.Generic.IDictionary<'Key,'T>
            -> System.Collections.Generic.IDictionary<'Key,'T>
        
        ///Returns the intersection of two Dicts, preferring values from the first in case of duplicate keys.
        val intersect:
          source1: System.Collections.Generic.IDictionary<'Key,'T>
          -> source2: System.Collections.Generic.IDictionary<'Key,'T>
            -> System.Collections.Generic.IDictionary<'Key,'T>
        
        /// <summary>Choose with access to the key.</summary>
        /// <param name="f">The mapping function, taking key and element as parameters.</param>
        /// <param name="x">The input dictionary.</param>
        ///
        /// <returns>Dictionary with values (k, x) for each dictionary value where the function returns Some(x).</returns>
        val choosei:
          f: ('Key -> 'T -> 'U option)
          -> x: System.Collections.Generic.IDictionary<'Key,'T>
            -> System.Collections.Generic.IDictionary<'Key,'U>
            when 'Key: equality

