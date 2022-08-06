namespace FSharpPlus
    
    /// Additional operations on IReadOnlyDictionary<'Key, 'Value>
    module IReadOnlyDictionary =
        
        /// Replaces or sets the item associated with a specified key with the specified value.
        val add:
          key: 'Key -> value: 'Value
          -> table: System.Collections.Generic.IReadOnlyDictionary<'Key,'Value>
            -> System.Collections.Generic.IReadOnlyDictionary<'Key,'Value>
            when 'Key: comparison
        
        /// Removes the given key from the read-only dictionary.
        val remove:
          key: 'Key
          -> table: System.Collections.Generic.IReadOnlyDictionary<'Key,'Value>
            -> System.Collections.Generic.IReadOnlyDictionary<'Key,'Value>
            when 'Key: comparison
        
        /// <summary>Tries to get the value of the given key.</summary>
        /// <remarks>This is a function wrapper for the IReadOnlyDictionary.TryGetValue method,
        /// representing the result as an Option&lt;value&gt; instead of a bool plus an out-value.
        /// </remarks>
        /// <param name="k">The key whose value you wish to find.</param>
        /// <param name="dct">The input IReadOnlyDictionary.</param>
        ///
        /// <returns>An option wrapped value.</returns>
        val tryGetValue:
          k: 'Key
          -> dct: System.Collections.Generic.IReadOnlyDictionary<'Key,'Value>
            -> 'Value option
        
        /// <summary>Does the read-only dictionary contain the given key?</summary>
        /// <remarks>Note: this is a function wrapper for the IReadOnlyDictionary.ContainsKey method.</remarks>
        /// <param name="k">The key to find.</param>
        /// <param name="dct">The input IReadOnlyDictionary.</param>
        ///
        /// <returns>A bool indicating if the key was found.</returns>
        val containsKey:
          k: 'Key
          -> dct: System.Collections.Generic.IReadOnlyDictionary<'Key,'Value>
            -> bool
        
        /// <summary>Returns the keys of the given read-only dictionary.</summary>
        /// <param name="source">The input IReadOnlyDictionary.</param>
        ///
        /// <returns>A seq of the keys in the IReadOnlyDictionary.</returns>
        val keys:
          source: System.Collections.Generic.IReadOnlyDictionary<'Key,'Value>
            -> seq<'Key>
        
        /// <summary>Returns the values of the given read-only dictionary.</summary>
        /// <param name="source">The input IReadOnlyDictionary.</param>
        ///
        /// <returns>A seq of the values in the read-only dictionary.</returns>
        val values:
          source: System.Collections.Generic.IReadOnlyDictionary<'Key,'Value>
            -> seq<'Value>
        
        /// <summary>Maps the given function over each value in the read-only dictionary.</summary>
        /// <param name="f">The mapping function.</param>
        /// <param name="x">The input IReadOnlyDictionary.</param>
        ///
        /// <returns>The mapped IReadOnlyDictionary.</returns>
        val map:
          f: ('T -> 'U)
          -> x: System.Collections.Generic.IReadOnlyDictionary<'Key,'T>
            -> System.Collections.Generic.IReadOnlyDictionary<'Key,'U>
            when 'Key: equality
        
        /// <summary>Creates a read-only dictionary value from a pair of read-only dictionaries,
        /// using a function to combine them.</summary>
        /// <remarks>Keys that are not present on both read-only dictionaries are dropped.</remarks>
        /// <param name="f">The mapping function.</param>
        /// <param name="x">The first input IReadOnlyDictionary.</param>
        /// <param name="y">The second input IReadOnlyDictionary.</param>
        ///
        /// <returns>The combined IReadOnlyDictionary.</returns>
        val map2:
          f: ('T1 -> 'T2 -> 'U)
          -> x: System.Collections.Generic.IReadOnlyDictionary<'Key,'T1>
          -> y: System.Collections.Generic.IReadOnlyDictionary<'Key,'T2>
            -> System.Collections.Generic.IReadOnlyDictionary<'Key,'U>
            when 'Key: equality
        
        /// <summary>Applies given function to each value of the given read-only dictionary.</summary>
        /// <param name="f">The mapping function.</param>
        /// <param name="x">The input IReadOnlyDictionary.</param>
        ///
        /// <returns>Returns IReadOnlyDictionary with values x for each dictionary value where the function returns Some(x).</returns>
        val chooseValues:
          f: ('T -> 'U option)
          -> x: System.Collections.Generic.IReadOnlyDictionary<'Key,'T>
            -> System.Collections.Generic.IReadOnlyDictionary<'Key,'U>
            when 'Key: equality
        
        /// <summary>Tuples values of two read-only dictionaries.</summary>
        /// <remarks>Keys that are not present on both read-only dictionaries are dropped.</remarks>
        /// <param name="x">The first input IReadOnlyDictionary.</param>
        /// <param name="y">The second input IReadOnlyDictionary.</param>
        ///
        /// <returns>The tupled IReadOnlyDictionary.</returns>
        val zip:
          x: System.Collections.Generic.IReadOnlyDictionary<'Key,'T1>
          -> y: System.Collections.Generic.IReadOnlyDictionary<'Key,'T2>
            -> System.Collections.Generic.IReadOnlyDictionary<'Key,('T1 * 'T2)>
            when 'Key: equality
        
        /// <summary>Splits a read-only dictionary with tuple pair values to two separate read-only dictionaries.</summary>
        /// <param name="source">The source IReadOnlyDictionary.</param>
        ///
        /// <returns>A tuple of each untupled IReadOnlyDictionary.</returns>
        val unzip:
          source: System.Collections.Generic.IReadOnlyDictionary<'Key,
                                                                 ('T1 * 'T2)>
            -> System.Collections.Generic.IReadOnlyDictionary<'Key,'T1> *
               System.Collections.Generic.IReadOnlyDictionary<'Key,'T2>
            when 'Key: equality
        
        /// Returns the union of two read-only dictionaries, using the combiner function for duplicate keys.
        val unionWith:
          combiner: ('Value -> 'Value -> 'Value)
          -> source1: System.Collections.Generic.IReadOnlyDictionary<'Key,'Value>
          -> source2: System.Collections.Generic.IReadOnlyDictionary<'Key,'Value>
            -> System.Collections.Generic.IReadOnlyDictionary<'Key,'Value>
            when 'Key: equality
        
        /// Returns the union of two read-only dictionaries, preferring values from the first in case of duplicate keys.
        val union:
          source: System.Collections.Generic.IReadOnlyDictionary<'Key,'T>
          -> altSource: System.Collections.Generic.IReadOnlyDictionary<'Key,'T>
            -> System.Collections.Generic.IReadOnlyDictionary<'Key,'T>
            when 'Key: equality
        
        /// Returns the intersection of two read-only dictionaries, using the combiner function for duplicate keys.
        val intersectWith:
          combiner: ('T -> 'T -> 'T)
          -> source1: System.Collections.Generic.IReadOnlyDictionary<'Key,'T>
          -> source2: System.Collections.Generic.IReadOnlyDictionary<'Key,'T>
            -> System.Collections.Generic.IReadOnlyDictionary<'Key,'T>
        
        /// Returns the intersection of two read-only dictionaries, preferring values from the first in case of duplicate keys.
        val intersect:
          source1: System.Collections.Generic.IReadOnlyDictionary<'Key,'T>
          -> source2: System.Collections.Generic.IReadOnlyDictionary<'Key,'T>
            -> System.Collections.Generic.IReadOnlyDictionary<'Key,'T>

