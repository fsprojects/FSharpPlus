namespace FSharpPlus
    
    /// Additional operations on Array
    module Array =
        
        /// <summary>Applies an array of functions to an array of values and concatenates them.</summary>
        /// <param name="f">The array of functions.</param>
        /// <param name="x">The array of values.</param>
        /// <returns>A concatenated array of the resulting arrays from applying each function to each value</returns>
        /// 
        /// <example>
        /// <code>
        /// > Array.apply [|double; triple|] [|1; 2; 3|];;  
        /// val it : int [] = [|2; 4; 6; 3; 6; 9|]
        /// </code>
        /// </example>
        val apply: f: ('a -> 'b)[] -> x: 'a[] -> 'b[]
        
        /// Combines all values from the first array with the second, using the supplied mapping function.
        val lift2: f: ('a -> 'b -> 'c) -> x: 'a[] -> y: 'b[] -> 'c[]
        
        /// <summary>Combines all values from three arrays and calls a mapping function on this combination.</summary>
        /// <param name="mapping">Mapping function taking three element combination as input.</param>
        /// <param name="list1">First array.</param>
        /// <param name="list2">Second array.</param>
        /// <param name="list3">Third array.</param>
        ///
        /// <returns>Array with values returned from mapping function.</returns>
        val lift3:
          mapping: ('a -> 'b -> 'c -> 'd) -> list1: 'a[] -> list2: 'b[]
          -> list3: 'c[] -> 'd[]
        
        /// Concatenates all elements, using the specified separator between each element.
        val intercalate: separator: 'a[] -> source: seq<'a[]> -> 'a[]
        
        /// Inserts a separator element between each element in the source array.
        val intersperse: element: 'T -> source: 'T[] -> 'T[]
        
        /// Creates a sequence of arrays by splitting the source array on any of the given separators.
        val split:
          separators: seq<'a[]> -> source: 'a[] -> seq<'a[]> when 'a: equality
        
        /// Replaces a subsequence of the source array with the given replacement array.
        val replace:
          oldValue: 'T[] -> newValue: 'T[] -> source: 'T[] -> 'T[]
            when 'T: equality
        
        /// <summary>
        /// Returns the index of the first occurrence of the specified slice in the source.
        /// </summary>
        /// <exception cref="System.ArgumentException">
        /// Thrown when the slice was not found in the sequence.
        /// </exception>
        /// <returns>
        /// The index of the slice.
        /// </returns>
        /// <summary>
        /// Returns the index of the first occurrence of the specified slice in the source.
        /// Note: this is unsafe and will throw ArgumentException when the specified slice is not found.
        /// </summary>
        /// <returns>
        /// The index of the slice or <c>None</c>.
        /// </returns>
        val findSliceIndex: slice: 'a[] -> source: 'a[] -> int
        
        /// <summary>
        /// Returns the index of the first occurrence of the specified slice in the source.
        /// Returns <c>None</c> if not found.
        /// </summary>
        /// <returns>
        /// The index of the slice or <c>None</c>.
        /// </returns>
        val tryFindSliceIndex: slice: 'a[] -> source: 'a[] -> int option
        
        /// <summary>
        /// Creates two arrays by applying the mapper function to each element in the array
        /// and classifies the transformed values depending on whether they were wrapped with Choice1Of2 or Choice2Of2.
        /// </summary>
        /// <returns>
        /// A tuple with both resulting arrays.
        /// </returns>
        val partitionMap:
          mapper: ('T -> Choice<'T1,'T2>) -> source: 'T array -> 'T1[] * 'T2[]
        
        /// <summary>Safely build a new array whose elements are the results of applying the given function
        /// to each of the elements of the two arrays pairwise.</summary>
        /// <remark>If one array is shorter, excess elements are discarded from the right end of the longer array.</remark>
        val map2Shortest: f: ('T -> 'U -> 'a) -> a1: 'T[] -> a2: 'U[] -> 'a[]
        
        /// <summary>
        /// Zip safely two arrays. If one array is shorter, excess elements are discarded from the right end of the longer array. 
        /// </summary>
        /// <param name="a1">First input array.</param>
        /// <param name="a2">Second input array.</param>
        /// <returns>Array with corresponding pairs of input arrays.</returns>
        val zipShortest: a1: 'T1 array -> a2: 'T2 array -> ('T1 * 'T2)[]
        
        /// <summary>Same as choose but with access to the index.</summary>
        /// <param name="mapping">The mapping function, taking index and element as parameters.</param>
        /// <param name="source">The input array.</param>
        ///
        /// <returns>Array with values x for each Array value where the function returns Some(x).</returns>
        val choosei: mapping: (int -> 'a -> 'b option) -> source: 'a[] -> 'b[]

