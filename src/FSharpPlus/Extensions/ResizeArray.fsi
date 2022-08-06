namespace FSharpPlus
    
    /// Additional operations on ResizeArray
    module ResizeArray =
        
        /// <summary>Builds a new ResizeArray whose elements are the results of applying the given function
        /// to each of the elements of the ResizeArray.</summary>
        ///
        /// <param name="mapping">A function to transform items from the input ResizeArray.</param>
        /// <param name="source">The input ResizeArray.</param>
        ///
        /// <returns>The result ResizeArray.</returns>
        ///
        /// <exception cref="System.ArgumentNullException">Thrown when the input ResizeArray is null.</exception>
        val map:
          mapping: ('T -> 'U) -> source: ResizeArray<'T> -> ResizeArray<'U>
        
        /// <summary>Applies a ResizeArray of functions to a ResizeArray of values and concatenates them.</summary>
        /// <param name="f">The functions.</param>
        /// <param name="x">The values.</param>
        /// <returns>A concatenated list of the resulting ResizeArray after applying each function to each value.</returns>
        /// 
        /// <example>
        /// <code>
        /// > List.apply [double; triple] [1; 2; 3];;  
        /// val it : int list = [2; 4; 6; 3; 6; 9]
        /// </code>
        /// </example>
        val apply:
          f: ResizeArray<('T -> 'U)> -> x: ResizeArray<'T> -> ResizeArray<'U>
        
        /// Combines all values from the first ResizeArray with the second, using the supplied mapping function.
        val lift2:
          mapping: ('T -> 'U -> 'a) -> x1: ResizeArray<'T>
          -> x2: ResizeArray<'U> -> ResizeArray<'a>
        
        /// <summary>Combines values from three ResizeArrays and calls a mapping function on this combination.</summary>
        /// <param name="mapping">Mapping function taking three element combination as input.</param>
        /// <param name="x1">First ResizeArray.</param>
        /// <param name="x2">Second ResizeArray.</param>
        /// <param name="x3">Third ResizeArray.</param>
        ///
        /// <returns>ResizeArray with values returned from mapping function.</returns>
        val lift3:
          mapping: ('U -> 'V -> 'T -> 'a) -> x1: ResizeArray<'T>
          -> x2: ResizeArray<'U> -> x3: ResizeArray<'V> -> ResizeArray<'a>
        
        /// Concatenates all elements, using the specified separator between each element.
        val intercalate: separator: 'a[] -> source: seq<'a[]> -> 'a[]
        
        /// Inserts a separator element between each element in the source ResizeArray.
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
        /// and classifying the transformed values depending on whether they were wrapped with Choice1Of2 or Choice2Of2.
        /// </summary>
        /// <returns>
        /// A tuple with both resulting arrays.
        /// </returns>
        val partitionMap:
          mapper: ('T -> Choice<'T1,'T2>) -> source: 'T array -> 'T1[] * 'T2[]
        
        /// <summary>Safely build a new ResizeArray whose elements are the results of applying the given function
        /// to each of the elements of the two ResizeArrays pairwise.</summary>
        /// <remark>If one array is shorter, excess elements are discarded from the right end of the longer array.</remark>
        val map2Shortest:
          f: ('a -> 'b -> 'c) -> a1: ResizeArray<'a> -> a2: ResizeArray<'b>
            -> ResizeArray<'c>
        
        /// <summary>
        /// Zip safely two ResizeArrays. If one ResizeArray is shorter, excess elements are discarded from the right end of the longer ResizeArray. 
        /// </summary>
        /// <param name="a1">First input ResizeArray.</param>
        /// <param name="a2">Second input ResizeArray.</param>
        /// <returns>ResizeArray with corresponding pairs of input ResizeArrays.</returns>
        val zipShortest:
          a1: ResizeArray<'T1> -> a2: ResizeArray<'T2> -> ResizeArray<'T1 * 'T2>

