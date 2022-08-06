namespace FSharpPlus
    
    /// Additional operations on List
    module List =
        
        /// Creates a list with a single element.
        val singleton: x: 'a -> 'a list
        
        /// <summary>Adds an element to the beginning of the given list</summary>
        /// <param name="x">The element to add</param>
        /// <param name="list">The list to add to</param>
        /// <returns>A concatenated list of the result lists of applying each function to each value</returns>
        val cons: x: 'a -> list: 'a list -> 'a list
        
        /// <summary>Applies a list of functions to a list of values and concatenates them</summary>
        /// <param name="f">The list of functions.</param>
        /// <param name="x">The list of values.</param>
        /// <returns>A concatenated list of the result lists of applying each function to each value</returns>
        /// 
        /// <example>
        /// <code>
        /// > List.apply [double; triple] [1; 2; 3];;  
        /// val it : int list = [2; 4; 6; 3; 6; 9]
        /// </code>
        /// </example>
        val apply: f: ('a -> 'b) list -> x: 'a list -> 'b list
        
        /// Combines all values from the first list with the second, using the supplied mapping function.
        val lift2: f: ('a -> 'b -> 'c) -> x1: 'a list -> x2: 'b list -> 'c list
        
        /// <summary>Combines values from three list and calls a mapping function on this combination.</summary>
        /// <param name="f">Mapping function taking three element combination as input.</param>
        /// <param name="x1">First list.</param>
        /// <param name="x2">Second list.</param>
        /// <param name="x3">Third list.</param>
        ///
        /// <returns>List with values returned from mapping function.</returns>
        val lift3:
          f: ('a -> 'b -> 'c -> 'd) -> x1: 'c list -> x2: 'a list -> x3: 'b list
            -> 'd list
        
        /// Returns a list with all possible tails of the source list.
        val tails: x: 'a list -> 'a list list
        
        val take: i: int -> list: seq<'a> -> 'a list
        
        val skip: i: int -> list: 'a list -> 'a list
        
        /// <summary>Returns a list that drops N elements of the original list and then yields the
        /// remaining elements of the list.</summary>
        /// <remarks>When count exceeds the number of elements in the list it
        /// returns an empty list instead of throwing an exception.</remarks>
        /// <param name="count">The number of items to drop.</param>
        /// <param name="source">The input list.</param>
        ///
        /// <returns>The result list.</returns>
        val drop: count: int -> source: 'a list -> 'a list
        
        /// Concatenates all elements, using the specified separator between each element.
        val intercalate: separator: 'a list -> source: seq<'a list> -> 'a list
        
        /// Inserts a separator element between each element in the source list.
        val intersperse: element: 'T -> source: 'T list -> 'T list
        
        /// Creates a sequence of lists by splitting the source list on any of the given separators.
        val split:
          separators: seq<'a list> -> source: 'a list -> seq<'a list>
            when 'a: equality
        
        /// Replaces a subsequence of the source list with the given replacement list.
        val replace:
          oldValue: seq<'T> -> newValue: 'T list -> source: 'T list -> 'T list
            when 'T: equality
        
        /// <summary>Converts a list to an IReadOnlyList (from System.Collections.Generic).</summary>
        /// <param name="source">The list source</param>
        /// <returns>The list converted to a System.Collections.Generic.IReadOnlyList</returns>
        val toIReadOnlyList:
          source: 'a list -> System.Collections.Generic.IReadOnlyList<'a>
        
        /// <summary>
        /// Gets the index of the first occurrence of the specified slice in the source.
        /// </summary>
        /// <exception cref="System.ArgumentException">
        /// Thrown when the slice was not found in the sequence.
        /// </exception>
        /// <returns>
        /// The index of the slice.
        /// </returns>
        val findSliceIndex: slice: 'a list -> source: 'a list -> int
        
        /// <summary>
        /// Gets the index of the first occurrence of the specified slice in the source.
        /// Returns <c>None</c> if not found.
        /// </summary>
        /// <returns>
        /// The index of the slice or <c>None</c>.
        /// </returns>
        val tryFindSliceIndex: slice: 'a list -> source: 'a list -> int option
        
        /// <summary>
        /// Creates two lists by applying the mapping function to each element in the list
        /// and classifying the transformed values depending on whether they were wrapped with Choice1Of2 or Choice2Of2.
        /// </summary>
        /// <returns>
        /// A tuple with both resulting lists.
        /// </returns>
        val partitionMap:
          mapping: ('T -> Choice<'T1,'T2>) -> source: 'T list
            -> 'T1 list * 'T2 list
        
        /// <summary>Safely build a new list whose elements are the results of applying the given function
        /// to each of the elements of the two lists pairwise.</summary>
        /// <remark>If one list is shorter, excess elements are discarded from the right end of the longer list.</remark>
        val map2Shortest:
          f: ('a -> 'b -> 'c) -> l1: 'a list -> l2: 'b list -> 'c list
        
        /// <summary>
        /// Zip safely two lists. If one list is shorter, excess elements are discarded from the right end of the longer list. 
        /// </summary>
        /// <param name="list1">First input list.</param>
        /// <param name="list2">Second input list.</param>
        /// <returns>List with corresponding pairs of input lists.</returns>
        val zipShortest: list1: 'T1 list -> list2: 'T2 list -> ('T1 * 'T2) list
        
        /// <summary>Same as choose but with access to the index.</summary>
        /// <param name="mapping">The mapping function, taking index and element as parameters.</param>
        /// <param name="source">The input list.</param>
        ///
        /// <returns>List with values x for each List value where the function returns Some(x).</returns>
        val choosei:
          mapping: (int -> 'a -> 'b option) -> source: 'a list -> 'b list
        
        /// <summary>Attempts to remove an item from a list.</summary>
        /// <param name="i">The index of the item to remove </param>
        /// <param name="lst">The input list</param>
        /// 
        /// <returns>For invalid indexes, the input list.  Otherwise, a new list with the item removed.</returns>
        val removeAt: i: int -> lst: 'a list -> 'a list
        
        /// <summary>Updates the value of an item in a list</summary>
        /// <param name="i">The index of the item to update</param>
        /// <param name="x">The new value of the item</param>
        /// <param name="lst">The input list</param>
        ///
        /// <returns>A new list with the updated element</returns>
        val setAt: i: int -> x: 'a -> lst: 'a list -> 'a list

