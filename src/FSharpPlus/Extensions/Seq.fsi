namespace FSharpPlus
    
    /// Additional operations on Seq
    module Seq =
        
        /// <summary>Applies the given function to each element of the sequence and concatenates the results.</summary>
        ///
        /// <remarks>Remember sequence is lazy, effects are delayed until it is enumerated.</remarks>
        /// <remarks>This is the same as Seq.collect but the type of the mapping function is not flexible.</remarks>
        ///
        /// <param name="mapping">A function to transform elements of the input sequence into the sequences
        /// that will then be concatenated.</param>
        /// <param name="source">The input sequence.</param>
        ///
        /// <returns>The result sequence.</returns>
        ///
        /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
        val bind: mapping: ('T -> seq<'U>) -> source: seq<'T> -> seq<'U>
        
        /// <summary>Applies a sequence of functions to a sequence of values and concatenates them.</summary>
        /// <param name="f">The seq of functions.</param>
        /// <param name="x">The seq of values.</param>
        /// <returns>A seq concatenating the results from applying each function to each value.</returns>
        /// 
        /// <example>
        /// <code>
        /// > Seq.apply [double; triple] [1; 2; 3];;  
        /// val it : seq&lt;int&gt; = seq [2; 4; 6; 3; ...]
        /// </code>
        /// </example>
        val apply: f: seq<('a -> 'b)> -> x: seq<'a> -> seq<'b>
        
        /// Combines all values from the first seq with the second, using the supplied mapping function.
        val lift2: f: ('a -> 'b -> 'c) -> x1: seq<'a> -> x2: seq<'b> -> seq<'c>
        
        /// <summary>Combines values from three seq and calls a mapping function on this combination.</summary>
        /// <param name="f">Mapping function taking three element combination as input.</param>
        /// <param name="x1">First seq.</param>
        /// <param name="x2">Second seq.</param>
        /// <param name="x3">Third seq.</param>
        ///
        /// <returns>Seq with values returned from mapping function.</returns>
        val lift3:
          f: ('a -> 'b -> 'c -> 'd) -> x1: seq<'c> -> x2: seq<'a> -> x3: seq<'b>
            -> seq<'d>
        
        /// <summary>
        /// Applies a function to each element of the collection, starting from the end,
        /// threading an accumulator argument through the computation.
        /// </summary>
        /// <remarks>
        /// Note: this function has since been added to FSharpCore, so effectively
        /// overrides it. It will be removed in next major release of FSharpPlus.
        /// </remarks>
        val foldBack: f: ('a -> 'b -> 'b) -> x: seq<'a> -> z: 'b -> 'b
        
        /// <summary>
        /// Chunks the seq up into groups with the same projected key by applying
        /// the key-generating projection function to each element and yielding a sequence of 
        /// keys tupled with values.
        /// </summary>
        ///
        /// <remarks>
        /// Each key is tupled with an array of all adjacent elements that match 
        /// to the key, therefore keys are not unique but can't be adjacent
        /// as each time the key changes a new group is yield.
        /// 
        /// The ordering of the original sequence is respected.
        /// </remarks>
        ///
        /// <param name="projection">A function that transforms an element of the sequence into a comparable key.</param>
        /// <param name="source">The input seq.</param>
        ///
        /// <returns>The resulting sequence of keys tupled with an array of matching values</returns>
        val chunkBy:
          projection: ('T -> 'Key) -> source: seq<'T>
            -> seq<'Key * ResizeArray<'T>> when 'Key: equality
        
        /// Inserts a separator element between each element in the source seq.
        ///http://codebetter.com/matthewpodwysocki/2009/05/06/functionally-implementing-intersperse/
        val intersperse: sep: 'a -> list: seq<'a> -> seq<'a>
        
        /// Inserts a separator between each element in the source sequence.
        val intercalate: separator: seq<'a> -> source: seq<#seq<'a>> -> seq<'a>
        
        /// Creates a sequence of sequences by splitting the source sequence on any of the given separators.
        val split:
          separators: seq<#seq<'b>> -> source: seq<'b> -> seq<seq<'b>>
            when 'b: equality
        
        /// Replaces a subsequence of the source seq with the given replacement seq.
        val replace:
          oldValue: seq<'T> -> newValue: seq<'T> -> source: seq<'T> -> seq<'T>
            when 'T: equality
        
        /// <summary>Returns a sequence that drops N elements of the original sequence and then yields the
        /// remaining elements of the sequence.</summary>
        /// <remarks>When count exceeds the number of elements in the sequence it
        /// returns an empty sequence instead of throwing an exception.</remarks>
        /// <param name="count">The number of items to drop.</param>
        /// <param name="source">The input sequence.</param>
        ///
        /// <returns>The result sequence.</returns>
        val drop: count: int -> source: seq<'a> -> seq<'a>
        
        /// <summary>
        /// Creates a sequence by replicating the given initial value count times.
        /// </summary>
        /// <remarks>
        /// Note: this function has since been added to FSharpCore, so effectively
        /// overrides it. It will be removed in next major release of FSharpPlus.
        /// </remarks>
        val replicate:
          count: int -> initial: 'a
            -> System.Collections.Generic.IEnumerable<'a>
        
        /// <summary>Converts a seq to an IReadOnlyList (from System.Collections.Generic).</summary>
        /// <param name="source">The seq source</param>
        /// <returns>The seq converted to a System.Collections.Generic.IReadOnlyList</returns>
        val toIReadOnlyList:
          source: seq<'a> -> System.Collections.Generic.IReadOnlyList<'a>
        
        /// <summary>
        /// Gets the index of the first occurrence of the specified slice in the source.
        /// </summary>
        /// <remarks>
        /// It is assumed that 1) the slice is finite and 2) either the source is finite or actually contains the slice, otherwise it will not return forever.
        /// The slice will always be iterated to the end.
        /// The source will be iterated until the slice is found or it reaches the end.
        /// </remarks>
        /// <exception cref="System.ArgumentException">
        /// Thrown when the slice was not found in the sequence.
        /// </exception>
        /// <returns>
        /// The index of the slice.
        /// </returns>
        val findSliceIndex: slice: seq<'a> -> source: seq<'a> -> int
        
        /// <summary>
        /// Gets the index of the first occurrence of the specified slice in the source.
        /// Returns <c>None</c> if not found.
        /// </summary>
        /// <remarks>
        /// It is assumed that 1) the slice is finite and 2) either the source is finite or actually contains the slice, otherwise it will not return forever.
        /// The slice will always be iterated to the end.
        /// The source will be iterated until the slice is found or it reaches the end.
        /// </remarks>
        /// <returns>
        /// The index of the slice or <c>None</c>.
        /// </returns>
        val tryFindSliceIndex: slice: seq<'a> -> source: seq<'a> -> int option
        
        /// <summary>Choose with access to the index</summary>
        /// <param name="mapping">The mapping function, taking index and element as parameters.</param>
        /// <param name="source">The input seq.</param>
        ///
        /// <returns>Seq with values x for each List value where the function returns Some(x).</returns>
        val choosei:
          mapping: (int -> 'a -> 'b option) -> source: seq<'a> -> seq<'b>

