namespace FSharpPlus.Data
    
    /// A type-safe sequence that contains at least one element.
    type NonEmptySeq<'T> =
        inherit System.Collections.Generic.IEnumerable<'T>
        
        abstract First: 'T
    
    /// A type alias for NonEmptySeq<'t>
    type neseq<'t> = NonEmptySeq<'t>
    
    module NonEmptySeq =
        
        /// <summary>Builds a non empty sequence from the given sequence.</summary>
        /// <param name="seq">The input sequence.</param>
        /// <returns>Non empty sequence containing the elements of the original sequence.</returns>
        /// <remarks>
        ///   **This function does not check whether the sequence is actually non empty or not.**
        /// 
        ///   Use this function only if you are sure that the sequence is not empty and
        ///   you don't want to evaluate the first element of the sequence which would cause a
        ///   side effect.
        ///  
        ///   Otherwise, always use `ofSeq`. 
        /// </remarks>
        /// <seealso cref="ofSeq" />
        val unsafeOfSeq: seq: seq<'a> -> NonEmptySeq<'a>
        
        val internal unsafeOfArray: x: 'a[] -> NonEmptySeq<'a>
        
        /// <summary>Builds a non empty sequence.</summary>
        val create: x: 'a -> xs: seq<'a> -> NonEmptySeq<'a>
        
        /// Creates a NonEmptySeq range, containing at least the first element of the range
        val (|..) : starting: int -> ending: int -> NonEmptySeq<int>
        
        /// Creates a NonEmptySeq range, containing at least the last element of the range
        val (..|) : starting: int -> ending: int -> NonEmptySeq<int>
        
        /// <summary>Returns a new sequence that contains all pairings of elements from the first and second sequences.</summary>
        /// <param name="source1">The first sequence.</param>
        /// <param name="source2">The second sequence.</param>
        /// <returns>The result sequence.</returns>
        val allPairs:
          source1: NonEmptySeq<'a> -> source2: NonEmptySeq<'b>
            -> NonEmptySeq<'a * 'b>
        
        /// <summary>Wraps the two given enumerations as a single concatenated
        /// enumeration.</summary>
        ///
        /// <remarks>The returned sequence may be passed between threads safely. However, 
        /// individual IEnumerator values generated from the returned sequence should not be accessed
        /// concurrently.</remarks>
        ///
        /// <param name="source1">The first sequence.</param>
        /// <param name="source2">The second sequence.</param>
        ///
        /// <returns>The result sequence.</returns>
        val append:
          source1: NonEmptySeq<'a> -> source2: NonEmptySeq<'a>
            -> NonEmptySeq<'a>
        
        val appendSeq:
          source: NonEmptySeq<'a> -> seq: seq<'a> -> NonEmptySeq<'a>
        
        val appendSeqBack:
          seq: seq<'a> -> source: NonEmptySeq<'a> -> NonEmptySeq<'a>
        
        /// <summary>Returns a sequence that corresponds to a cached version of the input sequence.
        /// This result sequence will have the same elements as the input sequence. The result 
        /// can be enumerated multiple times. The input sequence will be enumerated at most 
        /// once and only as far as is necessary.    Caching a sequence is typically useful when repeatedly
        /// evaluating items in the original sequence is computationally expensive or if
        /// iterating the sequence causes side-effects that the user does not want to be
        /// repeated multiple times.
        ///
        /// Enumeration of the result sequence is thread safe in the sense that multiple independent IEnumerator
        /// values may be used simultaneously from different threads (accesses to 
        /// the internal lookaside table are thread safe). Each individual IEnumerator
        /// is not typically thread safe and should not be accessed concurrently.</summary>
        ///
        /// <remarks>Once enumeration of the input sequence has started,
        /// it's enumerator will be kept live by this object until the enumeration has completed.
        /// At that point, the enumerator will be disposed. 
        ///
        /// The enumerator may be disposed and underlying cache storage released by 
        /// converting the returned sequence object to type IDisposable, and calling the Dispose method
        /// on this object. The sequence object may then be re-enumerated and a fresh enumerator will
        /// be used.</remarks>
        ///
        /// <param name="source">The input sequence.</param>
        ///
        /// <returns>The result sequence.</returns>
        val cache: source: NonEmptySeq<'a> -> NonEmptySeq<'a>
        
        /// <summary>Applies the given function to each element of the sequence and concatenates all the
        /// results.</summary>
        ///
        /// <remarks>Remember sequence is lazy, effects are delayed until it is enumerated.</remarks>
        ///
        /// <param name="mapping">A function to transform elements of the input sequence into the sequences
        /// that will then be concatenated.</param>
        /// <param name="source">The input sequence.</param>
        ///
        /// <returns>The result sequence.</returns>
        val collect:
          mapping: ('a -> #NonEmptySeq<'b>) -> source: NonEmptySeq<'a>
            -> NonEmptySeq<'b>
        
        /// <summary>Combines the given enumeration-of-enumerations as a single concatenated
        /// enumeration.</summary>
        ///
        /// <remarks>The returned sequence may be passed between threads safely. However, 
        /// individual IEnumerator values generated from the returned sequence should not be accessed concurrently.</remarks>
        ///
        /// <param name="sources">The input enumeration-of-enumerations.</param>
        ///
        /// <returns>The result sequence.</returns>
        val concat: sources: NonEmptySeq<#NonEmptySeq<'a>> -> NonEmptySeq<'a>
        
        /// <summary>Returns a sequence that is built from the given delayed specification of a
        /// sequence.</summary>
        ///
        /// <remarks>The input function is evaluated each time an IEnumerator for the sequence 
        /// is requested.</remarks>
        ///
        /// <param name="generator">The generating function for the sequence.</param>
        val delay: generator: (unit -> NonEmptySeq<'a>) -> NonEmptySeq<'a>
        
        /// <summary>Returns the first element of the sequence.</summary>
        ///
        /// <param name="source">The input sequence.</param>
        ///
        /// <returns>The first element of the sequence.</returns>
        val head: source: NonEmptySeq<'a> -> 'a
        
        /// <summary>Builds a new collection whose elements are the corresponding elements of the input collection
        /// paired with the integer index (from 0) of each element.</summary>
        /// <param name="source">The input sequence.</param>
        /// <returns>The result sequence.</returns>
        val indexed: source: NonEmptySeq<'a> -> NonEmptySeq<int * 'a>
        
        /// <summary>Generates a new sequence which, when iterated, will return successive
        /// elements by calling the given function.    The results of calling the function
        /// will not be saved, that is the function will be reapplied as necessary to
        /// regenerate the elements.    The function is passed the index of the item being
        /// generated.</summary>
        ///
        /// <remarks>The returned sequence may be passed between threads safely. However, 
        /// individual IEnumerator values generated from the returned sequence should not be accessed concurrently.
        /// Iteration can continue up to <c>Int32.MaxValue</c>.</remarks>
        ///
        /// <param name="initializer">A function that generates an item in the sequence from a given index.</param>
        ///
        /// <returns>The result sequence.</returns>
        val initInfinite: initializer: (int -> 'a) -> NonEmptySeq<'a>
        
        /// <summary>Builds a new collection whose elements are the results of applying the given function
        /// to each of the elements of the collection.    The given function will be applied
        /// as elements are demanded using the <c>MoveNext</c> method on enumerators retrieved from the
        /// object.</summary>
        ///
        /// <remarks>The returned sequence may be passed between threads safely. However, 
        /// individual IEnumerator values generated from the returned sequence should not be accessed concurrently.</remarks>
        ///
        /// <param name="mapping">A function to transform items from the input sequence.</param>
        /// <param name="source">The input sequence.</param>
        ///
        /// <returns>The result sequence.</returns>
        val map:
          mapping: ('a -> 'b) -> source: NonEmptySeq<'a> -> NonEmptySeq<'b>
        
        /// <summary>Builds a new collection whose elements are the results of applying the given function
        /// to the corresponding pairs of elements from the two sequences. If one input sequence is shorter than 
        /// the other then the remaining elements of the longer sequence are ignored.</summary>
        ///
        /// <param name="mapping">A function to transform pairs of items from the input sequences.</param>
        /// <param name="source1">The first input sequence.</param>
        /// <param name="source2">The second input sequence.</param>
        ///
        /// <returns>The result sequence.</returns>
        val map2:
          mapping: ('a -> 'b -> 'c) -> source1: NonEmptySeq<'a>
          -> source2: NonEmptySeq<'b> -> NonEmptySeq<'c>
        
        /// <summary>Combines map and fold. Builds a new collection whose elements are the results of applying the given function
        /// to each of the elements of the collection. The function is also used to accumulate a final value.</summary>
        /// <remarks>This function digests the whole initial sequence as soon as it is called. As a result this function should
        /// not be used with large or infinite sequences.</remarks>
        /// <param name="mapping">The function to transform elements from the input collection and accumulate the final value.</param>
        /// <param name="state">The initial state.</param>
        /// <param name="source">The input collection.</param>
        /// <returns>The collection of transformed elements, and the final accumulated value.</returns>
        /// <remarks>This function consumes the whole input sequence before yielding the first element of the result sequence.</remarks>
        val mapFold:
          mapping: ('State -> 'T -> 'Result * 'State) -> state: 'State
          -> source: NonEmptySeq<'T> -> NonEmptySeq<'Result> * 'State
        
        /// <summary>Combines map and foldBack. Builds a new collection whose elements are the results of applying the given function
        /// to each of the elements of the collection. The function is also used to accumulate a final value.</summary>
        /// <remarks>This function digests the whole initial sequence as soon as it is called. As a result this function should
        /// not be used with large or infinite sequences.</remarks>
        /// <param name="mapping">The function to transform elements from the input collection and accumulate the final value.</param>
        /// <param name="source">The input collection.</param>
        /// <param name="state">The initial state.</param>
        /// <returns>The collection of transformed elements, and the final accumulated value.</returns>
        /// <remarks>This function consumes the whole input sequence before yielding the first element of the result sequence.</remarks>
        val mapFoldBack:
          mapping: ('T -> 'State -> 'Result * 'State) -> source: NonEmptySeq<'T>
          -> state: 'State -> NonEmptySeq<'Result> * 'State
        
        /// <summary>Builds a new collection whose elements are the results of applying the given function
        /// to the corresponding triples of elements from the three sequences. If one input sequence if shorter than
        /// the others then the remaining elements of the longer sequences are ignored.</summary>
        ///
        /// <param name="mapping">The function to transform triples of elements from the input sequences.</param>
        /// <param name="source1">The first input sequence.</param>
        /// <param name="source2">The second input sequence.</param>
        /// <param name="source3">The third input sequence.</param>
        ///
        /// <returns>The result sequence.</returns>
        val map3:
          mapping: ('a -> 'b -> 'c -> 'd) -> source1: NonEmptySeq<'a>
          -> source2: NonEmptySeq<'b> -> source3: NonEmptySeq<'c>
            -> NonEmptySeq<'d>
        
        /// <summary>Builds a new collection whose elements are the results of applying the given function
        /// to each of the elements of the collection. The integer index passed to the
        /// function indicates the index (from 0) of element being transformed.</summary>
        ///
        /// <param name="mapping">A function to transform items from the input sequence that also supplies the current index.</param>
        /// <param name="source">The input sequence.</param>
        ///
        /// <returns>The result sequence.</returns>
        val mapi:
          mapping: (int -> 'a -> 'b) -> source: NonEmptySeq<'a>
            -> NonEmptySeq<'b>
        
        /// <summary>Builds a new collection whose elements are the results of applying the given function
        /// to the corresponding pairs of elements from the two sequences. If one input sequence is shorter than 
        /// the other then the remaining elements of the longer sequence are ignored. The integer index passed to the
        /// function indicates the index (from 0) of element being transformed.</summary>
        ///
        /// <param name="mapping">A function to transform pairs of items from the input sequences that also supplies the current index.</param>
        /// <param name="source1">The first input sequence.</param>
        /// <param name="source2">The second input sequence.</param>
        ///
        /// <returns>The result sequence.</returns>
        val mapi2:
          mapping: (int -> 'a -> 'b -> 'c) -> source1: NonEmptySeq<'a>
          -> source2: NonEmptySeq<'b> -> NonEmptySeq<'c>
        
        /// <summary>Builds a non empty sequence from the given sequence.</summary>
        /// <param name="seq">The input sequence.</param>
        /// <returns>Non empty sequence containing the elements of the sequence.</returns>
        /// <exception cref="System.ArgumentException">Thrown when the input sequence is empty.</exception>
        /// <remarks>
        ///   Throws exception for empty sequence.
        /// 
        ///   Evaluates the first element of the sequence and may trigger side effects.
        ///   If you are sure that the sequence is not empty and want to avoid that, you can use `unsafeOfSeq` instead.
        /// </remarks>
        /// <seealso cref="unsafeOfSeq" />
        val ofSeq: seq: seq<'a> -> NonEmptySeq<'a>
        
        /// Transforms a sequence to a NonEmptySeq, returning an option to signal when the original sequence was empty.
        val tryOfSeq: seq: seq<'a> -> NonEmptySeq<'a> option
        
        /// <summary>Builds a non empty sequence from the given array.</summary>
        /// <param name="array">The input array.</param>
        /// <returns>Non empty sequence containing the elements of the array.</returns>
        /// <exception cref="System.ArgumentException">Thrown when the input array is empty.</exception>
        /// <remarks>Throws exception for empty array</remarks>
        val ofArray: array: 'a[] -> NonEmptySeq<'a>
        
        /// Transforms a array to a NonEmptySeq, returning an option to signal when the original array was empty.
        val tryOfArray: array: 'a[] -> NonEmptySeq<'a> option
        
        /// <summary>Builds a non empty sequence from the given list.</summary>
        /// <param name="list">The input list.</param>
        /// <returns>Non empty sequence containing the elements of the list.</returns>
        /// <exception cref="System.ArgumentException">Thrown when the input list is empty.</exception>
        /// <remarks>Throws exception for empty list</remarks>
        val ofList: list: 'a list -> NonEmptySeq<'a>
        
        /// Transforms a list to a NonEmptySeq, returning an option to signal when the original list was empty.
        val tryOfList: list: 'a list -> NonEmptySeq<'a> option
        
        /// <summary>Returns a sequence of each element in the input sequence and its predecessor, with the
        /// exception of the first element which is only returned as the predecessor of the second element.</summary>
        ///
        /// <param name="source">The input sequence.</param>
        ///
        /// <returns>The result sequence.</returns>
        val pairwise: source: NonEmptySeq<'a> -> NonEmptySeq<'a * 'a>
        
        /// <summary>Returns a sequence with all elements permuted according to the
        /// specified permutation.</summary>
        ///
        /// <remarks>Note that this function returns a sequence that digests the whole initial sequence as soon as
        /// that sequence is iterated. As a result this function should not be used with
        /// large or infinite sequences.</remarks>
        ///
        /// <param name="indexMap">The function that maps input indices to output indices.</param>
        /// <param name="source">The input sequence.</param>
        ///
        /// <returns>The result sequence.</returns>
        ///
        /// <exception cref="System.ArgumentException">Thrown when indexMap does not produce a valid permutation.</exception>
        /// <remarks>This function consumes the whole input sequence before yielding the first element of the result sequence.</remarks>
        val permute:
          indexMap: (int -> int) -> source: NonEmptySeq<'a> -> NonEmptySeq<'a>
        
        /// <summary>Builds a new sequence object that delegates to the given sequence object. This ensures 
        /// the original sequence cannot be rediscovered and mutated by a type cast. For example, 
        /// if given an array the returned sequence will return the elements of the array, but
        /// you cannot cast the returned sequence object to an array.</summary>
        ///
        /// <param name="source">The input sequence.</param>
        ///
        /// <returns>The result sequence.</returns>
        val readonly: source: NonEmptySeq<'a> -> NonEmptySeq<'a>
        
        /// <summary>Returns a new sequence with the elements in reverse order.</summary>
        /// <param name="source">The input sequence.</param>
        /// <returns>The reversed sequence.</returns>
        /// <remarks>This function consumes the whole input sequence before yielding the first element of the reversed sequence.</remarks>
        val rev: source: NonEmptySeq<'a> -> NonEmptySeq<'a>
        
        /// <summary>Like fold, but computes on-demand and returns the sequence of intermediary and final results.</summary>
        ///
        /// <param name="folder">A function that updates the state with each element from the sequence.</param>
        /// <param name="state">The initial state.</param>
        /// <param name="source">The input sequence.</param>
        ///
        /// <returns>The resulting sequence of computed states.</returns>
        val scan:
          folder: ('State -> 'T -> 'State) -> state: 'State
          -> source: NonEmptySeq<'T> -> NonEmptySeq<'State>
        
        /// <summary>Like <c>foldBack</c>, but returns the sequence of intermediary and final results.</summary>
        /// <remarks>This function returns a sequence that digests the whole initial sequence as soon as that
        /// sequence is iterated. As a result this function should not be used with large or infinite sequences.
        /// </remarks>
        /// <param name="folder">A function that updates the state with each element from the sequence.</param>
        /// <param name="source">The input sequence.</param>
        /// <param name="state">The initial state.</param>
        /// <returns>The resulting sequence of computed states.</returns>
        /// <remarks>This function consumes the whole input sequence before yielding the first element of the result sequence.</remarks>
        val scanBack:
          folder: ('T -> 'State -> 'State) -> source: NonEmptySeq<'T>
          -> state: 'State -> NonEmptySeq<'State>
        
        /// <summary>Returns a sequence that yields one item only.</summary>
        ///
        /// <param name="value">The input item.</param>
        ///
        /// <returns>The result sequence of one item.</returns>
        val singleton: value: 'a -> NonEmptySeq<'a>
        
        /// <summary>Yields a sequence ordered by keys.</summary>
        /// 
        /// <remarks>This function returns a sequence that digests the whole initial sequence as soon as 
        /// that sequence is iterated. As a result this function should not be used with 
        /// large or infinite sequences. The function makes no assumption on the ordering of the original 
        /// sequence.
        ///
        /// This is a stable sort, that is the original order of equal elements is preserved.</remarks>
        ///
        /// <param name="source">The input sequence.</param>
        ///
        /// <returns>The result sequence.</returns>
        ///
        /// <remarks>This function consumes the whole input sequence before yielding the first element of the result sequence.</remarks>
        val sort: source: NonEmptySeq<'a> -> NonEmptySeq<'a> when 'a: comparison
        
        /// <summary>Yields a sequence ordered using the given comparison function.</summary>
        /// <remarks>This function returns a sequence that digests the whole initial sequence as soon as
        /// that sequence is iterated. As a result this function should not be used with
        /// large or infinite sequences. The function makes no assumption on the ordering of the original
        /// sequence.
        ///
        /// This is a stable sort, that is the original order of equal elements is preserved.</remarks>
        /// <param name="comparer">The function to compare the collection elements.</param>
        /// <param name="source">The input sequence.</param>
        /// <returns>The result sequence.</returns>
        /// <remarks>This function consumes the whole input sequence before yielding the first element of the result sequence.</remarks>
        val sortWith:
          comparer: ('a -> 'a -> int) -> source: NonEmptySeq<'a>
            -> NonEmptySeq<'a>
        
        /// <summary>Applies a key-generating function to each element of a sequence and yield a sequence ordered
        /// by keys.    The keys are compared using generic comparison as implemented by <c>Operators.compare</c>.</summary> 
        /// 
        /// <remarks>This function returns a sequence that digests the whole initial sequence as soon as 
        /// that sequence is iterated. As a result this function should not be used with 
        /// large or infinite sequences. The function makes no assumption on the ordering of the original 
        /// sequence.
        ///
        /// This is a stable sort, that is the original order of equal elements is preserved.</remarks>
        ///
        /// <param name="projection">A function to transform items of the input sequence into comparable keys.</param>
        /// <param name="source">The input sequence.</param>
        ///
        /// <returns>The result sequence.</returns>
        ///
        /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
        val sortBy:
          projection: ('a -> 'b) -> source: NonEmptySeq<'a> -> NonEmptySeq<'a>
            when 'b: comparison
        
        /// <summary>Yields a sequence ordered descending by keys.</summary>
        /// 
        /// <remarks>This function returns a sequence that digests the whole initial sequence as soon as 
        /// that sequence is iterated. As a result this function should not be used with 
        /// large or infinite sequences. The function makes no assumption on the ordering of the original 
        /// sequence.
        ///
        /// This is a stable sort, that is the original order of equal elements is preserved.</remarks>
        ///
        /// <param name="source">The input sequence.</param>
        ///
        /// <returns>The result sequence.</returns>
        val sortDescending:
          source: NonEmptySeq<'a> -> NonEmptySeq<'a> when 'a: comparison
        
        /// <summary>Applies a key-generating function to each element of a sequence and yield a sequence ordered
        /// descending by keys.    The keys are compared using generic comparison as implemented by <c>Operators.compare</c>.</summary> 
        /// 
        /// <remarks>This function returns a sequence that digests the whole initial sequence as soon as 
        /// that sequence is iterated. As a result this function should not be used with 
        /// large or infinite sequences. The function makes no assumption on the ordering of the original 
        /// sequence.
        ///
        /// This is a stable sort, that is the original order of equal elements is preserved.</remarks>
        ///
        /// <param name="projection">A function to transform items of the input sequence into comparable keys.</param>
        /// <param name="source">The input sequence.</param>
        ///
        /// <returns>The result sequence.</returns>
        val sortByDescending:
          projection: ('a -> 'b) -> source: NonEmptySeq<'a> -> NonEmptySeq<'a>
            when 'b: comparison
        
        /// <summary>Returns a sequence that skips 1 element of the underlying sequence and then yields the
        /// remaining elements of the sequence.</summary>
        ///
        /// <param name="source">The input sequence.</param>
        ///
        /// <returns>The result sequence.</returns>
        val tail: source: NonEmptySeq<'a> -> seq<'a>
        
        val unfold:
          generator: ('T -> 'State -> ('T * 'State) option) -> head: 'T
          -> state: 'State -> NonEmptySeq<'T>
        
        /// <summary>Combines the two sequences into a sequence of pairs. The two sequences need not have equal lengths:
        /// when one sequence is exhausted any remaining elements in the other
        /// sequence are ignored.</summary>
        ///
        /// <param name="source1">The first input sequence.</param>
        /// <param name="source2">The second input sequence.</param>
        ///
        /// <returns>The result sequence.</returns>
        val zip:
          source1: NonEmptySeq<'a> -> source2: NonEmptySeq<'b>
            -> NonEmptySeq<'a * 'b>
        
        /// <summary>Combines the three sequences into a sequence of triples. The sequences need not have equal lengths:
        /// when one sequence is exhausted any remaining elements in the other
        /// sequences are ignored.</summary>
        ///
        /// <param name="source1">The first input sequence.</param>
        /// <param name="source2">The second input sequence.</param>
        /// <param name="source3">The third input sequence.</param>
        ///
        /// <returns>The result sequence.</returns>
        val zip3:
          source1: NonEmptySeq<'a> -> source2: NonEmptySeq<'b>
          -> source3: NonEmptySeq<'c> -> NonEmptySeq<'a * 'b * 'c>
        
        /// <summary>Applies the given function to each element of the NonEmptySequence and concatenates all the
        /// results.</summary>
        ///
        /// <remarks>Remember NonEmptySequence is lazy, effects are delayed until it is enumerated.</remarks>
        /// <remarks>This is the same as Seq.collect but the type of the mapping function is not flexible.</remarks>
        ///
        /// <param name="mapping">A function to transform elements of the input NonEmptySequence into the NonEmptySequences
        /// that will then be concatenated.</param>
        /// <param name="source">The input NonEmptySequence.</param>
        ///
        /// <returns>The result NonEmptySequence.</returns>
        ///
        /// <exception cref="System.ArgumentNullException">Thrown when the input NonEmptySequence is null.</exception>
        val bind:
          mapping: ('T -> NonEmptySeq<'U>) -> source: NonEmptySeq<'T>
            -> NonEmptySeq<'U>
        
        val apply:
          f: NonEmptySeq<('a -> 'b)> -> x: NonEmptySeq<'a> -> NonEmptySeq<'b>
        
        val lift2:
          f: ('a -> 'b -> 'c) -> x1: NonEmptySeq<'a> -> x2: NonEmptySeq<'b>
            -> NonEmptySeq<'c>
        
        /// <summary>Combines values from three NonEmptySeq and calls a mapping function on this combination.</summary>
        /// <param name="f">Mapping function taking three element combination as input.</param>
        /// <param name="x1">First NonEmptySeq.</param>
        /// <param name="x2">Second NonEmptySeq.</param>
        /// <param name="x3">Third NonEmptySeq.</param>
        ///
        /// <returns>NonEmptySeq with values returned from mapping function.</returns>
        val lift3:
          f: ('a -> 'b -> 'c -> 'd) -> x1: NonEmptySeq<'c>
          -> x2: NonEmptySeq<'a> -> x3: NonEmptySeq<'b> -> NonEmptySeq<'d>
        
        val replace:
          oldValue: NonEmptySeq<'T> -> newValue: NonEmptySeq<'T>
          -> source: NonEmptySeq<'T> -> NonEmptySeq<'T> when 'T: equality
        
        /// <summary>Applies a function to each element of the sequence, threading an accumulator argument
        /// through the computation. Apply the function to the first two elements of the sequence.
        /// Then feed this result into the function along with the third element and so on. 
        /// Return the final result. If the input function is <c>f</c> and the elements are <c>i0...iN</c> then computes 
        /// <c>f (... (f i0 i1) i2 ...) iN</c>.</summary>
        /// <param name="reduction">The function to reduce two sequence elements to a single element.</param>
        /// <param name="source">The input sequence.</param>
        /// <returns>The final reduced value.</returns>
        val reduce: reduction: ('T -> 'T -> 'T) -> source: seq<'T> -> 'T
    
    module NonEmptySeqBuilder =
        
        type NESeqBuilder =
            
            new: unit -> NESeqBuilder
            
            member
              Combine: a: NonEmptySeq<'T> * b: NonEmptySeq<'T>
                         -> NonEmptySeq<'T>
            
            member Delay: expr: (unit -> NonEmptySeq<'T>) -> NonEmptySeq<'T>
            
            member Yield: x: 'a -> NonEmptySeq<'a>
            
            [<CompilerMessage
              ("A NonEmptySeq doesn't support the Zero operation.", 708)>]
            member Zero: unit -> 'b
        
        val neseq: NESeqBuilder

