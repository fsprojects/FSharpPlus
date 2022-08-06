namespace FSharpPlus
    
    /// Additional operations on IEnumerator
    module Enumerator =
        
        /// [omit]
        val inline invalidArgFmt:
          arg: string -> format: string -> paramArray: obj[] -> 'a
        
        /// [omit]
        val noReset: unit -> 'a
        
        /// [omit]
        val notStarted: unit -> 'a
        
        /// [omit]
        val alreadyFinished: unit -> 'a
        
        /// [omit]
        val check: started: bool -> unit
        
        /// [omit]
        val dispose: r: System.IDisposable -> unit
        
        /// An enumerator that is empty -- useful in combination with other enumerators
        /// [omit]
        [<Sealed>]
        type EmptyEnumerator<'T> =
            interface System.IDisposable
            interface System.Collections.IEnumerator
            interface System.Collections.Generic.IEnumerator<'T>
            
            new: unit -> EmptyEnumerator<'T>
        
        /// Constructs an EmptyEnumerator of type 'T.
        val Empty: unit -> System.Collections.Generic.IEnumerator<'T>
        
        /// Constructs an Enumerator that yields the single value given.
        val singleton: x: 'a -> System.Collections.Generic.IEnumerator<'a>
        
        /// [omit]
        type IFinallyEnumerator =
            
            abstract AppendFinallyAction: (unit -> unit) -> unit
        
        /// Enumerate all sources in sequence
        /// [omit]
        [<Sealed>]
        type ConcatEnumerator<'T> =
            interface System.IDisposable
            interface System.Collections.IEnumerator
            interface System.Collections.Generic.IEnumerator<'T>
            interface IFinallyEnumerator
            
            new: sources: System.Collections.Generic.IEnumerator<System.Collections.Generic.IEnumerator<'T>>
                   -> ConcatEnumerator<'T>
            
            [<DefaultValue (false)>]
            val mutable private currElement: 'T
            
            member Finish: unit -> unit
            
            member GetCurrent: unit -> 'T
        
        /// <summary>
        /// Enumerates the elements of each of the Enumerators in order.
        /// </summary>
        /// <param name="sources">The source Enumerator of Enumerators.</param>
        /// <returns>A concatenated enumeration of the given Enumerator sources.</returns>
        val concat:
          sources: System.Collections.Generic.IEnumerator<System.Collections.Generic.IEnumerator<'a>>
            -> System.Collections.Generic.IEnumerator<'a>
        
        /// <summary>
        /// Tries to find the nth element in the Enumerator.
        /// Returns None if index is negative or the Enumerator does not contain enough elements. 
        /// </summary>
        /// <param name="index">The index to retrieve.</param>
        /// <param name="e">The input Enumerator.</param>
        /// <returns>The value at the given index or <c>None</c> if not found.</returns>
        val tryItem:
          index: int -> e: System.Collections.Generic.IEnumerator<'T>
            -> 'T option
        
        /// <summary>
        /// Retuns the nth element in the Enumerator.
        /// </summary>
        /// <remarks>
        /// This is called <c>item</c> in some other parts of core.
        /// </remarks>
        /// <param name="index">The index to retrieve.</param>
        /// <param name="e">The input Enumerator.</param>
        /// <returns>The value at the given index or an exception is thrown if not found.</returns>
        /// <exception cref="System.ArgumentException">
        /// Thrown if the index is negative or the Enumerator does not contain enough elements.
        /// </exception>
        val nth:
          index: int -> e: System.Collections.Generic.IEnumerator<'T> -> 'T
        
        /// Defines the possible states of a MapEnumerator.
        /// [omit]
        [<NoEquality; NoComparison>]
        type MapEnumeratorState =
            | NotStarted
            | InProcess
            | Finished
        
        /// An abstract enumerator, useful when mapping over enumerators.
        /// 
        /// It maintains a mutable `curr` item, and a process MapEnumeratorState `state`.
        /// 
        /// Implement DoMoveNext such that `curr` is set after calling, and return
        /// whether the enumerator actually moved next.
        /// [omit]
        [<AbstractClass>]
        type MapEnumerator<'T> =
            interface System.IDisposable
            interface System.Collections.IEnumerator
            interface System.Collections.Generic.IEnumerator<'T>
            
            new: unit -> MapEnumerator<'T>
            
            [<DefaultValue (false)>]
            val mutable private curr: 'T
            
            abstract Dispose: unit -> unit
            
            abstract DoMoveNext: byref<'T> -> bool
            
            member GetCurrent: unit -> 'T
        
        /// <summary>
        /// Maps over an enumerator.
        /// </summary>
        /// <param name="f">The function to apply.</param>
        /// <param name="e">The input Enumerator.</param>
        /// <returns>A new Enumerator of mapped elements.</returns>
        val map:
          f: ('a -> 'b) -> e: System.Collections.Generic.IEnumerator<'a>
            -> System.Collections.Generic.IEnumerator<'b>
        
        /// <summary>
        /// Maps over an Enumerator, with the mapping function also given the index.
        /// </summary>
        /// <param name="f">The function to apply, which is given both the index and the element.</param>
        /// <param name="e">The input Enumerator.</param>
        /// <returns>A new Enumerator of mapped elements.</returns>
        val mapi:
          f: (int -> 'a -> 'b) -> e: System.Collections.Generic.IEnumerator<'a>
            -> System.Collections.Generic.IEnumerator<'b>
        
        /// <summary>
        /// Maps over two Enumerators, with the mapping function is given the corresponding elements
        /// of the two Enumerators pairwise.
        /// </summary>
        /// <remarks>
        /// Stops enumerating when either of the input Enumerators are finished enumerating.
        /// </remarks>
        /// <param name="f">The function to apply to each pair of elements from the input Enumerators.</param>
        /// <param name="e1">The first input Enumerator.</param>
        /// <param name="e2">The second input Enumerator.</param>
        /// <returns>A new Enumerator of mapped elements.</returns>
        val map2:
          f: ('a -> 'b -> 'c) -> e1: System.Collections.Generic.IEnumerator<'a>
          -> e2: System.Collections.Generic.IEnumerator<'b>
            -> System.Collections.Generic.IEnumerator<'c>
        
        /// <summary>
        /// Maps over two Enumerators, where the mapping function is given the index and corresponding elements
        /// of the two input Enumerators pairwise.
        /// </summary>
        /// <remarks>
        /// Stops enumerating when either of the input Enumerators are finished enumerating.
        /// </remarks>
        /// <param name="f">The function to apply to the index and each pair of elements from the input Enumerators.</param>
        /// <param name="e1">The first input Enumerator.</param>
        /// <param name="e2">The second input Enumerator.</param>
        /// <returns>A new Enumerator of mapped elements.</returns>
        val mapi2:
          f: (int -> 'a -> 'b -> 'c)
          -> e1: System.Collections.Generic.IEnumerator<'a>
          -> e2: System.Collections.Generic.IEnumerator<'b>
            -> System.Collections.Generic.IEnumerator<'c>
        
        /// <summary>
        /// Maps over three Enumerators, where the mapping function is given the corresponding elements
        /// of the three Enumerators.
        /// </summary>
        /// <remarks>
        /// Stops enumerating when any of the input Enumerators are finished enumerating.
        /// </remarks>
        /// <param name="f">The function to apply to each triple of elements from the input Enumerators.</param>
        /// <param name="e1">The first input Enumerator.</param>
        /// <param name="e2">The second input Enumerator.</param>
        /// <param name="e3">The third input Enumerator.</param>
        /// <returns>A new Enumerator of mapped elements.</returns>
        val map3:
          f: ('a -> 'b -> 'c -> 'd)
          -> e1: System.Collections.Generic.IEnumerator<'a>
          -> e2: System.Collections.Generic.IEnumerator<'b>
          -> e3: System.Collections.Generic.IEnumerator<'c>
            -> System.Collections.Generic.IEnumerator<'d>
        
        /// <summary>
        /// Applies the given function to each element in the input Enumerator.
        /// Returns an Enumerator comprised of the resuls <c>x</c> for each element
        /// where the function returns <c>Some(x)</c>.
        /// </summary>
        /// <param name="chooser">The function to apply to each triple of elements from the input Enumerators.</param>
        /// <param name="e">The input Enumerator.</param>
        /// <returns>A new Enumerator of values selected from the chooser function.</returns>
        val choose:
          chooser: ('T -> 'U option)
          -> e: System.Collections.Generic.IEnumerator<'T>
            -> System.Collections.Generic.IEnumerator<'U>
        
        /// <summary>
        /// Returns a new Enumerator yielding only the elements of the input Enumerator for which the
        /// given predicate returns "true".
        /// </summary>
        /// <param name="predicate">The function to test the input elements.</param>
        /// <param name="e">The input Enumerator.</param>
        /// <returns>A new Enumerator yielding only elements that satsify the predicate.</returns>
        val filter:
          predicate: ('T -> bool)
          -> e: System.Collections.Generic.IEnumerator<'T>
            -> System.Collections.Generic.IEnumerator<'T>
        
        /// <summary>
        /// Returns a new Enumerator yielding elements <c>x</c> generated by the given computation
        /// so long as it generates a <c>Some(x)</c> - and stops when it generates a <c>None</c>.
        /// The given initial <c>state</c> argument is passed to the element generator.
        /// </summary>
        /// <param name="generator">The function that takes the current state and returns an
        /// option tuple of the next element of the list and the next state value.</param>
        /// <param name="initialState">The intitial state value.</param>
        /// <returns>A new Enumerator yielding only elements that satsify the predicate.</returns>
        val unfold:
          generator: ('a -> ('b * 'a) option) -> initialState: 'a
            -> System.Collections.Generic.IEnumerator<'b>
        
        /// <summary>
        /// Enumerates from zero up to the given <c>lastOption</c>, yielding elements
        /// generated by the given function applied to the index.
        /// </summary>
        /// <remarks>
        /// The Current value for a valid index is "f i".
        ///
        /// Lazy&lt;_&gt; values are used as caches, to store either the result or an exception if thrown.
        /// 
        /// These "Lazy&lt;_&gt;" caches are created only on the first call to current and forced immediately.
        /// The lazy creation of the cache nodes means enumerations that skip many Current values are not delayed by GC.
        /// For example, the full enumeration of Seq.initInfinite in the tests.
        /// </remarks>
        /// <param name="lastOption">The last index to stop at -- or <c>None</c> to run forever, well as far as Int32.MaxValue.</param>
        /// <param name="f">The function to apply to each index.</param>
        /// <returns>An enumerator that yields upto the lastOption.</returns>
        val upto:
          lastOption: int option -> f: (int -> 'U)
            -> System.Collections.Generic.IEnumerator<'U>
        
        /// <summary>
        /// Zip two input Enumerators into a new Enumerator yielding pairs.
        /// </summary>
        /// <param name="e1">The first input Enumerator.</param>
        /// <param name="e2">The second input Enumerator.</param>
        /// <returns>An Enumerator that enumerates pairs of two input Enumerators.</returns>
        val zip:
          e1: System.Collections.Generic.IEnumerator<'a>
          -> e2: System.Collections.Generic.IEnumerator<'b>
            -> System.Collections.Generic.IEnumerator<'a * 'b>
        
        /// <summary>
        /// Zip three input Enumerators into a new Enumerator yielding triples.
        /// </summary>
        /// <param name="e1">The first input Enumerator.</param>
        /// <param name="e2">The second input Enumerator.</param>
        /// <param name="e3">The third input Enumerator.</param>
        /// <returns>An Enumerator that enumerates triples of three input Enumerators.</returns>
        val zip3:
          e1: System.Collections.Generic.IEnumerator<'a>
          -> e2: System.Collections.Generic.IEnumerator<'b>
          -> e3: System.Collections.Generic.IEnumerator<'c>
            -> System.Collections.Generic.IEnumerator<'a * 'b * 'c>

