namespace FSharpPlus.Data
    
    /// A type-safe set that contains at least one element.
    [<StructuredFormatDisplay ("{Value}")>]
    type NonEmptySet<[<EqualityConditionalOn>] 'a
                       when [<EqualityConditionalOn>] 'a: comparison> =
        private { Value: Set<[<EqualityConditionalOn>] 'a> }
        interface NonEmptySeq<[<EqualityConditionalOn>] 'a>
        interface
            System.Collections.Generic.IReadOnlyCollection<[<EqualityConditionalOn>] 'a>
        interface
            System.Collections.Generic.IEnumerable<[<EqualityConditionalOn>] 'a>
        interface System.Collections.IEnumerable
        
        static member
          (+) : set1: NonEmptySet<'a> * set2: NonEmptySet<'a> -> NonEmptySet<'a>
        
        static member
          Create: first: 'a * [<System.ParamArray>] rest: 'a[]
                    -> NonEmptySet<'a>
        
        [<System.ComponentModel.EditorBrowsable
          (enum<System.ComponentModel.EditorBrowsableState> (1))>]
        static member
          FoldBack: set: NonEmptySet<'a> * f: ('a -> 'a0 -> 'a0) * z: 'a0 -> 'a0
        
        [<System.ComponentModel.EditorBrowsable
          (enum<System.ComponentModel.EditorBrowsableState> (1))>]
        static member
          Map: x: NonEmptySet<'a> * f: ('a -> 'b) -> NonEmptySet<'b>
                 when 'b: comparison
        
        [<System.ComponentModel.EditorBrowsable
          (enum<System.ComponentModel.EditorBrowsableState> (1))>]
        static member
          Reduce: s: NonEmptySet<'a> * reduction: ('a -> 'a -> 'a) -> 'a
        
        [<System.ComponentModel.EditorBrowsable
          (enum<System.ComponentModel.EditorBrowsableState> (1))>]
        static member Return: x: 'a -> NonEmptySet<'a>
        
        [<System.ComponentModel.EditorBrowsable
          (enum<System.ComponentModel.EditorBrowsableState> (1))>]
        static member
          ToList: s: NonEmptySet<'a> * _impl: Control.ToList -> 'a list
        
        [<System.ComponentModel.EditorBrowsable
          (enum<System.ComponentModel.EditorBrowsableState> (1))>]
        static member
          ToSeq: s: NonEmptySet<'a> * _impl: Control.ToSeq -> seq<'a>
        
        member Add: value: 'a -> NonEmptySet<'a>
        
        member Contains: value: 'a -> bool
        
        member IsProperSubsetOf: other: NonEmptySet<'a> -> bool
        
        member IsProperSupersetOf: other: NonEmptySet<'a> -> bool
        
        member IsSubsetOf: other: NonEmptySet<'a> -> bool
        
        member IsSupersetOf: other: NonEmptySet<'a> -> bool
        
        member Count: int
        
        member MaximumElement: 'a
        
        member MinimumElement: 'a
    
    /// A type alias for NonEmptySet<'t>
    type neset<'t when 't: comparison> = NonEmptySet<'t>
    
    /// Basic operations on NonEmptySet
    module NonEmptySet =
        
        /// <summary>Builds a non empty set.</summary>
        val create: x: 'a -> xs: seq<'a> -> NonEmptySet<'a> when 'a: comparison
        
        /// <summary>Builds a non empty set with a single element.</summary>
        val singleton: x: 'a -> NonEmptySet<'a> when 'a: comparison
        
        /// <summary>Builds a list from the given non empty set.</summary>
        val toList: NonEmptySet<'a> -> 'a list when 'a: comparison
        
        /// <summary>Builds a sequence from the given non empty set.</summary>
        val toSeq: NonEmptySet<'a> -> seq<'a> when 'a: comparison
        
        /// <summary>Builds an array from the given non empty set.</summary>
        val toArray: NonEmptySet<'a> -> 'a[] when 'a: comparison
        
        /// <summary>Builds a set from the given non empty set.</summary>
        val toSet: NonEmptySet<'a> -> Set<'a> when 'a: comparison
        
        /// <summary>Builds a non-empty list from the given non empty set.</summary>
        val toNonEmptyList:
          NonEmptySet<'a> -> NonEmptyList<'a> when 'a: comparison
        
        /// <summary>Builds a non-empty sequence from the given non empty set.</summary>
        val toNonEmptySeq:
          set: NonEmptySet<'a> -> NonEmptySeq<'a> when 'a: comparison
        
        /// <summary>Builds a non empty set from the given array.</summary>
        /// <param name="array">The input array.</param>
        /// <returns>Non empty set containing the elements of the array.</returns>
        /// <exception cref="System.ArgumentException">Thrown when the input array is empty.</exception>
        /// <remarks>Throws exception for empty array</remarks>
        val ofArray: array: 'a array -> NonEmptySet<'a> when 'a: comparison
        
        /// <summary>Builds a non empty set from the given list.</summary>
        /// <param name="list">The input list.</param>
        /// <returns>Non empty set containing the elements of the list.</returns>
        /// <exception cref="System.ArgumentException">Thrown when the input list is empty.</exception>
        /// <remarks>Throws exception for empty list</remarks>
        val ofList: list: 'a list -> NonEmptySet<'a> when 'a: comparison
        
        /// <summary>Builds a non empty set from the given non-empty list.</summary>
        /// <param name="list">The input list.</param>
        /// <returns>Non empty set containing the elements of the non-empty list.</returns>
        val ofNonEmptyList:
          list: NonEmptyList<'a> -> NonEmptySet<'a> when 'a: comparison
        
        /// <summary>Builds a non empty set from the given sequence.</summary>
        /// <param name="seq">The input list.</param>
        /// <returns>Non empty set containing the elements of the list.</returns>
        /// <exception cref="System.ArgumentException">Thrown when the input list is empty.</exception>
        /// <remarks>Throws exception for empty sequence</remarks>
        val ofSeq: seq: seq<'a> -> NonEmptySet<'a> when 'a: comparison
        
        /// <summary>Builds a non empty set from the given non-empty sequence.</summary>
        /// <param name="source">The input sequence.</param>
        /// <returns>Non empty set containing the elements of the non-empty sequence.</returns>
        val ofNonEmptySeq:
          source: NonEmptySeq<'a> -> NonEmptySet<'a> when 'a: comparison
        
        /// <summary>Builds a non empty set from the given set.</summary>
        /// <param name="set">The input set.</param>
        /// <returns>Non empty set containing the elements of the set.</returns>
        /// <exception cref="System.ArgumentException">Thrown when the input set is empty.</exception>
        /// <remarks>Throws exception for empty set</remarks>
        val ofSet: set: Set<'a> -> NonEmptySet<'a> when 'a: comparison
        
        /// Transforms a set to a NonEmptySet, returning an option to signal when the original set was empty.
        val tryOfSet: set: Set<'a> -> NonEmptySet<'a> option when 'a: comparison
        
        /// Returns the count of a non empty set. You can also use property nes.Count
        val count: nes: NonEmptySet<'a> -> int when 'a: comparison
        
        /// <summary>Returns a new set with an element added to the set. No exception is raised if
        /// the set already contains the given element.</summary>
        /// <param name="value">The value to add.</param>
        /// <param name="source">The input set.</param>
        /// <returns>A new set containing <c>value</c>.</returns>
        val add:
          value: 'a -> source: NonEmptySet<'a> -> NonEmptySet<'a>
            when 'a: comparison
        
        /// <summary>Evaluates to "true" if the given element is in the given set.</summary>
        /// <param name="element">The element to test.</param>
        /// <param name="source">The input set.</param>
        /// <returns>True if <c>element</c> is in <c>set</c>.</returns>
        val contains:
          element: 'a -> source: NonEmptySet<'a> -> bool when 'a: comparison
        
        /// <summary>Evaluates to "true" if all elements of the first set are in the second</summary>
        /// <param name="set1">The potential subset.</param>
        /// <param name="set2">The set to test against.</param>
        /// <returns>True if <c>set1</c> is a subset of <c>set2</c>.</returns>
        val isSubset:
          set1: NonEmptySet<'a> -> set2: NonEmptySet<'a> -> bool
            when 'a: comparison
        
        /// <summary>Evaluates to "true" if all elements of the first set are in the second, and at least
        /// one element of the second is not in the first.</summary>
        /// <param name="set1">The potential subset.</param>
        /// <param name="set2">The set to test against.</param>
        /// <returns>True if <c>set1</c> is a proper subset of <c>set2</c>.</returns>
        val isProperSubset:
          set1: NonEmptySet<'a> -> set2: NonEmptySet<'a> -> bool
            when 'a: comparison
        
        /// <summary>Evaluates to "true" if all elements of the second set are in the first.</summary>
        /// <param name="set1">The potential superset.</param>
        /// <param name="set2">The set to test against.</param>
        /// <returns>True if <c>set1</c> is a superset of <c>set2</c>.</returns>
        val isSuperset:
          set1: NonEmptySet<'a> -> set2: NonEmptySet<'a> -> bool
            when 'a: comparison
        
        /// <summary>Evaluates to "true" if all elements of the second set are in the first, and at least
        /// one element of the first is not in the second.</summary>
        /// <param name="set1">The potential superset.</param>
        /// <param name="set2">The set to test against.</param>
        /// <returns>True if <c>set1</c> is a proper superset of <c>set2</c>.</returns>
        val isProperSuperset:
          set1: NonEmptySet<'a> -> set2: NonEmptySet<'a> -> bool
            when 'a: comparison
        
        /// <summary>Tests if any element of the collection satisfies the given predicate.
        /// If the input function is <c>predicate</c> and the elements are <c>i0...iN</c>
        /// then computes <c>p i0 or ... or p iN</c>.</summary>
        /// <param name="predicate">The function to test set elements.</param>
        /// <param name="set">The input set.</param>
        /// <returns>True if any element of <c>set</c> satisfies <c>predicate</c>.</returns>
        val exists:
          predicate: ('a -> bool) -> set: NonEmptySet<'a> -> bool
            when 'a: comparison
        
        /// <summary>Returns a new collection containing the results of applying the
        /// given function to each element of the input set.</summary>
        /// <param name="mapping">The function to transform elements of the input set.</param>
        /// <param name="set">The input set.</param>
        /// <returns>A set containing the transformed elements.</returns>
        val map:
          mapping: ('a -> 'b) -> set: NonEmptySet<'a> -> NonEmptySet<'b>
            when 'a: comparison and 'b: comparison
        
        /// <summary>Tests if all elements of the collection satisfy the given predicate.
        /// If the input function is <c>f</c> and the elements are <c>i0...iN</c> and "j0...jN"
        /// then computes <c>p i0 &amp;&amp; ... &amp;&amp; p iN</c>.</summary>
        /// <param name="predicate">The function to test set elements.</param>
        /// <param name="set">The input set.</param>
        /// <returns>True if all elements of <c>set</c> satisfy <c>predicate</c>.</returns>
        val forall:
          predicate: ('a -> bool) -> set: NonEmptySet<'a> -> bool
            when 'a: comparison
        
        /// <summary>Computes the union of the two sets.</summary>
        /// <param name="set1">The first input set.</param>
        /// <param name="set2">The second input set.</param>
        /// <returns>The union of <c>set1</c> and <c>set2</c>.</returns>
        val union:
          set1: NonEmptySet<'a> -> set2: NonEmptySet<'a> -> NonEmptySet<'a>
            when 'a: comparison
        
        /// <summary>Computes the union of a non empty list of sets.</summary>
        /// <param name="sets">The sequence of sets to union.</param>
        /// <returns>The union of the input sets.</returns>
        val unionMany:
          sets: NonEmptyList<NonEmptySet<'a>> -> NonEmptySet<'a>
            when 'a: comparison
        
        /// <summary>Applies the given function to each element of the set, in order according
        /// to the comparison function.</summary>
        /// <param name="action">The function to apply to each element.</param>
        /// <param name="set">The input set.</param>
        val iter:
          action: ('a -> unit) -> set: NonEmptySet<'a> -> unit
            when 'a: comparison
        
        /// <summary>Returns the lowest element in the set according to the ordering being used for the set.</summary>
        /// <param name="set">The input set.</param>
        /// <returns>The min value from the set.</returns>
        val minElement: set: NonEmptySet<'a> -> 'a when 'a: comparison
        
        /// <summary>Returns the highest element in the set according to the ordering being used for the set.</summary>
        /// <param name="set">The input set.</param>
        /// <returns>The max value from the set.</returns>
        val maxElement: set: NonEmptySet<'a> -> 'a when 'a: comparison
        
        /// <summary>Applies the given accumulating function to all the elements of the set</summary>
        /// <param name="folder">The accumulating function.</param>
        /// <param name="state">The initial state.</param>
        /// <param name="set">The input set.</param>
        /// <returns>The final state.</returns>
        val fold:
          folder: ('a -> 'b -> 'a) -> state: 'a -> set: NonEmptySet<'b> -> 'a
            when 'b: comparison
        
        /// <summary>Applies the given accumulating function to all the elements of the set.</summary>
        /// <param name="folder">The accumulating function.</param>
        /// <param name="set">The input set.</param>
        /// <param name="state">The initial state.</param>
        /// <returns>The final state.</returns>
        val foldBack:
          folder: ('a -> 'b -> 'b) -> set: NonEmptySet<'a> -> state: 'b -> 'b
            when 'a: comparison
        
        val reduce:
          reduction: ('a -> 'a -> 'a) -> set: NonEmptySet<'a> -> 'a
            when 'a: comparison

