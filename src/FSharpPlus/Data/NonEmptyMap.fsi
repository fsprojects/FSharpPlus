namespace FSharpPlus.Data
    
    /// A type-safe map that contains at least one element.
    [<StructuredFormatDisplay ("{Value}")>]
    type NonEmptyMap<[<EqualityConditionalOn>] 'Key,
                     [<EqualityConditionalOn; ComparisonConditionalOn>] 'Value
                       when [<EqualityConditionalOn>] 'Key: comparison> =
        private
        { Value:
            Map<[<EqualityConditionalOn>] 'Key,
                [<EqualityConditionalOn; ComparisonConditionalOn>] 'Value> }
        interface
            System.Collections.Generic.IReadOnlyDictionary<[<EqualityConditionalOn>] 'Key,
                                                           [<EqualityConditionalOn;
                                                             ComparisonConditionalOn>] 'Value>
        interface
            NonEmptySeq<System.Collections.Generic.KeyValuePair<[<EqualityConditionalOn>] 'Key,
                                                                [<EqualityConditionalOn;
                                                                  ComparisonConditionalOn>] 'Value>>
        interface
            System.Collections.Generic.IReadOnlyCollection<System.Collections.Generic.KeyValuePair<[<EqualityConditionalOn>] 'Key,
                                                                                                   [<EqualityConditionalOn;
                                                                                                     ComparisonConditionalOn>] 'Value>>
        interface
            System.Collections.Generic.IEnumerable<System.Collections.Generic.KeyValuePair<[<EqualityConditionalOn>] 'Key,
                                                                                           [<EqualityConditionalOn;
                                                                                             ComparisonConditionalOn>] 'Value>>
        interface System.Collections.IEnumerable
        
        static member
          Create: ('Key * 'Value) *
                  [<System.ParamArray>] rest: ('Key * 'Value)[]
                    -> NonEmptyMap<'Key,'Value>
        
        [<System.ComponentModel.EditorBrowsable
          (enum<System.ComponentModel.EditorBrowsableState> (1))>]
        static member
          FoldIndexed: x: NonEmptyMap<'k,'t> * f: ('a -> 'k -> 't -> 'a) * z: 'a
                         -> 'a when 'k: comparison
        
        [<System.ComponentModel.EditorBrowsable
          (enum<System.ComponentModel.EditorBrowsableState> (1))>]
        static member
          Iterate: x: NonEmptyMap<'a,'b> * action: ('b -> unit) -> unit
                     when 'a: comparison
        
        [<System.ComponentModel.EditorBrowsable
          (enum<System.ComponentModel.EditorBrowsableState> (1))>]
        static member
          IterateIndexed: x: NonEmptyMap<'K,'T> * f: ('K -> 'T -> unit) -> unit
                            when 'K: comparison
        
        [<System.ComponentModel.EditorBrowsable
          (enum<System.ComponentModel.EditorBrowsableState> (1))>]
        static member
          Map: x: NonEmptyMap<'a,'v> * mapping: ('v -> 'u) -> NonEmptyMap<'a,'u>
                 when 'a: comparison
        
        [<System.ComponentModel.EditorBrowsable
          (enum<System.ComponentModel.EditorBrowsableState> (1))>]
        static member
          MapIndexed: x: NonEmptyMap<'K,'T> * f: ('K -> 'T -> 'U)
                        -> NonEmptyMap<'K,'U> when 'K: comparison
        
        [<System.ComponentModel.EditorBrowsable
          (enum<System.ComponentModel.EditorBrowsableState> (1))>]
        static member
          ToList: s: NonEmptyMap<'a,'b> * _impl: Control.ToList
                    -> ('a * 'b) list when 'a: comparison
        
        [<System.ComponentModel.EditorBrowsable
          (enum<System.ComponentModel.EditorBrowsableState> (1))>]
        static member
          ToSeq: s: NonEmptyMap<'a,'b> * _impl: Control.ToSeq -> seq<'a * 'b>
                   when 'a: comparison
        
        [<System.ComponentModel.EditorBrowsable
          (enum<System.ComponentModel.EditorBrowsableState> (1))>]
        static member
          inline Traverse: x: NonEmptyMap<'K,'T> * f: ('T ->  ^Functor<'U>)
                             ->  ^Functor<NonEmptyMap<'K, 'U>>
                             when 'K: comparison and
                                  (Control.Map or  ^Functor<'U> or  ^a) :
                                    (static member Map:
                                       ( ^Functor<'U> *
                                        ('b -> Map<'K,'b> -> Map<'K,'b>)) *
                                       Control.Map ->  ^a) and
                                  (Control.Apply or  ^a or  ^c) :
                                    (static member ``<*>`` :
                                        ^a *  ^c *  ^c * Control.Apply ->  ^c) and
                                  (Control.Traverse or Map<'K,'T> or  ^c) :
                                    (static member Traverse:
                                       Map<'K,'T> * ('T ->  ^Functor<'U>) *  ^c *
                                       Control.Traverse ->  ^c) and
                                  (Control.Return or  ^c) :
                                    (static member Return:
                                        ^c * Control.Return
                                         -> (Map<'d,'e> ->  ^c)) and
                                  (Control.Map or  ^c or
                                    ^Functor<NonEmptyMap<'K, 'U>>) :
                                    (static member Map:
                                       ( ^c * (Map<'f,'g> -> NonEmptyMap<'f,'g>)) *
                                       Control.Map
                                         ->  ^Functor<NonEmptyMap<'K, 'U>>) and
                                  'd: comparison and 'f: comparison
        
        [<System.ComponentModel.EditorBrowsable
          (enum<System.ComponentModel.EditorBrowsableState> (1))>]
        static member
          inline TraverseIndexed: x: NonEmptyMap<'K,'T> *
                                  f: ('K -> 'T ->  ^Functor<'U>)
                                    ->  ^Functor<NonEmptyMap<'K, 'U>>
                                    when 'K: comparison and
                                         (Control.Map or  ^Functor<'U> or  ^a) :
                                           (static member Map:
                                              ( ^Functor<'U> *
                                               ('b -> Map<'K,'b> -> Map<'K,'b>)) *
                                              Control.Map ->  ^a) and
                                         (Control.Apply or  ^a or  ^c) :
                                           (static member ``<*>`` :
                                               ^a *  ^c *  ^c * Control.Apply
                                                ->  ^c) and
                                         (Control.TraverseIndexed or Map<'K,'T> or
                                           ^c) :
                                           (static member TraverseIndexed:
                                              Map<'K,'T> *
                                              ('K -> 'T ->  ^Functor<'U>) *  ^c *
                                              Control.TraverseIndexed ->  ^c) and
                                         (Control.Return or  ^c) :
                                           (static member Return:
                                               ^c * Control.Return
                                                -> (Map<'d,'e> ->  ^c)) and
                                         (Control.Map or  ^c or
                                           ^Functor<NonEmptyMap<'K, 'U>>) :
                                           (static member Map:
                                              ( ^c *
                                               (Map<'f,'g> -> NonEmptyMap<'f,'g>)) *
                                              Control.Map
                                                ->  ^Functor<NonEmptyMap<'K, 'U>>) and
                                         'd: comparison and 'f: comparison
        
        [<System.ComponentModel.EditorBrowsable
          (enum<System.ComponentModel.EditorBrowsableState> (1))>]
        static member
          Unzip: x: NonEmptyMap<'K,('T * 'U)>
                   -> NonEmptyMap<'K,'T> * NonEmptyMap<'K,'U>
                   when 'K: comparison
        
        [<System.ComponentModel.EditorBrowsable
          (enum<System.ComponentModel.EditorBrowsableState> (1))>]
        static member
          inline ``+`` : x: NonEmptyMap<'a, ^b> * y: NonEmptyMap<'a, ^b> *
                         _mthd: Control.Plus -> NonEmptyMap<'a, ^b>
                           when 'a: comparison and
                                (Control.Plus or  ^b) :
                                  (static member ``+`` :
                                      ^b *  ^b * Control.Plus ->  ^b)
        
        member Add: key: 'Key * value: 'Value -> NonEmptyMap<'Key,'Value>
        
        member ContainsKey: key: 'Key -> bool
        
        member TryFind: key: 'Key -> 'Value option
        
        member TryGetValue: key: 'Key * value: byref<'Value> -> bool
        
        member Item: key: 'Key -> 'Value with get
    
    /// A type alias for NonEmptyMap<'Key,'Value>
    type nemap<'Key,'Value when 'Key: comparison> = NonEmptyMap<'Key,'Value>
    
    /// Basic operations on NonEmptyMap
    module NonEmptyMap =
        
        /// <summary>Builds a non empty map.</summary>
        val create:
          k: 'k * v: 'v -> rest: seq<'k * 'v> -> NonEmptyMap<'k,'v>
            when 'k: comparison
        
        /// <summary>Builds a non empty map with a single element.</summary>
        val singleton:
          key: 'a -> value: 'b -> NonEmptyMap<'a,'b> when 'a: comparison
        
        /// <summary>Returns a new map with the binding added to the given map.
        /// If a binding with the given key already exists in the input map, the existing binding is replaced by the new binding in the result map.</summary>
        /// <param name="key">The input key.</param>
        /// <param name="value">The input value.</param>
        /// <param name="table">The input map.</param>
        /// <returns>The resulting map.</returns>
        val add:
          key: 'a -> value: 'b -> table: NonEmptyMap<'a,'b>
            -> NonEmptyMap<'a,'b> when 'a: comparison
        
        /// <summary>Builds a list from the given non empty map.</summary>
        val toList: NonEmptyMap<'a,'b> -> ('a * 'b) list when 'a: comparison
        
        /// <summary>Builds a non-empty list from the given non empty map.</summary>
        val toNonEmptyList:
          NonEmptyMap<'a,'b> -> NonEmptyList<'a * 'b> when 'a: comparison
        
        /// <summary>Builds a sequence from the given non empty map.</summary>
        val toSeq: NonEmptyMap<'a,'b> -> seq<'a * 'b> when 'a: comparison
        
        /// <summary>Builds a non-empty sequence from the given non empty map.</summary>
        val toNonEmptySeq:
          NonEmptyMap<'a,'b> -> NonEmptySeq<'a * 'b> when 'a: comparison
        
        /// <summary>Builds an array from the given non empty map.</summary>
        val toArray: NonEmptyMap<'a,'b> -> ('a * 'b)[] when 'a: comparison
        
        /// <summary>Builds a map from the given non empty map.</summary>
        val toMap: NonEmptyMap<'a,'b> -> Map<'a,'b> when 'a: comparison
        
        /// <summary>Builds a non empty map from the given array.</summary>
        /// <param name="array">The input array.</param>
        /// <returns>Non empty map containing the elements of the array.</returns>
        /// <exception cref="System.ArgumentException">Thrown when the input array is empty.</exception>
        /// <remarks>Throws exception for empty array</remarks>
        val ofArray:
          array: ('a * 'b) array -> NonEmptyMap<'a,'b> when 'a: comparison
        
        /// <summary>Builds a non empty map from the given list.</summary>
        /// <param name="list">The input list.</param>
        /// <returns>Non empty map containing the elements of the list.</returns>
        /// <exception cref="System.ArgumentException">Thrown when the input list is empty.</exception>
        /// <remarks>Throws exception for empty list</remarks>
        val ofList:
          list: ('a * 'b) list -> NonEmptyMap<'a,'b> when 'a: comparison
        
        /// <summary>Builds a non empty map from the given non-empty list.</summary>
        /// <param name="list">The input list.</param>
        /// <returns>Non empty map containing the elements of the non-empty list.</returns>
        val ofNonEmptyList:
          list: NonEmptyList<'a * 'b> -> NonEmptyMap<'a,'b> when 'a: comparison
        
        /// <summary>Builds a non empty map from the given sequence.</summary>
        /// <param name="seq">The input list.</param>
        /// <returns>Non empty map containing the elements of the list.</returns>
        /// <exception cref="System.ArgumentException">Thrown when the input list is empty.</exception>
        /// <remarks>Throws exception for empty sequence</remarks>
        val ofSeq: seq: seq<'a * 'b> -> NonEmptyMap<'a,'b> when 'a: comparison
        
        /// <summary>Builds a non empty map from the given non-empty sequence.</summary>
        /// <param name="source">The input sequence.</param>
        /// <returns>Non empty map containing the elements of the non-empty sequence.</returns>
        val ofNonEmptySeq:
          source: NonEmptySeq<'a * 'b> -> NonEmptyMap<'a,'b> when 'a: comparison
        
        /// <summary>Builds a non empty map from the given map.</summary>
        /// <param name="map">The input map.</param>
        /// <returns>Non empty map containing the elements of the map.</returns>
        /// <exception cref="System.ArgumentException">Thrown when the input map is empty.</exception>
        /// <remarks>Throws exception for empty map</remarks>
        val ofMap: map: Map<'a,'b> -> NonEmptyMap<'a,'b> when 'a: comparison
        
        /// Transforms a map to a NonEmptyMap, returning an option to signal when the original map was empty.
        val tryOfMap:
          map: Map<'a,'b> -> NonEmptyMap<'a,'b> option when 'a: comparison
        
        /// <summary>Lookup an element in the map, raising <c>KeyNotFoundException</c> if no binding
        /// exists in the map.</summary>
        /// <param name="key">The input key.</param>
        /// <param name="table">The input map.</param>
        /// <exception cref="System.Collections.Generic.KeyNotFoundException">Thrown when the key does not exist in the map.</exception>
        /// <returns>The value mapped to the given key.</returns>
        val find: key: 'a -> table: NonEmptyMap<'a,'b> -> 'b when 'a: comparison
        
        /// <summary>Searches the map looking for the first element where the given function returns a <c>Some</c> value.</summary>
        /// <param name="chooser">The function to generate options from the key/value pairs.</param>
        /// <param name="table">The input map.</param>
        /// <returns>The first result.</returns>
        val tryPick:
          chooser: ('a -> 'b -> 'c option) -> table: NonEmptyMap<'a,'b>
            -> 'c option when 'a: comparison
        
        /// <summary>Searches the map looking for the first element where the given function returns a <c>Some</c> value</summary>
        /// <param name="chooser">The function to generate options from the key/value pairs.</param>
        /// <param name="table">The input map.</param>
        /// <returns>The first result.</returns>
        val pick:
          chooser: ('a -> 'b -> 'c option) -> table: NonEmptyMap<'a,'b> -> 'c
            when 'a: comparison
        
        /// <summary>Folds over the bindings in the map.</summary>
        /// <param name="folder">The function to update the state given the input key/value pairs.</param>
        /// <param name="table">The input map.</param>
        /// <param name="state">The initial state.</param>
        /// <returns>The final state value.</returns>
        val foldBack:
          folder: ('a -> 'b -> 'c -> 'c) -> table: NonEmptyMap<'a,'b>
          -> state: 'c -> 'c when 'a: comparison
        
        /// <summary>Folds over the bindings in the map </summary>
        /// <param name="folder">The function to update the state given the input key/value pairs.</param>
        /// <param name="state">The initial state.</param>
        /// <param name="table">The input map.</param>
        /// <returns>The final state value.</returns>
        val fold:
          folder: ('a -> 'b -> 'c -> 'a) -> state: 'a
          -> table: NonEmptyMap<'b,'c> -> 'a when 'b: comparison
        
        /// <summary>Applies the given function to each binding in the dictionary</summary>
        /// <param name="action">The function to apply to each key/value pair.</param>
        /// <param name="table">The input map.</param>
        val iter:
          action: ('a -> 'b -> unit) -> table: NonEmptyMap<'a,'b> -> unit
            when 'a: comparison
        
        /// <summary>Returns true if the given predicate returns true for one of the
        /// bindings in the map.</summary>
        /// <param name="predicate">The function to test the input elements.</param>
        /// <param name="table">The input map.</param>
        /// <returns>True if the predicate returns true for one of the key/value pairs.</returns>
        val exists:
          predicate: ('a -> 'b -> bool) -> table: NonEmptyMap<'a,'b> -> bool
            when 'a: comparison
        
        /// <summary>Returns true if the given predicate returns true for all of the
        /// bindings in the map.</summary>
        /// <param name="predicate">The function to test the input elements.</param>
        /// <param name="table">The input map.</param>
        /// <returns>True if the predicate evaluates to true for all of the bindings in the map.</returns>
        val forall:
          predicate: ('a -> 'b -> bool) -> table: NonEmptyMap<'a,'b> -> bool
            when 'a: comparison
        
        /// <summary>Builds a new collection whose elements are the results of applying the given function
        /// to each of the elements of the collection. The key passed to the
        /// function indicates the key of element being transformed.</summary>
        /// <param name="mapping">The function to transform the key/value pairs.</param>
        /// <param name="table">The input map.</param>
        /// <returns>The resulting map of keys and transformed values.</returns>
        val map:
          mapping: ('a -> 'b -> 'c) -> table: NonEmptyMap<'a,'b>
            -> NonEmptyMap<'a,'c> when 'a: comparison
        
        /// <summary>Tests if an element is in the domain of the map.</summary>
        /// <param name="key">The input key.</param>
        /// <param name="table">The input map.</param>
        /// <returns>True if the map contains the key.</returns>
        val containsKey:
          key: 'a -> table: NonEmptyMap<'a,'b> -> bool when 'a: comparison
        
        /// <summary>Lookup an element in the map, returning a <c>Some</c> value if the element is in the domain
        /// of the map and <c>None</c> if not.</summary>
        /// <param name="key">The input key.</param>
        /// <param name="table">The input map.</param>
        /// <returns>The found <c>Some</c> value or <c>None</c>.</returns>
        val tryFind:
          key: 'a -> table: NonEmptyMap<'a,'b> -> 'b option when 'a: comparison
        
        /// <summary>Evaluates the function on each mapping in the collection. Returns the key for the first mapping
        /// where the function returns 'true'. Raise <c>KeyNotFoundException</c> if no such element exists.</summary>
        /// <param name="predicate">The function to test the input elements.</param>
        /// <param name="table">The input map.</param>
        /// <exception cref="System.Collections.Generic.KeyNotFoundException">Thrown if the key does not exist in the map.</exception>
        /// <returns>The first key for which the predicate evaluates true.</returns>
        val findKey:
          predicate: ('a -> 'b -> bool) -> table: NonEmptyMap<'a,'b> -> 'a
            when 'a: comparison
        
        /// <summary>Returns the key of the first mapping in the collection that satisfies the given predicate.
        /// Returns 'None' if no such element exists.</summary>
        /// <param name="predicate">The function to test the input elements.</param>
        /// <param name="table">The input map.</param>
        /// <returns>The first key for which the predicate returns true or None if the predicate evaluates to false for each key/value pair.</returns>
        val tryFindKey:
          predicate: ('a -> 'b -> bool) -> table: NonEmptyMap<'a,'b>
            -> 'a option when 'a: comparison
        
        /// <summary>The number of bindings in the map.</summary>
        val count: table: NonEmptyMap<'a,'b> -> int when 'a: comparison
        
        val reduce:
          reduction: ('a * 'b -> 'a * 'b -> 'a * 'b) -> map: NonEmptyMap<'a,'b>
            -> 'a * 'b when 'a: comparison
        
        val reduceBack:
          reduction: ('a * 'b -> 'a * 'b -> 'a * 'b) -> map: NonEmptyMap<'a,'b>
            -> 'a * 'b when 'a: comparison
        
        val keys:
          source: NonEmptyMap<'Key,'T> -> seq<'Key> when 'Key: comparison
        
        val values:
          source: NonEmptyMap<'Key,'T> -> seq<'T> when 'Key: comparison
        
        /// <summary>Map values of the original Map.</summary>
        /// <remarks>Keys remain unchanged.</remarks>
        /// <param name="f">The mapping function.</param>
        /// <param name="x">The input Map.</param>
        ///
        /// <returns>The mapped Map.</returns>
        val mapValues:
          f: ('T -> 'a) -> x: NonEmptyMap<'Key,'T> -> NonEmptyMap<'Key,'a>
            when 'Key: comparison
        
        val iterValues:
          f: ('T -> unit) -> x: NonEmptyMap<'a,'T> -> unit when 'a: comparison
        
        val unzip:
          source: NonEmptyMap<'Key,('T1 * 'T2)>
            -> NonEmptyMap<'Key,'T1> * NonEmptyMap<'Key,'T2>
            when 'Key: comparison
        
        /// Returns the union of two maps, using the combiner function for duplicate keys.
        val unionWith:
          combiner: ('Value -> 'Value -> 'Value)
          -> source1: NonEmptyMap<'Key,'Value>
          -> source2: NonEmptyMap<'Key,'Value> -> NonEmptyMap<'Key,'Value>
            when 'Key: comparison
        
        /// Returns the union of two maps, preferring values from the first in case of duplicate keys.
        val union:
          source: NonEmptyMap<'Key,'T> -> altSource: NonEmptyMap<'Key,'T>
            -> NonEmptyMap<'Key,'T> when 'Key: comparison
        
        val inline traverse:
          f: ('T ->  ^Functor<'U>) -> m: NonEmptyMap<'K,'T>
            ->  ^Functor<NonEmptyMap<'K, 'U>>
            when (Control.Map or  ^Functor<'U> or  ^a) :
                   (static member Map:
                      ( ^Functor<'U> * ('b -> Map<'K,'b> -> Map<'K,'b>)) *
                      Control.Map ->  ^a) and
                 (Control.Apply or  ^a or  ^Functor<Map<'K, 'U>>) :
                   (static member ``<*>`` :
                       ^a *  ^Functor<Map<'K, 'U>> *  ^Functor<Map<'K, 'U>> *
                      Control.Apply ->  ^Functor<Map<'K, 'U>>) and
                 'K: comparison and
                 (Control.Traverse or Map<'K,'T> or  ^Functor<Map<'K, 'U>>) :
                   (static member Traverse:
                      Map<'K,'T> * ('T ->  ^Functor<'U>) *
                       ^Functor<Map<'K, 'U>> * Control.Traverse
                        ->  ^Functor<Map<'K, 'U>>) and
                 (Control.Return or  ^Functor<Map<'K, 'U>>) :
                   (static member Return:
                       ^Functor<Map<'K, 'U>> * Control.Return
                        -> (Map<'c,'d> ->  ^Functor<Map<'K, 'U>>)) and
                 (Control.Map or  ^Functor<Map<'K, 'U>> or
                   ^Functor<NonEmptyMap<'K, 'U>>) :
                   (static member Map:
                      ( ^Functor<Map<'K, 'U>> *
                       (Map<'e,'f> -> NonEmptyMap<'e,'f>)) * Control.Map
                        ->  ^Functor<NonEmptyMap<'K, 'U>>) and 'c: comparison and
                 'e: comparison
        
        val inline traversei:
          f: ('K -> 'T ->  ^Functor<'U>) -> m: NonEmptyMap<'K,'T>
            ->  ^Functor<NonEmptyMap<'K, 'U>>
            when 'K: comparison and
                 (Control.Map or  ^Functor<'U> or  ^a) :
                   (static member Map:
                      ( ^Functor<'U> * ('b -> Map<'K,'b> -> Map<'K,'b>)) *
                      Control.Map ->  ^a) and
                 (Control.Apply or  ^a or  ^Functor<Map<'K, 'U>>) :
                   (static member ``<*>`` :
                       ^a *  ^Functor<Map<'K, 'U>> *  ^Functor<Map<'K, 'U>> *
                      Control.Apply ->  ^Functor<Map<'K, 'U>>) and
                 (Control.TraverseIndexed or Map<'K,'T> or
                   ^Functor<Map<'K, 'U>>) :
                   (static member TraverseIndexed:
                      Map<'K,'T> * ('K -> 'T ->  ^Functor<'U>) *
                       ^Functor<Map<'K, 'U>> * Control.TraverseIndexed
                        ->  ^Functor<Map<'K, 'U>>) and
                 (Control.Return or  ^Functor<Map<'K, 'U>>) :
                   (static member Return:
                       ^Functor<Map<'K, 'U>> * Control.Return
                        -> (Map<'c,'d> ->  ^Functor<Map<'K, 'U>>)) and
                 (Control.Map or  ^Functor<Map<'K, 'U>> or
                   ^Functor<NonEmptyMap<'K, 'U>>) :
                   (static member Map:
                      ( ^Functor<Map<'K, 'U>> *
                       (Map<'e,'f> -> NonEmptyMap<'e,'f>)) * Control.Map
                        ->  ^Functor<NonEmptyMap<'K, 'U>>) and 'c: comparison and
                 'e: comparison

