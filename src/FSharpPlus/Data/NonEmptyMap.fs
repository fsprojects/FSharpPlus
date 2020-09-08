namespace FSharpPlus.Data

open System
open System.Runtime.InteropServices
open System.ComponentModel
open System.Collections.Generic
open FSharpPlus
open FSharpPlus.Control

type NonEmptyMap<[<EqualityConditionalOn>]'Key,[<EqualityConditionalOn;ComparisonConditionalOn>]'Value when 'Key : comparison> =
    private { Value: Map<'Key, 'Value> } with
    interface Collections.IEnumerable with member x.GetEnumerator () = (x.Value :> _ seq).GetEnumerator() :> _
    interface IEnumerable<KeyValuePair<'Key, 'Value>> with member x.GetEnumerator() = (x.Value :> _ seq).GetEnumerator()
    interface IReadOnlyCollection<KeyValuePair<'Key, 'Value>> with member x.Count = x.Value.Count

    static member Create ((k, v), [<ParamArray>] rest: ('Key * 'Value)[]) =
        Map.ofArray rest |> Map.add k v

    member x.Add(key, value) = { Value = Map.add key value x.Value }
    member x.Item with get(key) = x.Value.[key]
    member x.ContainsKey(key) = x.Value.ContainsKey(key)
    member x.TryFind(key) = x.Value.TryFind(key)
    member x.TryGetValue(key, [<Out>] value:byref<'Value>) =
      x.Value.TryGetValue(key, &value)

    interface IReadOnlyDictionary<'Key, 'Value> with
      member x.Item with get(key) = x.[key]
      member x.Keys = (x.Value :> IReadOnlyDictionary<_, _>).Keys
      member x.TryGetValue(key, value: byref<'Value>) = x.TryGetValue(key, &value)
      member x.Values = (x.Value :> IReadOnlyDictionary<_, _>).Values
      member x.ContainsKey key = x.ContainsKey key

module NonEmptyMap =
    /// <summary>Builds a non empty map.</summary>
    let create (k, v) (rest: ('k * 'v) seq) : NonEmptyMap<_, _> =
      { Value = Map.ofSeq rest |> Map.add k v }
    /// <summary>Builds a non empty map with a single element.</summary>
    let singleton key value : NonEmptyMap<_, _> = { Value = Map.ofList [key, value] }

    /// <summary>Returns a new map with the binding added to the given map.
    /// If a binding with the given key already exists in the input map, the existing binding is replaced by the new binding in the result map.</summary>
    /// <param name="key">The input key.</param>
    /// <param name="value">The input value.</param>
    /// <param name="table">The input map.</param>
    /// <returns>The resulting map.</returns>
    let add key value (table: NonEmptyMap<_, _>) = { Value = Map.add key value table.Value }

    /// <summary>Builds a list from the given non empty map.</summary>
    let toList  ({ Value = v }: NonEmptyMap<_, _>) = Map.toList v
    /// <summary>Builds a sequence from the given non empty map.</summary>
    let toSeq   ({ Value = v }: NonEmptyMap<_, _>) = Map.toSeq v
    /// <summary>Builds an array from the given non empty map.</summary>
    let toArray ({ Value = v }: NonEmptyMap<_, _>) = Map.toArray v
    /// <summary>Builds a map from the given non empty map.</summary>
    let toMap ({ Value = v }: NonEmptyMap<_, _>) = v

    /// <summary>Builds a non empty map from the given array.</summary>
    /// <param name="array">The input array.</param>
    /// <returns>Non empty map containing the elements of the array.</returns>
    /// <exception cref="System.ArgumentException">Thrown when the input array is empty.</exception>
    /// <remarks>Throws exception for empty array</remarks>
    let ofArray (array : _ array) =
        match array |> Array.toList with
        | []    -> invalidArg "array" "The input array was empty."
        | x::xs -> create x xs
    /// <summary>Builds a non empty map from the given list.</summary>
    /// <param name="list">The input list.</param>
    /// <returns>Non empty map containing the elements of the list.</returns>
    /// <exception cref="System.ArgumentException">Thrown when the input list is empty.</exception>
    /// <remarks>Throws exception for empty list</remarks>
    let ofList (list : _ list) =
        match list with
        | []    -> invalidArg "list" "The input list was empty."
        | x::xs -> create x xs
    /// <summary>Builds a non empty map from the given sequence.</summary>
    /// <param name="seq">The input list.</param>
    /// <returns>Non empty map containing the elements of the list.</returns>
    /// <exception cref="System.ArgumentException">Thrown when the input list is empty.</exception>
    /// <remarks>Throws exception for empty sequence</remarks>
    let ofSeq (seq : _ seq) =
        match seq |> Seq.toList with
        | []    -> invalidArg "seq" "The input sequence was empty."
        | x::xs -> create x xs
    /// <summary>Builds a non empty map from the given map.</summary>
    /// <param name="map">The input map.</param>
    /// <returns>Non empty map containing the elements of the map.</returns>
    /// <exception cref="System.ArgumentException">Thrown when the input map is empty.</exception>
    /// <remarks>Throws exception for empty map</remarks>
    let ofMap (map: Map<_, _>) =
      if Map.isEmpty map then invalidArg "seq" "The input sequence was empty."
      else { Value = map }
    /// Transforms a map to a NonEmptyMap, returning an option to signal when the original map was empty.
    let tryOfMap (map: Map<_, _>) =
      if Map.isEmpty map then None
      else Some { Value = map }

    /// <summary>Lookup an element in the map, raising <c>KeyNotFoundException</c> if no binding
    /// exists in the map.</summary>
    /// <param name="key">The input key.</param>
    /// <param name="table">The input map.</param>
    /// <exception cref="System.Collections.Generic.KeyNotFoundException">Thrown when the key does not exist in the map.</exception>
    /// <returns>The value mapped to the given key.</returns>
    let find key (table: NonEmptyMap<_, _>) = Map.find key table.Value

    /// <summary>Searches the map looking for the first element where the given function returns a <c>Some</c> value.</summary>
    /// <param name="chooser">The function to generate options from the key/value pairs.</param>
    /// <param name="table">The input map.</param>
    /// <returns>The first result.</returns>
    let tryPick chooser (table: NonEmptyMap<_, _>) = Map.tryPick chooser table.Value

    /// <summary>Searches the map looking for the first element where the given function returns a <c>Some</c> value</summary>
    /// <param name="chooser">The function to generate options from the key/value pairs.</param>
    /// <param name="table">The input map.</param>
    /// <returns>The first result.</returns>
    let pick chooser (table: NonEmptyMap<_, _>) = Map.pick chooser table.Value

    /// <summary>Folds over the bindings in the map.</summary>
    /// <param name="folder">The function to update the state given the input key/value pairs.</param>
    /// <param name="table">The input map.</param>
    /// <param name="state">The initial state.</param>
    /// <returns>The final state value.</returns>
    let foldBack folder (table: NonEmptyMap<_, _>) state = Map.foldBack folder table.Value state

    /// <summary>Folds over the bindings in the map </summary>
    /// <param name="folder">The function to update the state given the input key/value pairs.</param>
    /// <param name="state">The initial state.</param>
    /// <param name="table">The input map.</param>
    /// <returns>The final state value.</returns>
    let fold folder state (table: NonEmptyMap<_, _>) = Map.fold folder state table.Value

    /// <summary>Applies the given function to each binding in the dictionary</summary>
    /// <param name="action">The function to apply to each key/value pair.</param>
    /// <param name="table">The input map.</param>
    let iter action (table: NonEmptyMap<_, _>) = Map.iter action table.Value

    /// <summary>Returns true if the given predicate returns true for one of the
    /// bindings in the map.</summary>
    /// <param name="predicate">The function to test the input elements.</param>
    /// <param name="table">The input map.</param>
    /// <returns>True if the predicate returns true for one of the key/value pairs.</returns>
    let exists predicate (table: NonEmptyMap<_, _>) = Map.exists predicate table.Value

    /// <summary>Returns true if the given predicate returns true for all of the
    /// bindings in the map.</summary>
    /// <param name="predicate">The function to test the input elements.</param>
    /// <param name="table">The input map.</param>
    /// <returns>True if the predicate evaluates to true for all of the bindings in the map.</returns>
    let forall predicate (table: NonEmptyMap<_, _>) = Map.forall predicate table.Value

    /// <summary>Builds a new collection whose elements are the results of applying the given function
    /// to each of the elements of the collection. The key passed to the
    /// function indicates the key of element being transformed.</summary>
    /// <param name="mapping">The function to transform the key/value pairs.</param>
    /// <param name="table">The input map.</param>
    /// <returns>The resulting map of keys and transformed values.</returns>
    let map mapping (table: NonEmptyMap<_, _>) : NonEmptyMap<_, _> = { Value = Map.map mapping table.Value }

    /// <summary>Tests if an element is in the domain of the map.</summary>
    /// <param name="key">The input key.</param>
    /// <param name="table">The input map.</param>
    /// <returns>True if the map contains the key.</returns>
    let containsKey key (table: NonEmptyMap<_, _>) = Map.containsKey key table.Value

    /// <summary>Lookup an element in the map, returning a <c>Some</c> value if the element is in the domain
    /// of the map and <c>None</c> if not.</summary>
    /// <param name="key">The input key.</param>
    /// <param name="table">The input map.</param>
    /// <returns>The found <c>Some</c> value or <c>None</c>.</returns>
    let tryFind key (table: NonEmptyMap<_, _>) = Map.tryFind key table.Value

    /// <summary>Evaluates the function on each mapping in the collection. Returns the key for the first mapping
    /// where the function returns 'true'. Raise <c>KeyNotFoundException</c> if no such element exists.</summary>
    /// <param name="predicate">The function to test the input elements.</param>
    /// <param name="table">The input map.</param>
    /// <exception cref="System.Collections.Generic.KeyNotFoundException">Thrown if the key does not exist in the map.</exception>
    /// <returns>The first key for which the predicate evaluates true.</returns>
    let findKey predicate (table: NonEmptyMap<_, _>) = Map.findKey predicate table.Value

    /// <summary>Returns the key of the first mapping in the collection that satisfies the given predicate.
    /// Returns 'None' if no such element exists.</summary>
    /// <param name="predicate">The function to test the input elements.</param>
    /// <param name="table">The input map.</param>
    /// <returns>The first key for which the predicate returns true or None if the predicate evaluates to false for each key/value pair.</returns>
    let tryFindKey predicate (table: NonEmptyMap<_, _>) = Map.tryFindKey predicate table.Value

    /// <summary>The number of bindings in the map.</summary>
    let count (table: NonEmptyMap<_, _>) = Map.count table.Value

    // reduce functions

    let reduce reduction (map: NonEmptyMap<_, _>) = Seq.reduce reduction (toSeq map)
    let reduceBack reduction (map: NonEmptyMap<_, _>) = Seq.reduceBack reduction (toSeq map)

    // extensions from FSharpPlus

    let keys   (source: NonEmptyMap<'Key, 'T>) = Seq.map (fun (KeyValue(k, _)) -> k) source
    let values (source: NonEmptyMap<'Key, 'T>) = Seq.map (fun (KeyValue(_, v)) -> v) source

    /// <summary>Map values of the original Map.</summary>
    /// <remarks>Keys remain unchanged.</remarks>
    /// <param name="f">The mapping function.</param>
    /// <param name="x">The input Map.</param>
    ///
    /// <returns>The mapped Map.</returns>
    let mapValues f (x: NonEmptyMap<'Key, 'T>) = map (fun _ -> f) x

    let iterValues f (x: NonEmptyMap<_, 'T>) = iter (fun _ -> f) x

    let unzip (source: NonEmptyMap<'Key, 'T1 * 'T2>) = mapValues fst source, mapValues snd source

    /// Returns the union of two maps, using the combiner function for duplicate keys.
    let unionWith combiner (source1: NonEmptyMap<'Key, 'Value>) (source2: NonEmptyMap<'Key, 'Value>) =
        fold (fun m k v' -> add k (match tryFind k m with Some v -> combiner v v' | None -> v') m) source1 source2

    /// Returns the union of two maps, preferring values from the first in case of duplicate keys.
    let union (source: NonEmptyMap<'Key, 'T>) (altSource: NonEmptyMap<'Key, 'T>) = unionWith (fun x _ -> x) source altSource

type NonEmptyMap<[<EqualityConditionalOn>]'Key,[<EqualityConditionalOn;ComparisonConditionalOn>]'Value when 'Key : comparison> with
    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member Iterate (x: NonEmptyMap<_, _>, action) = NonEmptyMap.iterValues action x

    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member Map (x: NonEmptyMap<_, 'v>, mapping: 'v -> 'u) = NonEmptyMap.mapValues mapping x : NonEmptyMap<_, 'u>

    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member Unzip (x: NonEmptyMap<'K, ('T * 'U)>) = NonEmptyMap.unzip x : NonEmptyMap<'K, 'T> * NonEmptyMap<'K, 'U>

    #if !FABLE_COMPILER
    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member inline ``+`` (x: NonEmptyMap<'a,'b>, y, [<Optional>]_mthd: Plus) = NonEmptyMap.unionWith Plus.Invoke x y

    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member ToList (s: NonEmptyMap<_, _>, [<Optional>]_impl: ToList) = NonEmptyMap.toList s

    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member ToSeq (s: NonEmptyMap<_, _>, [<Optional>]_impl: ToSeq) = NonEmptyMap.toSeq s

    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member MapIndexed (x: NonEmptyMap<'K,'T>, f, [<Optional>]_impl: MapIndexed) = NonEmptyMap.map f x : NonEmptyMap<'K,'U>

    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member IterateIndexed (x: NonEmptyMap<'K,'T>, f, [<Optional>]_impl: IterateIndexed) = NonEmptyMap.iter f x

    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member FoldIndexed (x: NonEmptyMap<'k,'t>, f, z, _impl: FoldIndexed) = NonEmptyMap.fold f z x
    #endif
