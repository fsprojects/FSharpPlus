namespace FSharpPlus

/// Additional operations on IDictionary<'Key, 'Value>
[<RequireQualifiedAccess>]
module Dict =
    open System.Collections.Generic
    open System.Collections.ObjectModel

    #if !FABLE_COMPILER
    open System.Linq

    /// Converts an IDictionary to an IReadOnlyDictionary.
    let toIReadOnlyDictionary source = ReadOnlyDictionary source :> IReadOnlyDictionary<_,_>
    #endif

    /// <summary>Tries to get the value of the given key.</summary>
    /// <remarks>This is a function wrapper for the IDictionary.TryGetValue method,
    /// representing the result as an Option&lt;value&gt; instead of a bool plus an out-value.
    /// </remarks>
    /// <param name="key">The key whose value you wish to find.</param>
    /// <param name="source">The input dictionary.</param>
    ///
    /// <returns>An option wrapped value</returns>
    let tryGetValue key (source: IDictionary<'Key, 'Value>) =
        match source.TryGetValue key with
        | true, v -> Some v
        | _       -> None

    /// <summary>Does the dictionary contain the given key?</summary>
    /// <remarks>Note: this is a function wrapper for the IDictionary.ContainsKey method</remarks>
    /// <param name="key">The key to find.</param>
    /// <param name="source">The input dictionary.</param>
    ///
    /// <returns>A bool indicating if the key was found</returns>
    let containsKey key (source: IDictionary<'Key, 'Value>) = source.ContainsKey key

    /// <summary>Returns the keys of the given dictionary.</summary>
    /// <param name="source">The input dictionary.</param>
    ///
    /// <returns>A seq of the keys in the dictionary.</returns>
    let keys   (source: IDictionary<_, _>) = Seq.map (fun (KeyValue(k, _)) -> k) source

    /// <summary>Returns the values of the given dictionary.</summary>
    /// <param name="source">The input dictionary.</param>
    ///
    /// <returns>A seq of the values in the dictionary.</returns>
    let values (source: IDictionary<_, _>) = Seq.map (fun (KeyValue(_, v)) -> v) source

    /// <summary>Maps the given function over each value in the dictionary.</summary>
    /// <param name="mapper">The mapping function.</param>
    /// <param name="source">The input dictionary.</param>
    ///
    /// <returns>The mapped dictionary.</returns>
    let map mapper (source: IDictionary<'Key, 'T>) =
        let dct = Dictionary<'Key, 'U> ()
        for KeyValue(k, v) in source do
            dct.Add (k, mapper v)
        dct :> IDictionary<'Key, 'U>

    /// <summary>Creates a Dictionary value from a pair of Dictionaries, using a function to combine them.</summary>
    /// <remarks>Keys that are not present on both dictionaries are dropped.</remarks>
    /// <param name="mapper">The mapping function.</param>
    /// <param name="source1">The first input dictionary.</param>
    /// <param name="source2">The second input dictionary.</param>
    ///
    /// <returns>The combined dictionary.</returns>
    let map2 mapper (source1: IDictionary<'Key, 'T1>) (source2: IDictionary<'Key, 'T2>) =
        let dct = Dictionary<'Key, 'U> ()
        let f = OptimizedClosures.FSharpFunc<_, _, _>.Adapt mapper
        for KeyValue(k, vx) in source1 do
            match tryGetValue k source2 with
            | Some vy -> dct.Add (k, f.Invoke (vx, vy))
            | None    -> ()
        dct :> IDictionary<'Key, 'U>

    /// <summary>Combines values from three dictionaries using mapping function.</summary>
    /// <remarks>Keys that are not present on every dictionary are dropped.</remarks>
    /// <param name="mapping">The mapping function.</param>
    /// <param name="source1">First input dictionary.</param>
    /// <param name="source2">Second input dictionary.</param>
    /// <param name="source3">Third input dictionary.</param>
    ///
    /// <returns>The mapped dictionary.</returns>
    let map3 mapping (source1: IDictionary<'Key, 'T1>) (source2: IDictionary<'Key, 'T2>) (source3: IDictionary<'Key, 'T3>) =
        let dct = Dictionary<'Key, 'U> ()
        let f = OptimizedClosures.FSharpFunc<_,_,_,_>.Adapt mapping
        for KeyValue(k, vx) in source1 do
            match tryGetValue k source2, tryGetValue k source3 with
            | Some vy, Some vz -> dct.Add (k, f.Invoke (vx, vy, vz))
            | _      , _       -> ()
        dct :> IDictionary<'Key, 'U>

    /// <summary>Applies given function to each value of the given dictionary.</summary>
    /// <param name="chooser">The mapping function.</param>
    /// <param name="source">The input dictionary.</param>
    ///
    /// <returns>Returns dictionary with values x for each dictionary value where the function returns Some(x).</returns>
    let chooseValues chooser (source: IDictionary<'Key, 'T>) =
        let dct = Dictionary<'Key, 'U> ()
        for KeyValue(k, v) in source do
            match chooser v with
            | Some v -> dct.Add (k, v)
            | None    -> ()
        dct :> IDictionary<'Key, 'U>
        
    /// <summary>Tuples values of two dictionaries.</summary>
    /// <remarks>Keys that are not present on both dictionaries are dropped.</remarks>
    /// <param name="source1">The first input dictionary.</param>
    /// <param name="source2">The second input dictionary.</param>
    ///
    /// <returns>The tupled dictionary.</returns>
    let zip (source1: IDictionary<'Key, 'T1>) (source2: IDictionary<'Key, 'T2>) =
        let dct = Dictionary<'Key, 'T1 * 'T2> ()
        for KeyValue(k, vx) in source1 do
            match tryGetValue k source2 with
            | Some vy -> dct.Add (k, (vx, vy))
            | None    -> ()
        dct :> IDictionary<'Key, 'T1 * 'T2>

    /// <summary>Splits a dictionary with tuple pair values to two separate dictionaries.</summary>
    /// <param name="source">The source dictionary.</param>
    ///
    /// <returns>A tuple of each untupled dictionary.</returns>
    let unzip (source: IDictionary<'Key, 'T1 * 'T2>) =
        let dct1 = Dictionary<'Key, 'T1> ()
        let dct2 = Dictionary<'Key, 'T2> ()
        for KeyValue(k, (vx, vy)) in source do
            dct1.Add (k, vx)
            dct2.Add (k, vy)
        dct1 :> IDictionary<'Key, 'T1>, dct2 :> IDictionary<'Key, 'T2>

    /// Returns the union of two dictionaries, using the combiner function for duplicate keys.
    let unionWith combiner (source1: IDictionary<'Key, 'Value>) (source2: IDictionary<'Key, 'Value>) =
        let d = Dictionary<'Key,'Value> ()
        let f = OptimizedClosures.FSharpFunc<_,_,_>.Adapt combiner
        for KeyValue(k, v ) in source1 do d.[k] <- v
        for KeyValue(k, v') in source2 do d.[k] <- match d.TryGetValue k with true, v -> f.Invoke (v, v') | _ -> v'
        d :> IDictionary<'Key,'Value>

    #if !FABLE_COMPILER
   ///Returns the union of two maps, preferring values from the first in case of duplicate keys.
    let union (source: IDictionary<'Key, 'T>) (altSource: IDictionary<'Key, 'T>) = 
        Enumerable
          .Union(
            source, 
            altSource,
            { new IEqualityComparer<KeyValuePair<'Key,'T>> with 
                      member _.Equals ((a:KeyValuePair<'Key,'T>),(b:KeyValuePair<'Key,'T>)) : bool = a.Key = b.Key
                      member _.GetHashCode (a:KeyValuePair<'Key,'T>) = a.Key.GetHashCode () })
          .ToDictionary((fun x -> x.Key), (fun y -> y.Value)) :> IDictionary<'Key, 'T>

    /// Returns the intersection of two Dicts, using the combiner function for duplicate keys.
    let intersectWith combiner (source1: IDictionary<'Key, 'T>) (source2: IDictionary<'Key, 'T>) =
        Enumerable
          .Join(
            source1, 
            source2, 
            (fun (x:KeyValuePair<'Key, 'T>) -> x.Key), 
            (fun (y:KeyValuePair<'Key, 'T>) -> y.Key), 
            (fun (x:KeyValuePair<'Key, 'T>) (y:KeyValuePair<'Key, 'T>) -> 
              KeyValuePair<'Key, 'T>(x.Key, combiner (x.Value) (y.Value))))
          .ToDictionary((fun x -> x.Key), (fun y -> y.Value)) :> IDictionary<'Key, 'T>

   ///Returns the intersection of two Dicts, preferring values from the first in case of duplicate keys.
    let intersect (source1: IDictionary<'Key, 'T>) (source2: IDictionary<'Key, 'T>) = 
        intersectWith (fun a _ -> a) source1 source2
    #endif
    
    /// <summary>Choose with access to the key.</summary>
    /// <param name="chooser">The mapping function, taking key and element as parameters.</param>
    /// <param name="source">The input dictionary.</param>
    ///
    /// <returns>Dictionary with values (k, x) for each dictionary value where the function returns Some(x).</returns>
    let choosei chooser (source: IDictionary<'Key, 'T>) =
        let dct = Dictionary<'Key, 'U> ()
        for KeyValue(k, v) in source do
            match chooser k v with
            | Some v -> dct.Add (k, v)
            | None   -> ()
        dct :> IDictionary<'Key, 'U>

    /// <summary>Creates a conceptually infinite dictionay containing the same value for all possible keys.</summary>
    /// <param name="source">The value for all possible keys.</param>
    let initInfinite<'TKey,'TValue> (source: 'TValue) : IDictionary<'TKey,'TValue> =

        let icollection value =
            {
                new  ICollection<'t> with
                    member __.Contains (item: 't) = obj.ReferenceEquals (item, value)
                    member __.GetEnumerator () = (Seq.initInfinite (fun _ -> value)).GetEnumerator () :> System.Collections.IEnumerator
                    member __.GetEnumerator () = (Seq.initInfinite (fun _ -> value)).GetEnumerator () : IEnumerator<'t>
                    member __.IsReadOnly = true

                    member __.Add (_item: 't) : unit                          = raise (System.NotImplementedException())
                    member __.Clear () : unit                                 = raise (System.NotImplementedException())
                    member __.CopyTo (_array: 't [], _arrayIndex: int) : unit = raise (System.NotImplementedException())
                    member __.Count : int                                     = raise (System.NotImplementedException())
                    member __.Remove (_item: 't): bool                        = raise (System.NotImplementedException())
            }

        {
            new IDictionary<'TKey,'TValue> with
                member __.TryGetValue (_key: 'TKey, value: byref<'TValue>) = value <- source; true
                member __.Count = System.Int32.MaxValue
                member __.ContainsKey (_key: 'TKey) = true
                member __.Contains (item: KeyValuePair<'TKey,'TValue>) = obj.ReferenceEquals (item.Value, source)
                member __.GetEnumerator () = invalidOp "Key set is potentially infinite." : System.Collections.IEnumerator
                member __.GetEnumerator () = invalidOp "Key set is potentially infinite." : IEnumerator<KeyValuePair<'TKey,'TValue>>
                member __.IsReadOnly = true
                member __.Values = icollection source
                member __.Item
                    with get (_key: 'TKey)   : 'TValue = source
                    and set  (_key: 'TKey) (_: 'TValue) : unit = raise (System.NotImplementedException())

                member __.Add (_key: 'TKey, _value: 'TValue) : unit                              = raise (System.NotImplementedException())
                member __.Add (_item: KeyValuePair<'TKey,'TValue>) : unit                        = raise (System.NotImplementedException())
                member __.Clear () : unit                                                        = raise (System.NotImplementedException())
                member __.CopyTo (_arr: KeyValuePair<'TKey,'TValue> [], _arrayIndex: int) : unit = raise (System.NotImplementedException())
                member __.Keys : ICollection<'TKey>                                              = raise (System.NotImplementedException())
                member __.Remove (_key: 'TKey) : bool                                            = raise (System.NotImplementedException())
                member __.Remove (_item: KeyValuePair<'TKey,'TValue>) : bool                     = raise (System.NotImplementedException())
            }
