namespace FSharpPlus

[<AutoOpen>]
module Auto =
    open System
    open System.Collections
    open System.Collections.Generic

    let icollection (konst: 'TValue) (source: IDictionary<'TKey,'TValue>) =
        {
            new  ICollection<'TValue> with
                member _.Contains item = source.Values.Contains item || obj.ReferenceEquals (item, konst)
                member _.GetEnumerator () = source.Values.GetEnumerator () :> System.Collections.IEnumerator
                member _.GetEnumerator () = source.Values.GetEnumerator () : IEnumerator<'TValue>
                member _.IsReadOnly = true
                member _.Add (_item: 'TValue) : unit                          = raise (NotImplementedException ())
                member _.Clear () : unit                                      = raise (NotImplementedException ())
                member _.CopyTo (_array: 'TValue [], _arrayIndex: int) : unit = raise (NotImplementedException ())
                member _.Count : int                                          = source.Count
                member _.Remove (_item: 'TValue): bool                        = raise (NotImplementedException ())
        }

    type DefaultableDict<'TKey, 'TValue> (konst: 'TValue, source: IDictionary<'TKey,'TValue>) =

        interface IDictionary<'TKey, 'TValue> with
            member _.TryGetValue (key: 'TKey, value: byref<'TValue>) =
                match source.TryGetValue key with
                | true, v  -> value <- v
                | _        -> value <- konst
                true
            member _.Count = source.Count
                // if typeof<'TKey>.IsValueType then                
                //     if typeof<'TKey> = typeof<bool> then 2
                //     else
                //         let s = sizeof<'TKey>
                //         if s < 4 then pown 2 (sizeof<'TKey> * 8)
                //         else -1 // infinity
                // elif typeof<'TKey> = typeof<unit> then 1
                // else -1
            member _.ContainsKey (_key: 'TKey) = true
            member _.Contains (item: KeyValuePair<'TKey,'TValue>) =
                match source.TryGetValue item.Key with
                | true, v -> obj.ReferenceEquals (item.Value, v)
                | _       -> obj.ReferenceEquals (item.Value, konst)
            member _.GetEnumerator () = source.GetEnumerator () : System.Collections.IEnumerator
            member _.GetEnumerator () = source.GetEnumerator () : IEnumerator<KeyValuePair<'TKey,'TValue>>
            member _.IsReadOnly = true
            member _.Values = icollection konst source
            member _.Item
                with get (key: 'TKey)   : 'TValue = match source.TryGetValue key with (true, v) -> v | _ -> konst
                and set  (_key: 'TKey) (_: 'TValue) : unit = raise (System.NotImplementedException())

            member _.Add (_key: 'TKey, _value: 'TValue) : unit                              = raise (NotImplementedException ())
            member _.Add (_item: KeyValuePair<'TKey,'TValue>) : unit                        = raise (NotImplementedException ())
            member _.Clear () : unit                                                        = raise (NotImplementedException ())
            member _.CopyTo (_arr: KeyValuePair<'TKey,'TValue> [], _arrayIndex: int) : unit = raise (NotImplementedException ())
            member _.Keys : ICollection<'TKey>                                              = raise (NotImplementedException ())
            member _.Remove (_key: 'TKey) : bool                                            = raise (NotImplementedException ())
            member _.Remove (_item: KeyValuePair<'TKey,'TValue>) : bool                     = raise (NotImplementedException ())
    
        member _.DefaultValue = konst

/// Additional operations on IDictionary<'Key, 'Value>
[<RequireQualifiedAccess>]
module Dict =
    open System.Collections.Generic
    open System.Collections.ObjectModel
    open Auto

    /// <summary>Creates a conceptually infinite dictionary containing the same value for all possible keys.</summary>
    /// <param name="source">The value for all possible keys.</param>
    let initInfinite<'TKey,'TValue when 'TKey : equality> (source: 'TValue) : IDictionary<'TKey,'TValue> = new DefaultableDict<'TKey,'TValue>(source, Dictionary<'TKey,'TValue> ())
    let initHybrid<'TKey,'TValue> (konst: 'TValue) (source: IDictionary<'TKey,'TValue>) : IDictionary<'TKey,'TValue> = new DefaultableDict<'TKey,'TValue>(konst, source)

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

    /// <summary>Applies the given function to each key and value pair of the dictionary.</summary>
    /// <param name="action">The function to apply to each key and value pair of the input dictionary.</param>
    /// <param name="source">The input dictionary.</param>
    let iter action (source: IDictionary<'Key, 'T>) = for KeyValue(k, v) in source do action k v

    /// <summary>Applies the given function to each value of the dictionary.</summary>
    /// <param name="action">The function to apply to each value of the input dictionary.</param>
    /// <param name="source">The input dictionary.</param>
    let iterValues action (source: IDictionary<'Key, 'T>) = for KeyValue(_, v) in source do action v

    /// <summary>Maps the given function over each value in the dictionary.</summary>
    /// <param name="mapper">The mapping function.</param>
    /// <param name="source">The input dictionary.</param>
    ///
    /// <returns>The mapped dictionary.</returns>
    let map mapper (source: IDictionary<'Key, 'T>) =
        match source with
        | :? DefaultableDict<'Key, 'T> as s ->
            let dct = DefaultableDict<'Key, 'U> (mapper s.DefaultValue, Dictionary<'Key, 'U> ())
            let dct = dct :> IDictionary<'Key, 'U>
            for KeyValue(k, v) in source do
                dct.Add (k, mapper v)
            dct
        | _ ->
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
        let map k1 k2 =
            let dct = Dictionary<'Key, 'U> ()
            let f = OptimizedClosures.FSharpFunc<_, _, _>.Adapt mapper
            for k in set source1.Keys + set source2.Keys do
                match tryGetValue k source1, tryGetValue k source2, k1, k2 with
                | Some vx, Some vy, _      , _
                | None   , Some vy, Some vx, _ 
                | Some vx, None   , _      , Some vy -> dct.Add (k, f.Invoke (vx, vy))
                | _      , _      , _      , _       -> ()
            dct :> IDictionary<'Key, 'U>
        match source1, source2 with
        | (:? DefaultableDict<'Key,'T1> as s1), (:? DefaultableDict<'Key,'T2> as s2) -> initHybrid (mapper s1.DefaultValue s2.DefaultValue) (map (Some s1.DefaultValue) (Some s2.DefaultValue))
        | (:? DefaultableDict<'Key,'T1> as s1), _ -> map (Some s1.DefaultValue) None
        | _, (:? DefaultableDict<'Key,'T2> as s2) -> map None (Some s2.DefaultValue)
        | _, _  -> map None None

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
    let zip (source1: IDictionary<'Key, 'T1>) (source2: IDictionary<'Key, 'T2>) = map2 (fun x y -> (x, y)) source1 source2

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
        let combine () =
            let d = Dictionary<'Key,'Value> ()
            let f = OptimizedClosures.FSharpFunc<_,_,_>.Adapt combiner
            for KeyValue(k, v ) in source1 do d.[k] <- v
            for KeyValue(k, v') in source2 do d.[k] <- match d.TryGetValue k with true, v -> f.Invoke (v, v') | _ -> v'
            d :> IDictionary<'Key,'Value>
        // let combineWithKonst source konst =
        //     let d = Dictionary<'Key,'Value> ()
        //     for KeyValue(k, v) in source do d.[k] <- combiner v konst
        //     d :> IDictionary<'Key,'Value>
        // let combineKonstWith konst source =
        //     let d = Dictionary<'Key,'Value> ()
        //     for KeyValue(k, v) in source do d.[k] <- combiner konst v
        //     d :> IDictionary<'Key,'Value>
        match source1, source2 with
        | (:? DefaultableDict<'Key,'Value> as s1), (:? DefaultableDict<'Key,'Value> as s2) -> initHybrid (combiner s1.DefaultValue s2.DefaultValue) (combine())
        | (:? DefaultableDict<'Key,'Value> as s), _ | _, (:? DefaultableDict<'Key,'Value> as s)  -> initHybrid s.DefaultValue (combine())
        | s, empty | empty, s when empty.Count = 0 -> s
        | _, _  -> combine()
            

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
