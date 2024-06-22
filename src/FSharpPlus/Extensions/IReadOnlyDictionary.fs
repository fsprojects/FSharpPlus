namespace FSharpPlus

/// Additional operations on IReadOnlyDictionary<'Key, 'Value>
[<RequireQualifiedAccess>]
module IReadOnlyDictionary =

    #if !FABLE_COMPILER
    open System.Linq
    #endif

    open System.Collections.Generic

    /// Replaces or sets the item associated with a specified key with the specified value.
    let add key value (source: IReadOnlyDictionary<'Key, 'Value>) = source |> Seq.map (|KeyValue|) |> Map |> Map.add key value :> IReadOnlyDictionary<_,_>

    /// Removes the given key from the read-only dictionary.
    let remove key (source: IReadOnlyDictionary<'Key, 'Value>) = source |> Seq.filter (fun t -> t.Key <> key) |> Seq.map (|KeyValue|) |> Map :> IReadOnlyDictionary<_,_>

    /// <summary>Tries to get the value of the given key.</summary>
    /// <remarks>This is a function wrapper for the IReadOnlyDictionary.TryGetValue method,
    /// representing the result as an Option&lt;value&gt; instead of a bool plus an out-value.
    /// </remarks>
    /// <param name="key">The key whose value you wish to find.</param>
    /// <param name="source">The input IReadOnlyDictionary.</param>
    ///
    /// <returns>An option wrapped value.</returns>
    let tryGetValue key (source: IReadOnlyDictionary<'Key, 'Value>) =
        match source.TryGetValue key with
        | true, v -> Some v
        | _       -> None

    /// <summary>Does the read-only dictionary contain the given key?</summary>
    /// <remarks>Note: this is a function wrapper for the IReadOnlyDictionary.ContainsKey method.</remarks>
    /// <param name="key">The key to find.</param>
    /// <param name="source">The input IReadOnlyDictionary.</param>
    ///
    /// <returns>A bool indicating if the key was found.</returns>
    let containsKey key (source: IReadOnlyDictionary<'Key, 'Value>) = source.ContainsKey key

    /// <summary>Returns the keys of the given read-only dictionary.</summary>
    /// <param name="source">The input IReadOnlyDictionary.</param>
    ///
    /// <returns>A seq of the keys in the IReadOnlyDictionary.</returns>
    let keys   (source: IReadOnlyDictionary<'Key, 'Value>) = Seq.map (fun (KeyValue(k, _)) -> k) source

    /// <summary>Returns the values of the given read-only dictionary.</summary>
    /// <param name="source">The input IReadOnlyDictionary.</param>
    ///
    /// <returns>A seq of the values in the read-only dictionary.</returns>
    let values (source: IReadOnlyDictionary<'Key, 'Value>) = Seq.map (fun (KeyValue(_, v)) -> v) source

    /// <summary>Maps the given function over each value in the read-only dictionary.</summary>
    /// <param name="mapper">The mapping function.</param>
    /// <param name="source">The input IReadOnlyDictionary.</param>
    ///
    /// <returns>The mapped IReadOnlyDictionary.</returns>
    let mapValues mapper (source: IReadOnlyDictionary<'Key, 'T>) =
        let dct = Dictionary<'Key, 'U> ()
        for KeyValue(k, v) in source do
            dct.Add (k, mapper v)
        dct :> IReadOnlyDictionary<'Key, 'U>

    [<System.Obsolete("Name is a bit ambiguous, use mapValues if the intention is to map only over the values or mapi to map over both keys and values.")>]
    let map f (x: IReadOnlyDictionary<'Key, 'T>) = mapValues f x // F#+ 2: if following F# core naming, it should point to mapi instead.

    /// <summary>Creates a read-only dictionary value from a pair of read-only dictionaries,
    /// using a function to combine them.</summary>
    /// <remarks>Keys that are not present on both read-only dictionaries are dropped.</remarks>
    /// <param name="mapper">The mapping function.</param>
    /// <param name="source1">The first input IReadOnlyDictionary.</param>
    /// <param name="source2">The second input IReadOnlyDictionary.</param>
    ///
    /// <returns>The combined IReadOnlyDictionary.</returns>
    let map2 mapper (source1: IReadOnlyDictionary<'Key, 'T1>) (source2: IReadOnlyDictionary<'Key, 'T2>) =
        let dct = Dictionary<'Key, 'U> ()
        let f = OptimizedClosures.FSharpFunc<_,_,_>.Adapt mapper
        for KeyValue(k, vx) in source1 do
            match tryGetValue k source2 with
            | Some vy -> dct.Add (k, f.Invoke (vx, vy))
            | None    -> ()
        dct :> IReadOnlyDictionary<'Key, 'U>

    /// <summary>Combines values from three read-only dictionaries using mapping function.</summary>
    /// <remarks>Keys that are not present on every dictionary are dropped.</remarks>
    /// <param name="mapping">The mapping function.</param>
    /// <param name="source1">First input dictionary.</param>
    /// <param name="source2">Second input dictionary.</param>
    /// <param name="source3">Third input dictionary.</param>
    ///
    /// <returns>The mapped IReadOnlyDictionary.</returns>
    let map3 mapping (source1: IReadOnlyDictionary<'Key, 'T1>) (source2: IReadOnlyDictionary<'Key, 'T2>) (source3: IReadOnlyDictionary<'Key, 'T3>) =
        let dct = Dictionary<'Key, 'U> ()
        let f = OptimizedClosures.FSharpFunc<_,_,_,_>.Adapt mapping
        for KeyValue(k, vx) in source1 do
            match tryGetValue k source2, tryGetValue k source3 with
            | Some vy, Some vz -> dct.Add (k, f.Invoke (vx, vy, vz))
            | _      , _       -> ()
        dct :> IReadOnlyDictionary<'Key, 'U>


    /// <summary>Maps the given function over each key and value in the read-only dictionary.</summary>
    /// <param name="mapper">The mapping function.</param>
    /// <param name="source">The input IReadOnlyDictionary.</param>
    ///
    /// <returns>The mapped IReadOnlyDictionary.</returns>
    let mapi mapper (source: IReadOnlyDictionary<'Key, 'T>) =
        let dct = Dictionary<'Key, 'U> ()
        for KeyValue(k, v) in source do
            dct.Add (k, mapper k v)
        dct :> IReadOnlyDictionary<'Key, 'U>

    /// <summary>Applies the given action over each key and value in the read-only dictionary.</summary>
    /// <param name="action">The action to apply.</param>
    /// <param name="source">The input IReadOnlyDictionary.</param>
    ///
    /// <returns>The mapped IReadOnlyDictionary.</returns>
    let iter action (source: IReadOnlyDictionary<'Key, 'T>) = for KeyValue(k, v) in source do action k v


    /// <summary>Applies a function to each value in a read-only dictionary and then returns
    /// a read-only dictionary of entries <c>v</c> where the applied function returned <c>Some(v)</c>.
    /// 
    /// Returns an empty read-only dictionary when the input read-only dictionary is empty or when the applied chooser function
    /// returns <c>None</c> for all elements.
    /// </summary>
    ///
    /// <param name="chooser">The function to be applied to the read-only dictionary values.</param>
    /// <param name="source">The input read-only dictionary.</param>
    ///
    /// <returns>The resulting read-only dictionary comprising the entries <c>v</c> where the chooser function returned <c>Some(x)</c>.</returns>
    let chooseValues chooser (source: IReadOnlyDictionary<'Key, 'T>) =
        let dct = Dictionary<'Key, 'U> ()
        for KeyValue(k, v) in source do
            match chooser v with
            | Some v -> dct.Add (k, v)
            | None    -> ()
        dct :> IReadOnlyDictionary<'Key, 'U>

    /// <summary>Applies a function to each key and value in a read-only dictionary and then returns
    /// a read-only dictionary of entries <c>v</c> where the applied function returned <c>Some(v)</c>.
    /// 
    /// Returns an empty read-only dictionary when the input read-only dictionary is empty or when the applied chooser function
    /// returns <c>None</c> for all elements.
    /// </summary>
    ///
    /// <param name="chooser">The function to be applied to the read-only dictionary values.</param>
    /// <param name="source">The input read-only dictionary.</param>
    ///
    /// <returns>The resulting read-only dictionary comprising the entries <c>v</c> where the chooser function returned <c>Some(x)</c>.</returns>
    let choose chooser (source: IReadOnlyDictionary<'Key, 'T>) =
        let dct = Dictionary<'Key, 'U> ()
        for KeyValue(k, v) in source do
            match chooser k v with
            | Some v -> dct.Add (k, v)
            | None    -> ()
        dct :> IReadOnlyDictionary<'Key, 'U>
        
    /// <summary>Tuples values of two read-only dictionaries.</summary>
    /// <remarks>Keys that are not present on both read-only dictionaries are dropped.</remarks>
    /// <param name="x">The first input IReadOnlyDictionary.</param>
    /// <param name="y">The second input IReadOnlyDictionary.</param>
    ///
    /// <returns>The tupled IReadOnlyDictionary.</returns>
    let zip (x: IReadOnlyDictionary<'Key, 'T1>) (y: IReadOnlyDictionary<'Key, 'T2>) =
        let dct = Dictionary<'Key, 'T1 * 'T2> ()
        for KeyValue(k, vx) in x do
            match tryGetValue k y with
            | Some vy -> dct.Add (k, (vx, vy))
            | None    -> ()
        dct :> IReadOnlyDictionary<'Key, 'T1 * 'T2>

    /// <summary>Splits a read-only dictionary with tuple pair values to two separate read-only dictionaries.</summary>
    /// <param name="source">The source IReadOnlyDictionary.</param>
    ///
    /// <returns>A tuple of each untupled IReadOnlyDictionary.</returns>
    let unzip (source: IReadOnlyDictionary<'Key, 'T1 * 'T2>) =
        let dct1 = Dictionary<'Key, 'T1> ()
        let dct2 = Dictionary<'Key, 'T2> ()
        for KeyValue(k, (vx, vy)) in source do
            dct1.Add (k, vx)
            dct2.Add (k, vy)
        dct1 :> IReadOnlyDictionary<'Key, 'T1>, dct2 :> IReadOnlyDictionary<'Key, 'T2>

    /// Returns the union of two read-only dictionaries, using the combiner function for duplicate keys.
    let unionWith combiner (source1: IReadOnlyDictionary<'Key, 'Value>) (source2: IReadOnlyDictionary<'Key, 'Value>) =
        let d = Dictionary<'Key,'Value> ()
        let f = OptimizedClosures.FSharpFunc<_,_,_>.Adapt combiner
        for KeyValue(k, v ) in source1 do d.[k] <- v
        for KeyValue(k, v') in source2 do d.[k] <- match d.TryGetValue k with true, v -> f.Invoke (v, v') | _ -> v'
        d :> IReadOnlyDictionary<'Key,'Value>

    #if !FABLE_COMPILER

    /// Returns the union of two read-only dictionaries, preferring values from the first in case of duplicate keys.
    let union (source: IReadOnlyDictionary<'Key, 'T>) (altSource: IReadOnlyDictionary<'Key, 'T>) =
        Enumerable
          .Union(
            source,
            altSource,
            { new IEqualityComparer<KeyValuePair<'Key,'T>> with
                      member _.Equals ((a:KeyValuePair<'Key,'T>),(b:KeyValuePair<'Key,'T>)) : bool = a.Key = b.Key
                      member _.GetHashCode (a:KeyValuePair<'Key,'T>) = a.Key.GetHashCode () })
          .ToDictionary((fun x -> x.Key), (fun y -> y.Value)) :> IReadOnlyDictionary<'Key, 'T>

    /// Returns the intersection of two read-only dictionaries, using the combiner function for duplicate keys.
    let intersectWith combiner (source1:IReadOnlyDictionary<'Key, 'T>) (source2:IReadOnlyDictionary<'Key, 'T>) =
        Enumerable
            .Join(
            source1,
            source2,
            (fun (x:KeyValuePair<'Key, 'T>) -> x.Key),
            (fun (y:KeyValuePair<'Key, 'T>) -> y.Key),
            (fun (x:KeyValuePair<'Key, 'T>) (y:KeyValuePair<'Key, 'T>) ->
                KeyValuePair<'Key, 'T>(x.Key, combiner (x.Value) (y.Value))))
            .ToDictionary((fun x -> x.Key), (fun y -> y.Value)) :> IReadOnlyDictionary<'Key, 'T>

    /// Returns the intersection of two read-only dictionaries, preferring values from the first in case of duplicate keys.
    let intersect (source1:IReadOnlyDictionary<'Key, 'T>) (source2:IReadOnlyDictionary<'Key, 'T>) =
        intersectWith (fun a _ -> a) source1 source2

    #endif

    let empty<'Key, 'U when 'Key : equality> = Dictionary<'Key, 'U> () :> IReadOnlyDictionary<_,_>

    /// <summary>Converts a read-only dictionary to a ResizeArray.</summary>
    /// <param name="source">The source IReadOnlyDictionary.</param>
    ///
    /// <returns>A ResizeArray containing the Key and Value of the original IReadOnlyDictionary.</returns>
    let toResizeArray (source: IReadOnlyDictionary<'Key, 'T>) =
        let arr = ResizeArray<KeyValuePair<'Key, 'T>> ()
        for KeyValue(k, x) in source do
            arr.Add (KeyValuePair (k, x))
        arr

    /// <summary>Converts a read-only dictionary to a sequence.</summary>
    /// <param name="source">The source IReadOnlyDictionary.</param>
    ///
    /// <returns>A sequence containing the Key and Value of the original IReadOnlyDictionary.</returns>
    let toSeq (source: IReadOnlyDictionary<'Key, 'T>) = toResizeArray source :> seq<_>

    /// Folds over the bindings in the Dictionary
    let fold     (folder: 'State -> 'Key -> 'T -> 'State) (state: 'State) (source: IReadOnlyDictionary<'Key, 'T>) =
        let unzip source = Seq.map fst source, Seq.map snd source
        source |> toSeq |> Seq.map (|KeyValue|) |> unzip ||> Seq.fold2 folder state

    /// Folds over the bindings in the Dictionary
    let foldBack (folder: 'Key -> 'T -> 'State -> 'State) (source: IReadOnlyDictionary<'Key, 'T>) state =
        let unzip source = Seq.map fst source, Seq.map snd source
        source |> toSeq |> Seq.map (|KeyValue|) |> unzip ||> Seq.foldBack2 folder <| state
