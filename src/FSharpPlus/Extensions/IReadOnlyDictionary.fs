namespace FSharpPlus

/// Additional operations on IReadOnlyDictionary<'Key, 'Value>
[<RequireQualifiedAccess>]
module IReadOnlyDictionary =

    #if !FABLE_COMPILER
    open System.Linq
    #endif

    open System.Collections.Generic

    /// Replaces or sets the item associated with a specified key with the specified value.
    let add key value (table: IReadOnlyDictionary<'Key, 'Value>) = table |> Seq.map (|KeyValue|) |> Map |> Map.add key value :> IReadOnlyDictionary<_,_>

    let remove key (table: IReadOnlyDictionary<'Key, 'Value>) = table |> Seq.filter (fun t -> t.Key <> key) |> Seq.map (|KeyValue|) |> Map :> IReadOnlyDictionary<_,_>

    /// Gets the value associated with the specified key. Returns None if a value associated with the key is not found.
    /// 
    /// <summary>Try and get the value of the given key</summary>
    /// <remarks>This is a function wrapper for the IReadOnlyDictionary.TryGetValue method,
    /// representing the result as an Option&lt;value&gt; instead of a bool plus an out-value.
    /// </remarks>
    /// <param name="k">The key whose value you wish to find.</param>
    /// <param name="dct">The input IReadOnlyDictionary.</param>
    ///
    /// <returns>An option wrapped value</returns>
    let tryGetValue k (dct: IReadOnlyDictionary<'Key, 'Value>) =
        match dct.TryGetValue k with
        | true, v -> Some v
        | _       -> None

    /// <summary>Does the read-only dictionary contain the given key?</summary>
    /// <remarks>Note: this is a function wrapper for the IReadOnlyDictionary.ContainsKey method</remarks>
    /// <param name="k">The key to find.</param>
    /// <param name="dct">The input IReadOnlyDictionary.</param>
    ///
    /// <returns>A bool indicating if the key was found</returns>
    let containsKey k (dct: IReadOnlyDictionary<'Key, 'Value>) = dct.ContainsKey k

    /// <summary>Return the keys of the given read-only dictionary.</summary>
    /// <param name="source">The input IReadOnlyDictionary.</param>
    ///
    /// <returns>A seq of the keys in the IReadOnlyDictionary.</returns>
    let keys   (source: IReadOnlyDictionary<'Key, 'Value>) = Seq.map (fun (KeyValue(k, _)) -> k) source

    /// <summary>Return the values of the given read-only dictionary.</summary>
    /// <param name="source">The input IReadOnlyDictionary.</param>
    ///
    /// <returns>A seq of the values in the read-only dictionary.</returns>
    let values (source: IReadOnlyDictionary<'Key, 'Value>) = Seq.map (fun (KeyValue(_, v)) -> v) source

    /// <summary>Map the given function over each value in the read-only dictionary</summary>
    /// <param name="f">The mapping function.</param>
    /// <param name="x">The input IReadOnlyDictionary.</param>
    ///
    /// <returns>The mapped IReadOnlyDictionary.</returns>
    let map f (x: IReadOnlyDictionary<'Key, 'T>) =
        let dct = Dictionary<'Key, 'U> ()
        for KeyValue(k, v) in x do
            dct.Add (k, f v)
        dct :> IReadOnlyDictionary<'Key, 'U>

    /// <summary>Creates a read-only dictionary value from a pair of read-only dictionaries,
    /// using a function to combine them.</summary>
    /// <remarks>Keys that are not present on both read-only dictionaries are dropped.</remarks>
    /// <param name="f">The mapping function.</param>
    /// <param name="x">The first input IReadOnlyDictionary.</param>
    /// <param name="y">The second input IReadOnlyDictionary.</param>
    ///
    /// <returns>The combined IReadOnlyDictionary.</returns>
    let map2 f (x: IReadOnlyDictionary<'Key, 'T1>) (y: IReadOnlyDictionary<'Key, 'T2>) =
        let dct = Dictionary<'Key, 'U> ()
        let f = OptimizedClosures.FSharpFunc<_,_,_>.Adapt f
        for KeyValue(k, vx) in x do
            match tryGetValue k y with
            | Some vy -> dct.Add (k, f.Invoke (vx, vy))
            | None    -> ()
        dct :> IReadOnlyDictionary<'Key, 'U>

    /// <summary>Tuple values of two read-only dictionaries.</summary>
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

    /// <summary>Split a read-only dictionary with tuple pair values to two separate read-only dictionaries.</summary>
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
                      member __.Equals ((a:KeyValuePair<'Key,'T>),(b:KeyValuePair<'Key,'T>)) : bool = a.Key = b.Key
                      member __.GetHashCode (a:KeyValuePair<'Key,'T>) = a.Key.GetHashCode () })
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