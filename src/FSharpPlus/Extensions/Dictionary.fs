﻿namespace FSharpPlus

/// Additional operations on Dictionary<'Key, 'Value>
[<RequireQualifiedAccess>]
module Dictionary =
    open System.Collections.Generic
    open System.Collections.ObjectModel

    #if !FABLE_COMPILER
    open System.Linq

    /// Converts a Dictionary to an IReadOnlyDictionary
    let toIReadOnlyDictionary (source: Dictionary<'Key, 'Value>) = ReadOnlyDictionary source :> IReadOnlyDictionary<_,_>
    
    #endif

    /// <summary>Tries to get the value of the given key.</summary>
    /// <remarks>Note: this is a function wrapper for the Dictionary.TryGetValue method,
    /// which also represents the result as an Option&lt;value&gt; instead of a bool
    /// and an out-value.
    /// </remarks>
    /// <param name="k">The key to find.</param>
    /// <param name="dct">The input dictionary.</param>
    ///
    /// <returns>An option wrapped value</returns>
    let tryGetValue k (dct: Dictionary<'Key, 'Value>) =
        match dct.TryGetValue k with
        | true, v -> Some v
        | _       -> None

    /// <summary>Does the dictionary contain the given key?</summary>
    /// <remarks>Note: this is a function wrapper for the Dictionary.ContainsKey method.</remarks>
    /// <param name="k">The key to find.</param>
    /// <param name="dct">The input dictionary.</param>
    ///
    /// <returns>A bool indicating if the key was found</returns>
    let containsKey k (dct: Dictionary<'Key, 'Value>) = dct.ContainsKey k

    /// <summary>Returns the keys of the given dictionary.</summary>
    /// <param name="source">The input dictionary.</param>
    ///
    /// <returns>A seq of the keys in the dictionary.</returns>
    let keys   (source: Dictionary<_,_>) = Seq.map (fun (KeyValue(k, _)) -> k) source

    /// <summary>Returns the values of the given dictionary.</summary>
    /// <param name="source">The input dictionary.</param>
    ///
    /// <returns>A seq of the values in the dictionary.</returns>
    let values (source: Dictionary<_,_>) = Seq.map (fun (KeyValue(_, v)) -> v) source

    /// <summary>Maps the given function over each value in the dictionary.</summary>
    /// <param name="mapping">The mapping function.</param>
    /// <param name="x">The input dictionary.</param>
    ///
    /// <returns>The mapped dictionary.</returns>
    let map mapping (x: Dictionary<'Key, 'T>) =
        let dct = Dictionary<'Key, 'U> ()
        for KeyValue(k, v) in x do
            dct.Add (k, mapping v)
        dct

    /// <summary>Creates a Dictionary value from a pair of Dictionaries, using a function to combine them.</summary>
    /// <remarks>Keys that are not present on both dictionaries are dropped.</remarks>
    /// <param name="mapping">The mapping function.</param>
    /// <param name="x">The first input dictionary.</param>
    /// <param name="y">The second input dictionary.</param>
    ///
    /// <returns>The combined dictionary.</returns>
    let map2 mapping (x: Dictionary<'Key, 'T1>) (y: Dictionary<'Key, 'T2>) =
        let dct = Dictionary<'Key, 'U> ()
        let f = OptimizedClosures.FSharpFunc<_,_,_>.Adapt mapping
        for KeyValue(k, vx) in x do
            match tryGetValue k y with
            | Some vy -> dct.Add (k, f.Invoke (vx, vy))
            | None    -> ()
        dct
        
    /// <summary>Combines values from three Dictionaries using mapping function.</summary>
    /// <remarks>Keys that are not present on every Dictionary are dropped.</remarks>
    /// <param name="mapping">The mapping function.</param>
    /// <param name="x">First input Dictionary.</param>
    /// <param name="y">Second input Dictionary.</param>
    /// <param name="z">Third input Dictionary.</param>
    ///
    /// <returns>The mapped Dictionary.</returns>
    let map3 mapping (x: Dictionary<'Key, 'T1>) (y: Dictionary<'Key, 'T2>) (z: Dictionary<'Key, 'T3>) =
        let dct = Dictionary<'Key, 'U> ()
        let f = OptimizedClosures.FSharpFunc<_,_,_,_>.Adapt mapping
        for KeyValue(k, vx) in x do
            match tryGetValue k y, tryGetValue k z with
            | Some vy, Some vz -> dct.Add (k, f.Invoke (vx, vy, vz))
            | _      , _       -> ()
        dct

    /// <summary>Applies given function to each value of the given dictionary.</summary>
    /// <param name="f">The mapping function.</param>
    /// <param name="x">The input dictionary.</param>
    ///
    /// <returns>Returns dictionary with values x for each dictionary value where the function returns Some(x).</returns>
    let chooseValues f (x: IDictionary<'Key, 'T>) =
        let dct = Dictionary<'Key, 'U> ()
        for KeyValue(k, v) in x do
            match f v with
            | Some v -> dct.Add (k, v)
            | None    -> ()
        dct
        
    /// <summary>Tuples values of two dictionaries.</summary>
    /// <remarks>Keys that are not present on both dictionaries are dropped.</remarks>
    /// <param name="x">The first input dictionary.</param>
    /// <param name="y">The second input dictionary.</param>
    ///
    /// <returns>The tupled dictionary.</returns>
    let zip (x: Dictionary<'Key, 'T1>) (y: Dictionary<'Key, 'T2>) =
        let dct = Dictionary<'Key, 'T1 * 'T2> ()
        for KeyValue(k, vx) in x do
            match tryGetValue k y with
            | Some vy -> dct.Add (k, (vx, vy))
            | None    -> ()
        dct

    /// <summary>Splits a dictionary with tuple pair values to two separate dictionaries.</summary>
    /// <param name="source">The source dictionary.</param>
    ///
    /// <returns>A tuple of each untupled dictionary.</returns>
    let unzip (source: Dictionary<'Key, 'T1 * 'T2>) =
        let dct1 = Dictionary<'Key, 'T1> ()
        let dct2 = Dictionary<'Key, 'T2> ()
        for KeyValue(k, (vx, vy)) in source do
            dct1.Add (k, vx)
            dct2.Add (k, vy)
        dct1, dct2

    /// Returns the union of two dictionaries, using the combiner function for duplicate keys.
    let unionWith combiner (source1: Dictionary<'Key, 'Value>) (source2: Dictionary<'Key, 'Value>) =
        let d = Dictionary<'Key,'Value> ()
        let f = OptimizedClosures.FSharpFunc<_,_,_>.Adapt combiner
        for KeyValue(k, v ) in source1 do d.[k] <- v
        for KeyValue(k, v') in source2 do d.[k] <- match d.TryGetValue k with true, v -> f.Invoke (v, v') | _ -> v'
        d

    #if !FABLE_COMPILER
   ///Returns the union of two maps, preferring values from the first in case of duplicate keys.
    let union (source: Dictionary<'Key, 'T>) (altSource: Dictionary<'Key, 'T>) =
        Enumerable
          .Union(
            source,
            altSource,
            { new IEqualityComparer<KeyValuePair<'Key,'T>> with
                      member _.Equals ((a:KeyValuePair<'Key,'T>),(b:KeyValuePair<'Key,'T>)) : bool = a.Key = b.Key
                      member _.GetHashCode (a:KeyValuePair<'Key,'T>) = a.Key.GetHashCode () })
          .ToDictionary((fun x -> x.Key), (fun y -> y.Value))

    /// Returns the intersection of two Dicts, using the combiner function for duplicate keys.
    let intersectWith combiner (source1: Dictionary<'Key, 'T>) (source2: Dictionary<'Key, 'T>) =
        Enumerable
          .Join(
            source1,
            source2,
            (fun (x: KeyValuePair<'Key, 'T>) -> x.Key),
            (fun (y: KeyValuePair<'Key, 'T>) -> y.Key),
            (fun (x: KeyValuePair<'Key, 'T>) (y: KeyValuePair<'Key, 'T>) ->
              KeyValuePair<'Key, 'T>(x.Key, combiner (x.Value) (y.Value))))
          .ToDictionary((fun x -> x.Key), (fun y -> y.Value))

   ///Returns the intersection of two maps, preferring values from the first in case of duplicate keys.
    let intersect (source1: Dictionary<'Key, 'T>) (source2: Dictionary<'Key, 'T>) =
        intersectWith (fun a _ -> a) source1 source2
    #endif
    
    /// <summary>Same as chooseValues but with access to the key.</summary>
    /// <param name="f">The mapping function, taking key and element as parameters.</param>
    /// <param name="x">The input dictionary.</param>
    ///
    /// <returns>Dictionary with values (k, x) for each dictionary value where the function returns Some(x).</returns>
    let choosei f (x: IDictionary<'Key, 'T>) =
        let dct = Dictionary<'Key, 'U> ()
        for KeyValue(k, v) in x do
            match f k v with
            | Some v -> dct.Add (k, v)
            | None   -> ()
        dct
