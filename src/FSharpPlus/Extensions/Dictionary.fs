namespace FSharpPlus

/// Additional operations on Dictionary<'Key, 'Value>
[<RequireQualifiedAccess>]
module Dictionary =
    open System.Collections.Generic
    open System.Collections.ObjectModel

    #if !FABLE_COMPILER
    open System.Linq
    let toIReadOnlyDictionary (source: Dictionary<'Key, 'Value>) = ReadOnlyDictionary source :> IReadOnlyDictionary<_,_>
    #endif

    let tryGetValue k (dct: Dictionary<'Key, 'Value>) =
        match dct.TryGetValue k with
        | true, v -> Some v
        | _       -> None
    let containsKey k (dct: Dictionary<'Key, 'Value>) = dct.ContainsKey k

    let keys   (source: Dictionary<_,_>) = Seq.map (fun (KeyValue(k, _)) -> k) source
    let values (source: Dictionary<_,_>) = Seq.map (fun (KeyValue(_, v)) -> v) source

    let map f (x: Dictionary<'Key, 'T>) =
        let dct = Dictionary<'Key, 'U> ()
        for KeyValue(k, v) in x do
            dct.Add (k, f v)
        dct

    /// <summary>Creates a Dictionary value from a pair of Dictionaries, using a function to combine them.</summary>
    /// <remarks>Keys that are not present on both dictionaries are dropped.</remarks>
    /// <param name="x">The first input dictionary.</param>
    /// <param name="y">The second input dictionary.</param>
    ///
    /// <returns>The combined dictionary.</returns>
    let map2 f (x: Dictionary<'Key, 'T1>) (y: Dictionary<'Key, 'T2>) =
        let dct = Dictionary<'Key, 'U> ()
        let f = OptimizedClosures.FSharpFunc<_,_,_>.Adapt f
        for KeyValue(k, vx) in x do
            match tryGetValue k y with
            | Some vy -> dct.Add (k, f.Invoke (vx, vy))
            | None    -> ()
        dct

    /// <summary>Tuple values of two dictionaries.</summary>
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
    // Returns the union of two maps, preferring values from the first in case of duplicate keys.
    let union (source: Dictionary<'Key, 'T>) (altSource: Dictionary<'Key, 'T>) =
        Enumerable
          .Union(
            source,
            altSource,
            { new IEqualityComparer<KeyValuePair<'Key,'T>> with
                      member __.Equals ((a:KeyValuePair<'Key,'T>),(b:KeyValuePair<'Key,'T>)) : bool = a.Key = b.Key
                      member __.GetHashCode (a:KeyValuePair<'Key,'T>) = a.Key.GetHashCode () })
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

    // Returns the intersection of two maps, preferring values from the first in case of duplicate keys.
    let intersect (source1: Dictionary<'Key, 'T>) (source2: Dictionary<'Key, 'T>) =
        intersectWith (fun a _ -> a) source1 source2
    #endif