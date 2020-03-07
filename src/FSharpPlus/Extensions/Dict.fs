namespace FSharpPlus

/// Additional operations on IDictionary<'Key, 'Value>
[<RequireQualifiedAccess>]
module Dict =
    open System.Collections.Generic
    open System.Collections.ObjectModel

    #if !FABLE_COMPILER
    open System.Linq
    let toIReadOnlyDictionary source = ReadOnlyDictionary source :> IReadOnlyDictionary<_,_>
    #endif

    let tryGetValue k (dct: IDictionary<'Key, 'Value>) =
        match dct.TryGetValue k with
        | true, v -> Some v
        | _       -> None
    let containsKey k (dct: IDictionary<'Key, 'Value>) = dct.ContainsKey k

    let keys   (source: IDictionary<_,_>) = Seq.map (fun (KeyValue(k, _)) -> k) source
    let values (source: IDictionary<_,_>) = Seq.map (fun (KeyValue(_, v)) -> v) source

    let map f (x: IDictionary<'Key, 'T>) =
        let dct = Dictionary<'Key, 'U> ()
        for KeyValue(k, v) in x do
            dct.Add (k, f v)
        dct :> IDictionary<'Key, 'U>

    let map2 f (x: IDictionary<'Key, 'T1>) (y: IDictionary<'Key, 'T2>) =
        let dct = Dictionary<'Key, 'U> ()
        let f = OptimizedClosures.FSharpFunc<_,_,_>.Adapt f
        for KeyValue(k, vx) in x do
            match tryGetValue k y with
            | Some vy -> dct.Add (k, f.Invoke (vx, vy))
            | None    -> ()
        dct :> IDictionary<'Key, 'U>

    /// <summary>Tuple values of two dictionaries.</summary>
    /// <remarks>Keys that are not present on both dictionaries are dropped.</remarks>
    /// <param name="x">The first input dictionary.</param>
    /// <param name="y">The second input dictionary.</param>
    ///
    /// <returns>The tupled dictionary.</returns>
    let zip (x: IDictionary<'Key, 'T1>) (y: IDictionary<'Key, 'T2>) =
        let dct = Dictionary<'Key, 'T1 * 'T2> ()
        for KeyValue(k, vx) in x do
            match tryGetValue k y with
            | Some vy -> dct.Add (k, (vx, vy))
            | None    -> ()
        dct :> IDictionary<'Key, 'T1 * 'T2>

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
    // Returns the union of two maps, preferring values from the first in case of duplicate keys.
    let union (source: IDictionary<'Key, 'T>) (altSource: IDictionary<'Key, 'T>) = 
        Enumerable
          .Union(
            source, 
            altSource,
            { new IEqualityComparer<KeyValuePair<'Key,'T>> with 
                      member __.Equals ((a:KeyValuePair<'Key,'T>),(b:KeyValuePair<'Key,'T>)) : bool = a.Key = b.Key
                      member __.GetHashCode (a:KeyValuePair<'Key,'T>) = a.Key.GetHashCode () })
          .ToDictionary((fun x -> x.Key), (fun y -> y.Value)) :> IDictionary<'Key, 'T>

    /// Returns the intersection of two Dicts, using the combiner function for duplicate keys.
    let intersectWith combiner (source1:IDictionary<'Key, 'T>) (source2:IDictionary<'Key, 'T>) =
        Enumerable
          .Join(
            source1, 
            source2, 
            (fun (x:KeyValuePair<'Key, 'T>) -> x.Key), 
            (fun (y:KeyValuePair<'Key, 'T>) -> y.Key), 
            (fun (x:KeyValuePair<'Key, 'T>) (y:KeyValuePair<'Key, 'T>) -> 
              KeyValuePair<'Key, 'T>(x.Key, combiner (x.Value) (y.Value))))
          .ToDictionary((fun x -> x.Key), (fun y -> y.Value)) :> IDictionary<'Key, 'T>

    // Returns the intersection of two maps, preferring values from the first in case of duplicate keys.
    let intersect (source1:IDictionary<'Key, 'T>) (source2:IDictionary<'Key, 'T>) = 
        intersectWith (fun a _ -> a) source1 source2
    #endif