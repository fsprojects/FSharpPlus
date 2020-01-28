namespace FSharpPlus

open System

/// Additional operations on IReadOnlyDictionary<'Key, 'Value>
[<RequireQualifiedAccess>]
module IReadOnlyDictionary =
    #if !FABLE_COMPILER
    
    open System.Linq
    #endif
    open System.Collections.Generic
    /// Replaces or sets the item associated with a specified key with the specified value.

    let add key value (table: IReadOnlyDictionary<'Key, 'Value>) = table |> Seq.map (|KeyValue|) |> Map |> Map.add key value :> IReadOnlyDictionary<_,_>

    /// Gets the value associated with the specified key. Returns None if a value associated with the key is not found.
    let tryGetValue k (dct: IReadOnlyDictionary<'Key, 'Value>) =
        match dct.TryGetValue k with
        | true, v -> Some v
        | _       -> None
    let containsKey k (dct: IReadOnlyDictionary<'Key, 'Value>) = dct.ContainsKey k

    let keys   (source: IReadOnlyDictionary<'Key, 'Value>) = Seq.map (fun (KeyValue(k, _)) -> k) source
    let values (source: IReadOnlyDictionary<'Key, 'Value>) = Seq.map (fun (KeyValue(_, v)) -> v) source

    let map f (x: IReadOnlyDictionary<'Key, 'T>) =
        let dct = Dictionary<'Key, 'U> ()
        for KeyValue(k, v) in x do
            dct.Add (k, f v)
        dct :> IReadOnlyDictionary<'Key, 'U>

    let map2 f (x: IReadOnlyDictionary<'Key, 'T1>) (y: IReadOnlyDictionary<'Key, 'T2>) =
        let dct = Dictionary<'Key, 'U> ()
        let f = OptimizedClosures.FSharpFunc<_,_,_>.Adapt f
        for KeyValue(k, vx) in x do
            match tryGetValue k y with
            | Some vy -> dct.Add (k, f.Invoke (vx, vy))
            | None    -> ()
        dct :> IReadOnlyDictionary<'Key, 'U>

    /// <summary>Tuple values of two dictionaries.</summary>
    /// <remarks>Keys that are not present on both dictionaries are dropped.</remarks>
    /// <param name="x">The first input dictionary.</param>
    /// <param name="y">The second input dictionary.</param>
    ///
    /// <returns>The tupled dictionary.</returns>
    let zip (x: IReadOnlyDictionary<'Key, 'T1>) (y: IReadOnlyDictionary<'Key, 'T2>) =
        let dct = Dictionary<'Key, 'T1 * 'T2> ()
        for KeyValue(k, vx) in x do
            match tryGetValue k y with
            | Some vy -> dct.Add (k, (vx, vy))
            | None    -> ()
        dct :> IReadOnlyDictionary<'Key, 'T1 * 'T2>

    let unzip (source: IReadOnlyDictionary<'Key, 'T1 * 'T2>) =
        let dct1 = Dictionary<'Key, 'T1> ()
        let dct2 = Dictionary<'Key, 'T2> ()
        for KeyValue(k, (vx, vy)) in source do
            dct1.Add (k, vx)
            dct2.Add (k, vy)
        dct1 :> IReadOnlyDictionary<'Key, 'T1>, dct2 :> IReadOnlyDictionary<'Key, 'T2>

    /// Returns the union of two dictionaries, using the combiner function for duplicate keys.
    let unionWith combiner (source1: IReadOnlyDictionary<'Key, 'Value>) (source2: IReadOnlyDictionary<'Key, 'Value>) =
        let d = Dictionary<'Key,'Value> ()
        let f = OptimizedClosures.FSharpFunc<_,_,_>.Adapt combiner
        for KeyValue(k, v ) in source1 do d.[k] <- v
        for KeyValue(k, v') in source2 do d.[k] <- match d.TryGetValue k with true, v -> f.Invoke (v, v') | _ -> v'
        d :> IReadOnlyDictionary<'Key,'Value>

    #if !FABLE_COMPILER
    
    /// Returns the union of two dictionaries, preferring values from the first in case of duplicate keys.
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

    /// Returns the intersection of two readonly dictionaries, preferring values from the first in case of duplicate keys.
    let intersect (source1:IReadOnlyDictionary<'Key, 'T>) (source2:IReadOnlyDictionary<'Key, 'T>) = 
        intersectWith (fun a _ -> a) source1 source2
    #endif