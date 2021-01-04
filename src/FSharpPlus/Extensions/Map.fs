namespace FSharpPlus

/// Additional operations on Map<'Key, 'Value>
[<RequireQualifiedAccess>]
module Map =
    open System.Collections.Generic
    #if !FABLE_COMPILER
    
    open System.Linq
    #endif

    /// <summary>Returns the keys of the given map.</summary>
    /// <param name="source">The input map.</param>
    ///
    /// <returns>A seq of the keys in the map.</returns>
    let keys   (source: Map<'Key, 'T>) = Seq.map (fun (KeyValue(k, _)) -> k) source

    /// <summary>Returns the values of the given map.</summary>
    /// <param name="source">The input map.</param>
    ///
    /// <returns>A seq of the values in the map.</returns>
    let values (source: Map<'Key, 'T>) = Seq.map (fun (KeyValue(_, v)) -> v) source

    /// <summary>Maps the values of the original Map.</summary>
    /// <remarks>
    /// The core `Map.map` function maps over values too, but it passes both
    /// key and value to the mapping function.
    /// </remarks>
    /// <param name="f">The mapping function - takes only the value, and returns the mapped value.</param>
    /// <param name="x">The input Map.</param>
    ///
    /// <returns>The mapped Map.</returns>
    let mapValues f (x: Map<'Key, 'T>) = Map.map (fun _ -> f) x

    /// <summary>Maps values of two Maps.</summary>
    /// <remarks>Keys that are not present on both Maps are dropped.</remarks>
    /// <param name="f">The mapping function.</param>
    /// <param name="x">The first input Map.</param>
    /// <param name="y">The second input Map.</param>
    ///
    /// <returns>The mapped Map.</returns>
    let mapValues2 f (x: Map<'Key, 'T1>) (y: Map<'Key, 'T2>) = Map <| seq {
        let f = OptimizedClosures.FSharpFunc<_,_,_>.Adapt f
        for KeyValue(k, vx) in x do
            match Map.tryFind k y with
            | Some vy -> yield (k, f.Invoke (vx, vy))
            | None    -> () }
    
    /// <summary>Combines values from three maps using mapping function.</summary>
    /// <remarks>Keys that are not present on every Map are dropped.</remarks>
    /// <param name="f">The mapping function.</param>
    /// <param name="x">First input Map.</param>
    /// <param name="y">Second input Map.</param>
    /// <param name="y">Third input Map.</param>
    ///
    /// <returns>The mapped Map.</returns>
    let mapValues3 f (x: Map<'Key, 'T1>) (y: Map<'Key, 'T2>) (z: Map<'Key, 'T3>) = Map <| seq {
        let f = OptimizedClosures.FSharpFunc<_,_,_,_>.Adapt f
        for KeyValue(k, vx) in x do
            match Map.tryFind k y, lazy (Map.tryFind k z) with
            | Some vy, Lazy (Some vz) -> yield (k, f.Invoke (vx, vy, vz))
            | _      , _         -> () }
    
    /// <summary>Applies given function to each value of the given Map.</summary>
    /// <param name="f">The mapping function.</param>
    /// <param name="x">The input Map.</param>
    ///
    /// <returns>Returns Map with values x for each Map value where the function returns Some(x).</returns>
    let chooseValues f (x: Map<'Key, 'T>) = Map <| seq {
        for KeyValue(k, v) in x do
            match f v with
            | Some v -> yield (k, v)
            | None    -> () }

    /// <summary>Tuples values of two Maps.</summary>
    /// <remarks>Keys that are not present on both Maps are dropped.</remarks>
    /// <param name="x">The first input Map.</param>
    /// <param name="y">The second input Map.</param>
    ///
    /// <returns>The tupled Map.</returns>
    let zip (x: Map<'Key, 'T1>) (y: Map<'Key, 'T2>) = Map <| seq {
        for KeyValue(k, vx) in x do
            match Map.tryFind k y with
            | Some vy -> yield (k, (vx, vy))
            | None    -> () }

    /// <summary>Splits a Map with tuple pair values to two separate Maps.</summary>
    /// <param name="source">The source Map.</param>
    ///
    /// <returns>A tuple of each untupled Map.</returns>
    let unzip (source: Map<'Key, 'T1 * 'T2>) = mapValues fst source, mapValues snd source

    /// Returns the union of two maps, using the combiner function for duplicate keys.
    let unionWith combiner (source1: Map<'Key, 'Value>) (source2: Map<'Key, 'Value>) =
        Map.fold (fun m k v' -> Map.add k (match Map.tryFind k m with Some v -> combiner v v' | None -> v') m) source1 source2

    /// Returns the union of two maps, preferring values from the first in case of duplicate keys.
    let union (source: Map<'Key, 'T>) (altSource: Map<'Key, 'T>) = unionWith (fun x _ -> x) source altSource

    #if !FABLE_COMPILER
    
    /// Returns the intersection of two maps, using the combiner function for duplicate keys.
    let intersectWith combiner (source1:Map<'Key, 'T>) (source2:Map<'Key, 'T>) =
        Enumerable
            .Join(
              source1, 
              source2, 
              (fun (x:KeyValuePair<'Key, 'T>) -> x.Key), 
              (fun (y:KeyValuePair<'Key, 'T>) -> y.Key), 
              (fun (x:KeyValuePair<'Key, 'T>) (y:KeyValuePair<'Key, 'T>) -> 
                KeyValuePair<'Key, 'T>(x.Key, combiner (x.Value) (y.Value))))
        |> Seq.map (fun kv -> (kv.Key, kv.Value))
        |> Map.ofSeq
    #else

    /// Returns the intersection of two maps, using the combiner function for duplicate keys.
    let intersectWith combiner (source1:Map<'Key, 'T>) (source2:Map<'Key, 'T>) =
        let keysSetOf = Map.toList>> (List.map fst) >> set
        let keyIntersection = Set.intersect (keysSetOf source1) (keysSetOf source2)
        let intersection = [
            for key in keyIntersection do
            let xValue=Map.find key source1
            let yValue=Map.find key source2
            yield (key, combiner xValue yValue)
        ]
        intersection |> Map.ofList
    #endif

    ///Returns the intersection of two maps, preferring values from the first in case of duplicate keys.
    let intersect (source1:Map<'Key, 'T>) (source2:Map<'Key, 'T>) = 
        intersectWith (fun a _ -> a) source1 source2
    
    /// <summary>Same as chooseValues but with access to the key.</summary>
    /// <param name="f">The mapping function, taking key and element as parameters.</param>
    /// <param name="x">The input map.</param>
    ///
    /// <returns>Returns Map with values (k, v) for each Map value where the function returns Some(v).</returns>
    let choosei (f: 'Key -> 'T -> 'U option) (x: Map<'Key, 'T>) = Map <| seq {
        for KeyValue(k, v) in x do
            match f k v with
            | Some v -> yield(k, v)
            | None   -> () }
