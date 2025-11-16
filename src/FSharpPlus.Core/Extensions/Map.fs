namespace FSharpPlus
open System

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
    /// <remarks>
    /// Note: this function has since been added to FSharp.Core.
    /// It will be removed in next major release of FSharpPlus.
    /// </remarks>
    let keys   (source: Map<'Key, 'T>) = Seq.map (fun (KeyValue(k, _)) -> k) source

    /// <summary>Returns the values of the given map.</summary>
    /// <param name="source">The input map.</param>
    ///
    /// <returns>A seq of the values in the map.</returns>
    /// <remarks>
    /// Note: this function has since been added to FSharp.Core.
    /// It will be removed in next major release of FSharpPlus.
    /// </remarks>
    let values (source: Map<'Key, 'T>) = Seq.map (fun (KeyValue(_, v)) -> v) source

    /// <summary>Applies the given function to each value of the Map.</summary>
    /// <param name="action">The function to apply to each value of the input Map.</param>
    /// <param name="source">The input Map.</param>
    let iterValues action (source: Map<'Key, 'T>) = Map.iter (fun _ v -> action v) source

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

    /// <summary>Applies each function in the Map of functions to the corresponding value in the Map of values,
    /// producing a new Map of values.</summary>
    /// <remarks>
    /// If a key is present in the function Map but not in the value Map, that key is simply ignored.
    /// If a key is present in the value Map but not in the function Map, that key is also ignored.
    /// </remarks>
    /// <param name="f">The Map of functions.</param>
    /// <param name="x">The Map of values.</param>
    /// <returns>The resulting Map of values.</returns>
    /// <typeparam name="'Key">The type of the keys in the dictionaries.</typeparam>
    /// <typeparam name="'T">The type of the values in the Map of values.</typeparam>
    /// <typeparam name="'U">The type of the values in the resulting Map.</typeparam>    
    /// <returns>A Map of values.</returns>
    /// <remarks>
    /// This function is useful for applying a set of transformations to a Map of values,
    /// where each transformation is defined by a function in a Map of functions.
    /// </remarks>
    let apply (f: Map<'Key, _>) (x: Map<'Key, 'T>) : Map<'Key, 'U> = Map (seq {
       for KeyValue (k, vf) in f do
           match Map.tryFind k x with
           | Some vx -> yield k, vf vx
           | _       -> () })

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
    /// <param name="mapping">The mapping function.</param>
    /// <param name="x">First input Map.</param>
    /// <param name="y">Second input Map.</param>
    /// <param name="z">Third input Map.</param>
    ///
    /// <returns>The mapped Map.</returns>
    let mapValues3 mapping (x: Map<'Key, 'T1>) (y: Map<'Key, 'T2>) (z: Map<'Key, 'T3>) = Map <| seq {
        let f = OptimizedClosures.FSharpFunc<_,_,_,_>.Adapt mapping
        for KeyValue(k, vx) in x do
            match Map.tryFind k y, lazy Map.tryFind k z with
            | Some vy, Lazy (Some vz) -> yield (k, f.Invoke (vx, vy, vz))
            | _      , _         -> () }
    
    /// <summary>Applies given function to each value of the given Map.</summary>
    /// <param name="mapping">The mapping function.</param>
    /// <param name="source">The input Map.</param>
    ///
    /// <returns>Returns Map with values x for each Map value where the function returns Some(x).</returns>
    let chooseValues mapping (source: Map<'Key, 'T>) = Map <| seq {
        for KeyValue(k, v) in source do
            match mapping v with
            | Some v -> yield (k, v)
            | None    -> () }

    /// <summary>Tuples values of two Maps.</summary>
    /// <remarks>Keys that are not present on both Maps are dropped.</remarks>
    /// <param name="source1">The first input Map.</param>
    /// <param name="source2">The second input Map.</param>
    ///
    /// <returns>The tupled Map.</returns>
    let zip (source1: Map<'Key, 'T1>) (source2: Map<'Key, 'T2>) = Map <| seq {
        for KeyValue(k, v1) in source1 do
            match Map.tryFind k source2 with
            | Some v2 -> yield (k, (v1, v2))
            | None    -> () }

    /// <summary>Tuples values of three Maps.</summary>
    /// <remarks>Keys that are not present on all three Maps are dropped.</remarks>
    /// <param name="source1">The first input Map.</param>
    /// <param name="source2">The second input Map.</param>
    /// <param name="source3">The third input Map.</param>
    ///
    /// <returns>The tupled Map.</returns>
    let zip3 (source1: Map<'Key, 'T1>) (source2: Map<'Key, 'T2>) (source3: Map<'Key, 'T3>) = Map <| seq {
        for KeyValue(k, v1) in source1 do
            match Map.tryFind k source2, Map.tryFind k source3 with
            | Some v2, Some v3 -> yield (k, (v1, v2, v3))
            | _    -> () }

    /// <summary>Splits a Map with tuple pair values to two separate Maps.</summary>
    /// <param name="source">The source Map.</param>
    ///
    /// <returns>A tuple of each untupled Map.</returns>
    let unzip (source: Map<'Key, 'T1 * 'T2>) = mapValues fst source, mapValues snd source

    /// <summary>Splits a Map with tuple of 3 values to three separate Maps.</summary>
    /// <param name="source">The source Map.</param>
    ///
    /// <returns>A tuple of each untupled Map.</returns>
    let unzip3 (source: Map<'Key, 'T1 * 'T2 * 'T3>) =
        mapValues (fun (x, _, _) -> x) source, mapValues (fun (_, x, _) -> x) source, mapValues (fun (_, _, x) -> x) source

    /// <summary>Flattens a Map of Maps to a single Map.</summary>
    /// <remarks>
    /// This function takes a Map where each value is another Map, and combines them into a single Map.
    /// If a key appears in multiple inner Maps, the value from the last Map is used.
    /// </remarks>
    /// <param name="source">The source Map of Maps.</param>
    ///
    /// <returns>A single Map with all keys and values from the inner Maps.</returns>
    let flatten (source: Map<_, _>) : Map<'Key, 'Value> =
        Map (seq {
            for KeyValue (k, v) in source do
                match Map.tryFind k v with
                | Some v -> yield k, v
                | _      -> () })

    /// <summary>Maps the values of the original Map using a function that returns a Map.</summary>
    /// <remarks>
    /// This function applies a mapping function to each value of the Map, where the mapping function returns a Map.
    /// The resulting Maps are then combined into a single Map.
    /// If a key appears in multiple Maps, the value from the last Map is used.
    /// </remarks>
    /// <param name="mapper">The mapping function that takes a value and returns a Map.</param>
    /// <param name="source">The input Map.</param>
    ///
    /// <returns>A single Map with all keys and values from the mapped Maps.</returns>
    let bind (mapper: 'T -> Map<'Key, 'U>) (source: Map<'Key, 'T>) = Map (seq {
        for KeyValue(k, v) in source do
            match Map.tryFind k (mapper v) with
            | Some v -> yield k, v
            | _      -> () })

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
