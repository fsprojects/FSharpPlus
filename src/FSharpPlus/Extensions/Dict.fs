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

    /// <summary>Creates an empty dictionary.</summary>
    [<GeneralizableValue>]
    let empty<'Key, 'U when 'Key : equality> = printfn "creating"; Dictionary<'Key, 'U> () :> IDictionary<_,_>

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
        let dct = Dictionary<'Key, 'U> ()
        for KeyValue(k, v) in source do
            dct.Add (k, mapper v)
        dct :> IDictionary<'Key, 'U>

    /// <summary>Applies each function in the dictionary of functions to the corresponding value in the dictionary of values,
    /// producing a new dictionary of values.</summary>
    /// <remarks>
    /// If a key is present in the function dictionary but not in the value dictionary, that key is simply ignored.
    /// If a key is present in the value dictionary but not in the function dictionary, that key is also ignored.
    /// </remarks>
    /// <param name="f">The dictionary of functions.</param>
    /// <param name="x">The dictionary of values.</param>
    /// <returns>The resulting dictionary of values.</returns>
    /// <typeparam name="'Key">The type of the keys in the dictionaries.</typeparam>
    /// <typeparam name="'T">The type of the values in the dictionary of values.</typeparam>
    /// <typeparam name="'U">The type of the values in the resulting dictionary.</typeparam>    
    /// <returns>A dictionary of values.</returns>
    /// <remarks>
    /// This function is useful for applying a set of transformations to a dictionary of values,
    /// where each transformation is defined by a function in a dictionary of functions.
    /// </remarks>
    let apply (f: IDictionary<'Key, _>) (x: IDictionary<'Key, 'T>) : IDictionary<'Key, 'U> =
        let dct = Dictionary ()
        for KeyValue (k, vf) in f do
            match x.TryGetValue k with
            | true, vx -> dct.Add (k, vf vx)
            | _        -> ()
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

    /// <summary>Tuples values of three dictionaries.</summary>
    /// <remarks>Keys that are not present on all three dictionaries are dropped.</remarks>
    /// <param name="source1">The first input dictionary.</param>
    /// <param name="source2">The second input dictionary.</param>
    /// <param name="source3">The third input dictionary.</param>
    ///
    /// <returns>The tupled dictionary.</returns>
    let zip3 (source1: IDictionary<'Key, 'T1>) (source2: IDictionary<'Key, 'T2>) (source3: IDictionary<'Key, 'T3>) =
        let dct = Dictionary<'Key, 'T1 * 'T2 * 'T3> ()
        for KeyValue(k, vx) in source1 do
            match tryGetValue k source2, tryGetValue k source3 with
            | Some vy, Some vz -> dct.Add (k, (vx, vy, vz))
            | _                -> ()
        dct :> IDictionary<'Key, 'T1 * 'T2 * 'T3>

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

    /// <summary>Splits a dictionary with tuple of 3 values to three separate dictionaries.</summary>
    /// <param name="source">The source dictionary.</param>
    ///
    /// <returns>A tuple of each untupled dictionary.</returns>
    let unzip3 (source: IDictionary<'Key, 'T1 * 'T2 * 'T3>) =
        let dct1 = Dictionary<'Key, 'T1> ()
        let dct2 = Dictionary<'Key, 'T2> ()
        let dct3 = Dictionary<'Key, 'T3> ()
        for KeyValue(k, (vx, vy, vz)) in source do
            dct1.Add (k, vx)
            dct2.Add (k, vy)
            dct3.Add (k, vz)
        dct1 :> IDictionary<'Key, 'T1>, dct2 :> IDictionary<'Key, 'T2>, dct3 :> IDictionary<'Key, 'T3>


    /// <summary>Flattens a dictionary of dictionaries into a single dictionary on matching keys.</summary>
    /// <remarks>Keys that are not present in the inner dictionaries are dropped.</remarks>
    /// <param name="source">The source IDictionary of IDictionaries.</param>
    /// <returns>A flattened IDictionary.</returns>
    let flatten (source: IDictionary<_, IDictionary<_, _>>) : IDictionary<'Key, 'Value> =
        let dct = Dictionary ()
        for KeyValue (k, v) in source do
            match v.TryGetValue k  with
            | true, v -> dct.Add (k, v)
            | _       -> ()
        dct :> IDictionary<'Key, 'Value>
    
    /// <summary>Applies a function to each value in a dictionary and then flattens the result on matching keys into a new dictionary.</summary>
    /// <remarks>Keys that are not present in the inner dictionaries are dropped.</remarks>
    /// <param name="mapper">The function to be applied to each value in the dictionary.</param>
    /// <param name="source">The input dictionary.</param>
    /// <returns>A flattened IDictionary.</returns>
    let bind (mapper: 'T -> IDictionary<'Key, 'U>) (source: IDictionary<'Key, 'T>) =
        let dct = Dictionary ()
        for KeyValue (k, v) in source do
            match (mapper v).TryGetValue k with
            | true, v -> dct.Add (k, v)
            | _       -> ()
        dct :> IDictionary<'Key, 'U>

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

    /// <summary>Converts a dictionary to a ResizeArray.</summary>
    /// <param name="source">The source dictionary.</param>
    ///
    /// <returns>A ResizeArray containing the Key and Value of the original dictionary.</returns>
    let toResizeArray (source: IDictionary<'Key, 'T>) =
        let arr = ResizeArray<KeyValuePair<'Key, 'T>> ()
        for KeyValue (k, x) in source do
            arr.Add (KeyValuePair (k, x))
        arr

    /// <summary>Converts a dictionary to a sequence.</summary>
    /// <param name="source">The source dictionary.</param>
    ///
    /// <returns>A sequence containing the Key and Value of the original dictionary.</returns>
    let toSeq (source: IDictionary<'Key, 'T>) = toResizeArray source :> seq<_>

    /// <summary>Folds over the bindings in the dictionary.</summary>
    /// <remarks>
    /// This function takes a folder function, an initial state, and a source dictionary.
    /// The folder function is applied to each key-value pair in the source, accumulating a state.
    /// The initial state is provided as the first argument to the folder function.
    /// The function returns the final accumulated state after processing all key-value pairs.
    /// </remarks>
    /// <param name="folder">The folder function that takes the current state, a key, and a value, and returns the new state.</param>
    /// <param name="state">The initial state to start the folding process.</param>
    /// <param name="source">The source dictionary to fold over.</param>
    /// <typeparam name="'State">The type of the state being accumulated.</typeparam>
    /// <typeparam name="'Key">The type of the keys in the dictionary.</typeparam>
    /// <typeparam name="'T">The type of the values in the dictionary.</typeparam>
    ///
    /// <returns>The final accumulated state after folding over all key-value pairs in the source.</returns>
    let fold (folder: 'State -> 'Key -> 'T -> 'State) (state: 'State) (source: IDictionary<'Key, 'T>) =
        let unzip source = Seq.map fst source, Seq.map snd source
        source |> toSeq |> Seq.map (|KeyValue|) |> unzip ||> Seq.fold2 folder state
