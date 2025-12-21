namespace FSharpPlus

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

    /// <summary>Creates an empty dictionary.</summary>
    [<GeneralizableValue>]
    let empty<'Key, 'U when 'Key : equality> = Dictionary<'Key, 'U> ()

    /// <summary>Tries to get the value of the given key.</summary>
    /// <remarks>Note: this is a function wrapper for the Dictionary.TryGetValue method,
    /// which also represents the result as an Option&lt;value&gt; instead of a bool
    /// and an out-value.
    /// </remarks>
    /// <param name="key">The key to find.</param>
    /// <param name="source">The input dictionary.</param>
    ///
    /// <returns>An option wrapped value</returns>
    let tryGetValue key (source: Dictionary<'Key, 'Value>) =
        match source.TryGetValue key with
        | true, v -> Some v
        | _       -> None

    /// <summary>Does the dictionary contain the given key?</summary>
    /// <remarks>Note: this is a function wrapper for the Dictionary.ContainsKey method.</remarks>
    /// <param name="key">The key to find.</param>
    /// <param name="source">The input dictionary.</param>
    ///
    /// <returns>A bool indicating if the key was found</returns>
    let containsKey key (source: Dictionary<'Key, 'Value>) = source.ContainsKey key

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

    /// <summary>Applies the given function to each key and value pair of the dictionary.</summary>
    /// <param name="action">The function to apply to each key and value pair of the input dictionary.</param>
    /// <param name="source">The input dictionary.</param>
    let iter action (source: Dictionary<'Key, 'T>) = for KeyValue(k, v) in source do action k v

    /// <summary>Applies the given function to each value of the dictionary.</summary>
    /// <param name="action">The function to apply to each value of the input dictionary.</param>
    /// <param name="source">The input dictionary.</param>
    let iterValues action (source: Dictionary<'Key, 'T>) = for KeyValue(_, v) in source do action v

    /// <summary>Maps the given function over each value in the dictionary.</summary>
    /// <param name="mapping">The mapping function.</param>
    /// <param name="source">The input dictionary.</param>
    ///
    /// <returns>The mapped dictionary.</returns>
    let map mapping (source: Dictionary<'Key, 'T>) =
        let dct = Dictionary<'Key, 'U> ()
        for KeyValue(k, v) in source do
            dct.Add (k, mapping v)
        dct

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
    let apply (f: Dictionary<'Key, _>) (x: Dictionary<'Key, 'T>) : Dictionary<'Key, 'U> =
        let dct = Dictionary ()
        for KeyValue (k, vf) in f do
            match x.TryGetValue k with
            | true, vx -> dct.Add (k, vf vx)
            | _        -> ()
        dct

    /// <summary>Creates a Dictionary value from a pair of Dictionaries, using a function to combine them.</summary>
    /// <remarks>Keys that are not present on both dictionaries are dropped.</remarks>
    /// <param name="mapping">The mapping function.</param>
    /// <param name="source1">The first input dictionary.</param>
    /// <param name="source2">The second input dictionary.</param>
    ///
    /// <returns>The combined dictionary.</returns>
    let map2 mapping (source1: Dictionary<'Key, 'T1>) (source2: Dictionary<'Key, 'T2>) =
        let dct = Dictionary<'Key, 'U> ()
        let f = OptimizedClosures.FSharpFunc<_,_,_>.Adapt mapping
        for KeyValue(k, vx) in source1 do
            match tryGetValue k source2 with
            | Some vy -> dct.Add (k, f.Invoke (vx, vy))
            | None    -> ()
        dct
        
    /// <summary>Combines values from three Dictionaries using mapping function.</summary>
    /// <remarks>Keys that are not present on every Dictionary are dropped.</remarks>
    /// <param name="mapping">The mapping function.</param>
    /// <param name="source1">First input Dictionary.</param>
    /// <param name="source2">Second input Dictionary.</param>
    /// <param name="source3">Third input Dictionary.</param>
    ///
    /// <returns>The mapped Dictionary.</returns>
    let map3 mapping (source1: Dictionary<'Key, 'T1>) (source2: Dictionary<'Key, 'T2>) (source3: Dictionary<'Key, 'T3>) =
        let dct = Dictionary<'Key, 'U> ()
        let f = OptimizedClosures.FSharpFunc<_,_,_,_>.Adapt mapping
        for KeyValue(k, vx) in source1 do
            match tryGetValue k source2, tryGetValue k source3 with
            | Some vy, Some vz -> dct.Add (k, f.Invoke (vx, vy, vz))
            | _      , _       -> ()
        dct

    /// <summary>Applies given function to each value of the given dictionary.</summary>
    /// <param name="mapper">The mapping function.</param>
    /// <param name="source">The input dictionary.</param>
    ///
    /// <returns>Returns dictionary with values x for each dictionary value where the function returns Some(x).</returns>
    let chooseValues mapper (source: IDictionary<'Key, 'T>) =
        let dct = Dictionary<'Key, 'U> ()
        for KeyValue(k, v) in source do
            match mapper v with
            | Some v -> dct.Add (k, v)
            | None    -> ()
        dct
        
    /// <summary>Tuples values of two dictionaries.</summary>
    /// <remarks>Keys that are not present on both dictionaries are dropped.</remarks>
    /// <param name="source1">The first input dictionary.</param>
    /// <param name="source2">The second input dictionary.</param>
    ///
    /// <returns>The tupled dictionary.</returns>
    let zip (source1: Dictionary<'Key, 'T1>) (source2: Dictionary<'Key, 'T2>) =
        let dct = Dictionary<'Key, 'T1 * 'T2> ()
        for KeyValue(k, vx) in source1 do
            match tryGetValue k source2 with
            | Some vy -> dct.Add (k, (vx, vy))
            | None    -> ()
        dct

    /// <summary>Tuples values of three dictionaries.</summary>
    /// <remarks>Keys that are not present on all three dictionaries are dropped.</remarks>
    /// <param name="source1">The first input dictionary.</param>
    /// <param name="source2">The second input dictionary.</param>
    /// <param name="source3">The third input dictionary.</param>
    ///
    /// <returns>The tupled dictionary.</returns>
    let zip3 (source1: Dictionary<'Key, 'T1>) (source2: Dictionary<'Key, 'T2>) (source3: Dictionary<'Key, 'T3>) =
        let dct = Dictionary<'Key, 'T1 * 'T2 * 'T3> ()
        for KeyValue(k, vx) in source1 do
            match tryGetValue k source2, tryGetValue k source3 with
            | Some vy, Some vz -> dct.Add (k, (vx, vy, vz))
            | _                -> ()
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

    /// <summary>Splits a dictionary with tuple of 3 values to three separate dictionaries.</summary>
    /// <param name="source">The source dictionary.</param>
    ///
    /// <returns>A tuple of each untupled dictionary.</returns>
    let unzip3 (source: Dictionary<'Key, 'T1 * 'T2 * 'T3>) =
        let dct1 = Dictionary<'Key, 'T1> ()
        let dct2 = Dictionary<'Key, 'T2> ()
        let dct3 = Dictionary<'Key, 'T3> ()
        for KeyValue(k, (vx, vy, vz)) in source do
            dct1.Add (k, vx)
            dct2.Add (k, vy)
            dct3.Add (k, vz)
        dct1, dct2, dct3

    /// <summary>Flattens a dictionary of dictionaries into a single dictionary on matching keys.</summary>
    /// <remarks>Keys that are not present in the inner dictionaries are dropped.</remarks>
    /// <param name="source">The source Dictionary of IDictionaries.</param>
    /// <returns>A flattened Dictionary.</returns>
    let flatten (source: Dictionary<_, Dictionary<_, _>>) : Dictionary<'Key, 'Value> =
        let dct = Dictionary ()
        for KeyValue (k, v) in source do
            match v.TryGetValue k  with
            | true, v -> dct.Add (k, v)
            | _       -> ()
        dct
    
    /// <summary>Applies a function to each value in a dictionary and then flattens the result on matching keys into a new dictionary.</summary>
    /// <remarks>Keys that are not present in the inner dictionaries are dropped.</remarks>
    /// <param name="mapper">The function to be applied to each value in the dictionary.</param>
    /// <param name="source">The input dictionary.</param>
    /// <returns>A flattened Dictionary.</returns>
    let bind (mapper: 'T -> Dictionary<'Key, 'U>) (source: Dictionary<'Key, 'T>) =
        let dct = Dictionary ()
        for KeyValue (k, v) in source do
            match (mapper v).TryGetValue k with
            | true, v -> dct.Add (k, v)
            | _       -> ()
        dct

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
        dct

    /// <summary>Converts a dictionary to a ResizeArray.</summary>
    /// <param name="source">The source dictionary.</param>
    ///
    /// <returns>A ResizeArray containing the Key and Value of the original dictionary.</returns>
    let toResizeArray (source: Dictionary<'Key, 'T>) =
        let arr = ResizeArray<KeyValuePair<'Key, 'T>> ()
        for KeyValue (k, x) in source do
            arr.Add (KeyValuePair (k, x))
        arr

    /// <summary>Converts a dictionary to a sequence.</summary>
    /// <param name="source">The source dictionary.</param>
    ///
    /// <returns>A sequence containing the Key and Value of the original dictionary.</returns>
    let toSeq (source: Dictionary<'Key, 'T>) = toResizeArray source :> seq<_>

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
    let fold (folder: 'State -> 'Key -> 'T -> 'State) (state: 'State) (source: Dictionary<'Key, 'T>) =
        let unzip source = Seq.map fst source, Seq.map snd source
        source |> toSeq |> Seq.map (|KeyValue|) |> unzip ||> Seq.fold2 folder state
