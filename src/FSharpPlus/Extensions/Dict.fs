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

    /// <summary>Tries to get the value of the given key.</summary>
    /// <remarks>This is a function wrapper for the IDictionary.TryGetValue method,
    /// representing the result as an Option&lt;value&gt; instead of a bool plus an out-value.
    /// </remarks>
    /// <param name="k">The key whose value you wish to find.</param>
    /// <param name="dct">The input dictionary.</param>
    ///
    /// <returns>An option wrapped value</returns>
    let tryGetValue k (dct: IDictionary<'Key, 'Value>) =
        match dct.TryGetValue k with
        | true, v -> Some v
        | _       -> None

    /// <summary>Does the dictionary contain the given key?</summary>
    /// <remarks>Note: this is a function wrapper for the IDictionary.ContainsKey method</remarks>
    /// <param name="k">The key to find.</param>
    /// <param name="dct">The input dictionary.</param>
    ///
    /// <returns>A bool indicating if the key was found</returns>
    let containsKey k (dct: IDictionary<'Key, 'Value>) = dct.ContainsKey k

    /// <summary>Returns the keys of the given dictionary.</summary>
    /// <param name="source">The input dictionary.</param>
    ///
    /// <returns>A seq of the keys in the dictionary.</returns>
    let keys   (source: IDictionary<_,_>) = Seq.map (fun (KeyValue(k, _)) -> k) source

    /// <summary>Returns the values of the given dictionary.</summary>
    /// <param name="source">The input dictionary.</param>
    ///
    /// <returns>A seq of the values in the dictionary.</returns>
    let values (source: IDictionary<_,_>) = Seq.map (fun (KeyValue(_, v)) -> v) source

    /// <summary>Maps the given function over each value in the dictionary.</summary>
    /// <param name="f">The mapping function.</param>
    /// <param name="x">The input dictionary.</param>
    ///
    /// <returns>The mapped dictionary.</returns>
    let map f (x: IDictionary<'Key, 'T>) =
        let dct = Dictionary<'Key, 'U> ()
        for KeyValue(k, v) in x do
            dct.Add (k, f v)
        dct :> IDictionary<'Key, 'U>

    /// <summary>Creates a Dictionary value from a pair of Dictionaries, using a function to combine them.</summary>
    /// <remarks>Keys that are not present on both dictionaries are dropped.</remarks>
    /// <param name="f">The mapping function.</param>
    /// <param name="x">The first input dictionary.</param>
    /// <param name="y">The second input dictionary.</param>
    ///
    /// <returns>The combined dictionary.</returns>
    let map2 f (x: IDictionary<'Key, 'T1>) (y: IDictionary<'Key, 'T2>) =
        let dct = Dictionary<'Key, 'U> ()
        let f = OptimizedClosures.FSharpFunc<_,_,_>.Adapt f
        for KeyValue(k, vx) in x do
            match tryGetValue k y with
            | Some vy -> dct.Add (k, f.Invoke (vx, vy))
            | None    -> ()
        dct :> IDictionary<'Key, 'U>

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
        dct :> IDictionary<'Key, 'U>
        
    /// <summary>Tuples values of two dictionaries.</summary>
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

   ///Returns the intersection of two Dicts, preferring values from the first in case of duplicate keys.
    let intersect (source1:IDictionary<'Key, 'T>) (source2:IDictionary<'Key, 'T>) = 
        intersectWith (fun a _ -> a) source1 source2
    #endif
    
    /// <summary>Choose with access to the key.</summary>
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
        dct :> IDictionary<'Key, 'U>
