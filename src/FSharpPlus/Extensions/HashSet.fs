namespace FSharpPlus

/// Additional operations on HashSet<'T>
[<RequireQualifiedAccess>]
module HashSet =
    open System.Collections.Generic
    open FSharpPlus.Internals.Errors

    /// <summary>The empty set for the type 'T.</summary>
    [<GeneralizableValue>]
    [<CompiledName("Empty")>]
    let empty<'T> : HashSet<'T> = HashSet<'T> ()

    /// <summary>The set containing the given element.</summary>
    /// <param name="value">The value for the set to contain.</param>
    /// <returns>The set containing <c>value</c>.</returns>
    [<CompiledName("Singleton")>]
    let singleton (value: 'T) : HashSet<'T> =
        let set =
            #if FABLE_COMPILER || NET45 || NETSTANDARD2_0
            HashSet<'T> ()
            #else
            HashSet<'T> 1
            #endif
        set.Add value |> ignore
        set

    /// <summary>Computes the union of the two sets.</summary>
    /// <param name="source1">The first input set.</param>
    /// <param name="source2">The second input set.</param>
    /// <returns>The union of <c>set1</c> and <c>set2</c>.</returns>
    [<CompiledName("Union")>]
    let union (source1: HashSet<'T>) (source2: HashSet<'T>) : HashSet<'T> =
        #if !NET45
        raiseIfNull (nameof source1) source1
        raiseIfNull (nameof source2) source2
        #endif
        let union =
            #if FABLE_COMPILER || NET45 || NETSTANDARD2_0
            HashSet<'T> ()
            #else
            HashSet<'T> (max source1.Count source2.Count)
            #endif
        for item in source1 do union.Add item |> ignore
        for item in source2 do union.Add item |> ignore
        union

    /// <summary>Returns a new collection containing the results of applying the
    /// given function to each element of the input set.</summary>
    /// <param name="mapping">The function to transform elements of the input set.</param>
    /// <param name="source">The input set.</param>
    /// <returns>A set containing the transformed elements.</returns>
    [<CompiledName("Map")>]
    let map (mapping: 'T -> 'U) (source: HashSet<'T>) : HashSet<'U> =
        #if !NET45
        raiseIfNull (nameof source) source
        #endif
        let result = empty<'U>
        for item in source do
            result.Add (mapping item) |> ignore
        result

    /// <summary>Determines whether a HashSet contains a specific value.</summary>
    /// <param name="value">The value to look for in the set.</param>
    /// <param name="source">The set to look in.</param>
    /// <returns><c>true</c> if the set contains <c>value</c>; otherwise, <c>false</c>.</returns>
    [<CompiledName("Contains")>]
    let contains (value: 'T) (source: HashSet<'T>) : bool =
        #if !NET45
        raiseIfNull (nameof source) source
        #endif
        source.Contains value

    /// <summary>Determines whether the first set is a subset of the second set.</summary>
    /// <param name="source1">The first input set.</param>
    /// <param name="source2">The second input set.</param>
    /// <returns><c>true</c> if <c>source1</c> is a subset of <c>source2</c>; otherwise, <c>false</c>.</returns>
    [<CompiledName("IsSubset")>]
    let isSubset (source1: HashSet<'T>) (source2: HashSet<'T>) : bool =
        #if !NET45
        raiseIfNull (nameof source1) source1
        raiseIfNull (nameof source2) source2
        #endif
        source1.IsSubsetOf source2
