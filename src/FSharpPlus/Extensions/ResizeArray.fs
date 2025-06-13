﻿namespace FSharpPlus

/// Additional operations on ResizeArray
[<RequireQualifiedAccess>]
module ResizeArray =

    open System
    open FSharpPlus.Internals.Errors

    /// <summary>Builds a new ResizeArray whose elements are the results of applying the given function
    /// to each of the elements of the ResizeArray.</summary>
    ///
    /// <param name="mapping">A function to transform items from the input ResizeArray.</param>
    /// <param name="source">The input ResizeArray.</param>
    ///
    /// <returns>The result ResizeArray.</returns>
    ///
    /// <exception cref="System.ArgumentNullException">Thrown when the input ResizeArray is null.</exception>
    let map (mapping: 'T->'U) (source: ResizeArray<'T>) =
        raiseIfNull (nameof source) source
        
        ResizeArray (Seq.map mapping source)

    /// <summary>Applies the given function to each element of the collection.</summary>
    /// <param name="action">The function to apply to elements from the input ResizeArray.</param>
    /// <param name="source">The input ResizeArray.</param>
    let iter (action: 'T -> 'U) (source: ResizeArray<'T>) =
        raiseIfNull (nameof source) source
        
        ResizeArray (Seq.map action source)

    /// <summary>Applies a ResizeArray of functions to a ResizeArray of values and concatenates them.</summary>
    /// <param name="f">The functions.</param>
    /// <param name="ra">The values.</param>
    /// <returns>A concatenated list of the resulting ResizeArray after applying each function to each value.</returns>
    /// 
    /// <example>
    /// <code>
    /// > List.apply [double; triple] [1; 2; 3];;  
    /// val it : int list = [2; 4; 6; 3; 6; 9]
    /// </code>
    /// </example>
    let apply (f: ResizeArray<'T->'U>) (ra: ResizeArray<'T>) = 
        raiseIfNull (nameof ra) ra
        
        ResizeArray (Seq.apply f ra)

    /// Combines all values from the first ResizeArray with the second, using the supplied mapping function.
    let lift2 mapping (ra1: ResizeArray<'T>) (ra2: ResizeArray<'U>) =
        raiseIfNull (nameof ra1) ra1
        raiseIfNull (nameof ra2) ra2
        
        ResizeArray (Seq.lift2 mapping ra1 ra2)

    /// <summary>Combines values from three ResizeArrays and calls a mapping function on this combination.</summary>
    /// <param name="mapping">Mapping function taking three element combination as input.</param>
    /// <param name="ra1">First ResizeArray.</param>
    /// <param name="ra2">Second ResizeArray.</param>
    /// <param name="ra3">Third ResizeArray.</param>
    ///
    /// <returns>ResizeArray with values returned from mapping function.</returns>
    let lift3 mapping (ra1: ResizeArray<'T>) (ra2: ResizeArray<'U>) (ra3: ResizeArray<'V>) =
        raiseIfNull (nameof ra1) ra1
        raiseIfNull (nameof ra2) ra2
        raiseIfNull (nameof ra3) ra3
        
        ResizeArray (Seq.lift3 mapping ra1 ra2 ra3)
    
    /// Concatenates all elements, using the specified separator between each element.
    let intercalate (separator: _ []) (source: seq<_ []>) =
        raiseIfNull (nameof separator) separator
        raiseIfNull (nameof source) source
        
        source |> Seq.intercalate separator |> Seq.toArray

    /// Inserts a separator element between each element in the source ResizeArray.
    let intersperse element source =
        raiseIfNull (nameof element) element
        raiseIfNull (nameof source) source
        
        source |> Array.toSeq |> Seq.intersperse element |> Seq.toArray : 'T []

    /// Creates a sequence of arrays by splitting the source array on any of the given separators.
    let split (separators: seq<_ []>) (source: _ []) =
        raiseIfNull (nameof separators) separators
        raiseIfNull (nameof source) source
        source |> Array.toSeq |> Seq.split separators |> Seq.map Seq.toArray

    /// Replaces a subsequence of the source array with the given replacement array.
    let replace (oldValue: _ []) (newValue: _ []) source =
        raiseIfNull (nameof oldValue) oldValue
        raiseIfNull (nameof source) source
        source |> Array.toSeq |> Seq.replace oldValue newValue |> Seq.toArray : 'T []

    #if !FABLE_COMPILER

    /// <summary>
    /// Returns the index of the first occurrence of the specified slice in the source.
    /// </summary>
    /// <exception cref="System.ArgumentException">
    /// Thrown when the slice was not found in the sequence.
    /// </exception>
    /// <returns>
    /// The index of the slice.
    /// </returns>
    let findSliceIndex (slice: _ []) (source: _ []) =
        raiseIfNull (nameof slice) slice
        raiseIfNull (nameof source) source
        
        let index = Internals.FindSliceIndex.arrayImpl slice source
        if index = -1 then
            ArgumentException("The specified slice was not found in the sequence.") |> raise
        else
            index

    /// <summary>
    /// Returns the index of the first occurrence of the specified slice in the source.
    /// Returns <c>None</c> if not found.
    /// </summary>
    /// <returns>
    /// The index of the slice or <c>None</c>.
    /// </returns>
    let tryFindSliceIndex (slice: _ []) (source: _ []) =
        raiseIfNull (nameof slice) slice
        raiseIfNull (nameof source) source
        
        let index = Internals.FindSliceIndex.arrayImpl slice source
        if index = -1 then None else Some index

    /// <summary>
    /// Returns the index of the last occurrence of the specified slice in the source.
    /// </summary>
    /// <exception cref="System.ArgumentException">
    /// Thrown when the slice was not found in the sequence.
    /// </exception>
    /// <returns>
    /// The index of the slice.
    /// </returns>
    let findLastSliceIndex (slice: _ []) (source: _ []) =
        let index = Internals.FindLastSliceIndex.arrayImpl slice source
        if index = -1 then
            ArgumentException("The specified slice was not found in the sequence.") |> raise
        else
            index

    /// <summary>
    /// Returns the index of the last occurrence of the specified slice in the source.
    /// Returns <c>None</c> if not found.
    /// </summary>
    /// <returns>
    /// The index of the slice or <c>None</c>.
    /// </returns>
    let tryFindLastSliceIndex (slice: _ []) (source: _ []) =
        let index = Internals.FindLastSliceIndex.arrayImpl slice source
        if index = -1 then None else Some index
    #endif

    /// <summary>
    /// Creates two arrays by applying the mapper function to each element in the array
    /// and classifying the transformed values depending on whether they were wrapped with Choice1Of2 or Choice2Of2.
    /// </summary>
    /// <returns>
    /// A tuple with both resulting arrays.
    /// </returns>
    let partitionMap (mapper: 'T -> Choice<'T1,'T2>) (source: array<'T>) =
        raiseIfNull (nameof source) source
        
        let (x, y) = ResizeArray (), ResizeArray ()
        Array.iter (mapper >> function Choice1Of2 e -> x.Add e | Choice2Of2 e -> y.Add e) source
        x.ToArray (), y.ToArray ()
        
    /// <summary>Safely build a new ResizeArray whose elements are the results of applying the given function
    /// to each of the elements of the two ResizeArrays pairwise.</summary>
    /// <remark>If one array is shorter, excess elements are discarded from the right end of the longer array.</remark>
    let map2Shortest f (a1: ResizeArray<_>) (a2: ResizeArray<_>) =
        raiseIfNull (nameof a1) a1
        raiseIfNull (nameof a2) a2
        
        let len = min a1.Count a2.Count
        let ra = ResizeArray(len)
        for i in 0..(len-1) do
            ra.Add (f a1[i] a2[i])
        ra
    
    /// <summary>Safely build a new ResizeArray whose elements are the results of applying the given function
    /// to each of the elements of the three ResizeArrays pairwise.</summary>
    /// <remark>If one array is shorter, excess elements are discarded from the right end of the longer array.</remark>
    let map3Shortest f (a1: ResizeArray<'T1>) (a2: ResizeArray<'T2>) (a3: ResizeArray<'T3>) =
        let len = min a1.Count a2.Count |> min a3.Count
        let ra = ResizeArray len
        for i in 0..(len-1) do
            ra.Add (f a1.[i] a2.[i] a3.[i])
        ra
    
    /// <summary>
    /// Zip safely two ResizeArrays. If one ResizeArray is shorter, excess elements are discarded from the right end of the longer ResizeArray. 
    /// </summary>
    /// <param name="a1">First input ResizeArray.</param>
    /// <param name="a2">Second input ResizeArray.</param>
    /// <returns>ResizeArray with corresponding pairs of input ResizeArrays.</returns>
    let zipShortest (a1: ResizeArray<'T1>) (a2: ResizeArray<'T2>) =
        raiseIfNull (nameof a1) a1
        raiseIfNull (nameof a2) a2
        
        let len = min a1.Count a2.Count
        let ra = ResizeArray(len)
        for i in 0..(len-1) do
            ra.Add (a1[i], a2[i])
        ra

    /// <summary>
    /// Zip safely three ResizeArrays. If one ResizeArray is shorter, excess elements are discarded from the right end of the longer ResizeArray.
    /// </summary>
    /// <param name="a1">First input ResizeArray.</param>
    /// <param name="a2">Second input ResizeArray.</param>
    /// <param name="a3">Third input ResizeArray.</param>
    /// <returns>ResizeArray with corresponding pairs of input ResizeArrays.</returns>
    let zip3Shortest (a1: ResizeArray<'T1>) (a2: ResizeArray<'T2>) (a3: ResizeArray<'T3>) =
        let len = min a1.Count a2.Count |> min a3.Count
        let ra = ResizeArray len
        for i in 0..(len-1) do
            ra.Add (a1.[i], a2.[i], a3.[i])
        ra
