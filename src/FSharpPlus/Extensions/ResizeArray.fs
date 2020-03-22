namespace FSharpPlus

/// Additional operations on Array
[<RequireQualifiedAccess>]
module ResizeArray =

    open System

    /// <summary>Builds a new ResizeArray whose elements are the results of applying the given function
    /// to each of the elements of the ResizeArray.</summary>
    ///
    /// <param name="mapping">A function to transform items from the input ResizeArray.</param>
    /// <param name="source">The input ResizeArray.</param>
    ///
    /// <returns>The result ResizeArray.</returns>
    ///
    /// <exception cref="System.ArgumentNullException">Thrown when the input ResizeArray is null.</exception>
    let map (f: 'T->'U) (x: ResizeArray<'T>) = ResizeArray (Seq.map f x)

    let apply (f: ResizeArray<'T->'U>) (x: ResizeArray<'T>) = ResizeArray (Seq.apply f x)

    /// Combine all values from the first ResizeArray with the second, using the supplied mapping function.
    let lift2 mapping (x1: ResizeArray<'T>) (x2: ResizeArray<'U>) = ResizeArray (Seq.lift2 mapping x1 x2)

    let intercalate (separator: _ []) (source: seq<_ []>) = source |> Seq.intercalate separator |> Seq.toArray

    /// Inserts a separator between each element in the source array.
    let intersperse element source = source |> Array.toSeq |> Seq.intersperse element |> Seq.toArray : 'T []

    /// Creates a sequence of arrays by splitting the source array on any of the given separators.
    let split (separators: seq<_ []>) (source: _ []) = source |> Array.toSeq |> Seq.split separators |> Seq.map Seq.toArray

    /// Replace a subsequence of the source array with the given replacement array.
    let replace (oldValue: _ []) (newValue: _ []) source = source |> Array.toSeq |> Seq.replace oldValue newValue |> Seq.toArray : 'T []

    /// <summary>
    /// Returns the index of the first occurrence of the specified slice in the source.
    /// </summary>
    /// <exception cref="System.ArgumentException">
    /// Thrown when the slice was not found in the sequence.
    /// </exception>
    /// <returns>
    /// The index of the slice.
    /// </returns>
    #if !FABLE_COMPILER

    let findSliceIndex (slice: _ []) (source: _ []) =
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
        let index = Internals.FindSliceIndex.arrayImpl slice source
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
        let (x, y) = ResizeArray (), ResizeArray ()
        Array.iter (mapper >> function Choice1Of2 e -> x.Add e | Choice2Of2 e -> y.Add e) source
        x.ToArray (), y.ToArray ()