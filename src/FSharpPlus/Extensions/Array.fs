namespace FSharpPlus

/// Additional operations on Array
[<RequireQualifiedAccess>]
module Array =

    open System

    /// <summary>Applies an array of functions to an array of values and concatenates them</summary>
    /// <param name="f">The array of functions.</param>
    /// <param name="x">The array of values.</param>
    /// <returns>A concatenated array of the resulting arrays from applying each function to each value</returns>
    /// 
    /// <example>
    /// <code>
    /// > Array.apply [|double; triple|] [|1; 2; 3|];;  
    /// val it : int [] = [|2; 4; 6; 3; 6; 9|]
    /// </code>
    /// </example>
    let apply f x =
        let lenf, lenx = Array.length f, Array.length x
        Array.init (lenf * lenx) (fun i -> f.[i / lenx] x.[i % lenx])

    /// Combines all values from the first array with the second, using the supplied mapping function.
    let lift2 f x y =
        let lenx, leny = Array.length x, Array.length y
        Array.init (lenx * leny) (fun i -> f x.[i / leny] y.[i % leny])

    /// Concatenates all elements, using the specified separator between each element.
    let intercalate (separator: _ []) (source: seq<_ []>) = source |> Seq.intercalate separator |> Seq.toArray

    /// Inserts a separator element between each element in the source array.
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

    /// <summary>
    /// Returns the index of the first occurrence of the specified slice in the source.
    /// Note: this is unsafe and will throw ArgumentException when the specified slice is not found.
    /// </summary>
    /// <returns>
    /// The index of the slice or <c>None</c>.
    /// </returns>
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