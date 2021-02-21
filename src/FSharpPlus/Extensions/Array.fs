namespace FSharpPlus

/// Additional operations on Array
[<RequireQualifiedAccess>]
module Array =

    open System

    /// <summary>Applies an array of functions to an array of values and concatenates them.</summary>
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
        
        
    /// <summary>Combines all values from three arrays and calls a mapping function on this combination.</summary>
    /// <param name="mapping">Mapping function taking three element combination as input.</param>
    /// <param name="list1">First array.</param>
    /// <param name="list2">Second array.</param>
    /// <param name="list3">Third array.</param>
    ///
    /// <returns>Array with values returned from mapping function.</returns>
    let lift3 mapping list1 list2 list3 =
        let lenx, leny, lenz = Array.length list1, Array.length list2, Array.length list3
        let combinedFirstTwo = Array.init (lenx * leny) (fun i -> (list1.[i / leny], list2.[i % leny]))

        Array.init (lenx * leny * lenz) (fun i -> combinedFirstTwo.[i/leny], list3.[i%leny])
        |> Array.map (fun x -> mapping (fst (fst x)) (snd (fst x)) (snd x))

    /// Concatenates all elements, using the specified separator between each element.
    let intercalate (separator: _ []) (source: seq<_ []>) = source |> Seq.intercalate separator |> Seq.toArray

    /// Inserts a separator element between each element in the source array.
    let intersperse element source = source |> Array.toSeq |> Seq.intersperse element |> Seq.toArray : 'T []

    /// Creates a sequence of arrays by splitting the source array on any of the given separators.
    let split (separators: seq<_ []>) (source: _ []) = source |> Array.toSeq |> Seq.split separators |> Seq.map Seq.toArray

    /// Replaces a subsequence of the source array with the given replacement array.
    let replace oldValue newValue source = source |> Array.toSeq |> Seq.replace oldValue newValue |> Seq.toArray                : 'T []

    /// <summary>
    /// Returns the index of the first occurrence of the specified slice in the source.
    /// </summary>
    /// <exception cref="System.ArgumentException">
    /// Thrown when the slice was not found in the sequence.
    /// </exception>
    /// <returns>
    /// The index of the slice.
    /// </returns>
    #if !FABLE_COMPILER || FABLE_COMPILER_3

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
    /// and classifies the transformed values depending on whether they were wrapped with Choice1Of2 or Choice2Of2.
    /// </summary>
    /// <returns>
    /// A tuple with both resulting arrays.
    /// </returns>
    let partitionMap (mapper: 'T -> Choice<'T1,'T2>) (source: array<'T>) =
        let (x, y) = ResizeArray (), ResizeArray ()
        Array.iter (mapper >> function Choice1Of2 e -> x.Add e | Choice2Of2 e -> y.Add e) source
        x.ToArray (), y.ToArray ()
        
    /// <summary>Safely build a new array whose elements are the results of applying the given function
    /// to each of the elements of the two arrays pairwise.</summary>
    /// <remark>If one array is shorter, excess elements are discarded from the right end of the longer array.</remark>
    let map2Shortest f (a1: 'T []) (a2: 'U []) =
        Array.init (min a1.Length a2.Length) (fun i -> f a1.[i] a2.[i])
    
    /// <summary>
    /// Zip safely two arrays. If one array is shorter, excess elements are discarded from the right end of the longer array. 
    /// </summary>
    /// <param name="a1">First input array.</param>
    /// <param name="a2">Second input array.</param>
    /// <returns>Array with corresponding pairs of input arrays.</returns>
    let zipShortest (a1: array<'T1>) (a2: array<'T2>) =
        Array.init (min a1.Length a2.Length) (fun i -> a1.[i], a2.[i])

    /// <summary>Same as choose but with access to the index.</summary>
    /// <param name="mapping">The mapping function, taking index and element as parameters.</param>
    /// <param name="source">The input array.</param>
    ///
    /// <returns>Array with values x for each Array value where the function returns Some(x).</returns>
    let choosei mapping source =
        let mutable i = ref -1
        let fi x =
            incr i
            mapping !i x
        Array.choose fi source
