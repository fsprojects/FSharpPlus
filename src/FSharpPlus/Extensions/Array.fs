namespace FSharpPlus

#nowarn "1204" // Suppress warning about using FSharp Compiler's error strings.

/// Additional operations on Array
[<RequireQualifiedAccess>]
module Array =

    open System
    open FSharp.Core.CompilerServices
    open FSharpPlus.Internals.Errors

    /// <summary>Adds an element to the beginning of the given array</summary>
    /// <param name="value">The element to add</param>
    /// <param name="array">The array to add to</param>
    /// <returns>A new array with the element added to the beginning.</returns>
    let cons value (array: 'T []) =
        let array = nullArgCheck (nameof array) array
        Array.insertAt 0 value array

    /// <summary>Splits the array in head and tail.</summary>
    /// <param name="array">The input array.</param>
    /// <returns>A tuple with the head and the tail of the original array.</returns>
    /// <exception cref="T:System.ArgumentException">Thrown when the input array is empty.</exception>
    let uncons (array: 'T []) =
        let array = nullArgCheck (nameof array) array
        if Array.isEmpty array then invalidArg (nameof array) LanguagePrimitives.ErrorStrings.InputSequenceEmptyString
        else array[0], array[1..]

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
    let apply (f: ('T -> 'U) []) (x: 'T []) : 'U [] =
        let x = nullArgCheck (nameof x) x

        let lenf, lenx = Array.length f, Array.length x
        Array.init (lenf * lenx) (fun i -> let (d, r) = Math.DivRem (i, lenx) in f.[d] x.[r])

    /// <summary>Combines all values from three arrays and calls a mapping function on this combination.</summary>
    /// <param name="mapping">Mapping function taking three element combination as input.</param>
    /// <param name="array1">First array.</param>
    /// <param name="array2">Second array.</param>
    ///
    /// <returns>Array with values returned from mapping function.</returns>
    let lift2 (mapping: 'T1 -> 'T2 -> 'U) (array1: 'T1 []) (array2: 'T2 []) : 'U [] =
        let array1 = nullArgCheck (nameof array1) array1
        let array2 = nullArgCheck (nameof array2) array2

        let lenx, leny = Array.length array1, Array.length array2
        Array.init (lenx * leny) (fun i -> let (d, r) = Math.DivRem (i, leny) in mapping array1.[d] array2.[r])
        
        
    /// <summary>Combines all values from three arrays and calls a mapping function on this combination.</summary>
    /// <param name="mapping">Mapping function taking three element combination as input.</param>
    /// <param name="array1">First array.</param>
    /// <param name="array2">Second array.</param>
    /// <param name="array3">Third array.</param>
    ///
    /// <returns>Array with values returned from mapping function.</returns>
    let lift3 (mapping: 'T1 -> 'T2 -> 'T3 -> 'U) (array1: 'T1 []) (array2: 'T2 []) (array3: 'T3 []) : 'U [] =
        let array1 = nullArgCheck (nameof array1) array1
        let array2 = nullArgCheck (nameof array2) array2
        let array3 = nullArgCheck (nameof array3) array3

        let lenx, leny, lenz = Array.length array1, Array.length array2, Array.length array3
        let combinedFirstTwo = Array.init (lenx * leny) (fun i -> let (d, r) = Math.DivRem (i, leny) in (array1.[d], array2.[r]))

        Array.init (lenx * leny * lenz) (fun i -> let (d, r) = Math.DivRem (i, lenz) in combinedFirstTwo.[d], array3.[r])
        |> Array.map (fun x -> mapping (fst (fst x)) (snd (fst x)) (snd x))

    /// Concatenates all elements, using the specified separator between each element.
    let intercalate (separator: 'T []) (source: seq<'T []>) : 'T [] =
        let separator = nullArgCheck (nameof separator) separator
        let source = nullArgCheck (nameof source) source

    #if FABLE_COMPILER
        source |> Seq.intercalate separator |> Seq.toArray
    #else
        let mutable coll = new ArrayCollector<'T> ()
        let mutable notFirst = false
        source |> Seq.iter (fun element ->
            if notFirst then coll.AddMany separator
            coll.AddMany element
            notFirst <- true)
        coll.Close ()
    #endif

    /// Inserts a separator element between each element in the source array.
    let intersperse element (source: 'T []) : 'T [] =
        let source = nullArgCheck (nameof source) source

        match source with
        | [||] -> [||]
        | _ ->
            let finalLength = Array.length source * 2 - 1
            Array.init finalLength (fun i ->
                match Math.DivRem (i, 2) with
                | i, 0 -> source.[i]
                | _    ->  element)

    /// Creates a sequence of arrays by splitting the source array on any of the given separators.
    let split (separators: seq<'T []>) (source: 'T []) : seq<'T []> =
        let separators = nullArgCheck (nameof separators) separators
        let source = nullArgCheck (nameof source) source

        source |> Array.toSeq |> Seq.split separators |> Seq.map Seq.toArray

    /// Replaces a subsequence of the source array with the given replacement array.
    let replace (oldValue: 'T []) (newValue: 'T []) (source: 'T[]) : 'T[] =
        let oldValue = nullArgCheck (nameof oldValue) oldValue
        let newValue = nullArgCheck (nameof newValue) newValue
        let source = nullArgCheck (nameof source) source

    #if FABLE_COMPILER
        source |> Array.toSeq |> Seq.replace oldValue newValue |> Seq.toArray: 'T []
    #else
        match source with
        | [||] -> [||]
        | _ ->
            let mutable candidate = new ArrayCollector<'T>()
            let mutable sourceIndex = 0

            while sourceIndex < source.Length do
                let sourceItem = source.[sourceIndex]

                if sourceItem = oldValue.[0]
                    && sourceIndex + newValue.Length <= source.Length then
                    let middleIndex = (oldValue.Length - 1) / 2
                    let mutable oldValueIndexLeft = 0

                    let mutable oldValueIndexRight =
                        oldValue.Length - 1

                    let mutable matchingElements =
                        source.[sourceIndex + oldValueIndexLeft] = oldValue.[oldValueIndexLeft]
                        && source.[sourceIndex + oldValueIndexRight] = oldValue.[oldValueIndexRight]

                    while oldValueIndexLeft <= middleIndex
                            && oldValueIndexRight >= middleIndex
                            && matchingElements do
                        matchingElements <-
                            source.[sourceIndex + oldValueIndexLeft] = oldValue.[oldValueIndexLeft]
                            && source.[sourceIndex + oldValueIndexRight] = oldValue.[oldValueIndexRight]

                        oldValueIndexLeft <- oldValueIndexLeft + 1
                        oldValueIndexRight <- oldValueIndexRight - 1

                    if matchingElements then
                        candidate.AddMany newValue
                        sourceIndex <- sourceIndex + oldValue.Length
                    else
                        candidate.Add sourceItem
                        sourceIndex <- sourceIndex + 1
                else
                    sourceIndex <- sourceIndex + 1
                    candidate.Add sourceItem

            candidate.Close()
    #endif

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
    let findSliceIndex (slice: 'T []) (source: 'T []) : int =
        let slice = nullArgCheck (nameof slice) slice
        let source = nullArgCheck (nameof source) source

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
    let tryFindSliceIndex (slice: 'T []) (source: 'T []) : int option =
        let slice = nullArgCheck (nameof slice) slice
        let source = nullArgCheck (nameof source) source

        let index = Internals.FindSliceIndex.arrayImpl slice source
        if index = -1 then None else Some index

    /// <summary>
    /// Returns the index of the last occurrence of the specified slice in the source.
    /// Note: this is unsafe and will throw ArgumentException when the specified slice is not found.
    /// </summary>
    /// <returns>
    /// The index of the slice or <c>None</c>.
    /// </returns>
    let findLastSliceIndex (slice: _ []) (source: _ []) : int =
        let slice = nullArgCheck (nameof slice) slice
        let source = nullArgCheck (nameof source) source

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
    let tryFindLastSliceIndex (slice: 'T []) (source: 'T []) : int option =
        let slice = nullArgCheck (nameof slice) slice
        let source = nullArgCheck (nameof source) source

        let index = Internals.FindLastSliceIndex.arrayImpl slice source
        if index = -1 then None else Some index
    #endif

    /// <summary>
    /// Creates two arrays by applying the mapper function to each element in the array
    /// and classifies the transformed values depending on whether they were wrapped with Choice1Of2 or Choice2Of2.
    /// </summary>
    /// <returns>
    /// A tuple with both resulting arrays.
    /// </returns>
    let partitionMap (mapper: 'T -> Choice<'T1, 'T2>) (source: array<'T>) : array<'T1> * array<'T2> =
        let source = nullArgCheck (nameof source) source

        let (x, y) = ResizeArray (), ResizeArray ()
        Array.iter (mapper >> function Choice1Of2 e -> x.Add e | Choice2Of2 e -> y.Add e) source
        x.ToArray (), y.ToArray ()
        
    /// <summary>Safely build a new array whose elements are the results of applying the given function
    /// to each of the elements of the two arrays pairwise.</summary>
    /// <remark>If one array is shorter, excess elements are discarded from the right end of the longer array.</remark>
    let map2Shortest (f: 'T1 -> 'T2 -> 'U) (a1: 'T1 []) (a2: 'T2 []) : 'U [] =
        let a1 = nullArgCheck (nameof a1) a1
        let a2 = nullArgCheck (nameof a2) a2

        Array.init (min a1.Length a2.Length) (fun i -> f a1.[i] a2.[i])
    
    /// <summary>Safely build a new array whose elements are the results of applying the given function
    /// to each of the elements of the three arrays pairwise.</summary>
    /// <remark>If one array is shorter, excess elements are discarded from the right end of the longer array.</remark>
    let map3Shortest (f: 'T1 -> 'T2 -> 'T3 -> 'U) (a1: 'T1 []) (a2: 'T2 []) (a3: 'T3 []) : 'U [] =
        let a1 = nullArgCheck (nameof a1) a1
        let a2 = nullArgCheck (nameof a2) a2
        let a3 = nullArgCheck (nameof a3) a3
        Array.init (min a1.Length a2.Length |> min a3.Length) (fun i -> f a1.[i] a2.[i] a3.[i])
    
    /// <summary>
    /// Zip safely two arrays. If one array is shorter, excess elements are discarded from the right end of the longer array. 
    /// </summary>
    /// <param name="a1">First input array.</param>
    /// <param name="a2">Second input array.</param>
    /// <returns>Array with corresponding pairs of input arrays.</returns>
    let zipShortest (a1: array<'T1>) (a2: array<'T2>) : array<'T1 * 'T2> =
        let a1 = nullArgCheck (nameof a1) a1
        let a2 = nullArgCheck (nameof a2) a2

        Array.init (min a1.Length a2.Length) (fun i -> a1.[i], a2.[i])

    /// <summary>
    /// Zip safely three arrays. If one array is shorter, excess elements are discarded from the right end of the longer array. 
    /// </summary>
    /// <param name="a1">First input array.</param>
    /// <param name="a2">Second input array.</param>
    /// <param name="a3">Third input array.</param>
    /// <returns>Array with corresponding tuple of input arrays.</returns>
    let zip3Shortest (a1: array<'T1>) (a2: array<'T2>) (a3: array<'T3>) : array<'T1 * 'T2 * 'T3> =
        let a1 = nullArgCheck (nameof a1) a1
        let a2 = nullArgCheck (nameof a2) a2
        let a3 = nullArgCheck (nameof a3) a3
        Array.init (min a1.Length a2.Length |> min a3.Length) (fun i -> a1.[i], a2.[i], a3.[i])

    /// <summary>Same as choose but with access to the index.</summary>
    /// <param name="mapping">The mapping function, taking index and element as parameters.</param>
    /// <param name="source">The input array.</param>
    ///
    /// <returns>Array with values x for each Array value where the function returns Some(x).</returns>
    let choosei (mapping: int -> 'T -> 'U option) (source: array<'T>) : array<'U> =
        let source = nullArgCheck (nameof source) source

        let mutable i = ref -1
        let fi x =
            i.Value <- i.Value + 1
            mapping i.Value x
        Array.choose fi source
