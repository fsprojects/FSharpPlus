namespace FSharpPlus

/// Additional operations on List
[<RequireQualifiedAccess>]
module List =

    open System

    /// Creates a list with a single element.
    let singleton x = [x]

    /// <summary>Adds an element to the beginning of the given list</summary>
    /// <param name="x">The element to add</param>
    /// <param name="list">The list to add to</param>
    /// <returns>A concatenated list of the result lists of applying each function to each value</returns>
    let cons x list = x :: list 

    /// <summary>Applies a list of functions to a list of values and concatenates them</summary>
    /// <param name="f">The list of functions.</param>
    /// <param name="x">The list of values.</param>
    /// <returns>A concatenated list of the result lists of applying each function to each value</returns>
    /// 
    /// <example>
    /// <code>
    /// > List.apply [double; triple] [1; 2; 3];;  
    /// val it : int list = [2; 4; 6; 3; 6; 9]
    /// </code>
    /// </example>
    let apply f x = List.collect (fun f -> List.map ((<|) f) x) f

    /// Combines all values from the first list with the second, using the supplied mapping function.
    let lift2 f x1 x2 = List.allPairs x1 x2 |> List.map (fun (x, y) -> f x y)

    /// Returns a list with all possible tails of the source list.
    let tails x = let rec loop = function [] -> [] | _::xs as s -> s::(loop xs) in loop x

    let take i list = Seq.take i list |> Seq.toList

    let skip i list =
        let rec listSkip lst = function 
            | 0 -> lst 
            | n -> listSkip (List.tail lst) (n-1)
        listSkip list i


    /// <summary>Returns a list that drops N elements of the original list and then yields the
    /// remaining elements of the list.</summary>
    /// <remarks>When count exceeds the number of elements in the list it
    /// returns an empty list instead of throwing an exception.</remarks>
    /// <param name="count">The number of items to drop.</param>
    /// <param name="source">The input list.</param>
    ///
    /// <returns>The result list.</returns>
    let drop i list = 
        let rec loop i lst = 
            match lst, i with
            | [] as x, _ | x, 0 -> x
            | x, n -> loop (n-1) (List.tail x)
        if i > 0 then loop i list else list

    /// Concatenates all elements, using the specified separator between each element.
    let intercalate (separator: list<_>) (source: seq<list<_>>) = source |> Seq.intercalate separator |> Seq.toList

    /// Inserts a separator element between each element in the source list.
    let intersperse element source = source |> List.toSeq |> Seq.intersperse element |> Seq.toList : list<'T>

    /// Creates a sequence of lists by splitting the source list on any of the given separators.
    let split (separators: seq<list<_>>) (source: list<_>) = source |> List.toSeq |> Seq.split separators |> Seq.map Seq.toList

    /// Replaces a subsequence of the source list with the given replacement list.
    let replace oldValue (newValue: _ list) (source: _ list) = source |> List.toSeq |> Seq.replace oldValue newValue |> Seq.toList : list<'T>

    /// <summary>Converts a list to an IReadOnlyList (from System.Collections.Generic).</summary>
    /// <param name="source">The list source</param>
    /// <returns>The list converted to a System.Collections.Generic.IReadOnlyList</returns>
    let toIReadOnlyList (source: _ list) =
        { new System.Collections.Generic.IReadOnlyList<_> with
            member __.Count = source.Length
            member __.Item with get index = source.[index]
            member __.GetEnumerator () = (source :> _ seq).GetEnumerator ()
            member __.GetEnumerator () = (source :> System.Collections.IEnumerable).GetEnumerator () }

    #if !FABLE_COMPILER

    /// <summary>
    /// Gets the index of the first occurrence of the specified slice in the source.
    /// </summary>
    /// <exception cref="System.ArgumentException">
    /// Thrown when the slice was not found in the sequence.
    /// </exception>
    /// <returns>
    /// The index of the slice.
    /// </returns>
    let findSliceIndex (slice: _ list) (source: _ list) =
        let index = Internals.FindSliceIndex.listImpl slice source
        if index = -1 then
            ArgumentException("The specified slice was not found in the sequence.") |> raise
        else
            index

    /// <summary>
    /// Gets the index of the first occurrence of the specified slice in the source.
    /// Returns <c>None</c> if not found.
    /// </summary>
    /// <returns>
    /// The index of the slice or <c>None</c>.
    /// </returns>
    let tryFindSliceIndex (slice: _ list) (source: _ list) =
        let index = Internals.FindSliceIndex.listImpl slice source
        if index = -1 then None else Some index
    #endif

    /// <summary>
    /// Creates two lists by applying the mapping function to each element in the list
    /// and classifying the transformed values depending on whether they were wrapped with Choice1Of2 or Choice2Of2.
    /// </summary>
    /// <returns>
    /// A tuple with both resulting lists.
    /// </returns>
    let partitionMap (mapping: 'T -> Choice<'T1,'T2>) (source: list<'T>) =
        let rec loop ((acc1, acc2) as acc) = function
            | [] -> acc
            | x::xs ->
                match mapping x with
                | Choice1Of2 x -> loop (x::acc1, acc2) xs
                | Choice2Of2 x -> loop (acc1, x::acc2) xs
        loop ([], []) (List.rev source)


    /// <summary>Safely build a new list whose elements are the results of applying the given function
    /// to each of the elements of the two lists pairwise.</summary>
    /// <remark>If one list is shorter, excess elements are discarded from the right end of the longer list.</remark>
    let map2Shortest f (l1: list<_>) (l2: list<_>) =
        let rec loop acc = function
            | (l::ls,r::rs) -> loop ((f l r)::acc) (ls,rs)
            | (_,_) -> acc
        loop [] (l1,l2) |> List.rev
        