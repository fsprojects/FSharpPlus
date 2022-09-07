namespace FSharpPlus

/// Additional operations on List
[<RequireQualifiedAccess>]
module List =

    open System
    open FSharp.Core.CompilerServices

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
    let apply (f: list<'T -> 'U>) (x: list<'T>) : list<'U> =
    #if FABLE_COMPILER
        List.collect (fun f -> List.map ((<|) f) x) f
    #else
        let mutable coll = ListCollector<'U> ()
        f |> List.iter (fun f ->
            x |> List.iter (fun x ->
                coll.Add (f x)))
        coll.Close ()
    #endif

    /// Combines all values from the first list with the second, using the supplied mapping function.
    let lift2 (f: 'T1 -> 'T2 -> 'U) (x1: list<'T1>) (x2: list<'T2>) =
    #if FABLE_COMPILER
        List.allPairs x1 x2 |> List.map (fun (x, y) -> f x y)
    #else
        let mutable coll = ListCollector<'U> ()
        x1 |> List.iter (fun x1 ->
            x2 |> List.iter (fun x2 ->
                coll.Add (f x1 x2)))
        coll.Close ()
    #endif
    
    /// <summary>Combines values from three list and calls a mapping function on this combination.</summary>
    /// <param name="f">Mapping function taking three element combination as input.</param>
    /// <param name="x1">First list.</param>
    /// <param name="x2">Second list.</param>
    /// <param name="x3">Third list.</param>
    ///
    /// <returns>List with values returned from mapping function.</returns>
    let lift3 f x1 x2 x3 =
    #if !FABLE_COMPILER || FABLE_COMPILER_3
        List.allPairs x2 x3
        |> List.allPairs x1
        |> List.map (fun x -> (fst (snd x), snd (snd x), fst x))
        |> List.map (fun (x, y, z) -> f x y z)
    #else
        let mutable coll = ListCollector<'U> ()
        x1 |> List.iter (fun x1 ->
            x2 |> List.iter (fun x2 ->
                x3 |> List.iter (fun x3 ->
                    coll.Add (f x1 x2 x3))))
        coll.Close ()
    #endif

    /// Returns a list with all possible tails of the source list.
    let tails x = let rec loop = function [] -> [] | _::xs as s -> s::(loop xs) in loop x

    let take i list = FSharp.Collections.List.take i list

    let skip i list = FSharp.Collections.List.skip i list


    /// <summary>Returns a list that drops N elements of the original list and then yields the
    /// remaining elements of the list.</summary>
    /// <remarks>When count exceeds the number of elements in the list it
    /// returns an empty list instead of throwing an exception.</remarks>
    /// <param name="count">The number of items to drop.</param>
    /// <param name="source">The input list.</param>
    ///
    /// <returns>The result list.</returns>
    let drop count source = 
        let rec loop i lst = 
            match lst, i with
            | [] as x, _ | x, 0 -> x
            | x, n -> loop (n-1) (List.tail x)
        if count > 0 then loop count source else source

    /// Concatenates all elements, using the specified separator between each element.
    let intercalate (separator: list<'T>) (source: seq<list<'T>>) =
    #if FABLE_COMPILER
        source |> Seq.intercalate separator |> Seq.toList
    #else
        let mutable coll = new ListCollector<'T> ()
        let mutable notFirst = false
        source |> Seq.iter (fun element ->
            if notFirst then coll.AddMany separator
            coll.AddMany element
            notFirst <- true)
        coll.Close ()
    #endif

    /// Inserts a separator element between each element in the source list.
    let intersperse separator (source: list<'T>) =
    #if FABLE_COMPILER
        source |> List.toSeq |> Seq.intersperse separator |> Seq.toList
    #else
        let mutable coll = new ListCollector<'T> ()
        let mutable notFirst = false
        source |> List.iter (fun element ->
            if notFirst then coll.Add separator
            coll.Add element
            notFirst <- true)
        coll.Close ()
    #endif

    /// Creates a sequence of lists by splitting the source list on any of the given separators.
    let split (separators: seq<list<_>>) (source: list<_>) = source |> List.toSeq |> Seq.split separators |> Seq.map Seq.toList

    /// Replaces a subsequence of the source list with the given replacement list.
    let replace oldValue (newValue: _ list) (source: _ list) = source |> List.toSeq |> Seq.replace oldValue newValue |> Seq.toList : list<'T>

    /// <summary>Converts a list to an IReadOnlyList (from System.Collections.Generic).</summary>
    /// <param name="source">The list source</param>
    /// <returns>The list converted to a System.Collections.Generic.IReadOnlyList</returns>
    let toIReadOnlyList (source: _ list) =
        { new System.Collections.Generic.IReadOnlyList<_> with
            member _.Count = source.Length
            member _.Item with get index = source.[index]
            member _.GetEnumerator () = (source :> _ seq).GetEnumerator ()
            member _.GetEnumerator () = (source :> System.Collections.IEnumerable).GetEnumerator () }

    #if !FABLE_COMPILER || FABLE_COMPILER_3

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
    let partitionMap (mapping: 'T -> Choice<'T1, 'T2>) (source: list<'T>) =
    #if FABLE_COMPILER
        let rec loop ((acc1, acc2) as acc) = function
            | []    -> acc
            | x::xs ->
                match mapping x with
                | Choice1Of2 x -> loop (x::acc1, acc2) xs
                | Choice2Of2 x -> loop (acc1, x::acc2) xs
        loop ([], []) (List.rev source)
    #else
        let mutable coll1 = new ListCollector<'T1> ()
        let mutable coll2 = new ListCollector<'T2> ()
        List.iter (mapping >> function Choice1Of2 e -> coll1.Add e | Choice2Of2 e -> coll2.Add e) source
        coll1.Close (), coll2.Close ()
    #endif

    /// <summary>Safely build a new list whose elements are the results of applying the given function
    /// to each of the elements of the two lists pairwise.</summary>
    /// <param name="mapping">Mapping function.</param>
    /// <param name="list1">First input list.</param>
    /// <param name="list2">Second input list.</param>
    /// <returns>List with corresponding results of applying the mapping function pairwise over both input lists elments.</returns>
    /// <remark>If one list is shorter, excess elements are discarded from the right end of the longer list.</remark>
    let map2Shortest mapping (list1: list<'T1>) (list2: list<'T2>) : list<'U> =
    #if FABLE_COMPILER
        let rec loop acc = function
            | (l::ls, r::rs) -> loop ((mapping l r)::acc) (ls, rs)
            | (_, _)         -> acc
        loop [] (list1, list2) |> List.rev
    #else
        let mutable coll = new ListCollector<'U> ()
        let rec loop = function
            | ([], _) | (_, []) -> coll.Close ()
            | (l::ls, r::rs)    ->
                coll.Add (mapping l r)
                loop (ls, rs)
        loop (list1, list2)
    #endif
        
    /// <summary>
    /// Zip safely two lists. If one list is shorter, excess elements are discarded from the right end of the longer list. 
    /// </summary>
    /// <param name="list1">First input list.</param>
    /// <param name="list2">Second input list.</param>
    /// <returns>List with corresponding pairs of input lists.</returns>
    let zipShortest (list1: list<'T1>) (list2: list<'T2>) : list<'T1 * 'T2> =
    #if FABLE_COMPILER
        let rec loop acc = function
            | (l::ls, r::rs) -> loop ((l, r)::acc) (ls, rs)
            | (_, _)         -> acc
        loop [] (list1, list2) |> List.rev
    #else
        let mutable coll = new ListCollector<'T1 * 'T2> ()
        let rec loop = function
            | ([], _) | (_, []) -> coll.Close ()
            | (l::ls,r::rs) ->
                coll.Add (l, r)
                loop (ls,rs)
        loop (list1, list2)
    #endif
        
    /// <summary>Same as choose but with access to the index.</summary>
    /// <param name="mapping">The mapping function, taking index and element as parameters.</param>
    /// <param name="source">The input list.</param>
    ///
    /// <returns>List with values x for each List value where the function returns Some(x).</returns>
    let choosei mapping source =
        let mutable i = ref -1
        let fi x =
            i.Value <- i.Value + 1
            mapping i.Value x
        List.choose fi source
        
    /// <summary>Attempts to remove an item from a list.</summary>
    /// <param name="i">The index of the item to remove </param>
    /// <param name="lst">The input list</param>
    /// 
    /// <returns>For invalid indexes, the input list.  Otherwise, a new list with the item removed.</returns>
    let removeAt i lst = FSharp.Collections.List.removeAt i lst

    /// <summary>Updates the value of an item in a list</summary>
    /// <param name="i">The index of the item to update</param>
    /// <param name="x">The new value of the item</param>
    /// <param name="lst">The input list</param>
    ///
    /// <returns>A new list with the updated element</returns>
    let setAt i x lst = FSharp.Collections.List.updateAt i x lst
