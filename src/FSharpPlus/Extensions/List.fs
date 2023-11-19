namespace FSharpPlus

/// Additional operations on List
[<RequireQualifiedAccess>]
module List =

    open System
    open FSharp.Core.CompilerServices

    /// <summary>Returns a list that contains one item only.</summary>
    ///
    /// <param name="value">The input item.</param>
    ///
    /// <returns>The result list of one item.</returns>
    ///
    /// <example id="singleton-1">
    /// <code lang="fsharp">
    /// List.singleton 7
    /// </code>
    /// Evaluates to <c>[ 7 ]</c>.
    /// </example>
    /// <remarks>
    /// Note: this function has since been added to FSharp.Core.
    /// It will be removed in next major release of FSharpPlus.
    /// </remarks>
    let singleton value = [value] : list<'T>

    /// <summary>Adds an element to the beginning of the given list</summary>
    /// <param name="value">The element to add</param>
    /// <param name="list">The list to add to</param>
    /// <returns>A concatenated list of the result lists of applying each function to each value</returns>
    /// <remarks>
    /// Note: this function has since been added to FSharp.Core.
    /// It will be removed in next major release of FSharpPlus.
    /// </remarks>
    let cons value list = value :: list : list<'T>

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
    #if !FABLE_COMPILER || FABLE_COMPILER_3 || FABLE_COMPILER_4
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
    let tails list = let rec loop = function [] -> [] | _::xs as s -> s::(loop xs) in loop list : list<list<'T>>


    /// <summary>Returns the first N elements of the list.</summary>
    /// <remarks>Throws <c>InvalidOperationException</c>
    /// if the count exceeds the number of elements in the list. <c>List.truncate</c>
    /// returns as many items as the list contains instead of throwing an exception.</remarks>
    ///
    /// <param name="count">The number of items to take.</param>
    /// <param name="list">The input list.</param>
    ///
    /// <returns>The result list.</returns>
    ///
    /// <exception cref="T:System.ArgumentException">Thrown when the input list is empty.</exception>
    /// <exception cref="T:System.InvalidOperationException">Thrown when count exceeds the number of elements
    /// in the list.</exception>
    ///
    /// <example id="take-1">
    /// <code lang="fsharp">
    /// let inputs = ["a"; "b"; "c"; "d"]
    ///
    /// inputs |> List.take 2
    /// </code>
    /// Evaluates to <c>["a"; "b"]</c>
    /// </example>
    ///
    /// <example id="take-2">
    /// <code lang="fsharp">
    /// let inputs = ["a"; "b"; "c"; "d"]
    ///
    /// inputs |> List.take 6
    /// </code>
    /// Throws <c>InvalidOperationException</c>.
    /// </example>
    ///
    /// <example id="take-3">
    /// <code lang="fsharp">
    /// let inputs = ["a"; "b"; "c"; "d"]
    ///
    /// inputs |> List.take 0
    /// </code>
    /// Evaluates to the empty list.
    /// </example>
    /// <remarks>
    /// Note: this function has since been added to FSharp.Core.
    /// It will be removed in next major release of FSharpPlus.
    /// </remarks>
    let take count list = FSharp.Collections.List.take<'T> count list

    /// <summary>Returns the list after removing the first N elements.</summary>
    ///
    /// <param name="count">The number of elements to skip. If the number is 0 or negative the input list is returned.</param>
    /// <param name="list">The input list.</param>
    ///
    /// <returns>The list after removing the first N elements.</returns>
    ///
    /// <exception cref="T:System.ArgumentException">Thrown when count exceeds the number of 
    /// elements in the list.</exception>
    ///
    /// <example id="skip-1">
    /// <code lang="fsharp">
    /// let inputs = ["a"; "b"; "c"; "d"]
    ///
    /// inputs |> List.skip 2
    /// </code>
    /// Evaluates to <c>["c"; "d"]</c>
    /// </example>
    ///
    /// <example id="skip-2">
    /// <code lang="fsharp">
    /// let inputs = ["a"; "b"; "c"; "d"]
    ///
    /// inputs |> List.skip 5
    /// </code>
    /// Throws <c>ArgumentException</c>.
    /// </example>
    ///
    /// <example id="skip-3">
    /// <code lang="fsharp">
    /// let inputs = ["a"; "b"; "c"; "d"]
    ///
    /// inputs |> List.skip -1
    /// </code>
    /// Evaluates to <c>["a"; "b"; "c"; "d"]</c>.
    /// </example>
    /// <remarks>
    /// Note: this function has since been added to FSharp.Core.
    /// It will be removed in next major release of FSharpPlus.
    /// </remarks>
    let skip count list = FSharp.Collections.List.skip<'T> count list


    /// <summary>Returns a list that drops N elements of the original list and then yields the
    /// remaining elements of the list.</summary>
    /// <remarks>When count exceeds the number of elements in the list it
    /// returns an empty list instead of throwing an exception.</remarks>
    /// <param name="count">The number of items to drop.</param>
    /// <param name="source">The input list.</param>
    ///
    /// <returns>The result list.</returns>
    let drop<'T> count source =
        let rec loop i lst = 
            match lst, i with
            | [] as x, _ | x, 0 -> x
            | x, n -> loop (n-1) (List.tail<'T> x)
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

    #if (!FABLE_COMPILER || FABLE_COMPILER_3) && !FABLE_COMPILER_4

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

    /// <summary>
    /// Chunks the list up into groups with the same projected key by applying
    /// the key-generating projection function to each element and yielding a list of 
    /// keys tupled with values.
    /// </summary>
    ///
    /// <remarks>
    /// Each key is tupled with an array of all adjacent elements that match 
    /// to the key, therefore keys are not unique but can't be adjacent
    /// as each time the key changes a new group is yield.
    /// 
    /// The ordering of the original list is respected.
    /// </remarks>
    ///
    /// <param name="projection">A function that transforms an element of the list into a comparable key.</param>
    /// <param name="source">The input list.</param>
    ///
    /// <returns>The resulting list of keys tupled with a list of matching values</returns>
    let chunkBy (projection: 'T -> 'Key) (source: _ list) =
        #if FABLE_COMPILER
        Seq.chunkBy projection source |> Seq.map (fun (x, y) -> x, Seq.toList y) |> Seq.toList
        #else
        match source with
        | [] -> []
        | x::xs ->
            let mutable acc = new ListCollector<_> ()
            let mutable members = new ListCollector<_> ()
            let rec loop source g =
                match source with
                | [] -> acc.Add (g, members.Close ())
                | x::xs ->
                    let key = projection x
                    if g <> key then
                        acc.Add (g, members.Close ())
                        members <- new ListCollector<_> ()
                    members.Add x
                    loop xs key
            members.Add x
            loop xs (projection x)
            acc.Close ()
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
    /// <returns>For invalid indexes, the input list. Otherwise, a new list with the item removed.</returns>
    /// <remarks>Use List.removeAt from FSharp.Core if you want to throw exceptions when using invalid indexes.</remarks>
    let deleteAt i lst =
         if List.length lst > i then
             lst.[0..i-1] @ lst.[i+1..]
         else lst

    [<Obsolete("This function was included in FSharp.Core but throwing. Use deletaAt instead or if you want to throw exceptions use the full path to removeAt in FSharp.Core until this function is removed from this library")>]
    let removeAt i lst = deleteAt i lst

    /// <summary>Updates the value of an item in a list</summary>
    /// <param name="i">The index of the item to update</param>
    /// <param name="x">The new value of the item</param>
    /// <param name="lst">The input list</param>
    ///
    /// <returns>A new list with the updated element</returns>
    /// <remarks>Use List.updateAt from FSharp.Core if you want to throw exceptions when using invalid indexes.</remarks>
    let setAt i x lst =
        if List.length lst > i && i >= 0 then
            lst.[0..i-1] @ x::lst.[i+1..]
        else lst
