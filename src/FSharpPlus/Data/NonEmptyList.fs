namespace FSharpPlus.Data

open System.Runtime.InteropServices
open System.ComponentModel
open System.Collections.Generic
open FSharpPlus
open FSharpPlus.Control


/// A type-safe list that contains at least one element.
type NonEmptyList<'t> = {Head: 't; Tail: 't list} with
    interface IEnumerable<'t> with member x.GetEnumerator () = (let {Head = x; Tail = xs} = x in seq (x::xs)).GetEnumerator ()
    interface System.Collections.IEnumerable with member x.GetEnumerator () = (let {Head = x; Tail = xs} = x in seq (x::xs)).GetEnumerator () :> System.Collections.IEnumerator
    interface IReadOnlyCollection<'t>        with member s.Count = 1 + List.length s.Tail
    interface IReadOnlyList<'t>              with member s.Item with get index = s.Item index
    interface NonEmptySeq<'t>                with member s.First = s.Head

    [<System.Obsolete("Use Head instead.")>]
    member this.head = let {Head = a; Tail = _} = this in a

    [<System.Obsolete("Use Tail instead.")>]
    member this.tail = let           {Tail = a} = this in a

    member this.Item = function 0 -> this.Head | n -> this.Tail.[n-1]
    member this.GetSlice = function
        | None  , None
        | Some 0, None
        | Some 0, Some 0 -> this
        | Some a, None   -> let {Head = _; Tail = xs} = this in {Head = xs.[a-1]; Tail = xs.[a..   ]}
        | None  , Some b 
        | Some 0, Some b -> let {Head = x; Tail = xs} = this in {Head = x       ; Tail = xs.[ ..b-1]}
        | Some a, Some b -> let {Head = _; Tail = xs} = this in {Head = xs.[a-1]; Tail = xs.[a..b-1]}
    member this.Length = 1 + List.length this.Tail


/// A type alias for NonEmptyList<'t>
type nelist<'t> = NonEmptyList<'t>

/// Basic operations on NonEmptyList
[<RequireQualifiedAccess>]
module NonEmptyList =

    /// <summary>Builds a non empty list.</summary>
    let create x xs = {Head = x; Tail = xs}

    /// <summary>Builds a non empty list with a single element.</summary>
    let singleton x = {Head = x; Tail = []}

    /// <summary>Builds a list from the given non empty list.</summary>
    let toList {Head = x; Tail = xs} = x::xs

    /// <summary>Builds a sequence from the given non empty list.</summary>
    let toSeq  {Head = x; Tail = xs} = seq { yield x; yield! xs; }

    /// <summary>Builds an array from the given non empty list.</summary>
    let toArray nel = toList nel |> List.toArray

    /// <summary>Builds a non empty list from the given array.</summary>
    /// <param name="array">The input array.</param>
    /// <returns>Non empty list containing the elements of the array.</returns>
    /// <exception cref="System.ArgumentException">Thrown when the input array is empty.</exception>
    /// <remarks>Throws exception for empty array</remarks>
    let ofArray (array : _ array) =
        match array |> Array.toList with
        | []    -> invalidArg "array" "The input array was empty."
        | x::xs -> create x xs

    /// <summary>Builds a non empty list from the given list.</summary>
    /// <param name="list">The input list.</param>
    /// <returns>Non empty list containing the elements of the list.</returns>
    /// <exception cref="System.ArgumentException">Thrown when the input list is empty.</exception>
    /// <remarks>Throws exception for empty list</remarks>
    let ofList (list : _ list) =
        match list with
        | []    -> invalidArg "list" "The input list was empty."
        | x::xs -> create x xs

    /// <summary>Builds a non empty list from the given sequence.</summary>
    /// <param name="seq">The input list.</param>
    /// <returns>Non empty list containing the elements of the list.</returns>
    /// <exception cref="System.ArgumentException">Thrown when the input list is empty.</exception>
    /// <remarks>Throws exception for empty list</remarks>
    let ofSeq (seq : _ seq) =
        match seq |> Seq.toList with
        | []    -> invalidArg "seq" "The input sequence was empty."
        | x::xs -> create x xs

    /// <summary>Attempts to build a non empty list from the given array.</summary>
    /// <param name="array">The input array.</param>
    /// <returns>Non empty list containing the elements of the array. Returns None if no the input list is empty.</returns>
    /// 
    let tryOfArray (array : _ array) =
        match array |> Array.toList with
        | []    -> None
        | x::xs -> create x xs |> Some

    /// <summary>Attempts to build a non empty list from the given list.</summary>
    /// <param name="list">The input list.</param>
    /// <returns>Non empty list containing the elements of the list. Returns None if no the input list is empty.</returns>
    let tryOfList (list : _ list) =
        match list with
        | []    -> None
        | x::xs -> create x xs |> Some

    /// <summary>Attempts to build a non empty list from the given sequence.</summary>
    /// <param name="seq">The input list.</param>
    /// <returns>Non empty list containing the elements of the list. Returns None if no the input list is empty.</returns>
    let tryOfSeq (seq : _ seq) =
        match seq |> Seq.toList with
        | []    -> None
        | x::xs -> create x xs |> Some

    /// Returns the length of a non empty list. You can also use property nel.Length.
    let length (nel:_ NonEmptyList) = nel.Length
    
    /// <summary>Build a new non empty list whose elements are the results of applying the given function
    /// to each of the elements of the non empty list.</summary>
    let map f  {Head = x; Tail = xs} = {Head = f x; Tail = List.map f xs}
    
    /// <summary>Safely build a new non empty list whose elements are the results of applying the given function
    /// to each of the elements of the two non empty list pairwise.</summary>
    /// <remark>If one list is shorter, excess elements are discarded from the right end of the longer list.</remark>
    let map2Shortest f l1 l2 = { Head = f l1.Head l2.Head; Tail = List.map2Shortest f l1.Tail l2.Tail }

    /// <summary>Safely build a new non empty list whose elements are the results of applying the given function
    /// to each of the elements of the three non empty list pointwise.</summary>
    /// <remark>If one list is shorter, excess elements are discarded from the right end of the longer list.</remark>
    let map3Shortest f l1 l2 l3 = { Head = f l1.Head l2.Head l3.Head; Tail = List.map3Shortest f l1.Tail l2.Tail l3.Tail }
    
    /// <summary>Build a new non empty list whose elements are the results of applying the given function with index
    /// to each of the elements of the non empty list.</summary>
    let mapi f { Head = x; Tail = xs } =
        let mapperI = (fun i item -> f (i + 1) item)
        { Head = f 0 x
          Tail = List.mapi mapperI xs }

    /// <summary>Splits a list of pairs into two lists.</summary>
    /// <param name="list">The input list.</param>
    /// <returns>Two lists of split elements.</returns>
    let unzip (list: NonEmptyList<'T1 * 'T2>) = let t1, t2 = List.unzip list.Tail in {Head = fst list.Head  ; Tail = t1}, {Head = snd list.Head; Tail = t2}

    /// <summary>Combines the two lists into a list of pairs. The two lists must have equal lengths.</summary>
    /// <param name="list1">The first input list.</param>
    /// <param name="list2">The second input list.</param>
    /// <returns>A single list containing pairs of matching elements from the input lists.</returns>
    let zip (list1: NonEmptyList<'T>) (list2: NonEmptyList<'U>) = {Head = (list1.Head, list2.Head); Tail = List.zip list1.Tail list2.Tail}
    
    /// <summary>
    /// Zip safely two lists. If one list is shorter, excess elements are discarded from the right end of the longer list. 
    /// </summary>
    /// <param name="list1">First input list.</param>
    /// <param name="list2">Second input list.</param>
    /// <returns>List with corresponding pairs of input lists.</returns>
    let zipShortest (list1: NonEmptyList<'T>) (list2: NonEmptyList<'U>) =
        { Head = (list1.Head, list2.Head); Tail = List.zipShortest list1.Tail list2.Tail }
    
    /// <summary>Adds an element to the beginning of the given list</summary>
    /// <param name="value">The element to add</param>
    /// <param name="list">The list to add to</param>
    /// <returns>A new list with the element added to the beginning.</returns>
    let cons e { Head = x; Tail = xs } = { Head = e ; Tail = x::xs }

    /// <summary>Splits the list in head and tail.</summary>
    /// <param name="list">The input list.</param>
    /// <returns>A tuple with the head and the tail of the original list.</returns>
    /// <exception cref="T:System.ArgumentException">Thrown when the input list tail is empty.</exception>
    let uncons ({ Head = x; Tail = xs } as list) =
        match xs with
        | [] -> invalidArg (nameof(list)) "The input sequence has an empty tail"
        | _  -> x, ofList xs

    /// <summary>Splits the list in head and tail.</summary>
    /// <param name="list">The input list.</param>
    /// <returns>A tuple with the head and the tail of the original list.</returns>
    let unconsAsList ({ Head = x; Tail = xs } as list) = x, xs

    /// Returns the first element of a new non empty list. You can also use property nel.Head.
    let head {Head = x; Tail = _ } = x

    /// <summary>Returns a new NonEmptyList of the elements trailing the first element.</summary>
    /// <exception cref="System.ArgumentException">Thrown when the tail is empty.</exception>
    /// <remarks>Throws exception for empty tail</remarks>
    let tail ({ Head = _; Tail = xs } as list) =
        match xs with
        | [] -> invalidArg (nameof(list)) "The input sequence has an empty tail"
        | _  -> ofList xs

    /// <summary>Returns a new NonEmptyList of the elements trailing the first element or None.</summary>
    let tryTail { Head = _; Tail = xs } = tryOfList xs
    let rec tails s =
        let {Tail = xs} = s
        match xs with
        | []   -> {Head = s; Tail = []}
        | h::t -> cons s (tails {Head = h; Tail = t})

#if (!FABLE_COMPILER || FABLE_COMPILER_3) && !FABLE_COMPILER_4

    /// <summary>
    /// Maps each element of the list to an action, evaluates these actions from left to right and collect the results.
    /// </summary>
    let inline traverse (f: 'T -> '``Functor<'U>``) (source: NonEmptyList<'T>) =
        let lst = traverse f (toList source) : '``Functor<'List<'U>>``
        (create << List.head |> fun f x -> f x (List.tail x)) <!> lst : '``Functor<NonEmptyList<'U>>``

    /// <summary>
    /// Evaluates each action in the list from left to right and collect the results.
    /// </summary>
    let inline sequence (source: NonEmptyList<'``Functor<'T>``>)  : '``Functor<NonEmptyList<'T>>`` = traverse id source

    /// <summary>
    /// Maps each element of the list to an action, evaluates these actions from left to right, pointwise, and/or in parallel then collect results.
    /// </summary>
    let inline gather (f: 'T -> '``ZipFunctor<'U>``) (source: NonEmptyList<'T>) =
        Transpose.ForInfiniteSequences (Seq.map f source, IsZipLeftZero.Invoke, ofList, fun _ -> invalidOp "Unreacheable code.")

    /// <summary>
    /// Evaluates each action in the list from left to right, pointwise, and/or in parallel then collect results.
    /// </summary>
    let inline transpose (source: NonEmptyList<'``ZipFunctor<'T>``>)  : '``Functor<NonEmptyList<'T>>`` =
        Transpose.ForInfiniteSequences (source, IsZipLeftZero.Invoke, ofList, fun _ -> invalidOp "Unreacheable code.")

#endif

    /// <summary>Returns a new list that contains all pairings of elements from two lists.</summary>
    /// <param name="list1">The first input list.</param>
    /// <param name="list2">The second input list.</param>
    /// <returns>The resulting list of pairs.</returns>
    let inline allPairs (list1: NonEmptyList<'T>) (list2: NonEmptyList<'U>) = Seq.allPairs list1 list2 |> ofSeq

    /// <summary>Concatenates two lists.</summary>
    /// <param name="list1">The first input list.</param>
    /// <param name="list2">The second input list.</param>
    /// <returns>The resulting list.</returns>
    let inline append (list1: NonEmptyList<'T>) (list2: NonEmptyList<'T>) = 
        { Head = list1.Head; Tail = list1.Tail @ list2.Head :: list2.Tail }

    /// <summary>Returns the average of the elements in the list.</summary>
    /// <param name="list">The input list.</param>
    /// <returns>The resulting average.</returns>
    let inline average (list: NonEmptyList<'T>) = 
        Seq.average list

    /// <summary>Returns the average of the elements generated by applying the function to each element of the list.</summary>
    /// <param name="projection">The function to transform the list elements into the type to be averaged.</param>
    /// <param name="list">The input list.</param>
    /// <returns>The resulting average.</returns>
    let inline averageBy (projection: 'T -> ^U)  (list: NonEmptyList<'T>) = 
        Seq.averageBy projection list

    /// <summary>
    /// Applies a function to each element in a list and then returns a list of values v where the applied function returned Some(v).
    /// </summary>
    /// <param name="chooser">The function to be applied to the list elements.</param>
    /// <param name="list">The input list.</param>
    /// <returns>The resulting list comprising the values v where the chooser function returned Some(x).</returns>
    let inline tryChoose chooser (list: NonEmptyList<'T>) = 
        list |> Seq.choose chooser |> tryOfSeq

    /// <summary>
    /// Applies a function to each element in a list and then returns a list of values v where the applied function returned Some(v).
    /// </summary>
    /// <param name="chooser">The function to be applied to the list elements.</param>
    /// <param name="list">The input list.</param>
    /// <returns>The resulting list comprising the values v where the chooser function returned Some(x).</returns>
    /// <exception cref="System.ArgumentException">Thrown when the chooser function returns None for all elements.</exception>
    let inline choose chooser (list: NonEmptyList<'T>) = 
        list |> Seq.choose chooser |> ofSeq

    /// <summary>Divides the input list into lists (chunks) of size at most chunkSize.
    /// Returns a new list containing the generated lists (chunks) as its elements.</summary>
    /// <param name="chunkSize">The maximum size of each chunk.</param>
    /// <param name="list">The input list.</param>
    /// <returns>The list divided into chunks.</returns>
    let inline chunkBySize chunkSize (list: NonEmptyList<'T>): NonEmptyList<NonEmptyList<'T>> = 
        list.Head :: list.Tail |> List.chunkBySize chunkSize  |> List.map ofList |> ofList

    /// <summary>For each element of the list, applies the given function.
    /// Concatenates all the results and returns the combined list.</summary>
    /// <param name="mapping">The function to transform each input element into a sublist to be concatenated.</param>
    /// <param name="list">The input list.</param>
    /// <returns>The concatenation of the transformed sublists.</returns>
    let inline tryCollect mapping (list: NonEmptyList<'T>) = 
        list |> Seq.collect mapping |> tryOfSeq

    /// <summary>For each element of the list, applies the given function.
    /// Concatenates all the results and returns the combined list.</summary>
    /// <param name="mapping">The function to transform each input element into a sublist to be concatenated.</param>
    /// <param name="list">The input list.</param>
    /// <returns>The concatenation of the transformed sublists.</returns>
    /// <exception cref="System.ArgumentException">Thrown when the mapping function returns an empty list for all element.</exception>
    let inline collect mapping (list: NonEmptyList<'T>) = 
        list >>= mapping

    /// <summary>Returns a new list that contains the elements of each of the lists in order.</summary>
    /// <param name="lists">The input list of lists.</param>
    /// <returns>The resulting concatenated list.</returns>
    let inline concat (lists: NonEmptyList<NonEmptyList<'T>>) = 
        lists |> Seq.concat |> ofSeq

    /// <summary>Returns a new list that contains the elements of each of the lists in order.
    /// Returns None if all of the inner lists are empty.</summary>
    /// <param name="lists">The input list of lists.</param>
    /// <returns>The resulting concatenated list or None.</returns>
    let inline tryConcat (lists: NonEmptyList<#seq<'T>>) = 
        lists |> Seq.concat |> tryOfSeq

    /// <summary>Compares two lists using the given comparison function, element by element.</summary>
    /// <param name="comparer">A function that takes an element from each list and returns an int. If it evaluates to a non-zero value iteration is stopped and that value is returned.</param>
    /// <param name="list1">The first input list.</param>
    /// <param name="list2">The second input list.</param>
    /// <returns>Returns the first non-zero result from the comparison function.
    /// If the first list has a larger element, the return value is always positive.
    /// If the second list has a larger element, the return value is always negative.
    /// When the elements are equal in the two lists, 1 is returned if the first list is longer, 0 is returned if they are equal in length, and -1 is returned when the second list is longer.
    /// </returns>
    let inline compareWith comparer (list1: NonEmptyList<'T>) (list2: NonEmptyList<'T>) = 
        Seq.compareWith comparer list1 list2

    /// <summary>Tests if the list contains the specified element.</summary>
    /// <param name="value">The value to locate in the input list.</param>
    /// <param name="list">The input list.</param>
    /// <returns>True if the input list contains the specified element; false otherwise.</returns>
    let inline contains (value: 'T) (list: NonEmptyList<'T>) = 
        Seq.contains value list

    /// <summary>Applies a key-generating function to each element of a list and returns a list yielding unique keys and their number of occurrences in the original list.</summary>
    /// <param name="projection">A function transforming each item of the input list into a key to be compared against the others.</param>
    /// <param name="list">The input list.</param>
    /// <returns>The resulting list of unique keys and their number of occurrences.</returns>
    let inline countBy (projection: 'T -> 'U) (list: NonEmptyList<'T>) = 
        Seq.countBy projection list

    /// <summary>Returns a list that contains no duplicate entries according to the generic hash and equality comparisons
    /// on the keys returned by the given key-generating function.
    /// If an element occurs multiple times in the list then the later occurrences are discarded.</summary>
    /// <param name="list">The input list.</param>
    /// <returns>The resulting list without duplicates.</returns>
    let distinct (list: NonEmptyList<'T>) = 
        list |> Seq.distinct |> ofSeq

    /// <summary>Returns a list that contains no duplicate entries according to the generic hash and equality comparisons on the keys returned by the given key-generating function.
    /// If an element occurs multiple times in the list then the later occurrences are discarded.</summary>
    /// <param name="projection">A function transforming the list items into comparable keys.</param>
    /// <param name="list">The input list.</param>
    /// <returns>The resulting list.</returns>
    let inline distinctBy (projection: 'T -> 'U) (list: NonEmptyList<'T>) = 
        Seq.distinctBy projection list |> ofSeq

    /// <summary>Returns the only element of the list.</summary>
    /// <param name="list">The input list.</param>
    /// <returns>The only element of the list.</returns>
    /// <exception cref="System.ArgumentException">Thrown when the input does not have precisely one element.</exception>
    let inline exactlyOne (list: NonEmptyList<'T>) = 
        Seq.exactlyOne list

    /// <summary>Returns a new list with the distinct elements of the input list which do not appear in the itemsToExclude sequence, using generic hash and equality comparisons to compare values.</summary>
    /// <param name="itemsToExclude">The sequence of items to exclude from the input list.</param>
    /// <param name="list">The input list.</param>
    /// <returns>A list that contains the distinct elements of list that do not appear in itemsToExclude.</returns>
    /// <exception cref="System.ArgumentException">Thrown when itemsToExclude is null.</exception>
    let inline except (itemsToExclude: #seq<'T>) (list: NonEmptyList<'T>) = 
        Seq.except itemsToExclude list |> ofSeq

    /// <summary>Tests if any element of the list satisfies the given predicate.</summary>
    /// <param name="predicate">The function to test the input elements.</param>
    /// <param name="list">The input list.</param>
    /// <returns>True if any element satisfies the predicate.</returns>
    let inline exists (predicate: 'T -> bool) (list: NonEmptyList<'T>) = 
        Seq.exists predicate list

    /// <summary>Tests if any pair of corresponding elements of the lists satisfies the given predicate.</summary>
    /// <param name="predicate">The function to test the input elements.</param>
    /// <param name="list1">The first input list.</param>
    /// <param name="list2">The second input list.</param>
    /// <returns>True if any pair of elements satisfy the predicate.</returns>
    /// <exception cref="System.ArgumentException">Thrown when the input lists are of different lengths.</exception>    
    let inline exists2 (predicate: 'T1 -> 'T2 -> bool) (list1: NonEmptyList<'T1>) (list2: NonEmptyList<'T2>) = 
        Seq.exists2 predicate list1 list2

    /// <summary>Returns a new collection containing only the elements of the collection for which the given predicate returns "true."</summary>
    /// <param name="predicate">The function to test the input elements.</param>
    /// <param name="list">The input list.</param>
    /// <returns>A list containing only the elements that satisfy the predicate.</returns>
    let inline tryFilter (predicate: 'T -> bool) (list: NonEmptyList<'T>): NonEmptyList<'T> option = 
        list |> Seq.filter predicate |> tryOfSeq

    /// <summary>Returns a new collection containing only the elements of the collection for which the given predicate returns "true."</summary>
    /// <param name="predicate">The function to test the input elements.</param>
    /// <param name="list">The input list.</param>
    /// <returns>A list containing only the elements that satisfy the predicate.</returns>
    /// <exception cref="System.ArgumentException">Thrown when the predicate evaluates to false for all the elements of the list.</exception>
    let inline filter (predicate: 'T -> bool) (list: NonEmptyList<'T>): NonEmptyList<'T> = 
        list |> Seq.filter predicate |> ofSeq

    /// <summary>Returns the first element for which the given function returns True. 
    /// Raises <see cref="KeyNotFoundException"/> if no such element exists.</summary>
    /// <param name="predicate">The function to test the input elements.</param>
    /// <param name="list">The input list.</param>
    /// <returns>The first element that satisfies the predicate.</returns>
    /// <exception cref="KeyNotFoundException">Thrown if the predicate evaluates to false for all the elements of the list.</exception>
    let inline find (predicate: 'T -> bool) (list: NonEmptyList<'T>) = 
        Seq.find predicate list

    /// <summary>Returns the last element for which the given function returns True. 
    /// Raises <see cref="KeyNotFoundException"/> if no such element exists.</summary>
    /// <param name="predicate">The function to test the input elements.</param>
    /// <param name="list">The input list.</param>
    /// <returns>The first element that satisfies the predicate.</returns>
    /// <exception cref="KeyNotFoundException">Thrown if the predicate evaluates to false for all the elements of the list.</exception>
    let inline findBack (predicate: 'T -> bool) (list: NonEmptyList<'T>) = 
        Seq.findBack predicate list

    /// <summary>Returns the index of the first element in the list that satisfies the given predicate.
    /// Raises <see cref="KeyNotFoundException"/> if no such element exists.</summary>
    /// <param name="predicate">The function to test the input elements.</param>
    /// <param name="list">The input list.</param>
    /// <returns>The first element that satisfies the predicate.</returns>
    /// <exception cref="KeyNotFoundException">Thrown if the predicate evaluates to false for all the elements of the list.</exception>
    let inline findIndex (predicate: 'T -> bool) (list: NonEmptyList<'T>) = 
        Seq.findIndex predicate list

    /// <summary>Returns the index of the last element in the list that satisfies the given predicate. 
    /// Raises <see cref="KeyNotFoundException"/> if no such element exists.</summary>
    /// <param name="predicate">The function to test the input elements.</param>
    /// <param name="list">The input list.</param>
    /// <returns>The first element that satisfies the predicate.</returns>
    /// <exception cref="KeyNotFoundException">Thrown if the predicate evaluates to false for all the elements of the list.</exception>
    let inline findIndexBack (predicate: 'T -> bool) (list: NonEmptyList<'T>) = 
        Seq.findIndexBack predicate list

    /// <summary>Applies a function to each element of the collection, threading an accumulator argument through the computation.
    /// Take the second argument, and apply the function to it and the first element of the list.
    /// Then feed this result into the function along with the second element and so on.
    /// Return the final result.
    /// If the input function is <c>f</c> and the elements are <c>i0...iN</c> then computes <c>f (... (f s i0) i1 ...) iN</c>.</summary>
    /// <param name="folder">The function to update the state given the input elements.</param>
    /// <param name="state">The initial state.</param>
    /// <param name="list">The input list.</param>
    /// <returns>The final state value.</returns>
    let inline fold (folder: 'State -> 'T -> 'State) (state: 'State) (list: NonEmptyList<'T>) = 
        Seq.fold folder state list

    /// <summary>Applies a function to corresponding elements of two collections, threading an accumulator argument through the computation.
    /// The collections must have identical sizes.
    /// If the input function is <c>f</c> and the elements are <c>i0...iN</c> and <c>j0...jN</c> then computes <c>f (... (f s i0 j0)...) iN jN</c>.</summary>
    /// <param name="folder">The function to update the state given the input elements.</param>
    /// <param name="state">The initial state.</param>
    /// <param name="list1">The first input list.</param>
    /// <param name="list2">The second input list.</param>
    /// <returns>The final state value.</returns>
    let inline fold2 (folder: 'State -> 'T1 -> 'T2 -> 'State) (state: 'State) (list1: NonEmptyList<'T1>) (list2: NonEmptyList<'T2>) = 
        Seq.fold2 folder state list1 list2

    /// <summary>Applies a function to each element of the collection, starting from the end, threading an accumulator argument through the computation.
    /// Take the second argument, and apply the function to it and the first element of the list.
    /// Then feed this result into the function along with the second element and so on.
    /// Return the final result.
    /// If the input function is <c>f</c> and the elements are <c>i0...iN</c> then computes <c>f i0 (...(f iN s))</c>.</summary>
    /// <param name="folder">The function to update the state given the input elements.</param>
    /// <param name="list">The input list.</param>
    /// <param name="state">The initial state.</param>
    /// <returns>The final state value.</returns>
    let inline foldBack (folder: 'T -> 'State -> 'State) (list: NonEmptyList<'T>) (state: 'State) = 
        Seq.foldBack folder list state

    /// <summary>Applies a function to corresponding elements of two collections, threading an accumulator argument through the computation.
    /// The collections must have identical sizes.
    /// If the input function is <c>f</c> and the elements are <c>i0...iN</c> and <c>j0...jN</c> then computes <c>f (... (f s i0 j0)...) iN jN</c>.</summary>
    /// <param name="folder">The function to update the state given the input elements.</param>
    /// <param name="list1">The first input list.</param>
    /// <param name="list2">The second input list.</param>
    /// <param name="state">The initial state.</param>
    /// <returns>The final state value.</returns>
    let inline foldBack2 (folder: 'T1 -> 'T2 -> 'State -> 'State) (list1: NonEmptyList<'T1>) (list2: NonEmptyList<'T2>) (state: 'State) = 
        Seq.foldBack2 folder list1 list2 state

    /// <summary>Tests if all elements of the collection satisfy the given predicate.</summary>
    /// <param name="predicate">The function to test the input elements.</param>
    /// <param name="list">The input list.</param>
    /// <returns>True if all of the elements satisfy the predicate.</returns>
    let inline forall (predicate: 'T -> bool) (list: NonEmptyList<'T>) = 
        Seq.forall predicate list

    /// <summary>Tests if all corresponding elements of the collection satisfy the given predicate pairwise.</summary>
    /// <param name="predicate">The function to test the input elements.</param>
    /// <param name="list1">The first input list.</param>
    /// <param name="list2">The second input list.</param>
    /// <returns>True if all of the pairs of elements satisfy the predicate.</returns>
    /// <exception cref="System.ArgumentException">Thrown when the input lists differ in length.</exception>
    let inline forall2 (predicate: 'T1 -> 'T2 -> bool) (list1: NonEmptyList<'T1>) (list2: NonEmptyList<'T2>) = 
        Seq.forall2 predicate list1 list2

    /// <summary>Applies a key-generating function to each element of a list and yields a list of unique keys.
    /// Each unique key contains a list of all elements that match to this key.</summary>
    /// <param name="projection">A function that transforms an element of the list into a comparable key.</param>
    /// <param name="list">The input list.</param>
    /// <returns>The result list.</returns>
    let inline groupBy (projection: 'T -> 'U) (list: NonEmptyList<'T>) = 
        Seq.groupBy projection list
        |> Seq.map (fun (k, v) -> (k, ofSeq v))
        |> ofSeq

    /// <summary>Returns a list of each element in the list and its index.</summary>
    /// <param name="list">The input list.</param>
    /// <returns>The result list.</returns>
    let indexed (list: NonEmptyList<'T>) : NonEmptyList<int * 'T> = 
        Seq.indexed list |> ofSeq

    /// <summary>Creates a list by applying a function to each index.</summary>
    /// <param name="count">The number of elements to initialize.</param>
    /// <param name="initializer">A function that produces an element from an index.</param>
    /// <returns>The result list.</returns>
    /// <exception cref="System.ArgumentException">Thrown when count is less than or equal to zero.</exception>
    let init (count: int) (initializer: int -> 'T) : NonEmptyList<'T> = 
        Seq.init count initializer |> ofSeq

#if !NET45
    /// <summary>Inserts an element at the specified index.</summary>
    /// <param name="index">The index at which to insert the element.</param>
    /// <param name="value">The value to insert.</param>
    /// <param name="list">The input list.</param>
    /// <returns>The result list.</returns>
    let insertAt (index: int) (value: 'T) (list: NonEmptyList<'T>) : NonEmptyList<'T> = 
        Seq.insertAt index value list |> ofSeq

    /// <summary>Inserts multiple elements at the specified index.</summary>
    /// <param name="index">The index at which to insert the elements.</param>
    /// <param name="values">The values to insert.</param>
    /// <param name="list">The input list.</param>
    /// <returns>The result list.</returns>
    let insertManyAt (index: int) (values: seq<'T>) (list: NonEmptyList<'T>) : NonEmptyList<'T> = 
        Seq.insertManyAt index values list |> ofSeq
#endif

    /// <summary>Returns the element at the specified index.</summary>
    /// <param name="index">The index of the element to retrieve.</param>
    /// <param name="list">The input list.</param>
    /// <returns>The element at the specified index.</returns>
    /// <exception cref="System.ArgumentException">Thrown when index is out of range.</exception>
    let item (index: int) (list: NonEmptyList<'T>) : 'T = 
        Seq.item index list

    /// <summary>Applies a function to each element of the list.</summary>
    /// <param name="action">The function to apply to each element.</param>
    /// <param name="list">The input list.</param>
    let iter (action: 'T -> unit) (list: NonEmptyList<'T>) : unit = 
        Seq.iter action list

    /// <summary>Applies a function to each element of the two lists.</summary>
    /// <param name="action">The function to apply to each pair of elements.</param>
    /// <param name="list1">The first input list.</param>
    /// <param name="list2">The second input list.</param>
    let iter2 (action: 'T1 -> 'T2 -> unit) (list1: NonEmptyList<'T1>) (list2: NonEmptyList<'T2>) : unit = 
        Seq.iter2 action list1 list2

    /// <summary>Applies a function to each element of the list, passing the index of the element as the first argument to the function.</summary>
    /// <param name="action">The function to apply to each element and its index.</param>
    /// <param name="list">The input list.</param>
    let iteri (action: int -> 'T -> unit) (list: NonEmptyList<'T>) : unit = 
        Seq.iteri action list

    /// <summary>Applies a function to each element of the two lists, passing the index of the elements as the first argument to the function.</summary>
    /// <param name="action">The function to apply to each pair of elements and their index.</param>
    /// <param name="list1">The first input list.</param>
    /// <param name="list2">The second input list.</param>
    let iteri2 (action: int -> 'T1 -> 'T2 -> unit) (list1: NonEmptyList<'T1>) (list2: NonEmptyList<'T2>) : unit = 
        Seq.iteri2 action list1 list2

    /// <summary>Returns the last element of the list.</summary>
    /// <param name="list">The input list.</param>
    /// <returns>The last element of the list.</returns>
    let last (list: NonEmptyList<'T>) : 'T = 
        Seq.last list

    /// <summary>Applies a function to the corresponding elements of two lists, returning a list of the results.</summary>
    /// <param name="mapping">The function to apply to each pair of elements.</param>
    /// <param name="list1">The first input list.</param>
    /// <param name="list2">The second input list.</param>
    /// <returns>The result list.</returns>
    let map2 (mapping: 'T1 -> 'T2 -> 'U) (list1: NonEmptyList<'T1>) (list2: NonEmptyList<'T2>) : NonEmptyList<'U> = 
        Seq.map2 mapping list1 list2 |> ofSeq

    /// <summary>Applies a function to the corresponding elements of three lists, returning a list of the results.</summary>
    /// <param name="mapping">The function to apply to each triplet of elements.</param>
    /// <param name="list1">The first input list.</param>
    /// <param name="list2">The second input list.</param>
    /// <param name="list3">The third input list.</param>
    /// <returns>The result list.</returns>
    let map3 (mapping: 'T1 -> 'T2 -> 'T3 -> 'U) (list1: NonEmptyList<'T1>) (list2: NonEmptyList<'T2>) (list3: NonEmptyList<'T3>) : NonEmptyList<'U> = 
        Seq.map3 mapping list1 list2 list3 |> ofSeq

    /// <summary>Applies a function to each element of the list, threading an accumulator argument through the computation.</summary>
    /// <param name="mapping">The function to apply to each element and the accumulator.</param>
    /// <param name="state">The initial state.</param>
    /// <param name="list">The input list.</param>
    /// <returns>The result list and the final state.</returns>
    let mapFold (mapping: 'State -> 'T -> 'Result * 'State) (state: 'State) (list: NonEmptyList<'T>) : NonEmptyList<'Result> * 'State = 
        let result, state = Seq.mapFold mapping state list
        (ofSeq result, state)

    /// <summary>Applies a function to each element of the list, threading an accumulator argument through the computation. The function is applied to the elements of the list in reverse order.</summary>
    /// <param name="mapping">The function to apply to each element and the accumulator.</param>
    /// <param name="list">The input list.</param>
    /// <param name="state">The initial state.</param>
    /// <returns>The result list and the final state.</returns>
    let mapFoldBack (mapping: 'T -> 'State -> 'Result * 'State) (list: NonEmptyList<'T>) (state: 'State) : NonEmptyList<'Result> * 'State = 
        let result, state = Seq.mapFoldBack mapping list state
        (ofSeq result, state)

    /// <summary>Applies a function to each element of the two lists, passing the index of the elements as the first argument to the function.</summary>
    /// <param name="mapping">The function to apply to each pair of elements and their index.</param>
    /// <param name="list1">The first input list.</param>
    /// <param name="list2">The second input list.</param>
    /// <returns>The result list.</returns>
    let mapi2 (mapping: int -> 'T1 -> 'T2 -> 'U) (list1: NonEmptyList<'T1>) (list2: NonEmptyList<'T2>) : NonEmptyList<'U> = 
        Seq.mapi2 mapping list1 list2 |> ofSeq

    /// <summary>Returns the greatest of all elements of the list, compared via Operators.max.</summary>
    /// <param name="list">The input list.</param>
    /// <returns>The maximum element.</returns>
    let max (list: NonEmptyList<'T>) = 
        List.max (list.Head :: list.Tail)

    /// <summary>Returns the greatest of all elements of the list, compared via Operators.max on the function result.</summary>
    /// <param name="projection">The function to transform the list elements into the type to be compared.</param>
    /// <param name="list">The input list.</param>
    /// <returns>The maximum element.</returns>
    let maxBy (projection: 'T -> 'U) list = 
        List.maxBy projection (list.Head :: list.Tail)

    /// <summary>Returns the lowest of all elements of the list, compared via Operators.min.</summary>
    /// <param name="list">The input list.</param>
    /// <returns>The minimum value.</returns>
    let min (list: NonEmptyList<'T>) = 
        List.min (list.Head :: list.Tail)

    /// <summary>Returns the lowest of all elements of the list, compared via Operators.min on the function result</summary>
    /// <param name="projection">The function to transform list elements into the type to be compared.</param>
    /// <param name="list">The input list.</param>
    /// <returns>The minimum value.</returns>
    let minBy (projection: 'T -> 'U) list = 
        List.minBy projection (list.Head :: list.Tail)

    /// <summary>Returns a list of each pair of consecutive elements in the list.</summary>
    /// <param name="list">The input list.</param>
    /// <returns>The result list.</returns>
    let pairwise (list: NonEmptyList<'T>) : NonEmptyList<'T * 'T> = 
        Seq.pairwise list |> ofSeq

    /// <summary>Splits the list into two lists, containing the elements for which the given function returns
    /// <c>true</c> and <c>false</c> respectively.</summary>
    /// <param name="predicate">A function to test each element of the list.</param>
    /// <param name="list">The input list.</param>
    /// <returns>A tuple containing the two lists.</returns>
    let partition (predicate: 'T -> bool) (list: NonEmptyList<'T>) : 'T list * 'T list =
      list |> toList |> List.partition predicate |> fun (a, b) -> (a, b)

    /// <summary>Applies a function to each element of the list, returning a list of the results in a random order.</summary>
    /// <param name="permutation">A function to generate a permutation of the list indices.</param>
    /// <param name="list">The input list.</param>
    /// <returns>The result list.</returns>
    let permute (permutation: int -> int) (list: NonEmptyList<'T>) : NonEmptyList<'T> = 
        Seq.permute permutation list |> ofSeq

    /// <summary>Returns the first element for which the given function returns <c>Some</c>. If no such element exists, raises <c>KeyNotFoundException</c>.</summary>
    /// <param name="chooser">A function to transform elements of the list into options.</param>
    /// <param name="list">The input list.</param>
    /// <returns>The first chosen element.</returns>
    /// <exception cref="System.Collections.Generic.KeyNotFoundException">Thrown when no element is chosen.</exception>
    let pick (chooser: 'T -> 'U option) (list: NonEmptyList<'T>) : 'U = 
        Seq.pick chooser list

    /// <summary>Applies a function to each element of the list, threading an accumulator argument
    /// through the computation. Apply the function to the first two elements of the list.
    /// Then feed this result into the function along with the third element and so on. 
    /// Return the final result. If the input function is <c>f</c> and the elements are <c>i0...iN</c> then computes 
    /// <c>f (... (f i0 i1) i2 ...) iN</c>.</summary>
    /// <param name="reduction">The function to reduce two list elements to a single element.</param>
    /// <param name="list">The input list.</param>
    /// <returns>The final reduced value.</returns>
    let reduce (reduction: 'T -> 'T -> 'T)  (list: NonEmptyList<'T>) = 
        List.reduce reduction (list.Head :: list.Tail)

    /// <summary>Applies a function to each element of the list, starting from the end, threading an accumulator argument
    /// through the computation. If the input function is <c>f</c> and the elements are <c>i0...iN</c> then computes 
    /// <c>f i0 (...(f iN-1 iN))</c>.</summary>
    /// <param name="reduction">A function that takes in the next-to-last element of the list and the
    /// current accumulated result to produce the next accumulated result.</param>
    /// <param name="list">The input list.</param>
    /// <returns>The final result of the reductions.</returns>
    let reduceBack (reduction: 'T -> 'T -> 'T)  (list: NonEmptyList<'T>) = 
        List.reduceBack reduction (list.Head :: list.Tail)

    /// Equivalent to [start..stop] on regular lists.
    let inline range (start: 'T) stop = 
        create start (List.drop 1 [start..stop])

#if !NET45
    /// <summary>Removes the element at the specified index.</summary>
    /// <param name="index">The index of the element to remove.</param>
    /// <param name="list">The input list.</param>
    /// <returns>The resulting list.</returns>
    let tryRemoveAt (index: int) (list: NonEmptyList<'T>) : NonEmptyList<'T> option = 
        list |> Seq.removeAt index |> tryOfSeq

    /// <summary>Removes the element at the specified index.</summary>
    /// <param name="index">The index of the element to remove.</param>
    /// <param name="list">The input list.</param>
    /// <returns>The resulting list.</returns>
    /// <exception cref="System.ArgumentException">Thrown when removing the item results in an empty list.</exception>
    let removeAt (index: int) (list: NonEmptyList<'T>) : NonEmptyList<'T> = 
        list |> Seq.removeAt index |> ofSeq
    
    /// <summary>Removes multiple elements starting at the specified index.</summary>
    /// <param name="index">The index at which to start removing elements.</param>
    /// <param name="count">The number of elements to remove.</param>
    /// <param name="list">The input list.</param>
    /// <returns>The result list.</returns>
    let tryRemoveManyAt (index: int) (count: int) (list: NonEmptyList<'T>) : NonEmptyList<'T> option = 
        list |> Seq.removeManyAt index count |> tryOfSeq
    
    /// <summary>Removes multiple elements starting at the specified index.</summary>
    /// <param name="index">The index at which to start removing elements.</param>
    /// <param name="count">The number of elements to remove.</param>
    /// <param name="list">The input list.</param>
    /// <returns>The result list.</returns>
    /// <exception cref="System.ArgumentException">Thrown when removing the items results in an empty list.</exception>
    let removeManyAt (index: int) (count: int) (list: NonEmptyList<'T>) : NonEmptyList<'T> = 
        list |> Seq.removeManyAt index count |> ofSeq
#endif

    /// <summary>Creates a list that contains one repeated value.</summary>
    /// <param name="count">The number of elements.</param>
    /// <param name="value">The value to replicate.</param>
    /// <returns>The result list.</returns>
    let replicate (count: int) (value: 'T) : NonEmptyList<'T> = 
        Seq.replicate count value |> ofSeq
    
    /// <summary>Reverses the elements of the list.</summary>
    /// <param name="list">The input list.</param>
    /// <returns>The reversed list.</returns>
    let rev (list: NonEmptyList<'T>) : NonEmptyList<'T> = 
        Seq.rev list |> ofSeq
    
    /// <summary>Applies a function to each element of the list, threading an accumulator argument through the computation.</summary>
    /// <param name="folder">A function that updates the state with each element.</param>
    /// <param name="state">The initial state.</param>
    /// <param name="list">The input list.</param>
    /// <returns>The list of state values.</returns>
    let scan (folder: 'State -> 'T -> 'State) (state: 'State) (list: NonEmptyList<'T>) : NonEmptyList<'State> = 
        Seq.scan folder state list |> ofSeq
    
    /// <summary>Applies a function to each element of the list, threading an accumulator argument through the computation. The function is applied to the elements of the list in reverse order.</summary>
    /// <param name="folder">A function that updates the state with each element.</param>
    /// <param name="state">The initial state.</param>
    /// <param name="list">The input list.</param>
    /// <returns>The list of state values.</returns>
    let scanBack (folder: 'T -> 'State -> 'State) (list: NonEmptyList<'T>) (state: 'State) : NonEmptyList<'State> = 
        Seq.scanBack folder list state |> ofSeq
        
    /// <summary>Returns a list that skips the first N elements of the list.</summary>
    /// <param name="count">The number of elements to skip.</param>
    /// <param name="list">The input list.</param>
    /// <returns>The result list.</returns>
    let trySkip (count: int) (list: NonEmptyList<'T>) : NonEmptyList<'T> option = 
        list |> Seq.skip count |> tryOfSeq
        
    /// <summary>Returns a list that skips the first N elements of the list.</summary>
    /// <param name="count">The number of elements to skip.</param>
    /// <param name="list">The input list.</param>
    /// <returns>The result list.</returns>
    /// <exception cref="System.ArgumentException">Thrown when resulting list is empty.</exception>
    let skip (count: int) (list: NonEmptyList<'T>) : NonEmptyList<'T> = 
        list |> Seq.skip count |> ofSeq
    
    /// <summary>Returns a list that skips elements while the predicate is true.</summary>
    /// <param name="predicate">A function to test each element of the list.</param>
    /// <param name="list">The input list.</param>
    /// <returns>The result list.</returns>
    let trySkipWhile (predicate: 'T -> bool) (list: NonEmptyList<'T>) : NonEmptyList<'T> option = 
        list |> Seq.skipWhile predicate |> tryOfSeq
    
    /// <summary>Returns a list that skips elements while the predicate is true.</summary>
    /// <param name="predicate">A function to test each element of the list.</param>
    /// <param name="list">The input list.</param>
    /// <returns>The result list.</returns>
    /// <exception cref="System.ArgumentException">Thrown when resulting list is empty.</exception>
    let skipWhile (predicate: 'T -> bool) (list: NonEmptyList<'T>) : NonEmptyList<'T> = 
        list |> Seq.skipWhile predicate |> ofSeq
    
    /// <summary>Sorts the elements of the list in ascending order.</summary>
    /// <param name="list">The input list.</param>
    /// <returns>The sorted list.</returns>
    let sort (list: NonEmptyList<'T>) : NonEmptyList<'T> = 
        Seq.sort list |> ofSeq
    
    /// <summary>Sorts the elements of the list in ascending order, using the given projection for comparison.</summary>
    /// <param name="projection">A function to transform the list elements before comparison.</param>
    /// <param name="list">The input list.</param>
    /// <returns>The sorted list.</returns>
    let sortBy (projection: 'T -> 'Key) (list: NonEmptyList<'T>) : NonEmptyList<'T> = 
        Seq.sortBy projection list |> ofSeq
    
    /// <summary>Sorts the elements of the list in descending order, using the given projection for comparison.</summary>
    /// <param name="projection">A function to transform the list elements before comparison.</param>
    /// <param name="list">The input list.</param>
    /// <returns>The sorted list.</returns>
    let sortByDescending (projection: 'T -> 'Key) (list: NonEmptyList<'T>) : NonEmptyList<'T> = 
        Seq.sortByDescending projection list |> ofSeq
    
    /// <summary>Sorts the elements of the list in descending order.</summary>
    /// <param name="list">The input list.</param>
    /// <returns>The sorted list.</returns>
    let sortDescending (list: NonEmptyList<'T>) : NonEmptyList<'T> = 
        Seq.sortDescending list |> ofSeq
    
    /// <summary>Sorts the elements of the list using the given comparison function.</summary>
    /// <param name="comparer">A function to compare pairs of elements.</param>
    /// <param name="list">The input list.</param>
    /// <returns>The sorted list.</returns>
    let sortWith (comparer: 'T -> 'T -> int) (list: NonEmptyList<'T>) : NonEmptyList<'T> = 
        Seq.sortWith comparer list |> ofSeq
    
    /// <summary>Splits the list at the specified index.</summary>
    /// <param name="index">The index at which to split the list.</param>
    /// <param name="list">The input list.</param>
    /// <returns>A tuple containing the two lists.</returns>
    /// <exception cref="System.InvalidOperationException">Thrown when the index is 0, equal to the size of the list, or is larger than the list.</exception>
    let splitAt (index: int) (list: NonEmptyList<'T>) : NonEmptyList<'T> * NonEmptyList<'T> = 
        if index <= 0 then
            raise <| new System.InvalidOperationException("Index must be greater than 0.")
        else if index >= list.Length then
            raise <| new System.InvalidOperationException("Index must be less than the length of the list.")
        else
            list |> toList |> List.splitAt index |> fun (a, b) -> (ofList a, ofList b)
    
    /// <summary>Splits the list into the specified number of lists.</summary>
    /// <param name="count">The number of lists to create.</param>
    /// <param name="list">The input list.</param>
    /// <returns>A list of lists.</returns>
    let splitInto (count: int) (list: NonEmptyList<'T>) : NonEmptyList<NonEmptyList<'T>> = 
        Seq.splitInto count list |> Seq.map ofSeq |> ofSeq
    
    /// <summary>Computes the sum of the elements of the list.</summary>
    /// <param name="list">The input list.</param>
    /// <returns>The sum of the elements.</returns>
    let inline sum list = 
        Seq.sum list
    
    /// <summary>Computes the sum of the elements of the list, using the given projection.</summary>
    /// <param name="projection">A function to transform the list elements before summing.</param>
    /// <param name="list">The input list.</param>
    /// <returns>The sum of the transformed elements.</returns>
    let inline sumBy projection list = 
        Seq.sumBy projection list
        
    /// <summary>Returns a list that contains the first N elements of the list.</summary>
    /// <param name="count">The number of elements to take.</param>
    /// <param name="list">The input list.</param>
    /// <returns>The result list.</returns>
    let tryTake (count: int) (list: NonEmptyList<'T>) : NonEmptyList<'T> option = 
        Seq.take count list |> tryOfSeq
        
    /// <summary>Returns a list that contains the first N elements of the list.</summary>
    /// <param name="count">The number of elements to take.</param>
    /// <param name="list">The input list.</param>
    /// <returns>The result list.</returns>
    /// <exception cref="System.ArgumentException">Thrown when the count is less than or equal to zero.</exception>
    let take (count: int) (list: NonEmptyList<'T>) : NonEmptyList<'T> = 
        if count <= 0 then
            raise <| new System.ArgumentException("Count must be greater than 0.")
        else
        Seq.take count list |> ofSeq
    
    /// <summary>Returns a list that contains the elements of the list while the predicate is true.</summary>
    /// <param name="predicate">A function to test each element of the list.</param>
    /// <param name="list">The input list.</param>
    /// <returns>The result list.</returns>
    let tryTakeWhile (predicate: 'T -> bool) (list: NonEmptyList<'T>) : NonEmptyList<'T> option = 
        Seq.takeWhile predicate list |> tryOfSeq
    
    /// <summary>Returns a list that contains the elements of the list while the predicate is true.</summary>
    /// <param name="predicate">A function to test each element of the list.</param>
    /// <param name="list">The input list.</param>
    /// <returns>The result list.</returns>
    /// <exception cref="System.ArgumentException">Thrown when resulting list is empty.</exception>
    let takeWhile (predicate: 'T -> bool) (list: NonEmptyList<'T>) : NonEmptyList<'T> = 
        Seq.takeWhile predicate list |> ofSeq
    
    /// <summary>Truncates the list to the specified length.</summary>
    /// <param name="count">The maximum number of elements to include in the list.</param>
    /// <param name="list">The input list.</param>
    /// <returns>The truncated list.</returns>
    let tryTruncate (count: int) (list: NonEmptyList<'T>) : NonEmptyList<'T> option = 
        Seq.truncate count list |> tryOfSeq
    
    /// <summary>Truncates the list to the specified length.</summary>
    /// <param name="count">The maximum number of elements to include in the list.</param>
    /// <param name="list">The input list.</param>
    /// <returns>The truncated list.</returns>
    let truncate (count: int) (list: NonEmptyList<'T>) : NonEmptyList<'T> = 
        if count <= 0 then
            raise <| new System.ArgumentException("Count must be greater than 0.")
        else
        Seq.truncate count list |> ofSeq
    
    /// <summary>Returns the only element of the list, or <c>None</c> if the list does not contain exactly one element.</summary>
    /// <param name="list">The input list.</param>
    /// <returns>The only element of the list, or <c>None</c>.</returns>
    let tryExactlyOne (list: NonEmptyList<'T>) : 'T option = 
        Seq.tryExactlyOne list
    
    /// <summary>Returns the first element for which the given function returns <c>true</c>, or <c>None</c> if no such element exists.</summary>
    /// <param name="predicate">A function to test each element of the list.</param>
    /// <param name="list">The input list.</param>
    /// <returns>The first element for which the predicate returns <c>true</c>, or <c>None</c>.</returns>
    let tryFind (predicate: 'T -> bool) (list: NonEmptyList<'T>) : 'T option = 
        Seq.tryFind predicate list
    
    /// <summary>Returns the last element for which the given function returns <c>true</c>, or <c>None</c> if no such element exists.</summary>
    /// <param name="predicate">A function to test each element of the list.</param>
    /// <param name="list">The input list.</param>
    /// <returns>The last element for which the predicate returns <c>true</c>, or <c>None</c>.</returns>
    let tryFindBack (predicate: 'T -> bool) (list: NonEmptyList<'T>) : 'T option = 
        Seq.tryFindBack predicate list
    
    /// <summary>Returns the index of the first element for which the given function returns <c>true</c>, or <c>None</c> if no such element exists.</summary>
    /// <param name="predicate">A function to test each element of the list.</param>
    /// <param name="list">The input list.</param>
    /// <returns>The index of the first element for which the predicate returns <c>true</c>, or <c>None</c>.</returns>
    let tryFindIndex (predicate: 'T -> bool) (list: NonEmptyList<'T>) : int option = 
        Seq.tryFindIndex predicate list
    
    /// <summary>Returns the index of the last element for which the given function returns <c>true</c>, or <c>None</c> if no such element exists.</summary>
    /// <param name="predicate">A function to test each element of the list.</param>
    /// <param name="list">The input list.</param>
    /// <returns>The index of the last element for which the predicate returns <c>true</c>, or <c>None</c>.</returns>
    let tryFindIndexBack (predicate: 'T -> bool) (list: NonEmptyList<'T>) : int option = 
        Seq.tryFindIndexBack predicate list

    /// <summary>Returns the element at the specified index, or <c>None</c> if the index is out of range.</summary>
    /// <param name="index">The index of the element to retrieve.</param>
    /// <param name="list">The input list.</param>
    /// <returns>The element at the specified index, or <c>None</c>.</returns>
    let tryItem (index: int) (list: NonEmptyList<'T>) : 'T option = 
        Seq.tryItem index list

    /// <summary>Returns the last element of the list, or <c>None</c> if the list is empty.</summary>
    /// <param name="list">The input list.</param>
    /// <returns>The last element of the list, or <c>None</c>.</returns>
    let tryLast (list: NonEmptyList<'T>) : 'T option = 
        Seq.tryLast list
    
    /// <summary>Returns the first element for which the given function returns <c>Some</c>, or <c>None</c> if no such element exists.</summary>
    /// <param name="chooser">A function to transform elements of the list into options.</param>
    /// <param name="list">The input list.</param>
    /// <returns>The first chosen element, or <c>None</c>.</returns>
    let tryPick (chooser: 'T -> 'U option) (list: NonEmptyList<'T>) : 'U option = 
        Seq.tryPick chooser list
    
    /// <summary>Generates a list by repeatedly applying a function to a state.</summary>
    /// <param name="generator">A function that takes the current state and returns an option tuple of the next element and the next state.</param>
    /// <param name="state">The initial state.</param>
    /// <returns>The result list.</returns>
    let unfold (generator: 'State -> ('T * 'State) option) (state: 'State) : NonEmptyList<'T> = 
        Seq.unfold generator state |> ofSeq
    
    /// <summary>Splits a list of triples into three lists.</summary>
    /// <param name="list">The input list.</param>
    /// <returns>A tuple containing the three lists.</returns>
    let unzip3 (list: NonEmptyList<'T1 * 'T2 * 'T3>) : NonEmptyList<'T1> * NonEmptyList<'T2> * NonEmptyList<'T3> = 
        list |> toList |> List.unzip3 |> fun (a, b, c) -> (ofList a, ofList b, ofList c)

#if !NET45
    /// <summary>Updates the element at the specified index.</summary>
    /// <param name="index">The index of the element to update.</param>
    /// <param name="value">The new value.</param>
    /// <param name="list">The input list.</param>
    /// <returns>The result list.</returns>
    let updateAt (index: int) (value: 'T) (list: NonEmptyList<'T>) : NonEmptyList<'T> = 
        Seq.updateAt index value list |> ofSeq
#endif

    /// <summary>Returns a list that contains the elements of the list for which the given function returns <c>true</c>.</summary>
    /// <param name="predicate">A function to test each element of the list.</param>
    /// <param name="list">The input list.</param>
    /// <returns>The result list.</returns>
    let where (predicate: 'T -> bool) (list: NonEmptyList<'T>) : NonEmptyList<'T> = 
        Seq.where predicate list |> ofSeq
    
    /// <summary>Returns a list of sliding windows containing elements drawn from the list.</summary>
    /// <param name="windowSize">The number of elements in each window.</param>
    /// <param name="list">The input list.</param>
    /// <returns>The list of windows.</returns>
    let windowed (windowSize: int) (list: NonEmptyList<'T>) : NonEmptyList<NonEmptyList<'T>> = 
        Seq.windowed windowSize list |> Seq.map ofSeq |> ofSeq
    
    /// <summary>Combines the elements of three lists into a list of triples.</summary>
    /// <param name="list1">The first input list.</param>
    /// <param name="list2">The second input list.</param>
    /// <param name="list3">The third input list.</param>
    /// <returns>The list of triples.</returns>
    let zip3 (list1: NonEmptyList<'T1>) (list2: NonEmptyList<'T2>) (list3: NonEmptyList<'T3>) : NonEmptyList<'T1 * 'T2 * 'T3> = 
        Seq.zip3 list1 list2 list3 |> ofSeq

#if (!FABLE_COMPILER || FABLE_COMPILER_3) && !FABLE_COMPILER_4
    /// Reduces using alternative operator `<|>`.
    let inline choice (list: NonEmptyList<'``Alt<'T>``>) = reduce (<|>) list : '``Alt<'T>``
#endif

    let ofNonEmptySeq (s: NonEmptySeq<_>) =
      create s.First (Seq.tail s |> List.ofSeq)

    let toNonEmptySeq (list: NonEmptyList<_>) = list :> NonEmptySeq<_>

type NonEmptyList<'t> with
    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member Map (x: NonEmptyList<'a>, f: 'a->'b) = NonEmptyList.map f x
    
    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member MapIndexed (x: NonEmptyList<_>, f) = NonEmptyList.mapi f x
    
    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member Unzip s = NonEmptyList.unzip s

    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member Zip (x, y) = NonEmptyList.zipShortest x y
    
    static member (>>=) ({Head = x; Tail = xs}, f: _->NonEmptyList<'b>) =
        let {Head = y; Tail = ys} = f x
        let ys' = List.collect (NonEmptyList.toList << f) xs
        {Head = y; Tail = (ys @ ys')}

    static member Return (x: 'a) = {Head = x; Tail = []}
    static member (<*>)  (f: NonEmptyList<'T->'U>, x: NonEmptyList<'T>) =
        let r = NonEmptyList.toList f </List.apply/> NonEmptyList.toList x
        {Head = r.Head; Tail = r.Tail}

    static member Pure (x: 'a) = { Head = x; Tail = List.cycle [x] }
    static member (<.>)  (f: NonEmptyList<'T->'U>, x: NonEmptyList<'T>) = NonEmptyList.map2Shortest (<|) f x

    static member Lift2 (f: 'T -> 'U -> 'V, x, y) = NonEmptyList.ofList (List.lift2 f (NonEmptyList.toList x) (NonEmptyList.toList y))
    static member Lift3 (f: 'T -> 'U -> 'V -> 'W, x, y, z) = NonEmptyList.ofList (List.lift3 f (NonEmptyList.toList x) (NonEmptyList.toList y) (NonEmptyList.toList z))

    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member Map2 (f: 'T -> 'U -> 'V, x, y) = NonEmptyList.map2Shortest f x y

    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member Map3 (f: 'T -> 'U -> 'V -> 'W, x, y, z) = NonEmptyList.map3Shortest f x y z

    static member Extract   {Head = h; Tail = _} = h : 't

    #if (!FABLE_COMPILER || FABLE_COMPILER_3) && !FABLE_COMPILER_4
    static member Duplicate (s: NonEmptyList<'a>, [<Optional>]_impl: Duplicate) = NonEmptyList.tails s
    #endif

    static member (=>>)     (s, g) = NonEmptyList.map g (NonEmptyList.tails s) : NonEmptyList<'b>
    

    static member (+) ({Head = h; Tail = t},  x) = {Head = h; Tail = t @ NonEmptyList.toList x}

    static member Fold     ({Head = x; Tail = xs}, f, z) = List.fold     f z (x::xs)
    static member FoldBack ({Head = x; Tail = xs}, f, z) = List.foldBack f (x::xs) z
    static member Sum (source: seq<NonEmptyList<'T>>) = source |> Seq.map NonEmptyList.toList |> List.concat |> NonEmptyList.ofList

    #if (!FABLE_COMPILER || FABLE_COMPILER_3) && !FABLE_COMPILER_4
    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member ToList (s: NonEmptyList<'a>, [<Optional>]_impl: ToList) = NonEmptyList.toList s    

    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member ToSeq (s: NonEmptyList<'a>, [<Optional>]_impl: ToSeq ) = NonEmptyList.toList s |> List.toSeq

    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member inline Traverse (s: NonEmptyList<'T>, f: 'T -> '``Functor<'U>``) : '``Functor<NonEmptyList<'U>>`` = NonEmptyList.traverse f s

    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member inline Sequence (s: NonEmptyList<'``Functor<'T>``>) : '``Functor<NonEmptyList<'T>>`` = NonEmptyList.sequence s

    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member inline Gather (s: NonEmptyList<'T>, f: 'T -> '``Functor<'U>``) : '``Functor<NonEmptyList<'U>>`` = NonEmptyList.gather f s

    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member inline Transpose (s: NonEmptyList<'``Functor<'T>``>) : '``Functor<NonEmptyList<'T>>`` = NonEmptyList.transpose s

    static member Replace (source: NonEmptyList<'T>, oldValue: NonEmptyList<'T>, newValue: NonEmptyList<'T>, _impl: Replace ) =
        let lst = source |> NonEmptyList.toSeq |> Seq.replace oldValue newValue |> Seq.toList
        {Head = lst.Head; Tail = lst.Tail}

    static member Reduce ({Head = x; Tail = xs}, reduction: 'T -> 'T -> 'T) = List.reduce reduction (x :: xs)

    static member inline Choice (source: NonEmptyList<'``Alt<'T>``>) =
        use e = (NonEmptyList.toSeq source).GetEnumerator ()
        e.MoveNext() |> ignore
        let mutable res = e.Current
        while e.MoveNext() && not (IsAltLeftZero.Invoke res) do
            res <- Append.Invoke res e.Current
        res
    #endif


[<AutoOpen>]
module NonEmptyListBuilder =
    type NelBuilder () =
        [<CompilerMessage("A NonEmptyList doesn't support the Zero operation.", 708, IsError = true)>]
        member _.Zero () = raise Internals.Errors.exnUnreachable
        member _.Combine (a: 'T, { Head = b; Tail = c }) = { Head = a; Tail = b::c }
        member _.Yield x = x
        member _.Delay expr = expr ()
        member _.Run (x: NonEmptyList<_>) = x
        
    [<System.Obsolete("Use nelist instead.")>]
    let nel = NelBuilder ()
    
    let nelist = NelBuilder ()

[<AutoOpen>]
module NonEmptyListBuilderExtensions =
    type NelBuilder with
        member _.Combine (a: 'T, b: 'T) = { Head = a; Tail = [b] }
        member _.Run x = { Head = x; Tail = [] }
