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
    /// Returns the length of a non empty list. You can also use property nel.Length.
    let length (nel:_ NonEmptyList) = nel.Length
    /// <summary>Build a new non empty list whose elements are the results of applying the given function
    /// to each of the elements of the non empty list.</summary>
    let map f  {Head = x; Tail = xs} = {Head = f x; Tail = List.map f xs}
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
    /// Returns a new NonEmptyList with the element added to the beginning.
    let cons e {Head = x; Tail = xs} = {Head = e ; Tail = x::xs}
    /// Returns the first element of a new non empty list. You can also use property nel.Head.
    let head {Head = x; Tail = _ } = x
    /// <summary>Returns a new NonEmptyList of the elements trailing the first element.</summary>
    /// <exception cref="System.ArgumentException">Thrown when the tail is empty.</exception>
    /// <remarks>Throws exception for empty tail</remarks>
    let tail {Head = _; Tail = xs } = ofList xs
    let rec tails s =
        let {Tail = xs} = s
        match xs with
        | []   -> {Head = s; Tail = []}
        | h::t -> cons s (tails {Head = h; Tail = t})

#if !FABLE_COMPILER
    let inline traverse (f: 'T->'``Functor<'U>``) (s: NonEmptyList<'T>) =
        let lst = traverse f (toList s) : '``Functor<'List<'U>>``
        (create << List.head |> fun f x -> f x (List.tail x)) <!> lst : '``Functor<NonEmptyList<'U>>``
#endif

    /// <summary>Returns the average of the elements in the list.</summary>
    /// <param name="list">The input list.</param>
    /// <returns>The resulting average.</returns>
    let inline average (list: NonEmptyList<'T>) = List.average (list.Head :: list.Tail)

    /// <summary>Returns the average of the elements generated by applying the function to each element of the list.</summary>
    /// <param name="projection">The function to transform the list elements into the type to be averaged.</param>
    /// <param name="list">The input list.</param>
    /// <returns>The resulting average.</returns>
    let inline averageBy (projection: 'T -> ^U) list = List.averageBy projection (list.Head :: list.Tail)

    /// <summary>Applies a function to each element of the list, threading an accumulator argument
    /// through the computation. Apply the function to the first two elements of the list.
    /// Then feed this result into the function along with the third element and so on. 
    /// Return the final result. If the input function is <c>f</c> and the elements are <c>i0...iN</c> then computes 
    /// <c>f (... (f i0 i1) i2 ...) iN</c>.</summary>
    /// <param name="reduction">The function to reduce two list elements to a single element.</param>
    /// <param name="list">The input list.</param>
    /// <returns>The final reduced value.</returns>
    let reduce (reduction: 'T -> 'T -> 'T) list = List.reduce reduction (list.Head :: list.Tail)

    /// <summary>Applies a function to each element of the list, starting from the end, threading an accumulator argument
    /// through the computation. If the input function is <c>f</c> and the elements are <c>i0...iN</c> then computes 
    /// <c>f i0 (...(f iN-1 iN))</c>.</summary>
    /// <param name="reduction">A function that takes in the next-to-last element of the list and the
    /// current accumulated result to produce the next accumulated result.</param>
    /// <param name="list">The input list.</param>
    /// <returns>The final result of the reductions.</returns>
    let reduceBack (reduction: 'T -> 'T -> 'T) list = List.reduceBack reduction (list.Head :: list.Tail)

    /// <summary>Returns the greatest of all elements of the list, compared via Operators.max.</summary>
    /// <param name="list">The input list.</param>
    /// <returns>The maximum element.</returns>
    let max (list: NonEmptyList<'T>) = List.max (list.Head :: list.Tail)

    /// <summary>Returns the greatest of all elements of the list, compared via Operators.max on the function result.</summary>
    /// <param name="projection">The function to transform the list elements into the type to be compared.</param>
    /// <param name="list">The input list.</param>
    /// <returns>The maximum element.</returns>
    let maxBy (projection: 'T -> 'U) list = List.maxBy projection (list.Head :: list.Tail)

    /// <summary>Returns the lowest of all elements of the list, compared via Operators.min.</summary>
    /// <param name="list">The input list.</param>
    /// <returns>The minimum value.</returns>
    let min (list: NonEmptyList<'T>) = List.min (list.Head :: list.Tail)

    /// <summary>Returns the lowest of all elements of the list, compared via Operators.min on the function result</summary>
    /// <param name="projection">The function to transform list elements into the type to be compared.</param>
    /// <param name="list">The input list.</param>
    /// <returns>The minimum value.</returns>
    let minBy (projection: 'T -> 'U) list = List.minBy projection (list.Head :: list.Tail)

    /// Equivalent to [start..stop] on regular lists.
    let inline range (start: 'T) stop = create start (List.drop 1 [start..stop])

#if !FABLE_COMPILER
    /// Reduces using alternative operator `<|>`.
    let inline choice (list: NonEmptyList<'``Alt<'T>``>) = reduce (<|>) list : '``Alt<'T>``
#endif

    /// Transforms a list to a NonEmptyList, returning an option to signal when the original list was empty.
    let tryOfList s =
        match s with
        | []    -> None
        | x::xs -> Some (create x xs)

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
    static member Zip (x, y) = NonEmptyList.zip x y

    static member (>>=) ({Head = x; Tail = xs}, f: _->NonEmptyList<'b>) =
        let {Head = y; Tail = ys} = f x
        let ys' = List.collect (NonEmptyList.toList << f) xs
        {Head = y; Tail = (ys @ ys')}

    static member Return (x: 'a) = {Head = x; Tail = []}
    static member (<*>)  (f: NonEmptyList<'T->'U>, x: NonEmptyList<'T>) =
        let r = NonEmptyList.toList f </List.apply/> NonEmptyList.toList x
        {Head = r.Head; Tail = r.Tail}

    static member Lift2 (f: 'T -> 'U -> 'V, x, y) = NonEmptyList.ofList (List.lift2 f (NonEmptyList.toList x) (NonEmptyList.toList y))

    static member Extract   {Head = h; Tail = _} = h : 't

    #if !FABLE_COMPILER
    static member Duplicate (s: NonEmptyList<'a>, [<Optional>]_impl: Duplicate) = NonEmptyList.tails s
    #endif

    static member (=>>)     (s, g) = NonEmptyList.map g (NonEmptyList.tails s) : NonEmptyList<'b>
    

    static member (+) ({Head = h; Tail = t},  x) = {Head = h; Tail = t @ NonEmptyList.toList x}

    static member FoldBack ({Head = x; Tail = xs}, f, z) = List.foldBack f (x::xs) z

    #if !FABLE_COMPILER
    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member ToList (s: NonEmptyList<'a>, [<Optional>]_impl: ToList) = NonEmptyList.toList s    

    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member ToSeq (s: NonEmptyList<'a>, [<Optional>]_impl: ToSeq ) = NonEmptyList.toList s |> List.toSeq

    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member inline Traverse (s: NonEmptyList<'T>, f: 'T->'``Functor<'U>``) : '``Functor<NonEmptyList<'U>>`` = NonEmptyList.traverse f s

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
        member __.Zero () = raise Internals.Errors.exnUnreachable
        member __.Combine (a: NonEmptyList<'T>, b) = a + b
        member __.Yield x = NonEmptyList.singleton x
        member __.Delay expr = expr () : NonEmptyList<'T>
    let nel = NelBuilder ()