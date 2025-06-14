namespace FSharpPlus.Data

open System
open System.Runtime.InteropServices
open System.ComponentModel
open System.Collections.Generic
open FSharpPlus
open FSharpPlus.Internals
open FSharpPlus.Extensions
open FSharpPlus.Control
open Microsoft.FSharp.Core.CompilerServices

/// A type-safe sequence that contains at least one element.
[<Interface>]
type NonEmptySeq<'t> =
    inherit IEnumerable<'t>
    abstract member First: 't

    static member inline private unsafeOfSeq (seq: _ seq) = {
        new NonEmptySeq<_> with
            member _.First = Seq.head seq
            member _.GetEnumerator() = seq.GetEnumerator()
            member _.GetEnumerator() = seq.GetEnumerator() :> Collections.IEnumerator }

    static member inline private ofList (list: _ list) = match list with [] -> invalidArg "list" "The input list was empty." | _ -> NonEmptySeq<_>.unsafeOfSeq list
    static member private collect (mapping: 'a -> '``#NonEmptySeq<'b>``) (source: NonEmptySeq<'a>) : NonEmptySeq<'b> when '``#NonEmptySeq<'b>`` :> NonEmptySeq<'b> = Seq.collect mapping source |> NonEmptySeq<_>.unsafeOfSeq    
    static member private concat (sources: NonEmptySeq<'``#NonEmptySeq<'a>``>) : NonEmptySeq<'a> when '``#NonEmptySeq<'a>`` :> NonEmptySeq<'a> = Seq.concat sources |> NonEmptySeq<'a>.unsafeOfSeq
    static member private allPairs (source1: _ NonEmptySeq) (source2: _ NonEmptySeq) = Seq.allPairs source1 source2 |> NonEmptySeq<_>.unsafeOfSeq    
    static member inline private append (source1: 'a NonEmptySeq) (source2: 'a NonEmptySeq) = Seq.append source1 source2 |> NonEmptySeq<'a>.unsafeOfSeq
    static member inline private map mapping (source: NonEmptySeq<_>) = source |> Seq.map mapping |> NonEmptySeq<_>.unsafeOfSeq
    static member private bind (mapping: 'T->NonEmptySeq<'U>) source = NonEmptySeq<'U>.collect mapping source
    static member private apply f x = NonEmptySeq<_>.bind (fun f -> NonEmptySeq<_>.map ((<|) f) x) f
    static member private lift2 f x1 x2 = NonEmptySeq<_>.allPairs x1 x2 |> NonEmptySeq<_>.map (fun (x, y) -> f x y)
    static member private lift3 f x1 x2 x3 =
        NonEmptySeq<_>.allPairs x2 x3
        |> NonEmptySeq<_>.allPairs x1
        |> NonEmptySeq<_>.map (fun x -> (fst (snd x), snd (snd x), fst x))
        |> NonEmptySeq<_>.map (fun (x, y, z) -> f x y z)

    static member private zip (source1: NonEmptySeq<_>) (source2: NonEmptySeq<_>) = Seq.zip source1 source2 |> NonEmptySeq<_>.unsafeOfSeq
    static member private singleton value = Seq.singleton value |> NonEmptySeq<_>.unsafeOfSeq
    static member private delay (generator: unit -> NonEmptySeq<'a>) : NonEmptySeq<'a> = Seq.delay (fun () -> generator () :> _) |> NonEmptySeq<_>.unsafeOfSeq
    

    static member (<|>) (x: NonEmptySeq<'T>, y) = NonEmptySeq<'T>.append x y

    #if !FABLE_COMPILER
    static member inline Choice (x: NonEmptySeq<'``Alternative<'T>``>) =
        use e = x.GetEnumerator ()
        e.MoveNext() |> ignore
        let mutable res = e.Current
        while e.MoveNext() && not (IsAltLeftZero.Invoke res) do
            res <- Append.Invoke res e.Current

    static member Head (x: NonEmptySeq<'T>, [<Optional>]_impl: Head) = x.First
    static member TryHead (x: NonEmptySeq<'T>,[<Optional>]_impl: TryHead) = Some x.First
    static member TryLast (x: NonEmptySeq<'T>, [<Optional>]_impl: TryLast)  = Some <| Seq.last x
    #endif

    static member Unzip (source: NonEmptySeq<'T * 'U>) = NonEmptySeq<_>.map fst source, NonEmptySeq<_>.map snd source

    static member (<*>) (f: NonEmptySeq<_>   , x: NonEmptySeq<'T>) : NonEmptySeq<'U> = NonEmptySeq<_>.apply f x
    static member Lift2 (f, x: NonEmptySeq<_>     , y: NonEmptySeq<_>) = NonEmptySeq<_>.lift2 f x y
    static member Lift3 (f, x: NonEmptySeq<_>     , y: NonEmptySeq<_>     , z: NonEmptySeq<_>) = NonEmptySeq<_>.lift3 f x y z
    static member IsLeftZero (_: NonEmptySeq<_>) = false

    

    static member Map (x: NonEmptySeq<_>, f: 'T -> 'U) = NonEmptySeq<_>.map f x      : NonEmptySeq<'U>

    static member Pure  (x: 'T) : NonEmptySeq<'T> = Seq.initInfinite (fun _ -> x) |> NonEmptySeq<_>.unsafeOfSeq
    static member (<.>) (f: NonEmptySeq<_>, x: NonEmptySeq<'T>) : NonEmptySeq<'U> = Seq.map2 (<|) f x |> NonEmptySeq<_>.unsafeOfSeq
    static member Map2  (f, x: NonEmptySeq<_>, y: NonEmptySeq<_>) = Seq.map2 f x y |> NonEmptySeq<_>.unsafeOfSeq
    static member Map3  (f, x: NonEmptySeq<_>, y: NonEmptySeq<_>, z: NonEmptySeq<_>) = Seq.map3 f x y z |> NonEmptySeq<_>.unsafeOfSeq
    static member IsZipLeftZero (_: NonEmptySeq<_>) = false

    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member ``<.>`` (struct (f: NonEmptySeq<_>, x: NonEmptySeq<'T> ), [<Optional>]_output: NonEmptySeq<'U>, [<Optional>]_mthd: ZipApply) = Seq.map2 (<|) f x |> NonEmptySeq<_>.unsafeOfSeq

    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member ``<*>`` (struct (f: NonEmptySeq<_>, x: NonEmptySeq<'T> ), [<Optional>]_output: NonEmptySeq<'U>, [<Optional>]_mthd: ZipApply) = NonEmptySeq<_>.apply f x
    
    static member Zip (x: NonEmptySeq<'T>, y: NonEmptySeq<'U>) = NonEmptySeq<_>.zip         x y
    static member (>>=) (source: NonEmptySeq<'T>, f: 'T -> NonEmptySeq<'U>) = NonEmptySeq<_>.collect f source : NonEmptySeq<'U>

    static member Join (x: NonEmptySeq<NonEmptySeq<'T>>) = NonEmptySeq<_>.concat x : NonEmptySeq<'T> 

    static member Return (x: 'a) = NonEmptySeq<_>.singleton x : NonEmptySeq<'a>

    static member Delay (x: unit-> _ ) = NonEmptySeq<_>.delay x : NonEmptySeq<'T>
    static member TryWith (computation: unit -> NonEmptySeq<_>, catchHandler: exn -> NonEmptySeq<_>) = seq (try (Seq.toArray (computation ())) with e -> Seq.toArray (catchHandler e)) |> NonEmptySeq<_>.unsafeOfSeq
    static member TryFinally (computation: unit -> NonEmptySeq<_>, compensation: unit -> unit) = seq { try for e in computation () do yield e finally compensation () } |> NonEmptySeq<_>.unsafeOfSeq
    static member Using (resource: 'T when 'T :> IDisposable, body: 'T -> NonEmptySeq<'U>) = seq { try for e in body resource do yield e finally if not (isNull (box resource)) then resource.Dispose () } |> NonEmptySeq<_>.unsafeOfSeq : NonEmptySeq<'U>
    static member inline (+) (x: _ NonEmptySeq             , y: _ NonEmptySeq) = NonEmptySeq<_>.append x y

    #if !FABLE_COMPILER

    static member inline Traverse (t: NonEmptySeq<'T>, f: 'T -> '``Functor<'U>``, [<Optional>]_output: '``Functor<NonEmptySeq<'U>>``, [<Optional>]_impl: Default2) : '``Functor<NonEmptySeq<'U>>`` =
        #if TEST_TRACE
        Traces.add "Traverse NonEmptySeq: NonEmptySeq, 'T -> Functor<'U>"
        #endif
        let mapped = NonEmptySeq<_>.map f t
        Sequence.ForInfiniteSequences (mapped, IsLeftZero.Invoke, NonEmptySeq<_>.ofList, Return.Invoke)

    static member Traverse (t: 'T NonEmptySeq, f: 'T -> Async<'U>) : Async<NonEmptySeq<_>> =
        #if TEST_TRACE
        Traces.add "Traverse NonEmptySeq: 'T NonEmptySeq, 'T -> Async<'U>"
        #endif
        async {
            let! ct = Async.CancellationToken
            return seq {
                use enum = t.GetEnumerator ()
                while enum.MoveNext() do
                    yield Async.RunSynchronously (f enum.Current, cancellationToken = ct) } |> NonEmptySeq<_>.unsafeOfSeq }

    static member inline Sequence (t: NonEmptySeq<'``Applicative<'T>``>) : '``Applicative<NonEmptySeq<'T>>`` =
        Sequence.ForInfiniteSequences (t, IsLeftZero.Invoke, NonEmptySeq<_>.ofList, Return.Invoke)



    static member inline Gather (t: NonEmptySeq<'T>, f: 'T -> '``Functor<'U>``, [<Optional>]_output: '``Functor<NonEmptySeq<'U>>``, [<Optional>]_impl: Default2) : '``Functor<NonEmptySeq<'U>>`` =
        #if TEST_TRACE
        Traces.add "Gather NonEmptySeq: NonEmptySeq, 'T -> Functor<'U>"
        #endif
        let mapped = NonEmptySeq<_>.map f t
        Transpose.ForInfiniteSequences (mapped, IsZipLeftZero.Invoke, NonEmptySeq<_>.ofList, Pure.Invoke)

    static member Gather (t: 'T NonEmptySeq, f: 'T -> Async<'U>) : Async<NonEmptySeq<_>> =
        #if TEST_TRACE
        Traces.add "Gather NonEmptySeq: 'T NonEmptySeq, 'T -> Async<'U>"
        #endif
        async {
            let! ct = Async.CancellationToken
            return seq {
                use enum = t.GetEnumerator ()
                while enum.MoveNext() do
                    yield Async.AsTask(f enum.Current, cancellationToken = ct).Result } |> NonEmptySeq<_>.unsafeOfSeq }

    static member inline Transpose (t: NonEmptySeq<'``ZipApplicative<'T>``>) : '``ZipApplicative<NonEmptySeq<'T>>`` =
        Transpose.ForInfiniteSequences (t, IsZipLeftZero.Invoke, NonEmptySeq<_>.ofList, Pure.Invoke)
    
    #endif





/// A type alias for NonEmptySeq<'t>
type neseq<'t> = NonEmptySeq<'t>

module NonEmptySeq =
    /// <summary>Builds a non empty sequence from the given sequence.</summary>
    /// <param name="seq">The input sequence.</param>
    /// <returns>Non empty sequence containing the elements of the original sequence.</returns>
    /// <remarks>
    ///   **This function does not check whether the sequence is actually non empty or not.**
    /// 
    ///   Use this function only if you are sure that the sequence is not empty and
    ///   you don't want to evaluate the first element of the sequence which would cause a
    ///   side effect.
    ///  
    ///   Otherwise, always use `ofSeq`. 
    /// </remarks>
    /// <seealso cref="ofSeq" />
    let unsafeOfSeq (seq: _ seq) =
        { new NonEmptySeq<_> with
            member _.First = Seq.head seq
            member _.GetEnumerator() = seq.GetEnumerator()
            member _.GetEnumerator() = seq.GetEnumerator() :> Collections.IEnumerator }
    
    let internal unsafeOfArray (x: _[]) =
        { new NonEmptySeq<_> with
            member _.First = x.[0]
            member _.GetEnumerator() = (x :> seq<_>).GetEnumerator()
            member _.GetEnumerator() = x.GetEnumerator() }

    /// <summary>Builds a non empty sequence from the given sequence.</summary>
    /// <param name="seq">The input sequence.</param>
    /// <returns>Non empty sequence containing the elements of the sequence.</returns>
    /// <exception cref="System.ArgumentException">Thrown when the input sequence is empty.</exception>
    /// <remarks>
    ///   Throws exception for empty sequence.
    /// 
    ///   Evaluates the first element of the sequence and may trigger side effects.
    ///   If you are sure that the sequence is not empty and want to avoid that, you can use `unsafeOfSeq` instead.
    /// </remarks>
    /// <seealso cref="unsafeOfSeq" />
    let ofSeq (seq: _ seq) =
        if isNull seq || Seq.isEmpty seq then invalidArg (nameof seq) "The input sequence was empty."
        else unsafeOfSeq seq

    /// Transforms a sequence to a NonEmptySeq, returning an option to signal when the original sequence was empty.
    let tryOfSeq (seq: _ seq) =
        if isNull seq || Seq.isEmpty seq then None
        else Some (unsafeOfSeq seq)

    /// <summary>Builds a non empty sequence from the given array.</summary>
    /// <param name="array">The input array.</param>
    /// <returns>Non empty sequence containing the elements of the array.</returns>
    /// <exception cref="System.ArgumentException">Thrown when the input array is empty.</exception>
    /// <remarks>Throws exception for empty array</remarks>
    let ofArray (array: _[]) =
        if isNull array || Array.isEmpty array then invalidArg (nameof array) "The input array was empty."
        else unsafeOfArray array

    /// Transforms a array to a NonEmptySeq, returning an option to signal when the original array was empty.
    let tryOfArray (array: _[]) =
        if isNull array || Array.isEmpty array then None
        else Some (unsafeOfArray array)

    /// <summary>Builds a non empty sequence from the given list.</summary>
    /// <param name="list">The input list.</param>
    /// <returns>Non empty sequence containing the elements of the list.</returns>
    /// <exception cref="System.ArgumentException">Thrown when the input list is empty.</exception>
    /// <remarks>Throws exception for empty list</remarks>
    let ofList (list: _ list) =
        match list with [] -> invalidArg (nameof list) "The input list was empty." | _ -> unsafeOfSeq list

    /// Transforms a list to a NonEmptySeq, returning an option to signal when the original list was empty.
    let tryOfList (list: _ list) =
        match list with [] -> None | _ -> unsafeOfSeq list |> Some
    
    /// <summary>Builds a non empty sequence.</summary>
    let create x xs = seq { yield x; yield! xs } |> unsafeOfSeq
    
    /// Creates a NonEmptySeq range, containing at least the first element of the range
    let (|..) starting ending = (if starting < ending then { starting .. ending } else Seq.singleton starting) |> unsafeOfSeq

    /// Creates a NonEmptySeq range, containing at least the last element of the range
    let (..|) starting ending = (if starting < ending then { starting .. ending } else Seq.singleton ending)   |> unsafeOfSeq
    
    
    /// <summary>Returns a new sequence that contains all pairings of elements from the first and second sequences.</summary>
    /// <param name="source1">The first sequence.</param>
    /// <param name="source2">The second sequence.</param>
    /// <returns>The result sequence.</returns>
    let allPairs (source1: _ NonEmptySeq) (source2: _ NonEmptySeq) =
        Seq.allPairs source1 source2 |> unsafeOfSeq

    /// <summary>Wraps the two given enumerations as a single concatenated
    /// enumeration.</summary>
    ///
    /// <remarks>The returned sequence may be passed between threads safely. However, 
    /// individual IEnumerator values generated from the returned sequence should not be accessed
    /// concurrently.</remarks>
    ///
    /// <param name="source1">The first sequence.</param>
    /// <param name="source2">The second sequence.</param>
    ///
    /// <returns>The result sequence.</returns>
    let append (source1: _ NonEmptySeq) (source2: _ NonEmptySeq) =
        Seq.append source1 source2 |> unsafeOfSeq

    let appendSeq (source: _ NonEmptySeq) (seq: _ seq) =
        Seq.append source seq |> unsafeOfSeq

    let appendSeqBack (seq: _ seq) (source: _ NonEmptySeq) =
        Seq.append seq source |> unsafeOfSeq

    /// <summary>Returns the average of the elements in the list.</summary>
    /// <param name="list">The input list.</param>
    /// <returns>The resulting average.</returns>
    let inline average (list: NonEmptySeq<'T>) = Seq.average list

    /// <summary>Returns the average of the elements generated by applying the function to each element of the list.</summary>
    /// <param name="projection">The function to transform the list elements into the type to be averaged.</param>
    /// <param name="list">The input list.</param>
    /// <returns>The resulting average.</returns>
    let inline averageBy (projection: 'T -> ^U)  (list: NonEmptySeq<'T>) = 
        Seq.averageBy projection list

    /// <summary>Returns a sequence that corresponds to a cached version of the input sequence.
    /// This result sequence will have the same elements as the input sequence. The result 
    /// can be enumerated multiple times. The input sequence will be enumerated at most 
    /// once and only as far as is necessary.    Caching a sequence is typically useful when repeatedly
    /// evaluating items in the original sequence is computationally expensive or if
    /// iterating the sequence causes side-effects that the user does not want to be
    /// repeated multiple times.
    ///
    /// Enumeration of the result sequence is thread safe in the sense that multiple independent IEnumerator
    /// values may be used simultaneously from different threads (accesses to 
    /// the internal lookaside table are thread safe). Each individual IEnumerator
    /// is not typically thread safe and should not be accessed concurrently.</summary>
    ///
    /// <remarks>Once enumeration of the input sequence has started,
    /// it's enumerator will be kept live by this object until the enumeration has completed.
    /// At that point, the enumerator will be disposed. 
    ///
    /// The enumerator may be disposed and underlying cache storage released by 
    /// converting the returned sequence object to type IDisposable, and calling the Dispose method
    /// on this object. The sequence object may then be re-enumerated and a fresh enumerator will
    /// be used.</remarks>
    ///
    /// <param name="source">The input sequence.</param>
    ///
    /// <returns>The result sequence.</returns>
    let cache (source: _ NonEmptySeq) = Seq.cache source |> unsafeOfSeq

    /// <summary>Wraps a loosely-typed System.Collections sequence as a typed sequence.</summary>
    /// <param name="source">The input sequence.</param>
    /// <returns>The result sequence.</returns>
    /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
    let cast (source: _ NonEmptySeq) = Seq.cast source |> unsafeOfSeq

    /// <summary>
    /// Applies a function to each element in a sequence and then returns a sequence of values v where the applied function returned Some(v).
    /// </summary>
    /// <param name="chooser">The function to be applied to the list elements.</param>
    /// <param name="source">The input sequence.</param>
    /// <returns>The resulting sequence comprising the values v where the chooser function returned Some(x).</returns>
    let inline tryChoose chooser (source: NonEmptySeq<'T>) = 
      source |> Seq.choose chooser |> tryOfSeq
    
    /// <summary>
    /// Applies a function to each element in a sequence and then returns a sequence of values v where the applied function returned Some(v).
    /// </summary>
    /// <param name="chooser">The function to be applied to the sequence elements.</param>
    /// <param name="source">The input sequence.</param>
    /// <returns>The resulting sequence comprising the values v where the chooser function returned Some(x).</returns>
    /// <exception cref="System.ArgumentException">Thrown when the chooser function returns None for all elements.</exception>
    let inline choose chooser (source: NonEmptySeq<'T>) = 
      source |> Seq.choose chooser |> ofSeq

    /// <summary>Divides the input sequence into sequences (chunks) of size at most chunkSize.
    /// Returns a new sequence containing the generated sequences (chunks) as its elements.</summary>
    /// <param name="chunkSize">The maximum size of each chunk.</param>
    /// <param name="source">The input sequence.</param>
    /// <returns>The sequence divided into chunks.</returns>
    let inline chunkBySize chunkSize (source: NonEmptySeq<'T>): NonEmptySeq<NonEmptySeq<'T>> = 
        source |> Seq.chunkBySize chunkSize |> Seq.map unsafeOfSeq |> unsafeOfSeq

    /// <summary>Applies the given function to each element of the sequence and concatenates all the
    /// results.</summary>
    ///
    /// <remarks>Remember sequence is lazy, effects are delayed until it is enumerated.</remarks>
    ///
    /// <param name="mapping">A function to transform elements of the input sequence into the sequences
    /// that will then be concatenated.</param>
    /// <param name="source">The input sequence.</param>
    ///
    /// <returns>The result sequence.</returns>
    let collect (mapping: 'a -> '``#NonEmptySeq<'b>``) (source: NonEmptySeq<'a>) : NonEmptySeq<'b> when '``#NonEmptySeq<'b>`` :> NonEmptySeq<'b> =
        Seq.collect mapping source |> unsafeOfSeq

    /// <summary>For each element of the list, applies the given function.
    /// Concatenates all the results and returns the combined list.</summary>
    /// <param name="mapping">The function to transform each input element into a sublist to be concatenated.</param>
    /// <param name="source">The input sequence.</param>
    /// <returns>The concatenation of the transformed sublists.</returns>
    let tryCollect (mapping: 'a -> #seq<'b>) (source: NonEmptySeq<'a>) : NonEmptySeq<'b> option = 
        Seq.collect mapping source |> tryOfSeq
    
    /// <summary>Compares two sequences using the given comparison function, element by element.</summary>
    /// <param name="comparer">A function that takes an element from each sequence and returns an int. If it evaluates to a non-zero value iteration is stopped and that value is returned.</param>
    /// <param name="source1">The first input sequence.</param>
    /// <param name="source2">The second input sequence.</param>
    /// <returns>Returns the first non-zero result from the comparison function.
    /// If the first sequence has a larger element, the return value is always positive.
    /// If the second sequence has a larger element, the return value is always negative.
    /// When the elements are equal in the two sequences, 1 is returned if the first sequence is longer, 0 is returned if they are equal in length, and -1 is returned when the second list is longer.
    /// </returns>
    let compareWith comparer (source1: NonEmptySeq<'T>) (source2: NonEmptySeq<'T>) = 
        Seq.compareWith comparer source1 source2
    
    /// <summary>Combines the given enumeration-of-enumerations as a single concatenated
    /// enumeration.</summary>
    ///
    /// <remarks>The returned sequence may be passed between threads safely. However, 
    /// individual IEnumerator values generated from the returned sequence should not be accessed concurrently.</remarks>
    ///
    /// <param name="sources">The input enumeration-of-enumerations.</param>
    ///
    /// <returns>The result sequence.</returns>
    let concat (sources: NonEmptySeq<'``#NonEmptySeq<'a>``>) : NonEmptySeq<'a> when '``#NonEmptySeq<'a>`` :> NonEmptySeq<'a> =
        Seq.concat sources |> unsafeOfSeq

    /// <summary>Returns a new list that contains the elements of each of the lists in order.
    /// Returns None if all of the inner lists are empty.</summary>
    /// <param name="sources">The input list of lists.</param>
    /// <returns>The resulting concatenated list or None.</returns>
    let inline tryConcat (sources: NonEmptySeq<#seq<'T>>) = 
        sources |> Seq.concat |> tryOfSeq

    /// <summary>Tests if the sequence contains the specified element.</summary>
    /// <param name="value">The value to locate in the input sequence.</param>
    /// <param name="source">The input sequence.</param>
    /// <returns>True if the input sequence contains the specified element; false otherwise.</returns>
    let inline contains (value: 'T) (source: NonEmptySeq<'T>) = 
        Seq.contains value source

    /// <summary>Applies a key-generating function to each element of a sequence and returns a sequence yielding unique keys and their number of occurrences in the original list.</summary>
    /// <param name="projection">A function transforming each item of the input sequence into a key to be compared against the others.</param>
    /// <param name="source">The input sequence.</param>
    /// <returns>The resulting sequence of unique keys and their number of occurrences.</returns>
    let inline countBy (projection: 'T -> 'U) (source: NonEmptySeq<'T>) = 
        Seq.countBy projection source

    /// <summary>Returns a sequence that is built from the given delayed specification of a
    /// sequence.</summary>
    ///
    /// <remarks>The input function is evaluated each time an IEnumerator for the sequence 
    /// is requested.</remarks>
    ///
    /// <param name="generator">The generating function for the sequence.</param>
    let delay (generator: unit -> NonEmptySeq<'a>) : NonEmptySeq<'a> = 
        Seq.delay (fun () -> generator () :> _) |> unsafeOfSeq

    /// <summary>Returns a sequence that contains no duplicate entries according to the generic hash and equality comparisons
    /// on the keys returned by the given key-generating function.
    /// If an element occurs multiple times in the sequence then the later occurrences are discarded.</summary>
    /// <param name="source">The input sequence.</param>
    /// <returns>The resulting sequence without duplicates.</returns>
    let distinct (source: NonEmptySeq<'T>) = 
        source |> Seq.distinct |> unsafeOfSeq

    /// <summary>Returns a sequence that contains no duplicate entries according to the generic hash and equality comparisons on the keys returned by the given key-generating function.
    /// If an element occurs multiple times in the sequence then the later occurrences are discarded.</summary>
    /// <param name="projection">A function transforming the sequence items into comparable keys.</param>
    /// <param name="source">The input sequence.</param>
    /// <returns>The resulting sequence.</returns>
    let inline distinctBy (projection: 'T -> 'U) (source: NonEmptySeq<'T>) = 
        Seq.distinctBy projection source |> unsafeOfSeq

    /// <summary>Returns the only element of the sequence.</summary>
    /// <param name="source">The input sequence.</param>
    /// <returns>The only element of the sequence.</returns>
    /// <exception cref="System.ArgumentException">Thrown when the input does not have precisely one element.</exception>
    let inline exactlyOne (source: NonEmptySeq<'T>) = 
        Seq.exactlyOne source

    /// <summary>Returns a new sequence with the distinct elements of the input sequence which do not appear in the itemsToExclude sequence, using generic hash and equality comparisons to compare values.</summary>
    /// <param name="itemsToExclude">The sequence of items to exclude from the input sequence.</param>
    /// <param name="source">The input sequence.</param>
    /// <returns>A sequence that contains the distinct elements of sequence that do not appear in itemsToExclude.</returns>
    /// <exception cref="System.ArgumentException">Thrown when itemsToExclude is null.</exception>
    let inline except (itemsToExclude: #seq<'T>) (source: NonEmptySeq<'T>) = 
        Seq.except itemsToExclude source |> ofSeq

    /// <summary>Tests if any element of the sequence satisfies the given predicate.</summary>
    /// <param name="predicate">The function to test the input elements.</param>
    /// <param name="source">The input sequence.</param>
    /// <returns>True if any element satisfies the predicate.</returns>
    let inline exists (predicate: 'T -> bool) (source: NonEmptySeq<'T>) = 
        Seq.exists predicate source

    /// <summary>Tests if any pair of corresponding elements of the sequences satisfies the given predicate.</summary>
    /// <param name="predicate">The function to test the input elements.</param>
    /// <param name="source1">The first input sequence.</param>
    /// <param name="source2">The second input sequence.</param>
    /// <returns>True if any pair of elements satisfy the predicate.</returns>
    /// <exception cref="System.ArgumentException">Thrown when the input sequences are of different lengths.</exception>    
    let inline exists2 (predicate: 'T1 -> 'T2 -> bool) (source1: NonEmptySeq<'T1>) (source2: NonEmptySeq<'T2>) = 
        Seq.exists2 predicate source1 source2

    /// <summary>Returns a new collection containing only the elements of the collection for which the given predicate returns "true."</summary>
    /// <param name="predicate">The function to test the input elements.</param>
    /// <param name="source">The input sequence.</param>
    /// <returns>A sequence containing only the elements that satisfy the predicate.</returns>
    let inline filter (predicate: 'T -> bool) (source: NonEmptySeq<'T>): NonEmptySeq<'T> = 
      source |> Seq.filter predicate |> ofSeq

    /// <summary>Returns a new collection containing only the elements of the collection for which the given predicate returns "true."</summary>
    /// <param name="predicate">The function to test the input elements.</param>
    /// <param name="source">The input sequence.</param>
    /// <returns>A sequence containing only the elements that satisfy the predicate.</returns>
    let inline tryFilter (predicate: 'T -> bool) (source: NonEmptySeq<'T>): NonEmptySeq<'T> option = 
      source |> Seq.filter predicate |> tryOfSeq

    /// <summary>Returns the first element for which the given function returns True. 
    /// Raises <see cref="KeyNotFoundException"/> if no such element exists.</summary>
    /// <param name="predicate">The function to test the input elements.</param>
    /// <param name="source">The input sequence.</param>
    /// <returns>The first element that satisfies the predicate.</returns>
    /// <exception cref="KeyNotFoundException">Thrown if the predicate evaluates to false for all the elements of the sequence.</exception>
    let inline find (predicate: 'T -> bool) (source: NonEmptySeq<'T>) = 
        Seq.find predicate source

    /// <summary>Returns the last element for which the given function returns True. 
    /// Raises <see cref="KeyNotFoundException"/> if no such element exists.</summary>
    /// <param name="predicate">The function to test the input elements.</param>
    /// <param name="source">The input sequence.</param>
    /// <returns>The first element that satisfies the predicate.</returns>
    /// <exception cref="KeyNotFoundException">Thrown if the predicate evaluates to false for all the elements of the sequence.</exception>
    let inline findBack (predicate: 'T -> bool) (source: NonEmptySeq<'T>) = 
        Seq.findBack predicate source

    /// <summary>Returns the index of the first element in the sequence that satisfies the given predicate.
    /// Raises <see cref="KeyNotFoundException"/> if no such element exists.</summary>
    /// <param name="predicate">The function to test the input elements.</param>
    /// <param name="source">The input sequence.</param>
    /// <returns>The first element that satisfies the predicate.</returns>
    /// <exception cref="KeyNotFoundException">Thrown if the predicate evaluates to false for all the elements of the sequence.</exception>
    let inline findIndex (predicate: 'T -> bool) (source: NonEmptySeq<'T>) = 
        Seq.findIndex predicate source

    /// <summary>Returns the index of the last element in the sequence that satisfies the given predicate. 
    /// Raises <see cref="KeyNotFoundException"/> if no such element exists.</summary>
    /// <param name="predicate">The function to test the input elements.</param>
    /// <param name="source">The input sequence.</param>
    /// <returns>The first element that satisfies the predicate.</returns>
    /// <exception cref="KeyNotFoundException">Thrown if the predicate evaluates to false for all the elements of the sequence.</exception>
    let inline findIndexBack (predicate: 'T -> bool) (source: NonEmptySeq<'T>) = 
        Seq.findIndexBack predicate source

    /// <summary>Applies a function to each element of the collection, threading an accumulator argument through the computation.
    /// Take the second argument, and apply the function to it and the first element of the sequence.
    /// Then feed this result into the function along with the second element and so on.
    /// Return the final result.
    /// If the input function is <c>f</c> and the elements are <c>i0...iN</c> then computes <c>f (... (f s i0) i1 ...) iN</c>.</summary>
    /// <param name="folder">The function to update the state given the input elements.</param>
    /// <param name="state">The initial state.</param>
    /// <param name="source">The input sequence.</param>
    /// <returns>The final state value.</returns>
    let inline fold (folder: 'State -> 'T -> 'State) (state: 'State) (source: NonEmptySeq<'T>) = 
        Seq.fold folder state source

    /// <summary>Applies a function to corresponding elements of two collections, threading an accumulator argument through the computation.
    /// The collections must have identical sizes.
    /// If the input function is <c>f</c> and the elements are <c>i0...iN</c> and <c>j0...jN</c> then computes <c>f (... (f s i0 j0)...) iN jN</c>.</summary>
    /// <param name="folder">The function to update the state given the input elements.</param>
    /// <param name="state">The initial state.</param>
    /// <param name="source1">The first input sequence.</param>
    /// <param name="source2">The second input sequence.</param>
    /// <returns>The final state value.</returns>
    let inline fold2 (folder: 'State -> 'T1 -> 'T2 -> 'State) (state: 'State) (source1: NonEmptySeq<'T1>) (source2: NonEmptySeq<'T2>) = 
        Seq.fold2 folder state source1 source2

    /// <summary>Applies a function to each element of the collection, starting from the end, threading an accumulator argument through the computation.
    /// Take the second argument, and apply the function to it and the first element of the sequence.
    /// Then feed this result into the function along with the second element and so on.
    /// Return the final result.
    /// If the input function is <c>f</c> and the elements are <c>i0...iN</c> then computes <c>f i0 (...(f iN s))</c>.</summary>
    /// <param name="folder">The function to update the state given the input elements.</param>
    /// <param name="source">The input sequence.</param>
    /// <param name="state">The initial state.</param>
    /// <returns>The final state value.</returns>
    let inline foldBack (folder: 'T -> 'State -> 'State) (source: NonEmptySeq<'T>) (state: 'State) = 
        Seq.foldBack folder source state

    /// <summary>Applies a function to corresponding elements of two collections, threading an accumulator argument through the computation.
    /// The collections must have identical sizes.
    /// If the input function is <c>f</c> and the elements are <c>i0...iN</c> and <c>j0...jN</c> then computes <c>f (... (f s i0 j0)...) iN jN</c>.</summary>
    /// <param name="folder">The function to update the state given the input elements.</param>
    /// <param name="source1">The first input sequence.</param>
    /// <param name="source2">The second input sequence.</param>
    /// <param name="state">The initial state.</param>
    /// <returns>The final state value.</returns>
    let inline foldBack2 (folder: 'T1 -> 'T2 -> 'State -> 'State) (source1: NonEmptySeq<'T1>) (source2: NonEmptySeq<'T2>) (state: 'State) = 
        Seq.foldBack2 folder source1 source2 state

    /// <summary>Tests if all elements of the collection satisfy the given predicate.</summary>
    /// <param name="predicate">The function to test the input elements.</param>
    /// <param name="source">The input sequence.</param>
    /// <returns>True if all of the elements satisfy the predicate.</returns>
    let inline forall (predicate: 'T -> bool) (source: NonEmptySeq<'T>) = 
        Seq.forall predicate source

    /// <summary>Tests if all corresponding elements of the collection satisfy the given predicate pairwise.</summary>
    /// <param name="predicate">The function to test the input elements.</param>
    /// <param name="source1">The first input sequence.</param>
    /// <param name="source2">The second input sequence.</param>
    /// <returns>True if all of the pairs of elements satisfy the predicate.</returns>
    /// <exception cref="System.ArgumentException">Thrown when the input sequences differ in length.</exception>
    let inline forall2 (predicate: 'T1 -> 'T2 -> bool) (source1: NonEmptySeq<'T1>) (source2: NonEmptySeq<'T2>) = 
      Seq.forall2 predicate source1 source2

    /// <summary>Applies a key-generating function to each element of a sequence and yields a sequence of unique keys.
    /// Each unique key contains a sequence of all elements that match to this key.</summary>
    /// <param name="projection">A function that transforms an element of the sequence into a comparable key.</param>
    /// <param name="source">The input sequence.</param>
    /// <returns>The result sequence.</returns>
    let inline groupBy (projection: 'T -> 'U) (source: NonEmptySeq<'T>) = 
        Seq.groupBy projection source
        |> Seq.map (fun (k, v) -> (k, unsafeOfSeq v))
        |> unsafeOfSeq

    /// <summary>Returns the first element of the sequence.</summary>
    ///
    /// <param name="source">The input sequence.</param>
    ///
    /// <returns>The first element of the sequence.</returns>
    let head (source: NonEmptySeq<'a>) = source.First

    /// <summary>Builds a new collection whose elements are the corresponding elements of the input collection
    /// paired with the integer index (from 0) of each element.</summary>
    /// <param name="source">The input sequence.</param>
    /// <returns>The result sequence.</returns>
    let indexed (source: NonEmptySeq<_>) = 
        Seq.indexed source |> unsafeOfSeq

    /// <summary>Creates a sequence by applying a function to each index.</summary>
    /// <param name="count">The number of elements to initialize.</param>
    /// <param name="initializer">A function that produces an element from an index.</param>
    /// <returns>The result sequence.</returns>
    /// <exception cref="System.ArgumentException">Thrown when count is less than or equal to zero.</exception>
    let init (count: int) (initializer: int -> 'T) : NonEmptySeq<'T> = 
        Seq.init count initializer |> unsafeOfSeq

    /// <summary>Generates a new sequence which, when iterated, will return successive
    /// elements by calling the given function.    The results of calling the function
    /// will not be saved, that is the function will be reapplied as necessary to
    /// regenerate the elements.    The function is passed the index of the item being
    /// generated.</summary>
    ///
    /// <remarks>The returned sequence may be passed between threads safely. However, 
    /// individual IEnumerator values generated from the returned sequence should not be accessed concurrently.
    /// Iteration can continue up to <c>Int32.MaxValue</c>.</remarks>
    ///
    /// <param name="initializer">A function that generates an item in the sequence from a given index.</param>
    ///
    /// <returns>The result sequence.</returns>
    let initInfinite initializer = 
        Seq.initInfinite initializer |> unsafeOfSeq

    /// <summary>Inserts an element at the specified index.</summary>
    /// <param name="index">The index at which to insert the element.</param>
    /// <param name="value">The value to insert.</param>
    /// <param name="source">The input sequence.</param>
    /// <returns>The result sequence.</returns>
    let insertAt (index: int) (value: 'T) (source: NonEmptySeq<'T>) : NonEmptySeq<'T> = 
        Seq.insertAt index value source |> unsafeOfSeq

    /// <summary>Inserts multiple elements at the specified index.</summary>
    /// <param name="index">The index at which to insert the elements.</param>
    /// <param name="values">The values to insert.</param>
    /// <param name="source">The input sequence.</param>
    /// <returns>The result sequence.</returns>
    let insertManyAt (index: int) (values: seq<'T>) (source: NonEmptySeq<'T>) : NonEmptySeq<'T> = 
        Seq.insertManyAt index values source |> unsafeOfSeq

    /// <summary>Returns the element at the specified index.</summary>
    /// <param name="index">The index of the element to retrieve.</param>
    /// <param name="source">The input sequence.</param>
    /// <returns>The element at the specified index.</returns>
    /// <exception cref="System.ArgumentException">Thrown when index is out of range.</exception>
    let item (index: int) (source: NonEmptySeq<'T>) : 'T = 
        Seq.item index source

    /// <summary>Applies a function to each element of the sequence.</summary>
    /// <param name="action">The function to apply to each element.</param>
    /// <param name="source">The input sequence.</param>
    let iter (action: 'T -> unit) (source: NonEmptySeq<'T>) : unit = 
        Seq.iter action source

    /// <summary>Applies a function to each element of the two sequences.</summary>
    /// <param name="action">The function to apply to each pair of elements.</param>
    /// <param name="source1">The first input sequence.</param>
    /// <param name="source2">The second input sequence.</param>
    let iter2 (action: 'T1 -> 'T2 -> unit) (source1: NonEmptySeq<'T1>) (source2: NonEmptySeq<'T2>) : unit = 
        Seq.iter2 action source1 source2

    /// <summary>Applies a function to each element of the sequence, passing the index of the element as the first argument to the function.</summary>
    /// <param name="action">The function to apply to each element and its index.</param>
    /// <param name="source">The input sequence.</param>
    let iteri (action: int -> 'T -> unit) (source: NonEmptySeq<'T>) : unit = 
        Seq.iteri action source

    /// <summary>Applies a function to each element of the two sequences, passing the index of the elements as the first argument to the function.</summary>
    /// <param name="action">The function to apply to each pair of elements and their index.</param>
    /// <param name="source1">The first input sequence.</param>
    /// <param name="source2">The second input sequence.</param>
    let iteri2 (action: int -> 'T1 -> 'T2 -> unit) (source1: NonEmptySeq<'T1>) (source2: NonEmptySeq<'T2>) : unit = 
        Seq.iteri2 action source1 source2

    /// <summary>Returns the last element of the sequence.</summary>
    /// <param name="source">The input sequence.</param>
    /// <returns>The last element of the sequence.</returns>
    let last (source: NonEmptySeq<'T>) : 'T = 
        Seq.last source

    /// <summary>Returns the last element of the sequence.</summary>
    /// <param name="source">The input sequence.</param>
    /// <returns>The last element of the sequence.</returns>
    let length (source: NonEmptySeq<'T>) : int = 
        Seq.length source

    /// <summary>Builds a new collection whose elements are the results of applying the given function
    /// to each of the elements of the collection.    The given function will be applied
    /// as elements are demanded using the <c>MoveNext</c> method on enumerators retrieved from the
    /// object.</summary>
    ///
    /// <remarks>The returned sequence may be passed between threads safely. However, 
    /// individual IEnumerator values generated from the returned sequence should not be accessed concurrently.</remarks>
    ///
    /// <param name="mapping">A function to transform items from the input sequence.</param>
    /// <param name="source">The input sequence.</param>
    ///
    /// <returns>The result sequence.</returns>
    let map mapping (source: NonEmptySeq<_>) = 
        source |> Seq.map mapping |> unsafeOfSeq

    /// <summary>Builds a new collection whose elements are the results of applying the given function
    /// to the corresponding pairs of elements from the two sequences. If one input sequence is shorter than 
    /// the other then the remaining elements of the longer sequence are ignored.</summary>
    ///
    /// <param name="mapping">A function to transform pairs of items from the input sequences.</param>
    /// <param name="source1">The first input sequence.</param>
    /// <param name="source2">The second input sequence.</param>
    ///
    /// <returns>The result sequence.</returns>
    let map2 mapping (source1: NonEmptySeq<_>) (source2: NonEmptySeq<_>) = 
        Seq.map2 mapping source1 source2 |> unsafeOfSeq

    /// <summary>Combines map and fold. Builds a new collection whose elements are the results of applying the given function
    /// to each of the elements of the collection. The function is also used to accumulate a final value.</summary>
    /// <remarks>This function digests the whole initial sequence as soon as it is called. As a result this function should
    /// not be used with large or infinite sequences.</remarks>
    /// <param name="mapping">The function to transform elements from the input collection and accumulate the final value.</param>
    /// <param name="state">The initial state.</param>
    /// <param name="source">The input collection.</param>
    /// <returns>The collection of transformed elements, and the final accumulated value.</returns>
    /// <remarks>This function consumes the whole input sequence before yielding the first element of the result sequence.</remarks>
    let mapFold (mapping: 'State -> 'T -> 'Result * 'State) state (source: NonEmptySeq<_>) =
        let xs, state = Seq.mapFold mapping state source
        unsafeOfSeq xs, state

    /// <summary>Combines map and foldBack. Builds a new collection whose elements are the results of applying the given function
    /// to each of the elements of the collection. The function is also used to accumulate a final value.</summary>
    /// <remarks>This function digests the whole initial sequence as soon as it is called. As a result this function should
    /// not be used with large or infinite sequences.</remarks>
    /// <param name="mapping">The function to transform elements from the input collection and accumulate the final value.</param>
    /// <param name="source">The input collection.</param>
    /// <param name="state">The initial state.</param>
    /// <returns>The collection of transformed elements, and the final accumulated value.</returns>
    /// <remarks>This function consumes the whole input sequence before yielding the first element of the result sequence.</remarks>
    let mapFoldBack (mapping: 'T -> 'State -> 'Result * 'State) (source: NonEmptySeq<_>) state =
        let xs, state = Seq.mapFoldBack mapping source state
        unsafeOfSeq xs, state

    /// <summary>Builds a new collection whose elements are the results of applying the given function
    /// to the corresponding triples of elements from the three sequences. If one input sequence if shorter than
    /// the others then the remaining elements of the longer sequences are ignored.</summary>
    ///
    /// <param name="mapping">The function to transform triples of elements from the input sequences.</param>
    /// <param name="source1">The first input sequence.</param>
    /// <param name="source2">The second input sequence.</param>
    /// <param name="source3">The third input sequence.</param>
    ///
    /// <returns>The result sequence.</returns>
    let map3 mapping (source1: NonEmptySeq<_>) (source2: NonEmptySeq<_>) (source3: NonEmptySeq<_>) =
        Seq.map3 mapping source1 source2 source3 |> unsafeOfSeq

    /// <summary>Builds a new collection whose elements are the results of applying the given function
    /// to each of the elements of the collection. The integer index passed to the
    /// function indicates the index (from 0) of element being transformed.</summary>
    ///
    /// <param name="mapping">A function to transform items from the input sequence that also supplies the current index.</param>
    /// <param name="source">The input sequence.</param>
    ///
    /// <returns>The result sequence.</returns>
    let mapi mapping (source: NonEmptySeq<_>) = 
        Seq.mapi mapping source |> unsafeOfSeq

    /// <summary>Builds a new collection whose elements are the results of applying the given function
    /// to the corresponding pairs of elements from the two sequences. If one input sequence is shorter than 
    /// the other then the remaining elements of the longer sequence are ignored. The integer index passed to the
    /// function indicates the index (from 0) of element being transformed.</summary>
    ///
    /// <param name="mapping">A function to transform pairs of items from the input sequences that also supplies the current index.</param>
    /// <param name="source1">The first input sequence.</param>
    /// <param name="source2">The second input sequence.</param>
    ///
    /// <returns>The result sequence.</returns>
    let mapi2 mapping (source1: NonEmptySeq<_>) (source2: NonEmptySeq<_>) = 
        Seq.mapi2 mapping source1 source2 |> unsafeOfSeq

    /// <summary>Returns a sequence of each element in the input sequence and its predecessor, with the
    /// exception of the first element which is only returned as the predecessor of the second element.</summary>
    ///
    /// <param name="source">The input sequence.</param>
    ///
    /// <returns>The result sequence.</returns>
    let pairwise (source: NonEmptySeq<_>) = 
        Seq.pairwise source |> unsafeOfSeq

    /// <summary>Returns a sequence with all elements permuted according to the
    /// specified permutation.</summary>
    ///
    /// <remarks>Note that this function returns a sequence that digests the whole initial sequence as soon as
    /// that sequence is iterated. As a result this function should not be used with
    /// large or infinite sequences.</remarks>
    ///
    /// <param name="indexMap">The function that maps input indices to output indices.</param>
    /// <param name="source">The input sequence.</param>
    ///
    /// <returns>The result sequence.</returns>
    ///
    /// <exception cref="System.ArgumentException">Thrown when indexMap does not produce a valid permutation.</exception>
    /// <remarks>This function consumes the whole input sequence before yielding the first element of the result sequence.</remarks>
    let permute indexMap (source: NonEmptySeq<_>) = 
        Seq.permute indexMap source |> unsafeOfSeq

    /// <summary>Returns the first element for which the given function returns <c>Some</c>. If no such element exists, raises <c>KeyNotFoundException</c>.</summary>
    /// <param name="chooser">A function to transform elements of the sequence into options.</param>
    /// <param name="source">The input sequence.</param>
    /// <returns>The first chosen element.</returns>
    /// <exception cref="System.Collections.Generic.KeyNotFoundException">Thrown when no element is chosen.</exception>
    let pick (chooser: 'T -> 'U option) (source: NonEmptySeq<'T>) : 'U = 
        Seq.pick chooser source

    /// <summary>Builds a new sequence object that delegates to the given sequence object. This ensures 
    /// the original sequence cannot be rediscovered and mutated by a type cast. For example, 
    /// if given an array the returned sequence will return the elements of the array, but
    /// you cannot cast the returned sequence object to an array.</summary>
    ///
    /// <param name="source">The input sequence.</param>
    ///
    /// <returns>The result sequence.</returns>
    let readonly (source: NonEmptySeq<_>) = 
        Seq.readonly source |> unsafeOfSeq

    /// <summary>Applies a function to each element of the sequence, threading an accumulator argument
    /// through the computation. Apply the function to the first two elements of the sequence.
    /// Then feed this result into the function along with the third element and so on. 
    /// Return the final result. If the input function is <c>f</c> and the elements are <c>i0...iN</c> then computes 
    /// <c>f (... (f i0 i1) i2 ...) iN</c>.</summary>
    /// <param name="reduction">The function to reduce two sequence elements to a single element.</param>
    /// <param name="source">The input sequence.</param>
    /// <returns>The final reduced value.</returns>
    let reduce (reduction: 'T -> 'T -> 'T) source = 
        Seq.reduce reduction source

    /// <summary>Applies a function to each element of the sequence, starting from the end, threading an accumulator argument
    /// through the computation. If the input function is <c>f</c> and the elements are <c>i0...iN</c> then computes 
    /// <c>f i0 (...(f iN-1 iN))</c>.</summary>
    /// <param name="reduction">A function that takes in the next-to-last element of the sequence and the
    /// current accumulated result to produce the next accumulated result.</param>
    /// <param name="source">The input sequence.</param>
    /// <returns>The final result of the reductions.</returns>
    let reduceBack (reduction: 'T -> 'T -> 'T)  (source: NonEmptySeq<'T>) = 
        Seq.reduceBack reduction source

    /// <summary>Removes the element at the specified index.</summary>
    /// <param name="index">The index of the element to remove.</param>
    /// <param name="source">The input sequence.</param>
    /// <returns>The result sequence.</returns>
    let tryRemoveAt (index: int) (source: NonEmptySeq<'T>) : NonEmptySeq<'T> option = 
        Seq.removeAt index source |> tryOfSeq

    /// <summary>Removes the element at the specified index.</summary>
    /// <param name="index">The index of the element to remove.</param>
    /// <param name="source">The input sequence.</param>
    /// <returns>The result sequence.</returns>
    /// <exception cref="System.ArgumentException">Thrown when removing the item results in an empty sequence.</exception>
    let removeAt (index: int) (source: NonEmptySeq<'T>) : NonEmptySeq<'T> = 
        Seq.removeAt index source |> ofSeq
    
    /// <summary>Removes multiple elements starting at the specified index.</summary>
    /// <param name="index">The index at which to start removing elements.</param>
    /// <param name="count">The number of elements to remove.</param>
    /// <param name="source">The input sequence.</param>
    /// <returns>The result sequence.</returns>
    let tryRemoveManyAt (index: int) (count: int) (source: NonEmptySeq<'T>) : NonEmptySeq<'T> option = 
        Seq.removeManyAt index count source |> tryOfSeq
    
    /// <summary>Removes multiple elements starting at the specified index.</summary>
    /// <param name="index">The index at which to start removing elements.</param>
    /// <param name="count">The number of elements to remove.</param>
    /// <param name="source">The input sequence.</param>
    /// <returns>The result sequence.</returns>
    /// <exception cref="System.ArgumentException">Thrown when removing the items results in an empty sequence.</exception>
    let removeManyAt (index: int) (count: int) (source: NonEmptySeq<'T>) : NonEmptySeq<'T> = 
        Seq.removeManyAt index count source |> ofSeq
    
    /// <summary>Creates a sequence that contains one repeated value.</summary>
    /// <param name="count">The number of elements.</param>
    /// <param name="value">The value to replicate.</param>
    /// <returns>The result sequence.</returns>
    let replicate (count: int) (value: 'T) : NonEmptySeq<'T> = 
        Seq.replicate count value |> unsafeOfSeq

    /// <summary>Returns a new sequence with the elements in reverse order.</summary>
    /// <param name="source">The input sequence.</param>
    /// <returns>The reversed sequence.</returns>
    /// <remarks>This function consumes the whole input sequence before yielding the first element of the reversed sequence.</remarks>
    let rev (source: NonEmptySeq<_>) = 
        Seq.rev source |> unsafeOfSeq

    /// <summary>Like fold, but computes on-demand and returns the sequence of intermediary and final results.</summary>
    ///
    /// <param name="folder">A function that updates the state with each element from the sequence.</param>
    /// <param name="state">The initial state.</param>
    /// <param name="source">The input sequence.</param>
    ///
    /// <returns>The resulting sequence of computed states.</returns>
    let scan (folder:'State -> 'T -> 'State) state (source: NonEmptySeq<_>) =
        Seq.scan folder state source |> unsafeOfSeq

    /// <summary>Like <c>foldBack</c>, but returns the sequence of intermediary and final results.</summary>
    /// <remarks>This function returns a sequence that digests the whole initial sequence as soon as that
    /// sequence is iterated. As a result this function should not be used with large or infinite sequences.
    /// </remarks>
    /// <param name="folder">A function that updates the state with each element from the sequence.</param>
    /// <param name="source">The input sequence.</param>
    /// <param name="state">The initial state.</param>
    /// <returns>The resulting sequence of computed states.</returns>
    /// <remarks>This function consumes the whole input sequence before yielding the first element of the result sequence.</remarks>
    let scanBack (folder:'T -> 'State -> 'State) (source: NonEmptySeq<_>) state =
        Seq.scanBack folder source state |> unsafeOfSeq

    /// <summary>Returns a sequence that yields one item only.</summary>
    ///
    /// <param name="value">The input item.</param>
    ///
    /// <returns>The result sequence of one item.</returns>
    let singleton value = 
        Seq.singleton value |> unsafeOfSeq

    /// <summary>Returns a sequence that skips the first N elements of the list.</summary>
    /// <param name="count">The number of elements to skip.</param>
    /// <param name="source">The input sequence.</param>
    /// <returns>The result sequence.</returns>
    let trySkip (count: int) (source: NonEmptySeq<'T>) : NonEmptySeq<'T> option = 
        Seq.skip count source |> tryOfSeq

    /// <summary>Returns a sequence that skips the first N elements of the list.</summary>
    /// <param name="count">The number of elements to skip.</param>
    /// <param name="source">The input sequence.</param>
    /// <returns>The result sequence.</returns>
    /// <exception cref="System.ArgumentException">Thrown when resulting list is empty.</exception>
    let skip (count: int) (source: NonEmptySeq<'T>) : NonEmptySeq<'T> = 
        Seq.skip count source |> unsafeOfSeq
    
    /// <summary>Returns a sequence that skips elements while the predicate is true.</summary>
    /// <param name="predicate">A function to test each element of the sequence.</param>
    /// <param name="source">The input sequence.</param>
    /// <returns>The result sequence.</returns>
    let trySkipWhile (predicate: 'T -> bool) (source: NonEmptySeq<'T>) : NonEmptySeq<'T> option = 
        Seq.skipWhile predicate source |> tryOfSeq
    
    /// <summary>Returns a sequence that skips elements while the predicate is true.</summary>
    /// <param name="predicate">A function to test each element of the sequence.</param>
    /// <param name="source">The input sequence.</param>
    /// <returns>The result sequence.</returns>
    /// <exception cref="System.ArgumentException">Thrown when resulting list is empty.</exception>
    let skipWhile (predicate: 'T -> bool) (source: NonEmptySeq<'T>) : NonEmptySeq<'T> = 
        Seq.skipWhile predicate source |> unsafeOfSeq

    /// <summary>Yields a sequence ordered by keys.</summary>
    /// 
    /// <remarks>This function returns a sequence that digests the whole initial sequence as soon as 
    /// that sequence is iterated. As a result this function should not be used with 
    /// large or infinite sequences. The function makes no assumption on the ordering of the original 
    /// sequence.
    ///
    /// This is a stable sort, that is the original order of equal elements is preserved.</remarks>
    ///
    /// <param name="source">The input sequence.</param>
    ///
    /// <returns>The result sequence.</returns>
    ///
    /// <remarks>This function consumes the whole input sequence before yielding the first element of the result sequence.</remarks>
    let sort (source: NonEmptySeq<_>) = 
        Seq.sort source |> unsafeOfSeq

    /// <summary>Yields a sequence ordered using the given comparison function.</summary>
    /// <remarks>This function returns a sequence that digests the whole initial sequence as soon as
    /// that sequence is iterated. As a result this function should not be used with
    /// large or infinite sequences. The function makes no assumption on the ordering of the original
    /// sequence.
    ///
    /// This is a stable sort, that is the original order of equal elements is preserved.</remarks>
    /// <param name="comparer">The function to compare the collection elements.</param>
    /// <param name="source">The input sequence.</param>
    /// <returns>The result sequence.</returns>
    /// <remarks>This function consumes the whole input sequence before yielding the first element of the result sequence.</remarks>
    let sortWith comparer (source: NonEmptySeq<_>) = 
        Seq.sortWith comparer source |> unsafeOfSeq

    /// <summary>Applies a key-generating function to each element of a sequence and yield a sequence ordered
    /// by keys.    The keys are compared using generic comparison as implemented by <c>Operators.compare</c>.</summary> 
    /// 
    /// <remarks>This function returns a sequence that digests the whole initial sequence as soon as 
    /// that sequence is iterated. As a result this function should not be used with 
    /// large or infinite sequences. The function makes no assumption on the ordering of the original 
    /// sequence.
    ///
    /// This is a stable sort, that is the original order of equal elements is preserved.</remarks>
    ///
    /// <param name="projection">A function to transform items of the input sequence into comparable keys.</param>
    /// <param name="source">The input sequence.</param>
    ///
    /// <returns>The result sequence.</returns>
    ///
    /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
    let sortBy projection (source: NonEmptySeq<_>) = 
        Seq.sortBy projection source |> unsafeOfSeq

    /// <summary>Yields a sequence ordered descending by keys.</summary>
    /// 
    /// <remarks>This function returns a sequence that digests the whole initial sequence as soon as 
    /// that sequence is iterated. As a result this function should not be used with 
    /// large or infinite sequences. The function makes no assumption on the ordering of the original 
    /// sequence.
    ///
    /// This is a stable sort, that is the original order of equal elements is preserved.</remarks>
    ///
    /// <param name="source">The input sequence.</param>
    ///
    /// <returns>The result sequence.</returns>
    let sortDescending (source: NonEmptySeq<_>) = 
        Seq.sortDescending source |> unsafeOfSeq

    /// <summary>Applies a key-generating function to each element of a sequence and yield a sequence ordered
    /// descending by keys.    The keys are compared using generic comparison as implemented by <c>Operators.compare</c>.</summary> 
    /// 
    /// <remarks>This function returns a sequence that digests the whole initial sequence as soon as 
    /// that sequence is iterated. As a result this function should not be used with 
    /// large or infinite sequences. The function makes no assumption on the ordering of the original 
    /// sequence.
    ///
    /// This is a stable sort, that is the original order of equal elements is preserved.</remarks>
    ///
    /// <param name="projection">A function to transform items of the input sequence into comparable keys.</param>
    /// <param name="source">The input sequence.</param>
    ///
    /// <returns>The result sequence.</returns>
    let sortByDescending projection (source: NonEmptySeq<_>) = 
        Seq.sortByDescending projection source |> unsafeOfSeq
    
    /// <summary>Splits the list into the specified number of sequences.</summary>
    /// <param name="count">The number of sequences to create.</param>
    /// <param name="source">The input list.</param>
    /// <returns>A sequence of sequences.</returns>
    let splitInto (count: int) (source: NonEmptySeq<'T>) : NonEmptySeq<NonEmptySeq<'T>> = 
        Seq.splitInto count source |> Seq.map unsafeOfSeq |> unsafeOfSeq
    
    /// <summary>Computes the sum of the elements of the sequence.</summary>
    /// <param name="source">The input sequence.</param>
    /// <returns>The sum of the elements.</returns>
    let inline sum source = 
        Seq.sum source
    
    /// <summary>Computes the sum of the elements of the sequence, using the given projection.</summary>
    /// <param name="projection">A function to transform the sequence elements before summing.</param>
    /// <param name="source">The input sequence.</param>
    /// <returns>The sum of the transformed elements.</returns>
    let inline sumBy projection source = 
        Seq.sumBy projection source

    /// <summary>Returns a sequence that skips 1 element of the underlying sequence and then yields the
    /// remaining elements of the sequence.</summary>
    ///
    /// <param name="source">The input sequence.</param>
    ///
    /// <returns>The result sequence.</returns>
    let tail (source: NonEmptySeq<_>) = 
        Seq.tail source

    /// <summary>Returns a sequence that skips 1 element of the underlying sequence and then yields the
    /// remaining elements of the sequence.</summary>
    ///
    /// <param name="source">The input sequence.</param>
    ///
    /// <returns>The result sequence.</returns>
    let tryTail (source: NonEmptySeq<_>) = 
        Seq.tail source |> tryOfSeq

    /// <summary>Returns a sequence that contains the first N elements of the sequence.</summary>
    /// <param name="count">The number of elements to take.</param>
    /// <param name="source">The input sequence.</param>
    /// <returns>The result sequence.</returns>
    let tryTake (count: int) (source: NonEmptySeq<'T>) : NonEmptySeq<'T> option = 
        Seq.take count source |> tryOfSeq

    /// <summary>Returns a sequence that contains the first N elements of the sequence.</summary>
    /// <param name="count">The number of elements to take.</param>
    /// <param name="source">The input sequence.</param>
    /// <returns>The result sequence.</returns>
    /// <exception cref="System.ArgumentException">Thrown when the count is less than or equal to zero.</exception>
    let take (count: int) (source: NonEmptySeq<'T>) : NonEmptySeq<'T> = 
        if count <= 0 then
            raise <| new System.ArgumentException("Count must be greater than 0.")
        else
        Seq.take count source |> unsafeOfSeq
    
    /// <summary>Returns a sequence that contains the elements of the sequence while the predicate is true.</summary>
    /// <param name="predicate">A function to test each element of the sequence.</param>
    /// <param name="source">The input sequence.</param>
    /// <returns>The result sequence.</returns>
    let tryTakeWhile (predicate: 'T -> bool) (source: NonEmptySeq<'T>) : NonEmptySeq<'T> option = 
        Seq.takeWhile predicate source |> tryOfSeq
    
    /// <summary>Returns a sequence that contains the elements of the sequence while the predicate is true.</summary>
    /// <param name="predicate">A function to test each element of the sequence.</param>
    /// <param name="source">The input sequence.</param>
    /// <returns>The result sequence.</returns>
    /// <exception cref="System.ArgumentException">Thrown when resulting sequence is empty.</exception>
    let takeWhile (predicate: 'T -> bool) (source: NonEmptySeq<'T>) : NonEmptySeq<'T> = 
        Seq.takeWhile predicate source |> unsafeOfSeq
    
    /// <summary>Truncates the sequence to the specified length.</summary>
    /// <param name="count">The maximum number of elements to include in the sequence.</param>
    /// <param name="source">The input sequence.</param>
    /// <returns>The truncated sequence.</returns>
    /// <exception cref="System.ArgumentException">Thrown when the count is less than or equal to zero.</exception>
    let truncate (count: int) (source: NonEmptySeq<'T>) : NonEmptySeq<'T> = 
        if count <= 0 then
            raise <| new System.ArgumentException("Count must be greater than 0.")
        else
        Seq.truncate count source |> unsafeOfSeq
    
    /// <summary>Returns the only element of the sequence, or <c>None</c> if the sequence does not contain exactly one element.</summary>
    /// <param name="source">The input sequence.</param>
    /// <returns>The only element of the sequence, or <c>None</c>.</returns>
    let tryExactlyOne (source: NonEmptySeq<'T>) : 'T option = 
        Seq.tryExactlyOne source
    
    /// <summary>Returns the first element for which the given function returns <c>true</c>, or <c>None</c> if no such element exists.</summary>
    /// <param name="predicate">A function to test each element of the sequence.</param>
    /// <param name="source">The input sequence.</param>
    /// <returns>The first element for which the predicate returns <c>true</c>, or <c>None</c>.</returns>
    let tryFind (predicate: 'T -> bool) (source: NonEmptySeq<'T>) : 'T option = 
        Seq.tryFind predicate source
    
    /// <summary>Returns the last element for which the given function returns <c>true</c>, or <c>None</c> if no such element exists.</summary>
    /// <param name="predicate">A function to test each element of the sequence.</param>
    /// <param name="source">The input sequence.</param>
    /// <returns>The last element for which the predicate returns <c>true</c>, or <c>None</c>.</returns>
    let tryFindBack (predicate: 'T -> bool) (source: NonEmptySeq<'T>) : 'T option = 
        Seq.tryFindBack predicate source
    
    /// <summary>Returns the index of the first element for which the given function returns <c>true</c>, or <c>None</c> if no such element exists.</summary>
    /// <param name="predicate">A function to test each element of the sequence.</param>
    /// <param name="source">The input sequence.</param>
    /// <returns>The index of the first element for which the predicate returns <c>true</c>, or <c>None</c>.</returns>
    let tryFindIndex (predicate: 'T -> bool) (source: NonEmptySeq<'T>) : int option = 
        Seq.tryFindIndex predicate source
    
    /// <summary>Returns the index of the last element for which the given function returns <c>true</c>, or <c>None</c> if no such element exists.</summary>
    /// <param name="predicate">A function to test each element of the sequence.</param>
    /// <param name="source">The input sequence.</param>
    /// <returns>The index of the last element for which the predicate returns <c>true</c>, or <c>None</c>.</returns>
    let tryFindIndexBack (predicate: 'T -> bool) (source: NonEmptySeq<'T>) : int option = 
        Seq.tryFindIndexBack predicate source

    /// <summary>Returns the element at the specified index, or <c>None</c> if the index is out of range.</summary>
    /// <param name="index">The index of the element to retrieve.</param>
    /// <param name="source">The input sequence.</param>
    /// <returns>The element at the specified index, or <c>None</c>.</returns>
    let tryItem (index: int) (source: NonEmptySeq<'T>) : 'T option = 
        Seq.tryItem index source

    /// <summary>Returns the last element of the sequence, or <c>None</c> if the sequence is empty.</summary>
    /// <param name="source">The input sequence.</param>
    /// <returns>The last element of the sequence, or <c>None</c>.</returns>
    let tryLast (source: NonEmptySeq<'T>) : 'T option = 
        Seq.tryLast source
    
    /// <summary>Returns the first element for which the given function returns <c>Some</c>, or <c>None</c> if no such element exists.</summary>
    /// <param name="chooser">A function to transform elements of the sequence into options.</param>
    /// <param name="source">The input sequence.</param>
    /// <returns>The first chosen element, or <c>None</c>.</returns>
    let tryPick (chooser: 'T -> 'U option) (source: NonEmptySeq<'T>) : 'U option = 
        Seq.tryPick chooser source

    /// <summary>Generates a list by repeatedly applying a function to a state.</summary>
    /// <param name="generator">A function that takes the current state and returns an option tuple of the next element and the next state.</param>
    /// <param name="head">The initial element.</param>
    /// <param name="state">The initial state.</param>
    /// <returns>The result list.</returns>
    let unfold (generator: 'T -> 'State -> ('T * 'State) option) (head: 'T) (state: 'State) =
        let rec go item state =
            seq {
                yield item
                match generator item state with
                | None -> ()
                | Some (item, state) -> yield! go item state
            }
        go head state |> unsafeOfSeq

    /// <summary>Splits a sequence of triples into three sequences.</summary>
    /// <param name="source">The input sequence.</param>
    /// <returns>A tuple containing the three sequences.</returns>
    let unzip3 (source: NonEmptySeq<'T1 * 'T2 * 'T3>) : NonEmptySeq<'T1> * NonEmptySeq<'T2> * NonEmptySeq<'T3> = 
        source |> Seq.toList |> List.unzip3 |> fun (a, b, c) -> (unsafeOfSeq a, unsafeOfSeq b, unsafeOfSeq c)
    
    /// <summary>Updates the element at the specified index.</summary>
    /// <param name="index">The index of the element to update.</param>
    /// <param name="value">The new value.</param>
    /// <param name="source">The input sequence.</param>
    /// <returns>The result sequence.</returns>
    let updateAt (index: int) (value: 'T) (source: NonEmptySeq<'T>) : NonEmptySeq<'T> = 
        Seq.updateAt index value source |> unsafeOfSeq
    
    /// <summary>Returns a sequence that contains the elements of the sequence for which the given function returns <c>true</c>.</summary>
    /// <param name="predicate">A function to test each element of the sequence.</param>
    /// <param name="source">The input sequence.</param>
    /// <returns>The result sequence.</returns>
    let where (predicate: 'T -> bool) (source: NonEmptySeq<'T>) : NonEmptySeq<'T> = 
        Seq.where predicate source |> unsafeOfSeq
    
    /// <summary>Returns a sequence of sliding windows containing elements drawn from the sequence.</summary>
    /// <param name="windowSize">The number of elements in each window.</param>
    /// <param name="source">The input sequence.</param>
    /// <returns>The sequence of windows.</returns>
    let windowed (windowSize: int) (source: NonEmptySeq<'T>) : NonEmptySeq<NonEmptySeq<'T>> = 
        Seq.windowed windowSize source |> Seq.map unsafeOfSeq |> unsafeOfSeq

    /// <summary>Combines the two sequences into a sequence of pairs. The two sequences need not have equal lengths:
    /// when one sequence is exhausted any remaining elements in the other
    /// sequence are ignored.</summary>
    ///
    /// <param name="source1">The first input sequence.</param>
    /// <param name="source2">The second input sequence.</param>
    ///
    /// <returns>The result sequence.</returns>
    let zip (source1: NonEmptySeq<_>) (source2: NonEmptySeq<_>) = 
        Seq.zip source1 source2 |> unsafeOfSeq

    /// <summary>Combines the three sequences into a sequence of triples. The sequences need not have equal lengths:
    /// when one sequence is exhausted any remaining elements in the other
    /// sequences are ignored.</summary>
    ///
    /// <param name="source1">The first input sequence.</param>
    /// <param name="source2">The second input sequence.</param>
    /// <param name="source3">The third input sequence.</param>
    ///
    /// <returns>The result sequence.</returns>
    let zip3 (source1: NonEmptySeq<_>) (source2: NonEmptySeq<_>) (source3: NonEmptySeq<_>) = 
        Seq.zip3 source1 source2 source3 |> unsafeOfSeq

    /// <summary>Applies the given function to each element of the NonEmptySequence and concatenates all the
    /// results.</summary>
    ///
    /// <remarks>Remember NonEmptySequence is lazy, effects are delayed until it is enumerated.</remarks>
    /// <remarks>This is the same as Seq.collect but the type of the mapping function is not flexible.</remarks>
    ///
    /// <param name="mapping">A function to transform elements of the input NonEmptySequence into the NonEmptySequences
    /// that will then be concatenated.</param>
    /// <param name="source">The input NonEmptySequence.</param>
    ///
    /// <returns>The result NonEmptySequence.</returns>
    ///
    /// <exception cref="System.ArgumentNullException">Thrown when the input NonEmptySequence is null.</exception>
    let bind (mapping: 'T->NonEmptySeq<'U>) source = collect mapping source

    let apply f x = bind (fun f -> map ((<|) f) x) f

    let lift2 f x1 x2 = allPairs x1 x2 |> map (fun (x, y) -> f x y)
    
    /// <summary>Combines values from three NonEmptySeq and calls a mapping function on this combination.</summary>
    /// <param name="f">Mapping function taking three element combination as input.</param>
    /// <param name="x1">First NonEmptySeq.</param>
    /// <param name="x2">Second NonEmptySeq.</param>
    /// <param name="x3">Third NonEmptySeq.</param>
    ///
    /// <returns>NonEmptySeq with values returned from mapping function.</returns>
    let lift3 f x1 x2 x3 =
        allPairs x2 x3
        |> allPairs x1
        |> map (fun x -> (fst (snd x), snd (snd x), fst x))
        |> map (fun (x, y, z) -> f x y z)

    let replace (oldValue: NonEmptySeq<'T>) (newValue: NonEmptySeq<'T>) (source: NonEmptySeq<'T>) : NonEmptySeq<'T> =
        Seq.replace oldValue newValue source |> unsafeOfSeq


[<AutoOpen>]
module NonEmptySeqBuilder =
    type NESeqBuilder () =
        [<CompilerMessage("A NonEmptySeq doesn't support the Zero operation.", 708, IsError = true)>]
        member _.Zero () = raise Internals.Errors.exnUnreachable
        member _.Combine (a: NonEmptySeq<'T>, b) = NonEmptySeq.append a b
        member _.Yield x = NonEmptySeq.singleton x
        member _.Delay expr = expr () : NonEmptySeq<'T>

    let neseq = NESeqBuilder ()
