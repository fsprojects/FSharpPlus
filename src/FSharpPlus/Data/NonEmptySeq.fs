namespace FSharpPlus.Data

open System
open System.Runtime.InteropServices
open System.ComponentModel
open System.Collections.Generic
open FSharpPlus
open FSharpPlus.Control

/// A type-safe sequence that contains at least one element.
[<Interface>]
type NonEmptySeq<'t> =
    inherit IEnumerable<'t>
    abstract member First: 't

    // boilerplate
    static member unsafeOfSeq (seq: _ seq) =
        { new NonEmptySeq<_> with
            member _.First = Seq.head seq
            member _.GetEnumerator() = seq.GetEnumerator()
            member _.GetEnumerator() = seq.GetEnumerator() :> Collections.IEnumerator }

    static member ofList (list: _ list) = match list with [] -> invalidArg "list" "The input list was empty." | _ -> NonEmptySeq<_>.unsafeOfSeq list
    static member collect (mapping: 'a -> '``#NonEmptySeq<'b>``) (source: NonEmptySeq<'a>) : NonEmptySeq<'b> when '``#NonEmptySeq<'b>`` :> NonEmptySeq<'b> = Seq.collect mapping source |> NonEmptySeq<_>.unsafeOfSeq    
    static member concat (sources: NonEmptySeq<'``#NonEmptySeq<'a>``>) : NonEmptySeq<'a> when '``#NonEmptySeq<'a>`` :> NonEmptySeq<'a> = Seq.concat sources |> NonEmptySeq<'a>.unsafeOfSeq
    static member allPairs (source1: _ NonEmptySeq) (source2: _ NonEmptySeq) = Seq.allPairs source1 source2 |> NonEmptySeq<_>.unsafeOfSeq    
    static member append (source1: 'a NonEmptySeq) (source2: 'a NonEmptySeq) = Seq.append source1 source2 |> NonEmptySeq<'a>.unsafeOfSeq
    static member map mapping (source: NonEmptySeq<_>) = source |> Seq.map mapping |> NonEmptySeq<_>.unsafeOfSeq
    static member bind (mapping: 'T->NonEmptySeq<'U>) source = NonEmptySeq<'U>.collect mapping source
    static member apply f x = NonEmptySeq<_>.bind (fun f -> NonEmptySeq<_>.map ((<|) f) x) f
    static member lift2 f x1 x2 = NonEmptySeq<_>.allPairs x1 x2 |> NonEmptySeq<_>.map (fun (x, y) -> f x y)
    static member lift3 f x1 x2 x3 =
        NonEmptySeq<_>.allPairs x2 x3
        |> NonEmptySeq<_>.allPairs x1
        |> NonEmptySeq<_>.map (fun x -> (fst (snd x), snd (snd x), fst x))
        |> NonEmptySeq<_>.map (fun (x, y, z) -> f x y z)

    static member zip (source1: NonEmptySeq<_>) (source2: NonEmptySeq<_>) = Seq.zip source1 source2 |> NonEmptySeq<_>.unsafeOfSeq
    static member singleton value = Seq.singleton value |> NonEmptySeq<_>.unsafeOfSeq
    static member delay (generator: unit -> NonEmptySeq<'a>) : NonEmptySeq<'a> = Seq.delay (fun () -> generator () :> _) |> NonEmptySeq<_>.unsafeOfSeq
    
    // Statics

    static member        (<|>) (x: 'T NonEmptySeq      , y) = NonEmptySeq<'T>.append x y

    static member inline Choice (x: ref<NonEmptySeq<'``Alternative<'T>``>>, _mthd: Choice) =
        use e = x.Value.GetEnumerator ()
        e.MoveNext() |> ignore
        let mutable res = e.Current
        while e.MoveNext() && not (IsAltLeftZero.Invoke res) do
            res <- Append.Invoke res e.Current

    static member (<*>) (f: NonEmptySeq<_>   , x: NonEmptySeq<'T>) : NonEmptySeq<'U> = NonEmptySeq<_>.apply f x
    static member        Lift2 (f, x: NonEmptySeq<_>     , y: NonEmptySeq<_>) = NonEmptySeq<_>.lift2 f x y
    static member        Lift3 (f, x: NonEmptySeq<_>     , y: NonEmptySeq<_>     , z: NonEmptySeq<_>) = NonEmptySeq<_>.lift3 f x y z
    static member IsLeftZero (_: NonEmptySeq<_>) = false

    // no need, it should take the one from Seq --> member this.Head (x: NonEmptySeq<'T>  , [<Optional>]_impl: Head    ) = x.First    
    
    static member        TryHead (x: NonEmptySeq<'T>,[<Optional>]_impl: TryHead) = Some x.First
    static member        TryLast (x: NonEmptySeq<'T>, [<Optional>]_impl: TryLast)  = Some <| Seq.last x

    static member        Map (x: NonEmptySeq<_>, f: 'T -> 'U) = NonEmptySeq<_>.map f x      : NonEmptySeq<'U>
    
    static member        Unzip (source: NonEmptySeq<'T * 'U>) = Map.Invoke fst source, Map.Invoke snd source
    static member Zip (x: NonEmptySeq<'T>, y: NonEmptySeq<'U>) = NonEmptySeq<_>.zip         x y
    static member (>>=) (source: NonEmptySeq<'T>, f: 'T -> NonEmptySeq<'U>) = NonEmptySeq<_>.collect f source : NonEmptySeq<'U>

    static member        Join (x: NonEmptySeq<NonEmptySeq<'T>>) = NonEmptySeq<_>.concat x : NonEmptySeq<'T> 

    static member        Return (x: 'a) = NonEmptySeq<_>.singleton x : NonEmptySeq<'a>

    static member        Delay (x: unit-> _ ) = NonEmptySeq<_>.delay x : NonEmptySeq<'T>
    static member        TryWith (computation: unit -> NonEmptySeq<_>, catchHandler: exn -> NonEmptySeq<_>) = seq (try (Seq.toArray (computation ())) with e -> Seq.toArray (catchHandler e)) |> NonEmptySeq<_>.unsafeOfSeq
    static member        TryFinally (computation: unit -> NonEmptySeq<_>, compensation: unit -> unit) = seq { try for e in computation () do yield e finally compensation () } |> NonEmptySeq<_>.unsafeOfSeq
    static member        Using (resource: 'T when 'T :> IDisposable, body: 'T -> NonEmptySeq<'U>) = seq { try for e in body resource do yield e finally if not (isNull (box resource)) then resource.Dispose () } |> NonEmptySeq<_>.unsafeOfSeq : NonEmptySeq<'U>
    static member inline (+) (x: _ NonEmptySeq             , y: _ NonEmptySeq) = NonEmptySeq<_>.append x y


    static member inline Traverse (t: _ seq, f) =
       let cons x y = seq {yield x; yield! y}
       let cons_f x ys = Map.Invoke (cons: 'a->seq<_>->seq<_>) (f x) <*> ys
       Map.Invoke NonEmptySeq<_>.unsafeOfSeq (Seq.foldBack cons_f t (result Seq.empty))

    static member inline Traverse (t: NonEmptySeq<'T>, f: 'T->'``Functor<'U>``) =
        let mapped = NonEmptySeq<_>.map f t
        Sequence.ForInfiniteSequences (mapped, IsLeftZero.Invoke, NonEmptySeq<_>.ofList) : '``Functor<NonEmptySeq<'U>>``

    #if !FABLE_COMPILER
    static member Traverse (t: 't NonEmptySeq, f: 't->Async<'u>) : Async<NonEmptySeq<_>> = async {
        let! ct = Async.CancellationToken
        return seq {
            use enum = t.GetEnumerator ()
            while enum.MoveNext() do
                yield Async.RunSynchronously (f enum.Current, cancellationToken = ct) } |> NonEmptySeq<_>.unsafeOfSeq }
    #endif

    static member inline Sequence (t: NonEmptySeq<'``Applicative<'T>``>) = Sequence.ForInfiniteSequences (t, IsLeftZero.Invoke, NonEmptySeq<_>.ofList)   : '``Applicative<NonEmptySeq<'T>>``

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

    /// <summary>Returns a sequence that is built from the given delayed specification of a
    /// sequence.</summary>
    ///
    /// <remarks>The input function is evaluated each time an IEnumerator for the sequence 
    /// is requested.</remarks>
    ///
    /// <param name="generator">The generating function for the sequence.</param>
    let delay (generator: unit -> NonEmptySeq<'a>) : NonEmptySeq<'a> = Seq.delay (fun () -> generator () :> _) |> unsafeOfSeq

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
    let indexed (source: NonEmptySeq<_>) = Seq.indexed source |> unsafeOfSeq

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
    let initInfinite initializer = Seq.initInfinite initializer |> unsafeOfSeq

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
    let map mapping (source: NonEmptySeq<_>) = source |> Seq.map mapping |> unsafeOfSeq

    /// <summary>Builds a new collection whose elements are the results of applying the given function
    /// to the corresponding pairs of elements from the two sequences. If one input sequence is shorter than 
    /// the other then the remaining elements of the longer sequence are ignored.</summary>
    ///
    /// <param name="mapping">A function to transform pairs of items from the input sequences.</param>
    /// <param name="source1">The first input sequence.</param>
    /// <param name="source2">The second input sequence.</param>
    ///
    /// <returns>The result sequence.</returns>
    let map2 mapping (source1: NonEmptySeq<_>) (source2: NonEmptySeq<_>) = Seq.map2 mapping source1 source2 |> unsafeOfSeq

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
    let mapi mapping (source: NonEmptySeq<_>) = Seq.mapi mapping source |> unsafeOfSeq

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
    let mapi2 mapping (source1: NonEmptySeq<_>) (source2: NonEmptySeq<_>) = Seq.mapi2 mapping source1 source2 |> unsafeOfSeq

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
        if isNull seq || Seq.isEmpty seq then invalidArg "seq" "The input sequence was empty."
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
        if isNull array || Array.isEmpty array then invalidArg "array" "The input array was empty."
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
        match list with [] -> invalidArg "list" "The input list was empty." | _ -> unsafeOfSeq list

    /// Transforms a list to a NonEmptySeq, returning an option to signal when the original list was empty.
    let tryOfList (list: _ list) =
        match list with [] -> None | _ -> unsafeOfSeq list |> Some

    /// <summary>Returns a sequence of each element in the input sequence and its predecessor, with the
    /// exception of the first element which is only returned as the predecessor of the second element.</summary>
    ///
    /// <param name="source">The input sequence.</param>
    ///
    /// <returns>The result sequence.</returns>
    let pairwise (source: NonEmptySeq<_>) = Seq.pairwise source |> unsafeOfSeq

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
    let permute indexMap (source: NonEmptySeq<_>) = Seq.permute indexMap source |> unsafeOfSeq

    /// <summary>Builds a new sequence object that delegates to the given sequence object. This ensures 
    /// the original sequence cannot be rediscovered and mutated by a type cast. For example, 
    /// if given an array the returned sequence will return the elements of the array, but
    /// you cannot cast the returned sequence object to an array.</summary>
    ///
    /// <param name="source">The input sequence.</param>
    ///
    /// <returns>The result sequence.</returns>
    let readonly (source: NonEmptySeq<_>) = Seq.readonly source |> unsafeOfSeq

    /// <summary>Returns a new sequence with the elements in reverse order.</summary>
    /// <param name="source">The input sequence.</param>
    /// <returns>The reversed sequence.</returns>
    /// <remarks>This function consumes the whole input sequence before yielding the first element of the reversed sequence.</remarks>
    let rev (source: NonEmptySeq<_>) = Seq.rev source |> unsafeOfSeq

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
    let singleton value = Seq.singleton value |> unsafeOfSeq

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
    let sort (source: NonEmptySeq<_>) = Seq.sort source |> unsafeOfSeq

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
    let sortWith comparer (source: NonEmptySeq<_>) = Seq.sortWith comparer source |> unsafeOfSeq

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
    let sortBy projection (source: NonEmptySeq<_>) = Seq.sortBy projection source |> unsafeOfSeq

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
    let sortDescending (source: NonEmptySeq<_>) = Seq.sortDescending source |> unsafeOfSeq

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
    let sortByDescending projection (source: NonEmptySeq<_>) = Seq.sortByDescending projection source |> unsafeOfSeq

    /// <summary>Returns a sequence that skips 1 element of the underlying sequence and then yields the
    /// remaining elements of the sequence.</summary>
    ///
    /// <param name="source">The input sequence.</param>
    ///
    /// <returns>The result sequence.</returns>
    let tail (source: NonEmptySeq<_>) = Seq.tail source

    let unfold (generator: 'T -> 'State -> ('T * 'State) option) (head: 'T) (state: 'State) =
        let rec go item state =
            seq {
                yield item
                match generator item state with
                | None -> ()
                | Some (item, state) -> yield! go item state
            }
        go head state |> unsafeOfSeq

    /// <summary>Combines the two sequences into a sequence of pairs. The two sequences need not have equal lengths:
    /// when one sequence is exhausted any remaining elements in the other
    /// sequence are ignored.</summary>
    ///
    /// <param name="source1">The first input sequence.</param>
    /// <param name="source2">The second input sequence.</param>
    ///
    /// <returns>The result sequence.</returns>
    let zip (source1: NonEmptySeq<_>) (source2: NonEmptySeq<_>) = Seq.zip source1 source2 |> unsafeOfSeq

    /// <summary>Combines the three sequences into a sequence of triples. The sequences need not have equal lengths:
    /// when one sequence is exhausted any remaining elements in the other
    /// sequences are ignored.</summary>
    ///
    /// <param name="source1">The first input sequence.</param>
    /// <param name="source2">The second input sequence.</param>
    /// <param name="source3">The third input sequence.</param>
    ///
    /// <returns>The result sequence.</returns>
    let zip3 (source1: NonEmptySeq<_>) (source2: NonEmptySeq<_>) (source3: NonEmptySeq<_>) = Seq.zip3 source1 source2 source3 |> unsafeOfSeq

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

    /// <summary>Returns a sequence that contains no duplicate entries according to the generic hash and equality comparisons
    /// on the keys returned by the given key-generating function.
    /// If an element occurs multiple times in the sequence then the later occurrences are discarded.</summary>
    /// <param name="source">The input sequence.</param>
    /// <returns>The resulting sequence without duplicates.</returns>
    let distinct (source: NonEmptySeq<'T>) = source |> Seq.distinct |> ofSeq

    /// <summary>Applies a function to each element of the sequence, threading an accumulator argument
    /// through the computation. Apply the function to the first two elements of the sequence.
    /// Then feed this result into the function along with the third element and so on. 
    /// Return the final result. If the input function is <c>f</c> and the elements are <c>i0...iN</c> then computes 
    /// <c>f (... (f i0 i1) i2 ...) iN</c>.</summary>
    /// <param name="reduction">The function to reduce two sequence elements to a single element.</param>
    /// <param name="source">The input sequence.</param>
    /// <returns>The final reduced value.</returns>
    let reduce (reduction: 'T -> 'T -> 'T) source = Seq.reduce reduction source




[<AutoOpen>]
module NonEmptySeqBuilder =
    type NESeqBuilder () =
        [<CompilerMessage("A NonEmptySeq doesn't support the Zero operation.", 708, IsError = true)>]
        member _.Zero () = raise Internals.Errors.exnUnreachable
        member _.Combine (a: NonEmptySeq<'T>, b) = NonEmptySeq.append a b
        member _.Yield x = NonEmptySeq.singleton x
        member _.Delay expr = expr () : NonEmptySeq<'T>

    let neseq = NESeqBuilder ()
