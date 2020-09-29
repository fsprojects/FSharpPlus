namespace FSharpPlus

#if !FABLE_COMPILER

/// Additional operations on IEnumerator
[<RequireQualifiedAccess>]
module Enumerator =

    open System
        
    // Helper functions used in Enumerators

    /// [omit]
    let inline invalidArgFmt (arg: string) (format: string) paramArray = 
        let msg = String.Format (format,paramArray)
        raise (ArgumentException (msg, arg))
    
    /// [omit]
    let noReset ()         = raise (System.NotSupportedException ("Reset is not supported on this enumerator."))
    
    /// [omit]
    let notStarted ()      = invalidOp "Enumeration has not started. Call MoveNext."

    /// [omit]
    let alreadyFinished () = invalidOp "Enumeration already finished."

    /// [omit]
    let check started = if not started then notStarted ()

    /// [omit]
    let dispose (r: System.IDisposable) = r.Dispose ()
    
    open System.Collections
    open System.Collections.Generic
    
    /// An enumerator that is empty -- useful in combination with other enumerators
    /// [omit]
    [<Sealed>]
    type EmptyEnumerator<'T> () =
        let mutable started = false
        interface IEnumerator<'T> with
            member __.Current =
                check started
                (alreadyFinished () : 'T)
    
        interface IEnumerator with
            member __.Current =
                check started
                (alreadyFinished () : obj)
            member __.MoveNext () =
                if not started then started <- true
                false
            member __.Reset() = noReset ()
        interface System.IDisposable with
            member __.Dispose () = ()
              
    /// Constructs an EmptyEnumerator of type 'T.
    let Empty<'T> () = new EmptyEnumerator<'T>() :> IEnumerator<'T>

    /// Constructs an Enumerator that yields the single value given.
    let singleton x = (Seq.singleton x).GetEnumerator()

    /// [omit]
    type IFinallyEnumerator = abstract AppendFinallyAction : (unit -> unit) -> unit

    /// Enumerate all sources in sequence
    /// [omit]
    [<Sealed>]
    type ConcatEnumerator<'T> (sources: IEnumerator<IEnumerator<'T>>) =
        let mutable outerEnum = sources
        let mutable currInnerEnum = Empty ()
        let mutable started  = false
        let mutable finished = false
        let mutable compensations = []

        [<DefaultValue(false)>]
        val mutable private currElement : 'T

        member __.Finish () =
            finished <- true
            try
                match currInnerEnum with
                | null -> ()
                | _ ->
                    try
                        currInnerEnum.Dispose ()
                    finally
                        currInnerEnum <- null
            finally
                try
                    match outerEnum with
                    | null -> ()
                    | _ ->
                        try
                            outerEnum.Dispose ()
                        finally
                            outerEnum <- null
                finally
                    let rec iter comps =
                        match comps with
                        | [] -> ()
                        | h::t -> try h () finally iter t
                    try
                        compensations |> List.rev |> iter
                    finally
                        compensations <- []

        member x.GetCurrent () =
            check started
            if finished then alreadyFinished () else x.currElement

        interface IFinallyEnumerator with
            member __.AppendFinallyAction f =
                compensations <- f :: compensations

        interface IEnumerator<'T> with
            member x.Current = x.GetCurrent ()

        interface IEnumerator with
            member x.Current = box (x.GetCurrent ())

            member x.MoveNext () =
                if not started then (started <- true)
                if finished then false
                else
                  let rec takeInner () =
                    // check inner
                    if currInnerEnum.MoveNext () then
                        x.currElement <- currInnerEnum.Current
                        true
                    else
                        // check outer
                        let rec takeOuter () =
                            if outerEnum.MoveNext () then
                                let ie = outerEnum.Current
                                // Optimization to detect the statically-allocated empty IEnumerators
                                match box ie with
                                | :? EmptyEnumerator<'T> ->
                                        // This one is empty, just skip, don't call GetEnumerator, try again
                                        takeOuter ()
                                | _ ->
                                        // OK, this one may not be empty.
                                        // Don't forget to dispose of the inner enumerator now we're done with it
                                        currInnerEnum.Dispose ()
                                        currInnerEnum <- ie
                                        takeInner ()
                            else
                                // We're done
                                x.Finish ()
                                false
                        takeOuter ()
                  takeInner ()

            member __.Reset () = noReset ()

        interface System.IDisposable with
            member x.Dispose () = if not finished then x.Finish ()

    
    /// <summary>
    /// Enumerates the elements of each of the Enumerators in order.
    /// </summary>
    /// <param name="sources">The source Enumerator of Enumerators.</param>
    /// <returns>A concatenated enumeration of the given Enumerator sources.</returns>
    let concat sources = new ConcatEnumerator<_> (sources) :> IEnumerator<_>
    
    /// <summary>
    /// Tries to find the nth element in the Enumerator.
    /// Returns None if index is negative or the Enumerator does not contain enough elements. 
    /// </summary>
    /// <param name="index">The index to retrieve.</param>
    /// <param name="e">The input Enumerator.</param>
    /// <returns>The value at the given index or <c>None</c> if not found.</returns>
    /// <exception cref="System.InvalidOperationException">
    /// Thrown when the Enumeratoris modified after the enumerator is created,
    /// </exception>
    let rec tryItem index (e: IEnumerator<'T>) =
        if not (e.MoveNext ()) then None
        elif index = 0 then Some e.Current
        else tryItem (index-1) e
    
    /// <summary>
    /// Retuns the nth element in the Enumerator.
    /// </summary>
    /// <remarks>
    /// This is called <c>item</c> in some other parts of core.
    /// </remarks>
    /// <param name="index">The index to retrieve.</param>
    /// <param name="e">The input Enumerator.</param>
    /// <returns>The value at the given index or an exception is thrown if not found.</returns>
    /// <exception cref="System.ArgumentException">
    /// Thrown if the index is negative or the Enumerator does not contain enough elements.
    /// </exception>
    let rec nth index (e: IEnumerator<'T>) =
        if not (e.MoveNext ()) then
            let shortBy = index + 1
            invalidArgFmt "index"
                "{0}\nseq was short by {1} {2}"
                [|"The input sequence has an insufficient number of elements."; shortBy; (if shortBy = 1 then "element" else "elements")|]
        if index = 0 then e.Current
        else nth (index-1) e
    
    /// Defines the possible states of a MapEnumerator.
    /// [omit]
    [<NoEquality; NoComparison>]
    type MapEnumeratorState =
        | NotStarted
        | InProcess
        | Finished
    
    /// An abstract enumerator, useful when mapping over enumerators.
    /// 
    /// It maintains a mutable `curr` item, and a process MapEnumeratorState `state`.
    /// 
    /// Implement DoMoveNext such that `curr` is set after calling, and return
    /// whether the enumerator actually moved next.
    /// [omit]
    [<AbstractClass>]
    type MapEnumerator<'T> () =
        let mutable state = NotStarted
        [<DefaultValue(false)>]
        val mutable private curr : 'T
    
        member this.GetCurrent () =
            match state with
            | NotStarted -> notStarted ()
            | Finished   -> alreadyFinished ()
            | InProcess  -> ()
            this.curr
    
        abstract DoMoveNext : byref<'T> -> bool
        abstract Dispose : unit -> unit
    
        interface IEnumerator<'T> with
            member this.Current = this.GetCurrent ()
    
        interface IEnumerator with
            member this.Current = box (this.GetCurrent ())
            member this.MoveNext () =
                state <- InProcess
                if this.DoMoveNext (&this.curr) then
                    true
                else
                    state <- Finished
                    false
            member __.Reset () = noReset ()

        interface System.IDisposable with
            member this.Dispose () = this.Dispose ()
    
    /// <summary>
    /// Maps over an enumerator.
    /// </summary>
    /// <param name="f">The function to apply.</param>
    /// <param name="e">The input Enumerator.</param>
    /// <returns>A new Enumerator of mapped elements.</returns>
    let map f (e: IEnumerator<_>) : IEnumerator<_> =
        upcast
            { new MapEnumerator<_>() with
                  member __.DoMoveNext (curr: byref<_>) =
                      if e.MoveNext () then
                          curr <- f e.Current
                          true
                      else
                          false
                  member __.Dispose () = e.Dispose ()
            }
    
    /// <summary>
    /// Maps over an Enumerator, with the mapping function also given the index.
    /// </summary>
    /// <param name="f">The function to apply, which is given both the index and the element.</param>
    /// <param name="e">The input Enumerator.</param>
    /// <returns>A new Enumerator of mapped elements.</returns>
    let mapi f (e: IEnumerator<_>) : IEnumerator<_> =
        let f = OptimizedClosures.FSharpFunc<_,_,_>.Adapt f
        let i = ref -1
        upcast {  
            new MapEnumerator<_> () with
                member __.DoMoveNext curr =
                    i := !i + 1
                    if e.MoveNext () then
                        curr <- f.Invoke (!i, e.Current)
                        true
                    else false
                member __.Dispose () = e.Dispose () }

    /// <summary>
    /// Maps over two Enumerators, with the mapping function is given the corresponding elements
    /// of the two Enumerators pairwise.
    /// </summary>
    /// <remarks>
    /// Stops enumerating when either of the input Enumerators are finished enumerating.
    /// </remarks>
    /// <param name="f">The function to apply to each pair of elements from the input Enumerators</param>
    /// <param name="e1">The first input Enumerator.</param>
    /// <param name="e2">The second input Enumerator.</param>
    /// <returns>A new Enumerator of mapped elements.</returns>
    let map2 f (e1: IEnumerator<_>) (e2: IEnumerator<_>) : IEnumerator<_> =
        let f = OptimizedClosures.FSharpFunc<_,_,_>.Adapt f
        upcast {
            new MapEnumerator<_> () with
                member __.DoMoveNext curr =
                    let n1 = e1.MoveNext ()
                    let n2 = e2.MoveNext ()
                    if n1 && n2 then
                        curr <- f.Invoke (e1.Current, e2.Current)
                        true
                    else false
                member __.Dispose () =
                    try e1.Dispose ()
                    finally e2.Dispose () }
    
    /// <summary>
    /// Maps over two Enumerators, where the mapping function is given the index and corresponding elements
    /// of the two input Enumerators pairwise.
    /// </summary>
    /// <remarks>
    /// Stops enumerating when either of the input Enumerators are finished enumerating.
    /// </remarks>
    /// <param name="f">The function to apply to the index and each pair of elements from the input Enumerators</param>
    /// <param name="e1">The first input Enumerator.</param>
    /// <param name="e2">The second input Enumerator.</param>
    /// <returns>A new Enumerator of mapped elements.</returns>
    let mapi2 f (e1: IEnumerator<_>) (e2: IEnumerator<_>) : IEnumerator<_> =
        let f = OptimizedClosures.FSharpFunc<_,_,_,_>.Adapt f
        let i = ref -1
        upcast {
            new MapEnumerator<_> () with
                member __.DoMoveNext curr =
                    i := !i + 1
                    if e1.MoveNext () && e2.MoveNext () then
                        curr <- f.Invoke (!i, e1.Current, e2.Current)
                        true
                    else false
                member __.Dispose () =
                    try e1.Dispose ()
                    finally e2.Dispose () }
    
    /// <summary>
    /// Maps over three Enumerators, where the mapping function is given the corresponding elements
    /// of the three Enumerators.
    /// </summary>
    /// <remarks>
    /// Stops enumerating when any of the input Enumerators are finished enumerating.
    /// </remarks>
    /// <param name="f">The function to apply to each triple of elements from the input Enumerators</param>
    /// <param name="e1">The first input Enumerator.</param>
    /// <param name="e2">The second input Enumerator.</param>
    /// <param name="e3">The third input Enumerator.</param>
    /// <returns>A new Enumerator of mapped elements.</returns>
    let map3 f (e1: IEnumerator<_>) (e2: IEnumerator<_>) (e3: IEnumerator<_>) : IEnumerator<_> =
        let f = OptimizedClosures.FSharpFunc<_,_,_,_>.Adapt f
        upcast {
            new MapEnumerator<_> () with
                member __.DoMoveNext curr =
                    let n1 = e1.MoveNext ()
                    let n2 = e2.MoveNext ()
                    let n3 = e3.MoveNext ()
                   
                    if n1 && n2 && n3 then
                        curr <- f.Invoke (e1.Current, e2.Current, e3.Current)
                        true
                    else false
                member __.Dispose () =
                    try e1.Dispose ()
                    finally
                        try e2.Dispose ()
                        finally e3.Dispose () }
    
    /// <summary>
    /// Applies the given function to each element in the input Enumerator.
    /// Returns an Enumerator comprised of the resuls <c>x</c> for each element
    /// where the function returns <c>Some(x)</c>.
    /// </summary>
    /// <param name="chooser">The function to apply to each triple of elements from the input Enumerators.</param>
    /// <param name="e">The input Enumerator.</param>
    /// <returns>A new Enumerator of values selected from the chooser function.</returns>
    let choose chooser (e: IEnumerator<'T>) =
        let started = ref false
        let curr = ref None
        let get () =  check !started; (match !curr with None -> alreadyFinished () | Some x -> x)
        { new IEnumerator<'U> with
              member __.Current = get ()
          interface IEnumerator with
              member __.Current = box (get ())
              member __.MoveNext () =
                  if not !started then started := true
                  curr := None
                  while (!curr).IsNone && e.MoveNext () do
                      curr := chooser e.Current
                  Option.isSome !curr
              member __.Reset() = noReset ()
          interface System.IDisposable with
              member __.Dispose () = e.Dispose () }
    
    /// <summary>
    /// Returns a new Enumerator yielding only the elements of the input Enumerator for which the
    /// given predicate returns "true".
    /// </summary>
    /// <param name="predicate">The function to test the input elements.</param>
    /// <param name="e">The input Enumerator.</param>
    /// <returns>A new Enumerator yielding only elements that satsify the predicate.</returns>
    let filter predicate (e: IEnumerator<'T>) =
        let started = ref false
        { new IEnumerator<'T> with
                member __.Current = check !started; e.Current
            interface IEnumerator with
                member __.Current = check !started; box e.Current
                member __.MoveNext () =
                    let rec next () =
                        if not !started then started := true
                        e.MoveNext () && (predicate e.Current || next ())
                    next ()
                member __.Reset () = noReset ()
            interface System.IDisposable with
                member __.Dispose () = e.Dispose () }
    
    /// <summary>
    /// Returns a new Enumerator yielding elements <c>x</c> generated by the given computation
    /// so long as it generates a <c>Some(x)</c> - and stops when it generates a <c>None</c>.
    /// The given initial <c>state</c> argument is passed to the element generator.
    /// </summary>
    /// <param name="generator">The function that takes the current state and returns an
    /// option tuple of the next element of the list and the next state value.</param>
    /// <param name="inititalState">The intitial state value.</param>
    /// <returns>A new Enumerator yielding only elements that satsify the predicate.</returns>
    let unfold generator initialState : IEnumerator<_> =
        let state = ref initialState
        upcast {
            new MapEnumerator<_> () with
                member __.DoMoveNext curr =
                    match generator !state with
                    |   None -> false
                    |   Some (r, s) ->
                            curr <- r
                            state := s
                            true
                member __.Dispose () = () }
    
    /// <summary>
    /// Enumerates from zero up to the given <c>lastOption</c>, yielding elements
    /// generated by the given function applied to the index.
    /// </summary>
    /// <remarks>
    /// The Current value for a valid index is "f i".
    ///
    /// Lazy&lt;_&gt; values are used as caches, to store either the result or an exception if thrown.
    /// 
    /// These "Lazy&lt;_&gt;" caches are created only on the first call to current and forced immediately.
    /// The lazy creation of the cache nodes means enumerations that skip many Current values are not delayed by GC.
    /// For example, the full enumeration of Seq.initInfinite in the tests.
    /// </remarks>
    /// <param name="lastOption">The last index to stop at -- or <c>None</c> to run forever, well as far as Int32.MaxValue.</param>
    /// <param name="f">The function to apply to each index.</param>
    /// <returns>An enumerator that yields upto the lastOption.</returns>
    let upto lastOption f =
        match lastOption with
        | Some b when b < 0 -> Empty ()    // a request for -ve length returns empty sequence
        | _ ->
            let unstarted   = -1  // index value means unstarted (and no valid index)
            let completed   = -2  // index value means completed (and no valid index)
            let unreachable = -3  // index is unreachable from 0,1,2,3,...
            let finalIndex  = 
                match lastOption with
                | Some b -> b             // here b>=0, a valid end value.
                | None   -> unreachable   // run "forever", well as far as Int32.MaxValue since indexing with a bounded type.
            let index = ref unstarted
            // a Lazy node to cache the result/exception
            let current = ref Unchecked.defaultof<_>
            let setIndex i = index := i; current := Unchecked.defaultof<_> // cache node unprimed, initialized on demand.
            let getCurrent () =
                if !index = unstarted then notStarted ()
                if !index = completed then alreadyFinished ()
                match box !current with
                | null -> current := Lazy<_>.Create (fun () -> f !index)
                | _    -> ()
                // forced or re-forced immediately.
                (!current).Force ()
            { new IEnumerator<'U> with
                  member __.Current = getCurrent ()
              interface IEnumerator with
                  member __.Current = box (getCurrent ())
                  member __.MoveNext () =
                      if !index = completed then false
                      elif !index = unstarted then
                          setIndex 0
                          true
                      else (
                          if !index = System.Int32.MaxValue then raise <| System.InvalidOperationException ("Enumeration based on System.Int32 exceeded System.Int32.MaxValue.")
                          if !index = finalIndex then false
                          else
                              setIndex (!index + 1)
                              true )
                  member __.Reset () = noReset ()
              interface System.IDisposable with
                  member __.Dispose () = () }

    /// <summary>
    /// Zip two input Enumerators into a new Enumerator yielding pairs.
    /// </summary>
    /// <param name="e1">The first input Enumerator.</param>
    /// <param name="e2">The second input Enumerator.</param>
    /// <returns>An Enumerator that enumerates pairs of two input Enumerators.</returns>
    let zip (e1: IEnumerator<_>) (e2: IEnumerator<_>) : IEnumerator<_> =
        upcast {
            new MapEnumerator<_> () with
                member __.DoMoveNext curr =
                    let n1 = e1.MoveNext ()
                    let n2 = e2.MoveNext ()
                    if n1 && n2 then curr <- (e1.Current, e2.Current); true
                    else false
                member __.Dispose () =
                    try e1.Dispose ()
                    finally e2.Dispose () }

    /// <summary>
    /// Zip three input Enumerators into a new Enumerator yielding triples.
    /// </summary>
    /// <param name="e1">The first input Enumerator.</param>
    /// <param name="e2">The second input Enumerator.</param>
    /// <param name="e3">The third input Enumerator.</param>
    /// <returns>An Enumerator that enumerates triples of three input Enumerators.</returns>
    let zip3 (e1: IEnumerator<_>) (e2: IEnumerator<_>) (e3: IEnumerator<_>) : IEnumerator<_> =
        upcast {
            new MapEnumerator<_> () with
                member __.DoMoveNext curr =
                    let n1 = e1.MoveNext ()
                    let n2 = e2.MoveNext ()
                    let n3 = e3.MoveNext ()
                    if n1 && n2 && n3 then curr <- (e1.Current, e2.Current, e3.Current); true
                    else false
                member __.Dispose () =
                    try e1.Dispose ()
                    finally
                        try e2.Dispose ()
                        finally e3.Dispose () }
#endif