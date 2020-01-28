namespace FSharpPlus

open System

#if !FABLE_COMPILER

/// Additional operations on IEnumerator
[<RequireQualifiedAccess>]
module Enumerator =
        
    let inline invalidArgFmt (arg: string) (format: string) paramArray = 
        let msg = String.Format (format,paramArray)
        raise (new ArgumentException (msg, arg))
    
    let noReset ()         = raise (new System.NotSupportedException ("Reset is not supported on this enumerator."))
    let notStarted ()      = invalidOp "Enumeration has not started. Call MoveNext."
    let alreadyFinished () = invalidOp "Enumeration already finished."
    let check started = if not started then notStarted ()
    let dispose (r: System.IDisposable) = r.Dispose ()
    
    open System.Collections
    open System.Collections.Generic
    
    /// A concrete implementation of an enumerator that returns no values
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
              
    let Empty<'T> () = new EmptyEnumerator<'T>() :> IEnumerator<'T>

    let singleton x = (Seq.singleton x).GetEnumerator()

    type IFinallyEnumerator = abstract AppendFinallyAction : (unit -> unit) -> unit

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

    let concat sources = new ConcatEnumerator<_> (sources) :> IEnumerator<_>
    
    let rec tryItem index (e: IEnumerator<'T>) =
        if not (e.MoveNext ()) then None
        elif index = 0 then Some e.Current
        else tryItem (index-1) e
    
    let rec nth index (e: IEnumerator<'T>) =
        if not (e.MoveNext ()) then
            let shortBy = index + 1
            invalidArgFmt "index"
                "{0}\nseq was short by {1} {2}"
                [|"The input sequence has an insufficient number of elements."; shortBy; (if shortBy = 1 then "element" else "elements")|]
        if index = 0 then e.Current
        else nth (index-1) e
    
    [<NoEquality; NoComparison>]
    type MapEnumeratorState =
        | NotStarted
        | InProcess
        | Finished
    
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
    
    let choose f (e: IEnumerator<'T>) =
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
                      curr := f e.Current
                  Option.isSome !curr
              member __.Reset() = noReset ()
          interface System.IDisposable with
              member __.Dispose () = e.Dispose () }
    
    let filter f (e: IEnumerator<'T>) =
        let started = ref false
        { new IEnumerator<'T> with
                member __.Current = check !started; e.Current
            interface IEnumerator with
                member __.Current = check !started; box e.Current
                member __.MoveNext () =
                    let rec next () =
                        if not !started then started := true
                        e.MoveNext () && (f e.Current || next ())
                    next ()
                member __.Reset () = noReset ()
            interface System.IDisposable with
                member __.Dispose () = e.Dispose () }
    
    let unfold f x : IEnumerator<_> =
        let state = ref x
        upcast {
            new MapEnumerator<_> () with
                member __.DoMoveNext curr =
                    match f !state with
                    |   None -> false
                    |   Some (r, s) ->
                            curr <- r
                            state := s
                            true
                member __.Dispose () = () }
    
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
            // The Current value for a valid index is "f i".
            // Lazy<_> values are used as caches, to store either the result or an exception if thrown.
            // These "Lazy<_>" caches are created only on the first call to current and forced immediately.
            // The lazy creation of the cache nodes means enumerations that skip many Current values are not delayed by GC.
            // For example, the full enumeration of Seq.initInfinite in the tests.
            // state
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