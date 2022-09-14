namespace FSharpPlus.Data

#if !FABLE_COMPILER || FABLE_COMPILER_3

open System.ComponentModel
open FSharpPlus
open FSharpPlus.Internals.Prelude


/// Additional operations on Seq
module Seq =

    /// <summary>
    /// Evaluates each action in the sequence from left to right and collect the results.
    /// </summary>
    let inline sequence (ms: seq<'``Applicative<'T>``>) : '``Applicative<seq<'T>>`` = sequence ms

    /// <summary>
    /// Maps each element of the sequence to an action, evaluates these actions from left to right and collect the results.
    /// </summary>
    let inline traverse (f: 'T->'``Applicative<'U>``) (xs: seq<'T>) : '``Applicative<seq<'U>>`` = traverse f xs

    let inline replicateM count (initial: '``Applicative<'T>``) = sequence (Seq.replicate count initial)

open System
open System.Collections.Generic
open FSharpPlus.Control

#endif
#nowarn "0193"
#if !FABLE_COMPILER


// BEGIN old SeqT code

[<Struct; EditorBrowsable(EditorBrowsableState.Never)>]
type SeqT<'``monad<seq<'t>>``> = SeqT of '``monad<seq<'t>>``

/// Basic operations on SeqT
[<RequireQualifiedAccess; EditorBrowsable(EditorBrowsableState.Never)>]
module SeqT =
    let run (SeqT m) = m

    /// Embed a Monad<'T> into a SeqT<'Monad<seq<'T>>>
    let inline lift (x: '``Monad<'T>``) : SeqT<'``Monad<seq<'T>>``> =
           if opaqueId false then x |> liftM Seq.singleton |> SeqT
           else x |> map Seq.singleton |> SeqT

    let inline internal sequence ms =
        let k m m' = m >>= fun (x: 'a) -> m' >>= fun (xs: seq<'a>) -> (result: seq<'a> -> 'M) (seq {yield x; yield! xs})
        Seq.foldBack k ms ((result: seq<'a> -> 'M) Seq.empty)

    let inline internal mapM f as' = sequence (Seq.map f as')

    let inline bind (f: 'T-> SeqT<'``Monad<seq<'U>``>) (SeqT m: SeqT<'``Monad<seq<'T>``>)          = SeqT (m >>= (mapM : _->seq<_>->_) (run << f) >>= ((Seq.concat: seq<seq<_>>->_) >> result))
    let inline apply (SeqT f: SeqT<'``Monad<seq<('T -> 'U)>``>) (SeqT x: SeqT<'``Monad<seq<'T>``>) = SeqT (map (Seq.apply : seq<_->_>->seq<_>->seq<_>) f <*> x) : SeqT<'``Monad<seq<'U>``>
    let inline lift2 (f: 'T->'U->'V) (SeqT x: SeqT<'``Monad<seq<'T>``>) (SeqT y: SeqT<'``Monad<seq<'U>``>) = SeqT (lift2 (Seq.lift2 f) x y)                     : SeqT<'``Monad<seq<'V>``>
    let inline lift3 (f: 'T->'U->'V->'W) (SeqT x: SeqT<'``Monad<seq<'T>``>) (SeqT y: SeqT<'``Monad<seq<'U>``>) (SeqT z: SeqT<'``Monad<seq<'V>``>) = SeqT (lift3 (Seq.lift3 f) x y z) : SeqT<'``Monad<seq<'W>``>
    let inline map (f: 'T->'U) (SeqT m: SeqT<'``Monad<seq<'T>``>)                                  = SeqT <| map (Seq.map f : (seq<_>->_)) m                    : SeqT<'``Monad<seq<'U>``>

type SeqT<'``monad<seq<'t>>``> with

    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member inline Return (x: 'T) = x |> Seq.singleton |> result |> SeqT                                     : SeqT<'``Monad<seq<'T>``>
    
    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member inline Map   (x: SeqT<'``Monad<seq<'T>``>, f: 'T->'U) = SeqT.map f x                             : SeqT<'``Monad<seq<'U>``>

    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member inline Lift2 (f: 'T->'U->'V, x: SeqT<'``Monad<seq<'T>``>, y: SeqT<'``Monad<seq<'U>``>) = SeqT.lift2 f x y : SeqT<'``Monad<seq<'V>``>

    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member inline Lift3 (f: 'T->'U->'V->'W, x: SeqT<'``Monad<seq<'T>``>, y: SeqT<'``Monad<seq<'U>``>, z: SeqT<'``Monad<seq<'V>``>) = SeqT.lift3 f x y z : SeqT<'``Monad<seq<'W>``>

    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member inline (<*>) (f: SeqT<'``Monad<seq<('T -> 'U)>``>, x: SeqT<'``Monad<seq<'T>``>) = SeqT.apply f x : SeqT<'``Monad<seq<'U>``>
    
    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member inline (>>=) (x: SeqT<'``Monad<seq<'T>``>, f: 'T -> SeqT<'``Monad<seq<'U>``>)   = SeqT.bind  f x

    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member inline get_Empty () = SeqT <| result Seq.empty : SeqT<'``MonadPlus<seq<'T>``>
    
    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member inline (<|>) (SeqT x, SeqT y) = SeqT <| (x >>= (fun a -> y >>= (fun b ->  result ((Seq.append:seq<_>->seq<_>->_) a b)))) : SeqT<'``MonadPlus<seq<'T>``>

    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member inline TryWith (source: SeqT<'``Monad<seq<'T>>``>, f: exn -> SeqT<'``Monad<seq<'T>>``>) = SeqT (TryWith.Invoke (SeqT.run source) (SeqT.run << f))
    
    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member inline TryFinally (computation: SeqT<'``Monad<seq<'T>>``>, f) = SeqT (TryFinally.Invoke     (SeqT.run computation) f)

    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member inline Using (resource, f: _ -> SeqT<'``Monad<seq<'T>>``>)    = SeqT (Using.Invoke resource (SeqT.run << f))
    
    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member inline Delay (body : unit   ->  SeqT<'``Monad<seq<'T>>``>)    = SeqT (Delay.Invoke (fun _ -> SeqT.run (body ()))) : SeqT<'``Monad<seq<'T>>``>
    
    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member inline Lift (x: '``Monad<'T>``) : SeqT<'``Monad<seq<'T>>``> = SeqT.lift x
    
    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member inline LiftAsync (x: Async<'T>) = SeqT.lift (liftAsync x) : SeqT<'``MonadAsync<'T>``>
    
    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member inline Throw (x: 'E) = x |> throw |> SeqT.lift
    
    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member inline Catch (m: SeqT<'``MonadError<'E1,'T>``>, h: 'E1 -> SeqT<'``MonadError<'E2,'T>``>) = SeqT ((fun v h -> catch v h) (SeqT.run m) (SeqT.run << h)) : SeqT<'``MonadError<'E2,'T>``>
    
    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member inline CallCC (f: (('T -> SeqT<'``MonadCont<'R,seq<'U>>``>) -> _)) = SeqT (callCC <| fun c -> SeqT.run (f (SeqT  << c << Seq.singleton ))) : SeqT<'``MonadCont<'R, seq<'T>>``>
    
    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member inline get_Get ()  = SeqT.lift get         : SeqT<'``MonadState<'S,'S>``>
    
    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member inline Put (x: 'S) = x |> put |> SeqT.lift : SeqT<'``MonadState<unit,'S>``>
    
    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member inline get_Ask () = SeqT.lift ask          : SeqT<'``MonadReader<'R,seq<'R>>``>
    
    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member inline Local (SeqT (m: '``MonadReader<'R2,'T>``), f: 'R1->'R2) = SeqT (local f m)

// END old SeqT code


[<EditorBrowsable(EditorBrowsableState.Never)>]
module Internal =
    let inline monomorphicBind (binder: 'T -> '``Monad<'T>``) (source: '``Monad<'T>``) : '``Monad<'T>`` =
        let inline call (_mthd: 'M, input: 'I, _output: 'I, f) = ((^M or ^I) : (static member (>>=) : _*_ -> _) input, f)
        call (Unchecked.defaultof<Bind>, source, Unchecked.defaultof<'``Monad<'T>``>, binder)

open Internal

[<EditorBrowsable(EditorBrowsableState.Never)>]
type MonadFxStrictBuilderMod<'``monad<'t>``> () =
    inherit FSharpPlus.GenericBuilders.MonadFxStrictBuilder<'``monad<'t>``> ()
    member inline _.Delay expr = (fun () -> Delay.Invoke expr) : unit -> '``Monad<'T>``

[<EditorBrowsable(EditorBrowsableState.Never)>]
type MonadPlusStrictBuilderMod<'``monad<'t>``> () =
    inherit FSharpPlus.GenericBuilders.MonadPlusStrictBuilder<'``monad<'t>``> ()
    member inline _.Delay expr = (fun () -> Delay.Invoke expr) : unit -> '``Monad<'T>``

[<EditorBrowsable(EditorBrowsableState.Never)>]
type MonadFxStrictBuilderMod2<'``monad<'t>``, ^``monad<unit>``>
                                    when (Return or ^``monad<unit>``) : (static member Return: ^``monad<unit>`` * Return -> (unit -> ^``monad<unit>``)) 
                                    and  (Bind   or ^``monad<unit>``) : (static member (>>=): ^``monad<unit>`` * (unit -> ^``monad<unit>``) -> ^``monad<unit>``)
                                    and  (Using  or ^``monad<unit>``) : (static member Using: IDisposable * (IDisposable -> ^``monad<unit>``) * Using -> ^``monad<unit>``)
                                         () =
    inherit StrictBuilder<'``monad<'t>``> ()
    member inline _.Zero () = result ()                                       : '``monad<unit>``
    member inline _.Combine (a: '``Monad<unit>``, b) = a >>= (fun () -> b ()) : '``Monad<'T>``
    
    member inline _.While (guard, body: unit -> '``monad<unit>``)             : '``monad<unit>`` =
        let rec loop guard body =
            if guard () then body () |> monomorphicBind (fun () -> loop guard body)
            else result ()
        loop guard body
    
     member inline this.For (p: #seq<'T>, rest: 'T->'``monad<unit>``) =
         Using.Invoke (p.GetEnumerator () :> IDisposable) (fun enum ->
             let enum = enum :?> IEnumerator<_>
             this.While (enum.MoveNext, fun () -> rest enum.Current) : '``monad<unit>``)

    member inline _.Delay expr = (fun () -> Delay.Invoke expr) : unit -> '``Monad<'T>``


[<EditorBrowsable(EditorBrowsableState.Never)>]
module SpecialBuilders =
    let innerMonad<'mt> = new MonadFxStrictBuilderMod<'mt> ()
    let inline innerMonad2<'mt, .. > () = new MonadFxStrictBuilderMod2<'mt, _> ()

open SpecialBuilders


type IEnumeratorM<'``Monad<bool>``, 'T> =
    abstract MoveNext : unit -> '``Monad<bool>``
    abstract Current : 'T with get
    inherit IDisposable

type IEnumerableM<'``Monad<bool>``, 'T> =
    abstract GetEnumerator : unit -> IEnumeratorM<'``Monad<bool>``, 'T>

// Monad Transformer for seq<'T>
[<Struct>]
type SeqT<'``monad``, 't> =
    | SeqT of IEnumerableM<'``monad``, 't>
    interface IEnumerableM<'``monad``, 't> with
        member x.GetEnumerator () = let (SeqT x) = x in x.GetEnumerator ()

[<AutoOpen>]
module SeqT_V2 =
  module SeqT =

    [<Literal>]
    let private enumNotStarted = "Enumeration has not started. Call MoveNext."

    /// Creates a SeqT sequence from an IEnumerableM.
    let ofIEnumerableM x : SeqT<'``Monad<bool>``, 'T> = SeqT x

    [<RequireQualifiedAccess; EditorBrowsable(EditorBrowsableState.Never)>]
    type SeqState<'``Monad<seq<'T>>``, 'T> =
       | NotStarted    of '``Monad<seq<'T>>``
       | HasEnumerator of IEnumerator<'T>
       | Finished

    let inline wrap (source: '``Monad<seq<'T>>``) : SeqT<'``Monad<bool>``, 'T> =
        SeqT
            { new IEnumerableM<'``Monad<bool>``, 'T> with
                member _.GetEnumerator () =
                    let mutable state   = SeqState.NotStarted source
                    let mutable current = Option<'T>.None
                    { new IEnumeratorM<'``Monad<bool>``, 'T> with
                        member _.Current =
                            match current with
                            | Some c -> c
                            | None -> invalidOp enumNotStarted
                        member x.MoveNext () = monad' {
                            match state with
                            | SeqState.NotStarted inp ->
                                let! (s: seq<'T>) = inp
                                let e1 = s.GetEnumerator ()
                                state <- SeqState.HasEnumerator e1
                                return! (x.MoveNext ())
                            | SeqState.HasEnumerator e1 ->
                                let res1 = e1.MoveNext ()
                                if res1 then
                                    current <- Some e1.Current
                                    return true
                                else
                                    x.Dispose ()
                                    return! x.MoveNext ()
                            | _ -> return false }
                        member _.Dispose () =
                            match state with
                            | SeqState.HasEnumerator e1 ->
                                state <- SeqState.Finished
                                dispose e1
                            | _ -> () } }

    
    /// Transforms a regular sequence into a SeqT, driven by the return type.
    let inline ofSeq (source: seq<'T>) : SeqT<'``Monad<bool>``, 'T> =
        SeqT
            { new IEnumerableM<'``Monad<bool>``, 'T> with
                member _.GetEnumerator () =
                    let mutable state   = SeqState.NotStarted source
                    let mutable current = Option<'T>.None
                    { new IEnumeratorM<'``Monad<bool>``, 'T> with
                        member _.Current =
                            match current with
                            | Some c -> c
                            | None -> invalidOp enumNotStarted
                        member x.MoveNext () = monad' {
                            match state with
                            | SeqState.NotStarted inp ->
                                let e = inp.GetEnumerator ()
                                state <- SeqState.HasEnumerator e
                                return! x.MoveNext ()
                            | SeqState.HasEnumerator e ->
                                return
                                    (if e.MoveNext () then
                                         current <- Some e.Current
                                         true
                                     else
                                         x.Dispose ()
                                         false)
                            | _ -> return false }
                        member _.Dispose () =
                            match state with
                            | SeqState.HasEnumerator e ->
                                state <- SeqState.Finished
                                dispose e
                            | _ -> () } }

    /// Transforms a regular sequence into a SeqT, driven by the return type.
    /// An alias of `ofSeq`.
    let inline hoist (source: seq<'T>) : SeqT<'``Monad<bool>``, 'T> = ofSeq source

    [<EditorBrowsable(EditorBrowsableState.Never)>]
    let inline runThen<'T, .. > (f: ResizeArray<'T> -> 'R) (source: SeqT<'``Monad<bool>``, 'T>) : '``Monad<'R>`` =
        let ra = new ResizeArray<_> ()
        Using.Invoke
            ((source :> IEnumerableM<'``Monad<bool>``, 'T>).GetEnumerator ())
            (fun ie ->
                ie.MoveNext () >>= fun (move: bool) ->
                    let mutable b = move
                    let rec loop guard (body: unit -> '``Monad<unit>``) : '``Monad<unit>`` =
                        if guard () then body () >>= (fun () -> loop guard body)
                        else result ()
                    loop
                        (fun () -> b)
                        (fun () ->
                            ra.Add ie.Current
                            ie.MoveNext () >>= fun moven ->
                                b <- moven
                                result () )
                    >>= fun () -> result (f ra))
    
    let inline runAsArray<'T, .. > (source: SeqT<'``Monad<bool>``, 'T>) : '``Monad<'T []>`` =
        runThen<'T, 'T [], '``Monad<bool>``, '``Monad<'T []>``, '``Monad<unit>``> toArray source
    
    let inline runAsList<'T, .. > (source: SeqT<'``Monad<bool>``, 'T>) : '``Monad<'T list>`` =
        runThen<'T, 'T list, '``Monad<bool>``, '``Monad<'T list>``, '``Monad<unit>``> toList source

    let inline run<'T, .. >       (source: SeqT<'``Monad<bool>``, 'T>) : '``Monad<'T seq>`` =
        runThen<'T, 'T seq, '``Monad<bool>``, '``Monad<'T seq>``, '``Monad<unit>``> toSeq source
    
    [<GeneralizableValue>]
    let inline empty<'T, .. > : SeqT<'``Monad<bool>``, 'T> =
        SeqT
            { new IEnumerableM<'``Monad<bool>``, 'T> with
                member _.GetEnumerator () =
                    { new IEnumeratorM<'``Monad<bool>``, 'T> with
                        member _.MoveNext () = result false
                        member _.Current = invalidOp enumNotStarted
                        member _.Dispose () = () } }

    let inline singleton (v: 'T) : SeqT<'``Monad<bool>``, 'T> =
        SeqT
            { new IEnumerableM<'``Monad<bool>``, 'T> with 
                member _.GetEnumerator () = 
                    let mutable started = false
                    { new IEnumeratorM<'``Monad<bool>``, 'T> with 
                        member _.MoveNext () = innerMonad {
                            let res = not started
                            started <- true
                            return res }
                        member _.Current =
                            if started then v
                            else invalidOp enumNotStarted
                        member _.Dispose () = () } }

    [<EditorBrowsable(EditorBrowsableState.Never)>]
    let inline make (f: unit -> '``Monad<SeqT<'Monad<bool>, 'T>>``) : SeqT<'``Monad<bool>``, 'T> =
        SeqT
            { new IEnumerableM<'``Monad<bool>``, 'T> with
                member _.GetEnumerator () = 
                    let mutable state   = -1
                    let mutable enum    = Unchecked.defaultof<IEnumeratorM<'``Monad<bool>``, 'T>>
                    let mutable current = Unchecked.defaultof<'T>
                    { new IEnumeratorM<'``Monad<bool>``, 'T> with 
                        member _.Current =
                            match state with
                            | -1 -> invalidOp enumNotStarted
                            | _  -> current
                        member x.MoveNext () = innerMonad {
                            match state with
                            | -1 -> 
                                let! (s: SeqT<'``Monad<bool>``, 'T>) = f ()
                                return! (
                                    let e = (s :> IEnumerableM<'``Monad<bool>``, 'T>).GetEnumerator ()
                                    enum  <- e
                                    state <- 0
                                    x.MoveNext ())
                            | 0 ->   
                                let e = enum
                                let! (res: bool) = e.MoveNext ()
                                do
                                    current <- e.Current
                                    if not res then x.Dispose ()
                                return res
                            | _ -> return false }
                        member _.Dispose () = 
                            match state with 
                            | 0 -> 
                                let e = enum
                                state <- 1
                                enum  <- Unchecked.defaultof<_>
                                dispose e 
                            | _ -> () } }

    let delay (f: unit -> SeqT<'``Monad<bool>``, 'T>) : SeqT<'``Monad<bool>``, 'T> =
        SeqT
            { new IEnumerableM<'``Monad<bool>``, 'T> with
                member _.GetEnumerator () = (f () :> IEnumerableM<'``Monad<bool>``, 'T>).GetEnumerator () }

    /// A combination of bind and lift operations.
    let inline bindLift<'T, 'U, .. > (f: 'T -> SeqT<'``Monad<bool>``, 'U>) (source: '``Monad<'T>``) : SeqT<'``Monad<bool>``, 'U> =
        make (fun () -> innerMonad<'``Monad<SeqT<'Monad<bool>, 'U>>``> { let! v = source in return f v })

    /// Lifts the source into the SeqT.
    let inline lift (source: '``Monad<'T>``) : SeqT<'``Monad<bool>``, 'T> =
        SeqT
            { new IEnumerableM<'``Monad<bool>``, 'T> with 
                member _.GetEnumerator () = 
                    let mutable started = None
                    { new IEnumeratorM<'``Monad<bool>``, 'T> with
                        member _.MoveNext () =
                            match started with
                            | Some _ -> result false
                            | None -> source |> (if opaqueId false then liftM else map) (fun v -> started <- Some v; true)
                        member _.Current =
                            match started with
                            | Some v -> v
                            | None -> invalidOp enumNotStarted
                        member _.Dispose () = () } }

    [<RequireQualifiedAccess; EditorBrowsable(EditorBrowsableState.Never)>]
    type CollectState<'T, 'U, '``Monad<bool>``> =
       | NotStarted    of SeqT<'``Monad<bool>``, 'T>
       | HaveInputEnumerator of IEnumeratorM<'``Monad<bool>``, 'T>
       | HaveInnerEnumerator of IEnumeratorM<'``Monad<bool>``, 'T> * IEnumeratorM<'``Monad<bool>``, 'U>
       | Finished

    let inline collect<'T, 'U, .. > (f: 'T -> SeqT<'``Monad<bool>``, 'U>) (source: SeqT<'``Monad<bool>``, 'T>) : SeqT<'``Monad<bool>``, 'U> =
        SeqT
            { new IEnumerableM<'``Monad<bool>``, 'U> with
                member _.GetEnumerator () =
                    let mutable state   = CollectState.NotStarted source
                    let mutable current = Option<'U>.None
                    { new IEnumeratorM<'``Monad<bool>``, 'U>  with
                        member _.Current =
                            match current with
                            | Some c -> c
                            | None -> invalidOp enumNotStarted
                        member x.MoveNext () = monad' {
                            match state with
                            | CollectState.NotStarted inp ->
                                return! (
                                    let e1 = (inp :> IEnumerableM<'``Monad<bool>``, 'T>).GetEnumerator ()
                                    state <- CollectState.HaveInputEnumerator e1
                                    x.MoveNext ())
                            | CollectState.HaveInputEnumerator e1 ->
                                let! res1 = e1.MoveNext ()
                                return! (
                                    if res1 then
                                        let e2 = (f e1.Current :> IEnumerableM<'``Monad<bool>``, 'U>).GetEnumerator ()
                                        state <- CollectState.HaveInnerEnumerator (e1, e2)
                                    else x.Dispose ()
                                    x.MoveNext ())
                            | CollectState.HaveInnerEnumerator (e1, e2) ->
                                let! (res2: bool) = e2.MoveNext ()
                                if res2 then
                                    current <- Some e2.Current
                                    return res2
                                else
                                    state <- CollectState.HaveInputEnumerator e1
                                    dispose e2
                                    return! x.MoveNext ()
                            | _ -> return false }
                        member _.Dispose () =
                            match state with
                            | CollectState.HaveInputEnumerator e1 ->
                                state <- CollectState.Finished
                                dispose e1
                            | CollectState.HaveInnerEnumerator (e1, e2) ->
                                state <- CollectState.Finished
                                dispose e2
                                dispose e1
                            | _ -> () } }

    let inline apply<'T, 'U, .. > (f: SeqT<'``Monad<bool>``, 'T -> 'U>) (x1: SeqT<'``Monad<bool>``, 'T>) : SeqT<'``Monad<bool>``, 'U> =
        SeqT
            { new IEnumerableM<'``Monad<bool>``, 'U> with
                member _.GetEnumerator () =
                    let mutable state   = CollectState.NotStarted f
                    let mutable current = Option<'U>.None
                    { new IEnumeratorM<'``Monad<bool>``, 'U>  with
                        member _.Current =
                            match current with
                            | Some c -> c
                            | None -> invalidOp enumNotStarted
                        member x.MoveNext () = monad' {
                            match state with
                            | CollectState.NotStarted f ->
                                return! (
                                    let e1 = (f :> IEnumerableM<'``Monad<bool>``, ('T -> 'U)>).GetEnumerator ()
                                    state <- CollectState.HaveInputEnumerator e1
                                    x.MoveNext ())
                            | CollectState.HaveInputEnumerator e1 ->
                                let! res1 = e1.MoveNext ()
                                return! (
                                    if res1 then
                                        let e2 = (x1 :> IEnumerableM<'``Monad<bool>``, 'T>).GetEnumerator ()
                                        state <- CollectState.HaveInnerEnumerator (e1, e2)
                                    else x.Dispose ()
                                    x.MoveNext ())
                            | CollectState.HaveInnerEnumerator (e1, e2) ->
                                let! (res2: bool) = e2.MoveNext ()
                                if res2 then
                                    current <- Some (e1.Current e2.Current)
                                    return res2
                                else
                                    state <- CollectState.HaveInputEnumerator e1
                                    dispose e2
                                    return! x.MoveNext ()
                            | _ -> return false }
                        member _.Dispose () =
                            match state with
                            | CollectState.HaveInputEnumerator e1 ->
                                state <- CollectState.Finished
                                dispose e1
                            | CollectState.HaveInnerEnumerator (e1, e2) ->
                                state <- CollectState.Finished
                                dispose e2
                                dispose e1
                            | _ -> () } }

    let inline lift2<'T1, 'T2, 'U, .. > (f: 'T1 -> 'T2 -> 'U) (x1: SeqT<'``Monad<bool>``, 'T1>) (x2: SeqT<'``Monad<bool>``, 'T2>) : SeqT<'``Monad<bool>``, 'U> =
        SeqT
            { new IEnumerableM<'``Monad<bool>``, 'U> with
                member _.GetEnumerator () =
                    let mutable state   = CollectState.NotStarted x1
                    let mutable current = Option<'U>.None
                    { new IEnumeratorM<'``Monad<bool>``, 'U>  with
                        member _.Current =
                            match current with
                            | Some c -> c
                            | None -> invalidOp enumNotStarted
                        member x.MoveNext () = monad' {
                            match state with
                            | CollectState.NotStarted x1 ->
                                return! (
                                    let e1 = (x1 :> IEnumerableM<'``Monad<bool>``, 'T1>).GetEnumerator ()
                                    state <- CollectState.HaveInputEnumerator e1
                                    x.MoveNext ())
                            | CollectState.HaveInputEnumerator e1 ->
                                let! res1 = e1.MoveNext ()
                                return! (
                                    if res1 then
                                        let e2 = (x2 :> IEnumerableM<'``Monad<bool>``, 'T2>).GetEnumerator ()
                                        state <- CollectState.HaveInnerEnumerator (e1, e2)
                                    else x.Dispose ()
                                    x.MoveNext ())
                            | CollectState.HaveInnerEnumerator (e1, e2) ->
                                let! (res2: bool) = e2.MoveNext ()
                                if res2 then
                                    current <- Some (f e1.Current e2.Current)
                                    return res2
                                else
                                    state <- CollectState.HaveInputEnumerator e1
                                    dispose e2
                                    return! x.MoveNext ()
                            | _ -> return false }
                        member _.Dispose () =
                            match state with
                            | CollectState.HaveInputEnumerator e1 ->
                                state <- CollectState.Finished
                                dispose e1
                            | CollectState.HaveInnerEnumerator (e1, e2) ->
                                state <- CollectState.Finished
                                dispose e2
                                dispose e1
                            | _ -> () } }
    
    [<RequireQualifiedAccess; EditorBrowsable(EditorBrowsableState.Never)>]
    type AppendState<'``Monad<bool>``, 'T> =
       | NotStarted1     of SeqT<'``Monad<bool>``, 'T> * SeqT<'``Monad<bool>``, 'T>
       | HasEnumerator1 of IEnumeratorM<'``Monad<bool>``, 'T> * SeqT<'``Monad<bool>``, 'T>
       | NotStarted2     of SeqT<'``Monad<bool>``, 'T>
       | HasEnumerator2 of IEnumeratorM<'``Monad<bool>``, 'T> 
       | Finished

    let inline append (source1: SeqT<'``Monad<bool>``, 'T>) (source2: SeqT<'``Monad<bool>``, 'T>) : SeqT<'``Monad<bool>``, 'T> =
        SeqT
            { new IEnumerableM<'``Monad<bool>``, 'T>  with 
                member _.GetEnumerator () = 
                    let mutable state   = AppendState.NotStarted1 (source1, source2)
                    let mutable current = Option<'T>.None
                    { new IEnumeratorM<'``Monad<bool>``, 'T>  with
                        member _.Current =
                            match current with
                            | Some c -> c
                            | None -> invalidOp enumNotStarted
                        member x.MoveNext () = innerMonad {
                            match state with 
                            | AppendState.NotStarted1 (inp1, inp2) -> 
                                return! (
                                    let enum1 = (inp1 :> IEnumerableM<'``Monad<bool>``, 'T>).GetEnumerator ()
                                    state <- AppendState.HasEnumerator1 (enum1, inp2)
                                    x.MoveNext ())
                            | AppendState.HasEnumerator1 (enum1, inp2) ->
                                let! (res: bool) = enum1.MoveNext ()
                                if res then
                                    current <- Some enum1.Current
                                    return res
                                else
                                    return! (
                                        state <- AppendState.NotStarted2 inp2
                                        dispose enum1
                                        x.MoveNext ())
                            | AppendState.NotStarted2 inp2 ->
                                return! (
                                    let enum2 = (inp2 :> IEnumerableM<'``Monad<bool>``, 'T>).GetEnumerator ()
                                    state <- AppendState.HasEnumerator2 enum2
                                    x.MoveNext ())
                            | AppendState.HasEnumerator2 enum2 ->   
                                let! (res: bool) = enum2.MoveNext ()
                                return (
                                    if res then current <- Some enum2.Current
                                    else
                                        state <- AppendState.Finished
                                        dispose enum2
                                    res)
                            | _ -> return false }
                          member _.Dispose () = 
                              match state with 
                              | AppendState.HasEnumerator1 (enum, _) 
                              | AppendState.HasEnumerator2 enum -> 
                                  state <- AppendState.Finished
                                  dispose enum 
                              | _ -> () } }

    /// A transformation, which traverses the sequence with an action in the inner monad.
    let inline mapM (f: 'T -> '``Monad<'U>``) (source: SeqT<'``Monad<bool>``, 'T>) : SeqT<'``Monad<bool>``, 'U> =
        source |> collect (fun itm ->
            f itm |> bindLift<_, _, _, '``Monad<SeqT<'Monad<bool>, 'U>>``, _> singleton)
    
    [<RequireQualifiedAccess; EditorBrowsable(EditorBrowsableState.Never)>]
    type MapState<'T, '``Monad<bool>``> =
       | NotStarted     of SeqT<'``Monad<bool>``, 'T>
       | HasEnumerator of IEnumeratorM<'``Monad<bool>``, 'T>
       | Finished

    let inline map (f: 'T -> 'U) (source: SeqT<'``Monad<bool>``, 'T>) : SeqT<'``Monad<bool>``, 'U> =
        SeqT
            { new IEnumerableM<'``Monad<bool>``, 'U> with
                member _.GetEnumerator () =
                    let mutable state   = MapState.NotStarted source
                    let mutable current = Option<'U>.None
                    { new IEnumeratorM<'``Monad<bool>``, 'U> with
                        member _.Current =
                            match current with
                            | Some c -> c
                            | None -> invalidOp enumNotStarted
                        member x.MoveNext () = innerMonad {
                              match state with
                              | MapState.NotStarted inp ->
                                  return! (
                                      let e = (inp :> IEnumerableM<'``Monad<bool>``, 'T>).GetEnumerator ()
                                      state <- MapState.HasEnumerator e
                                      x.MoveNext ())
                              | MapState.HasEnumerator e ->
                                  let! res1 = e.MoveNext ()
                                  if res1 then
                                      current <- Some (f e.Current)
                                      return true
                                  else
                                      x.Dispose ()
                                      return! x.MoveNext ()
                              | _ -> return false }
                          member _.Dispose () =
                              match state with
                              | MapState.HasEnumerator e ->
                                  state <- MapState.Finished
                                  dispose e
                              | _ -> () } }

    [<RequireQualifiedAccess; EditorBrowsable(EditorBrowsableState.Never)>]
    type Map2State<'T1, 'T2, '``Monad<bool>``> =
       | NotStarted     of SeqT<'``Monad<bool>``, 'T1> * SeqT<'``Monad<bool>``, 'T2>
       | HasEnumerator of IEnumeratorM<'``Monad<bool>``, 'T1> * IEnumeratorM<'``Monad<bool>``, 'T2>
       | Finished

    let inline map2 (f: 'T1 -> 'T2 -> 'U) (source1: SeqT<'``Monad<bool>``, 'T1>) (source2: SeqT<'``Monad<bool>``, 'T2>) : SeqT<'``Monad<bool>``, 'U> =
        SeqT
            { new IEnumerableM<'``Monad<bool>``, 'U> with
                member _.GetEnumerator () =
                    let mutable state   = Map2State.NotStarted (source1, source2)
                    let mutable current = Option<'U>.None
                    { new IEnumeratorM<'``Monad<bool>``, 'U> with
                        member _.Current =
                            match current with
                            | Some c -> c
                            | None -> invalidOp enumNotStarted
                        member x.MoveNext () = innerMonad {
                            match state with
                            | Map2State.NotStarted (s1, s2) -> return! (
                                let e1 = (s1 :> IEnumerableM<'``Monad<bool>``, 'T1>).GetEnumerator ()
                                let e2 = (s2 :> IEnumerableM<'``Monad<bool>``, 'T2>).GetEnumerator ()
                                state <- Map2State.HasEnumerator (e1, e2)
                                x.MoveNext ())
                            | Map2State.HasEnumerator (e1, e2) ->
                                let! res1 = e1.MoveNext ()
                                let! res2 = e2.MoveNext ()
                                if res1 && res2 then
                                    current <- Some (f e1.Current e2.Current)
                                    return true
                                else
                                    x.Dispose ()
                                    return! x.MoveNext ()
                            | _ -> return false }
                        member _.Dispose () =
                            match state with
                            | Map2State.HasEnumerator (e1, e2) ->
                                state <- Map2State.Finished
                                dispose e1
                                dispose e2
                            | _ -> () } }

    let inline map2M (f: 'T1 -> 'T2 -> '``Monad<'U>``) (source1: SeqT<'``Monad<bool>``, 'T1>) (source2: SeqT<'``Monad<bool>``, 'T2>) : SeqT<'``Monad<bool>``, 'U> =
        SeqT
            { new IEnumerableM<'``Monad<bool>``, 'U> with
                member _.GetEnumerator () =
                    let mutable state   = Map2State.NotStarted (source1, source2)
                    let mutable current = Option<'U>.None
                    { new IEnumeratorM<'``Monad<bool>``, 'U> with
                        member _.Current =
                            match current with
                            | Some c -> c
                            | None -> invalidOp enumNotStarted
                        member x.MoveNext () = innerMonad {
                            match state with
                            | Map2State.NotStarted (s1, s2) ->
                                return! (
                                    let e1 = (s1 :> IEnumerableM<'``Monad<bool>``, 'T1>).GetEnumerator ()
                                    let e2 = (s2 :> IEnumerableM<'``Monad<bool>``, 'T2>).GetEnumerator ()
                                    state <- Map2State.HasEnumerator (e1, e2)
                                    x.MoveNext ())
                            | Map2State.HasEnumerator (e1, e2) ->
                                let! res1 = e1.MoveNext ()
                                let! res2 = e2.MoveNext ()
                                if res1 && res2 then
                                    let! x = f e1.Current e2.Current
                                    current <- Some x
                                    return true
                                else
                                    x.Dispose ()
                                    return! x.MoveNext ()
                            | _ -> return false }
                        member _.Dispose () =
                            match state with
                            | Map2State.HasEnumerator (e1, e2) ->
                                state <- Map2State.Finished
                                dispose e1
                                dispose e2
                            | _ -> () } }

    let inline map3<'T1, 'T2, 'T3, 'U, .. > (f: 'T1 -> 'T2 -> 'T3 -> 'U) (source1: SeqT<'``Monad<bool>``, 'T1>) (source2: SeqT<'``Monad<bool>``, 'T2>) (source3: SeqT<'``Monad<bool>``, 'T3>) : SeqT<'``Monad<bool>``, 'U> =
        map2 (<|) (map2 f source1 source2) source3

    let inline map3M (f: 'T1 -> 'T2 -> 'T3 -> '``Monad<'U>``) (source1: SeqT<'``Monad<bool>``, 'T1>) (source2: SeqT<'``Monad<bool>``, 'T2>) (source3: SeqT<'``Monad<bool>``, 'T3>) : SeqT<'``Monad<bool>``, 'U> =
        map2M (<|) (map2 f source1 source2) source3

    let inline zip (source1: SeqT<'``Monad<bool>``, 'T1>) (source2: SeqT<'``Monad<bool>``, 'T2>) : SeqT<'``Monad<bool>``, ('T1 * 'T2)> =
        map2 tuple2 source1 source2

    let inline zip3 (source1: SeqT<'``Monad<bool>``, 'T1>) (source2: SeqT<'``Monad<bool>``, 'T2>) (source3: SeqT<'``Monad<bool>``, 'T3>) : SeqT<'``Monad<bool>``, ('T1 * 'T2 * 'T3)> =
        map3 tuple3 source1 source2 source3
    
    let inline lift3<'T1, 'T2, 'T3, 'U, .. > (f: 'T1 -> 'T2 -> 'T3 -> 'U) (x1: SeqT<'``Monad<bool>``, 'T1>) (x2: SeqT<'``Monad<bool>``, 'T2>) (x3: SeqT<'``Monad<bool>``, 'T3>) : SeqT<'``Monad<bool>``, 'U> =
        f </map/> x1 </apply/> x2 </apply/> x3

    let inline filter (f: 'T -> bool) (source: SeqT<'``Monad<bool>``, 'T>) : SeqT<'``Monad<bool>``, 'T> =
        SeqT
            { new IEnumerableM<'``Monad<bool>``, 'T> with
                member _.GetEnumerator () =
                    let mutable state   = CollectState.NotStarted source
                    let mutable current = Option<'T>.None
                    { new IEnumeratorM<'``Monad<bool>``, 'T>  with
                        member _.Current =
                            match current with
                            | Some c -> c
                            | None -> invalidOp enumNotStarted
                        member x.MoveNext () = innerMonad {
                              match state with
                              | CollectState.NotStarted inp ->
                                  return! (
                                      let e1 = (inp :> IEnumerableM<'``Monad<bool>``, 'T>).GetEnumerator ()
                                      state <- CollectState.HaveInputEnumerator e1
                                      x.MoveNext ())
                              | CollectState.HaveInputEnumerator e1 ->
                                  let! res1 = e1.MoveNext ()
                                  if res1 && f e1.Current then
                                      current <- Some e1.Current
                                      return true
                                  elif res1 then return! x.MoveNext ()
                                  else
                                      x.Dispose ()
                                      return! x.MoveNext ()
                                  | _ -> return false }
                        member _.Dispose () =
                            match state with
                            | CollectState.HaveInputEnumerator e1 ->
                                state <- CollectState.Finished
                                dispose e1
                            | CollectState.HaveInnerEnumerator (e1, e2) ->
                                state <- CollectState.Finished
                                dispose e2
                                dispose e1
                            | _ -> () } }

    let inline iteriM<'T, .. > (f: int -> 'T -> '``Monad<unit>``) (source: SeqT<'``Monad<bool>``, 'T>) : '``Monad<unit>`` = innerMonad { 
        use ie = (source :> IEnumerableM<'``Monad<bool>``, 'T>).GetEnumerator ()
        let mutable count = 0
        let! (move: bool) = ie.MoveNext ()
        let mutable b = move
        while b do
            do! f count ie.Current
            let! moven = ie.MoveNext ()
            count <- count + 1
            b <- moven }

    let inline iterM<'T, .. > (f: 'T -> '``Monad<unit>``) (source: SeqT<'``Monad<bool>``, 'T>) : '``Monad<unit>`` = iteriM (fun _ x -> f x) source    
    let inline iteri<'T, .. > (f: int -> 'T -> unit)      (source: SeqT<'``Monad<bool>``, 'T>) : '``Monad<unit>`` = iteriM (fun i x -> result (f i x)) source
    let inline iter<'T, .. > f (source: SeqT<'``Monad<bool>``, 'T>) : '``Monad<unit>`` = iterM (f >> result) source

    [<RequireQualifiedAccess; EditorBrowsable(EditorBrowsableState.Never)>]
    type TryWithState<'``Monad<bool>``, 'T> =
       | NotStarted of SeqT<'``Monad<bool>``, 'T>
       | HaveBodyEnumerator of IEnumeratorM<'``Monad<bool>``, 'T>
       | HaveHandlerEnumerator of IEnumeratorM<'``Monad<bool>``, 'T>
       | Finished

    /// Implements the 'TryWith' functionality for the computation expression builder.
    let inline tryWith<'T, .. > (source: SeqT<'``Monad<bool>``, 'T>) (handler : exn -> SeqT<'``Monad<bool>``, 'T>) : SeqT<'``Monad<bool>``, 'T> =
        SeqT
            { new IEnumerableM<'``Monad<bool>``, 'T> with
                member _.GetEnumerator () =
                    let mutable state   = TryWithState.NotStarted source
                    let mutable current = Option<'T>.None
                    { new IEnumeratorM<'``Monad<bool>``, 'T> with
                        member _.Current =
                            match current with
                            | Some c -> c
                            | None -> invalidOp enumNotStarted
                        member x.MoveNext () = innerMonad2<_, '``Monad<unit>``> () {
                            match state with
                            | TryWithState.NotStarted inp ->
                                let mutable res = Unchecked.defaultof<_>
                                try
                                    res <- Choice1Of2 ((inp :> IEnumerableM<'``Monad<bool>``, 'T>).GetEnumerator ())
                                with exn ->
                                    res <- Choice2Of2 exn
                                match res with
                                | Choice1Of2 r ->
                                    return! (
                                       state <- TryWithState.HaveBodyEnumerator r
                                       x.MoveNext ())
                                | Choice2Of2 exn ->
                                    return! (
                                        x.Dispose ()
                                        let enum = (handler exn :> IEnumerableM<'``Monad<bool>``, 'T>).GetEnumerator ()
                                        state <- TryWithState.HaveHandlerEnumerator enum
                                        x.MoveNext ())
                            | TryWithState.HaveBodyEnumerator e ->
                                let mutable res = Unchecked.defaultof<Choice<bool, exn>>
                                try
                                    let! r = e.MoveNext ()
                                    res <- Choice1Of2 r
                                with exn ->
                                    res <- Choice2Of2 exn
                                match res with
                                | Choice1Of2 res ->
                                    return (
                                        if res then current <- Some e.Current
                                        else x.Dispose ()
                                        res)
                                | Choice2Of2 exn ->
                                    return! (
                                        x.Dispose ()
                                        let e = (handler exn :> IEnumerableM<'``Monad<bool>``, 'T>).GetEnumerator ()
                                        state <- TryWithState.HaveHandlerEnumerator e
                                        x.MoveNext ())
                            | TryWithState.HaveHandlerEnumerator e ->
                                let! res = e.MoveNext()
                                return (
                                    if res then current <- Some e.Current
                                    else x.Dispose ()
                                    res)
                            | _ -> return false }
                        member _.Dispose () =
                            match state with
                            | TryWithState.HaveBodyEnumerator e | TryWithState.HaveHandlerEnumerator e ->
                                state <- TryWithState.Finished
                                dispose e
                            | _ -> () } }

    [<RequireQualifiedAccess; EditorBrowsable(EditorBrowsableState.Never)>]
    type TryFinallyState<'``Monad<bool>``, 'T> =
       | NotStarted    of SeqT<'``Monad<bool>``, 'T>
       | HaveBodyEnumerator of IEnumeratorM<'``Monad<bool>``, 'T>
       | Finished
    
    /// Implements the 'TryFinally' functionality for the computation expression builder.
    let inline tryFinally (source: SeqT<'``Monad<bool>``, 'T>) (compensation : unit -> unit) : SeqT<'``Monad<bool>``, 'T> =
        // This pushes the handler through all the monadic computations.
        // The (synchronous) compensation is run when the Dispose () is called.
        SeqT
            { new IEnumerableM<'``Monad<bool>``, 'T> with
                member _.GetEnumerator () =
                    let mutable state   = TryFinallyState.NotStarted source
                    let mutable current = Option<'T>.None
                    { new IEnumeratorM<'``Monad<bool>``, 'T> with
                        member _.Current =
                            match current with
                            | Some c -> c
                            | None -> invalidOp enumNotStarted
                        member x.MoveNext () = innerMonad {
                            match state with
                            | TryFinallyState.NotStarted inp ->
                                return! (
                                    let e = (inp :> IEnumerableM<'``Monad<bool>``, 'T>).GetEnumerator ()
                                    state <- TryFinallyState.HaveBodyEnumerator e
                                    x.MoveNext ())
                            | TryFinallyState.HaveBodyEnumerator e ->
                                let! (res: bool) = e.MoveNext ()
                                return (
                                    if res then current <- Some e.Current
                                    else x.Dispose ()
                                    res)
                            | _ -> return false }
                        member _.Dispose () =
                            match state with
                            | TryFinallyState.HaveBodyEnumerator e ->
                                state <- TryFinallyState.Finished
                                dispose e
                                compensation ()
                            | _ -> () } }

    let inline unfoldM (f: 'State -> '``Monad<('T * 'State) option>``) (s: 'State) : SeqT<'``Monad<bool>``, 'T> =
        SeqT
            { new IEnumerableM<'``Monad<bool>``, 'T> with
                member _.GetEnumerator () =
                    let mutable started = false
                    let mutable current = Option<'T>.None
                    let mutable state   = s
                    { new IEnumeratorM<'``Monad<bool>``, 'T>  with
                        member _.Current =
                            match current, started with
                            | Some c, true -> c
                            | _     , false -> invalidOp enumNotStarted
                            | None  , true  -> invalidOp "Enumeration finished."
                        member x.MoveNext () = monad' {
                            if not started then
                                started <- true
                                return! x.MoveNext ()
                            else
                                let! res = f state
                                match res with
                                | None -> return false
                                | Some (t, newState) ->
                                    current <- Some t
                                    state   <- newState
                                    return true }
                        member _.Dispose () = () } }

    let inline unfold (f: 'State -> ('T * 'State) option) (s: 'State) : SeqT<'``Monad<bool>``, 'T> = unfoldM (result << f: 'State -> '``Monad<('T * 'State) option>``) s

    /// <summary>Returns a sequence that when enumerated returns at most N elements.</summary>
    ///
    /// <param name="count">The maximum number of items to enumerate.</param>
    /// <param name="source">The input sequence.</param>
    ///
    /// <returns>The result sequence.</returns>
    ///
    /// <exception cref="T:System.ArgumentNullException">Thrown when count is negative.</exception>
    let inline truncate count (source: SeqT<'``Monad<bool>``, 'T>) : SeqT<'``Monad<bool>``, 'T> =
        if (count < 0) then invalidArg "count" "must be non-negative"
        SeqT
            { new IEnumerableM<'``Monad<bool>``, 'T> with
                member _.GetEnumerator () =
                    let mutable i = count
                    let e = (source :> IEnumerableM<'``Monad<bool>``, 'T>).GetEnumerator ()
                    { new IEnumeratorM<'``Monad<bool>``, 'T> with
                        member _.Current = e.Current                       
                        member x.MoveNext () =
                            if i > 0 then
                                i <- i - 1
                                e.MoveNext ()
                            else
                                x.Dispose ()
                                result false
                        member _.Dispose () = dispose e } }

    /// <summary>Returns the first N elements of the sequence.</summary>
    ///
    /// <remarks>Throws <c>InvalidOperationException</c>
    /// if the count exceeds the number of elements in the sequence. <c>SeqT.truncate</c>
    /// returns as many items as the sequence contains instead of throwing an exception.</remarks>
    ///
    /// <param name="count">The number of items to take.</param>
    /// <param name="source">The input sequence.</param>
    ///
    /// <returns>The result sequence.</returns>
    ///
    /// <exception cref="T:System.ArgumentNullException">Thrown when count is negative.</exception>
    /// <exception cref="T:System.InvalidOperationException">Thrown when count exceeds the number of elements.
    /// in the sequence.</exception>
    let inline take count (source: SeqT<'``Monad<bool>``, 'T>) : SeqT<'``Monad<bool>``, 'T> =
        if (count < 0) then invalidArg "count" "must be non-negative"
        SeqT
            { new IEnumerableM<'``Monad<bool>``, 'T> with
                member _.GetEnumerator () =
                    let mutable i = count
                    let e = (source :> IEnumerableM<'``Monad<bool>``, 'T>).GetEnumerator ()
                    { new IEnumeratorM<'``Monad<bool>``, 'T> with
                        member _.Current = e.Current                       
                        member x.MoveNext () =
                            if i > 0 then
                                i <- i - 1
                                e.MoveNext () |> monomorphicBind (fun res ->
                                    if not res then invalidOp (
                                        sprintf
                                            "The input sequence has an insufficient number of elements: tried to take %i %s past the end of the sequence. Use SeqT.truncate to get %i or less elements."
                                            (i + 1)
                                            (if i = 0 then "element" else "elements")
                                            count)
                                    result res)
                            else
                                x.Dispose ()
                                result false
                        member _.Dispose () = dispose e } }
    
    [<EditorBrowsable(EditorBrowsableState.Never)>]
    let inline skipImpl throw count (source: SeqT<'``Monad<bool>``, 'T>) : SeqT<'``Monad<bool>``, 'T> =
        if (count < 0) then invalidArg "count" "must be non-negative"
        SeqT
            { new IEnumerableM<'``Monad<bool>``, 'T> with
                member _.GetEnumerator () =
                    let mutable i = count
                    let e = (source :> IEnumerableM<'``Monad<bool>``, 'T>).GetEnumerator ()
                    { new IEnumeratorM<'``Monad<bool>``, 'T> with
                        member _.Current = e.Current                       
                        member x.MoveNext () =
                            if i > 0 then
                                i <- i - 1
                                e.MoveNext () |> monomorphicBind (fun res ->
                                    if res then
                                        x.MoveNext ()
                                    else
                                        if throw then
                                            invalidOp (
                                                sprintf
                                                    "tried to skip %i %s past the end of the seq. Use SeqT.drop to skip %i or less elements."
                                                    (i + 1)
                                                    (if i = 0 then "element" else "elements")
                                                    count)
                                        else
                                            x.Dispose ()
                                            result false)
                            else
                                e.MoveNext () |> monomorphicBind (fun res ->
                                    if not res then
                                        x.Dispose ()
                                    result res)
                        member _.Dispose () = dispose e } }

    /// <summary>Returns a sequence that skips at most N elements of the underlying sequence and then yields the
    /// remaining elements of the sequence.</summary>
    ///
    /// <param name="count">The number of items to skip.</param>
    /// <param name="source">The input sequence.</param>
    ///
    /// <returns>The result sequence.</returns>
    ///
    /// <exception cref="T:System.ArgumentNullException">Thrown when count is negative.</exception>
    let inline drop count (source: SeqT<'``Monad<bool>``, 'T>) : SeqT<'``Monad<bool>``, 'T> = skipImpl false count source

    /// <summary>Returns a sequence that skips N elements of the underlying sequence and then yields the
    /// remaining elements of the sequence.</summary>
    ///
    /// <remarks>Throws <c>InvalidOperationException</c>
    /// if the count exceeds the number of elements in the sequence. <c>SeqT.drop</c>
    /// returns as many items as the sequence contains instead of throwing an exception.</remarks>
    ///
    /// <param name="count">The number of items to skip.</param>
    /// <param name="source">The input sequence.</param>
    ///
    /// <returns>The result sequence.</returns>
    ///
    /// <exception cref="T:System.ArgumentNullException">Thrown when count is negative.</exception>
    /// <exception cref="T:System.InvalidOperationException">Thrown when count exceeds the number of elements
    /// in the sequence.</exception>
    let inline skip count (source: SeqT<'``Monad<bool>``, 'T>) : SeqT<'``Monad<bool>``, 'T> = skipImpl true count source


type [<AutoOpen>]SeqTOperations =
    static member inline SeqT (source: '``Monad<seq<'T>>``) : SeqT<'``Monad<bool>``, 'T> = SeqT.wrap source

module [<AutoOpen>]SeqTOperations =
    let inline seqT<'T, .. > (source: '``Monad<seq<'T>>``) : SeqT<'``Monad<bool>``, 'T> = SeqT.wrap source
    let inline (|SeqT|) (x: SeqT<'``Monad<bool>``, 'T>) : '``Monad<'T seq>`` = SeqT.run x


type SeqT<'``monad<bool>``, 'T> with
    
    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member inline Return (x: 'T) : SeqT<'``Monad<bool>``, 'T> = SeqT.singleton x
    
    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member inline Map   (x: SeqT<'``Monad<bool>``, 'T>, f: 'T -> 'U) : SeqT<'``Monad<bool>``, 'U> = SeqT.map f x

    static member inline (<!>) (x: SeqT<'``Monad<bool>``, 'T>, f: 'T -> 'U) : SeqT<'``Monad<bool>``, 'U> = SeqT.map f x
    static member inline (<*>) (f: SeqT<'``Monad<bool>``, ('T -> 'U)>, x: SeqT<'``Monad<bool>``, 'T>) : SeqT<'``Monad<bool>``, 'U> = SeqT.apply f x

    /// <summary>
    /// Sequences two lists left-to-right, discarding the value of the first argument.
    /// </summary>
    /// <category index="2">Applicative</category>
    static member inline ( *>) (x: SeqT<'``Monad<bool>``, 'T>, y: SeqT<'``Monad<bool>``, 'U>) : SeqT<'``Monad<bool>``, 'U> =
        let (<!>) = SeqT.map
        let (<*>) = SeqT.apply
        ((fun (_: 'T) (k: 'U) -> k) <!> x: SeqT<'``Monad<bool>``, ('U -> 'U)>) <*> y
    
    /// <summary>
    /// Sequences two lists left-to-right, discarding the value of the second argument.
    /// </summary>
    /// <category index="2">Applicative</category>
    static member inline (<* ) (x: SeqT<'``Monad<bool>``, 'U>, y: SeqT<'``Monad<bool>``, 'T>) : SeqT<'``Monad<bool>``, 'U> =
        let (<!>) = SeqT.map
        let (<*>) = SeqT.apply
        ((fun (k: 'U) (_: 'T) -> k) <!> x: SeqT<'``Monad<bool>``, ('T -> 'U)>) <*> y

    static member inline (>>=) (x: SeqT<'``Monad<bool>``, 'T>, f: 'T -> SeqT<'``Monad<bool>``, 'U>) : SeqT<'``Monad<bool>``, 'U> = SeqT.collect f x
    
    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member inline get_Empty () : SeqT<'``Monad<bool>``, 'T> = SeqT.empty

    static member inline (<|>) (x, y) : SeqT<'``Monad<bool>``, 'T> = SeqT.append x y

    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member inline Lift2 (f: 'T1 -> 'T2 -> 'U, x1: SeqT<'``Monad<bool>``, 'T1>, x2: SeqT<'``Monad<bool>``, 'T2>) : SeqT<'``Monad<bool>``, 'U> = SeqT.lift2 f x1 x2
    
    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member inline Lift3 (f: 'T1 -> 'T2 -> 'T3 -> 'U, x1: SeqT<'``Monad<bool>``, 'T1>, x2: SeqT<'``Monad<bool>``, 'T2>, x3: SeqT<'``Monad<bool>``, 'T3>) : SeqT<'``Monad<bool>``, 'U> = SeqT.lift3 f x1 x2 x3

    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member inline TryWith (source: SeqT<'``Monad<bool>``, 'T>, f: exn -> SeqT<'``Monad<bool>``, 'T>) = SeqT.tryWith<_, _, '``Monad<unit>``> source f
    
    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member inline TryFinally (computation: SeqT<'``Monad<bool>``, 'T>, f) = SeqT.tryFinally computation f
    
    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member inline Delay (body: unit -> SeqT<'``Monad<bool>``, 'T>) : SeqT<'``Monad<bool>``, 'T> = SeqT.delay body
    
    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member inline Using (resource, f: _ -> SeqT<'``Monad<bool>``, 'T>) =
        SeqT.tryFinally (f resource) (fun () -> if box resource <> null then dispose resource)

    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member inline Lift (m: '``Monad<'T>``) : SeqT<'``Monad<bool>``, 'T> = SeqT.lift m

    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member inline LiftAsync (x: Async<'T>) = SeqT.lift (liftAsync x: '``MonadAsync<'T>``) : SeqT<'MonadAsync, 'T>

    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member inline Throw (x: 'E) : SeqT<'``MonadError<'E>``, 'T> = x |> throw |> SeqT.lift
    
    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member inline Catch (m: SeqT<'``MonadError<'E1>``, 'T>, h: 'E1 -> SeqT<'``MonadError<'E2>``, 'T>) : SeqT<'``MonadError<'E2>``, 'T> =
        seqT (
            (fun v h -> Catch.Invoke v h)
                (SeqT.run m)
                (SeqT.run << h))
    
    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member inline CallCC (f: (('T -> SeqT<'``MonadCont<'R>``, 'U>) -> _)) : SeqT<'``MonadCont<'R>``, 'T> =
        seqT (callCC <| fun c -> SeqT.run (f (seqT << c << Seq.singleton)))
    
    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member inline get_Get ()  : SeqT<'``MonadState<'S>``, 'S> = SeqT.lift get
    
    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member inline Put (x: 'T) : SeqT<'``MonadState<unit>``, 'S> = x |> put |> SeqT.lift
    
    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member inline get_Ask () : SeqT<'``MonadReader<'R>``, 'R> = SeqT.lift ask

    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member inline Local (m: SeqT<'``MonadReader<'R2>``, 'T>, f: 'R1 -> 'R2) : SeqT<'``MonadReader<'R1>``, 'T> =
        seqT (local f (SeqT.run m))

    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member inline OfSeq (x: seq<'T>) : SeqT<'``Monad<bool>``, 'T> = SeqT.ofSeq x

    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member inline Take (source: SeqT<'``Monad<bool>``, 'T>, count, _: Take) : SeqT<'``Monad<bool>``, 'T> = SeqT.take count source

    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member inline Limit (source: SeqT<'``Monad<bool>``, 'T>, count, _: Limit) : SeqT<'``Monad<bool>``, 'T> = SeqT.truncate count source

    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member inline Skip (source: SeqT<'``Monad<bool>``, 'T>, count, _: Skip) : SeqT<'``Monad<bool>``, 'T> = SeqT.skip count source

    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member inline Drop (source: SeqT<'``Monad<bool>``, 'T>, count, _: Drop) : SeqT<'``Monad<bool>``, 'T> = SeqT.drop count source
    
    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member inline Zip (source1: SeqT<'``Monad<bool>``, 'T1>, source2: SeqT<'``Monad<bool>``, 'T2>) : SeqT<'``Monad<bool>``, ('T1 * 'T2)> = SeqT.zip source1 source2

#endif