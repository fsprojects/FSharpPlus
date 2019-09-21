namespace FSharpPlus.Data

open FSharpPlus
open System.ComponentModel

#if !FABLE_COMPILER
/// Additional operations on Seq
module Seq =

    let inline sequence (ms: seq<'``Applicative<'T>``>) : '``Applicative<seq<'T>>`` = sequence ms
    let inline traverse (f: 'T->'``Applicative<'U>``) (xs: seq<'T>) : '``Applicative<seq<'U>>`` = traverse f xs
    
    let inline replicateM count (initial: '``Applicative<'T>``) = sequence (Seq.replicate count initial)
#endif


open FSharpPlus.Control

/// Monad Transformer for seq<'T>
[<Struct>]
type SeqT<'``monad<seq<'t>>``> = SeqT of '``monad<seq<'t>>``

/// Basic operations on SeqT
[<RequireQualifiedAccess>]
module SeqT =
    let run (SeqT m) = m

    #if !FABLE_COMPILER
    let inline internal sequence ms =
        let k m m' = m >>= fun (x: 'a) -> m' >>= fun (xs: seq<'a>) -> (result: seq<'a> -> 'M) (seq {yield x; yield! xs})
        Seq.foldBack k ms ((result: seq<'a> -> 'M) Seq.empty)
    let inline internal mapM f as' = sequence (Seq.map f as')

    let inline bind (f: 'T-> SeqT<'``Monad<seq<'U>``>) (SeqT m: SeqT<'``Monad<seq<'T>``>)          = SeqT (m >>= (mapM : _->seq<_>->_) (run << f) >>= ((Seq.concat: seq<seq<_>>->_) >> result))
    let inline apply (SeqT f: SeqT<'``Monad<seq<('T -> 'U)>``>) (SeqT x: SeqT<'``Monad<seq<'T>``>) = SeqT (map (Seq.apply : seq<_->_>->seq<_>->seq<_>) f <*> x) : SeqT<'``Monad<seq<'U>``>
    let inline map (f: 'T->'U) (SeqT m: SeqT<'``Monad<seq<'T>``>)                                  = SeqT <| map (Seq.map f : (seq<_>->_)) m                    : SeqT<'``Monad<seq<'U>``>
    #endif

type SeqT<'``monad<seq<'t>>``> with
    #if !FABLE_COMPILER
    static member inline Return (x: 'T) = x |> Seq.singleton |> result |> SeqT                                     : SeqT<'``Monad<seq<'T>``>

    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member inline Map   (x: SeqT<'``Monad<seq<'T>``>, f: 'T->'U) = SeqT.map f x                             : SeqT<'``Monad<seq<'U>``>

    static member inline (<*>) (f: SeqT<'``Monad<seq<('T -> 'U)>``>, x: SeqT<'``Monad<seq<'T>``>) = SeqT.apply f x : SeqT<'``Monad<seq<'U>``>
    static member inline (>>=) (x: SeqT<'``Monad<seq<'T>``>, f: 'T -> SeqT<'``Monad<seq<'U>``>)   = SeqT.bind  f x

    static member inline get_Empty () = SeqT <| result Seq.empty : SeqT<'``MonadPlus<seq<'T>``>
    static member inline (<|>) (SeqT x, SeqT y) = SeqT <| (x >>= (fun a -> y >>= (fun b ->  result ((Seq.append:seq<_>->seq<_>->_) a b)))) : SeqT<'``MonadPlus<seq<'T>``>
    #endif
    static member inline TryWith (source: SeqT<'``Monad<seq<'T>>``>, f: exn -> SeqT<'``Monad<seq<'T>>``>) = SeqT (TryWith.Invoke (SeqT.run source) (SeqT.run << f))
    static member inline TryFinally (computation: SeqT<'``Monad<seq<'T>>``>, f) = SeqT (TryFinally.Invoke     (SeqT.run computation) f)
    static member inline Using (resource, f: _ -> SeqT<'``Monad<seq<'T>>``>)    = SeqT (Using.Invoke resource (SeqT.run << f))
    static member inline Delay (body : unit   ->  SeqT<'``Monad<seq<'T>>``>)    = SeqT (Delay.Invoke (fun _ -> SeqT.run (body ()))) : SeqT<'``Monad<seq<'T>>``>
    
    #if !FABLE_COMPILER
    static member inline Lift (x: '``Monad<'T>``) = x |> liftM Seq.singleton |> SeqT : SeqT<'``Monad<seq<'T>>``>
    #endif
    
    static member inline LiftAsync (x: Async<'T>) = lift (liftAsync x) : '``SeqT<'MonadAsync<'T>>``
    
    static member inline Throw (x: 'E) = x |> throw |> lift
    static member inline Catch (m: SeqT<'``MonadError<'E1,'T>``>, h: 'E1 -> SeqT<'``MonadError<'E2,'T>``>) = SeqT ((fun v h -> catch v h) (SeqT.run m) (SeqT.run << h)) : SeqT<'``MonadError<'E2,'T>``>
    
    static member inline CallCC (f: (('T -> SeqT<'``MonadCont<'R,seq<'U>>``>) -> _)) = SeqT (callCC <| fun c -> SeqT.run (f (SeqT  << c << Seq.singleton ))) : SeqT<'``MonadCont<'R, seq<'T>>``>
    
    static member inline get_Get ()  = lift get         : '``SeqT<'MonadState<'S,'S>>``
    static member inline Put (x: 'T) = x |> put |> lift : '``SeqT<'MonadState<unit,'S>>``
    
    static member inline get_Ask () = lift ask          : '``SeqT<'MonadReader<'R,seq<'R>>>``
    static member inline Local (SeqT (m: '``MonadReader<'R2,'T>``), f: 'R1->'R2) = SeqT (local f m)