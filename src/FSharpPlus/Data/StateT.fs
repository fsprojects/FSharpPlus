namespace FSharpPlus.Data
open System.ComponentModel
open FSharpPlus
open FSharpPlus.Control
open FSharpPlus.Internals.Prelude

/// Monad Transformer for State<'S, 'T>
[<Struct>]
type StateT<'s,'``monad<'t * 's>``> = StateT of ('s -> '``monad<'t * 's>``)

/// Basic operations on StateT
[<RequireQualifiedAccess>]
module StateT =
    let run (StateT x) = x : 'S -> '``Monad<'T * 'S>``

    #if !FABLE_COMPILER

    /// Embed a Monad<'T> into a StateT<'S,'``Monad<'T * 'S>``>
    let inline lift (m: '``Monad<'T>``) : StateT<'S,'``Monad<'T * 'S>``> =
        if opaqueId false then StateT <| fun s -> (m |> liftM (fun a -> (a, s)))
        else StateT <| fun s -> (m |> map (fun a -> (a, s)))

    /// Transform a State<'S, 'T> to a StateT<'S, '``Monad<'T * 'S>``>
    let inline hoist (x: State<'S, 'T>) = (StateT << (fun a -> result << a) << State.run) x : StateT<'S, '``Monad<'T * 'S>``>

    #endif

    let inline map (f: 'T->'U) (StateT (m :_->'``Monad<'T * 'S>``)) = StateT (m >> Map.Invoke (fun (a, s') -> (f a, s'))) : StateT<'S,'``Monad<'U * 'S>``>

    let inline apply (StateT f: StateT<'S,'``Monad<('T -> 'U) * 'S>``>) (StateT a: StateT<'S,'``Monad<'T * 'S>``>) = StateT (fun s -> f s >>= fun (g, t) -> Map.Invoke (fun (z: 'T, u: 'S) -> ((g z: 'U), u)) (a t)) : StateT<'S,'``Monad<'U * 'S>``>

    /// Zips two StateTs into one.
    let inline zip (x: StateT<'S,'``Monad<'T * 'S>``>) (y: StateT<'S,'``Monad<'U * 'S>``>) = apply (map tuple2 x) y : StateT<'S,'``Monad<('T * 'U) * 'S>``>

    let inline bind (f: 'T->StateT<'S,'``Monad<'U * 'S>``>) (StateT m: StateT<'S,'``Monad<'T * 'S>``>) = StateT <| fun s -> m s >>= (fun (a, s') -> run (f a) s')

type StateT<'s,'``monad<'t * 's>``> with
    #if !FABLE_COMPILER
    static member inline Return (x: 'T) = StateT (fun s -> result (x, s))                                                         : StateT<'S,'``Monad<'T * 'S>``>
    #endif

    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member inline Map    (x: StateT<'S,'``Monad<'T * 'S>``>, f : 'T->'U)                                = StateT.map   f x : StateT<'S,'``Monad<'U * 'S>``>

    static member inline (<*>)  (f: StateT<'S,'``Monad<('T -> 'U) * 'S>``>, x: StateT<'S,'``Monad<'T * 'S>``>) = StateT.apply f x : StateT<'S,'``Monad<'U * 'S>``>
    static member inline (>>=)  (x: StateT<'S,'``Monad<'T * 'S>``>, f: 'T->StateT<'S,'``Monad<'U * 'S>``>)     = StateT.bind  f x

    static member inline get_Empty () = StateT (fun _ -> getEmpty ()) : StateT<'S,'``MonadPlus<'T * 'S>``>
    static member inline (<|>) (StateT m, StateT n) = StateT (fun s -> m s <|> n s) : StateT<'S,'``MonadPlus<'T * 'S>``>

    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member inline Zip (x: StateT<'S,'``Monad<'T * 'S>``>, y: StateT<'S,'``Monad<'U * 'S>``>) = StateT.zip x y

    static member inline TryWith (source: StateT<'S,'``Monad<'T * 'S>``>, f: exn -> StateT<'S,'``Monad<'T * 'S>``>) = StateT (fun s -> TryWith.Invoke (StateT.run source s) (fun x -> StateT.run (f x) s))
    static member inline TryFinally (computation: StateT<'S,'``Monad<'T * 'S>``>, f) = StateT (fun s -> TryFinally.Invoke     (StateT.run computation s) f)
    static member inline Using (resource, f: _ -> StateT<'S,'``Monad<'T * 'S>``>)    = StateT (fun s -> Using.Invoke resource (fun x -> StateT.run (f x) s))
    static member inline Delay (body : unit   ->  StateT<'S,'``Monad<'T * 'S>``>)    = StateT (fun s -> Delay.Invoke (fun _ -> StateT.run (body ()) s)) : StateT<'S,'``Monad<'T * 'S>``>

    #if !FABLE_COMPILER
    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member inline Lift (m: '``Monad<'T>``) : StateT<'S,'``Monad<'T * 'S>``> = StateT.lift m
    #endif

    static member inline LiftAsync (x :Async<'T>) = StateT.lift (liftAsync x) : StateT<'S,'``MonadAsync<'T>``>
    
    #if !FABLE_COMPILER
    static member inline get_Get ()  = StateT (fun s -> result (s , s)) : StateT<'S, '``Monad<'S * 'S>``>
    static member inline Put (x: 'S) = StateT (fun _ -> result ((), x)) : StateT<'S, '``Monad<unit * 'S>``>
    #endif

    static member inline Throw (x: 'E) = x |> throw |> StateT.lift
    static member inline Catch (m: StateT<'S,'``MonadError<'E1,'T * 'S>``>, h: 'E1 -> _) =
        StateT (fun s -> catch (StateT.run m s) (fun e -> StateT.run (h e) s)) : StateT<'S,'``MonadError<'E2, 'T * 'S>``>
