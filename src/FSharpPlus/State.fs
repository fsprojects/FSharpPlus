namespace FSharpPlus.Data
open System.ComponentModel

/// <summary> Computation type: Computations which maintain state.
/// <para/>   Binding strategy: Threads a state parameter through the sequence of bound functions so that the same state value is never used twice, giving the illusion of in-place update.
/// <para/>   Useful for: Building computations from sequences of operations that require a shared state. </summary>
[<Struct>]
type State<'s,'t> = State of ('s->('t * 's))

/// Basic operations on State
[<RequireQualifiedAccess>]
module State =
    let run (State x) = x                                                                                        : 'S->('T * 'S)
    let map   f (State m) = State (fun s -> let (a:'T, s') = m s in (f a, s'))                                   : State<'S,'U>
    let bind  f (State m) = State (fun s -> let (a:'T, s') = m s in run (f a) s')                                : State<'S,'U>
    let apply (State f) (State x) = State (fun s -> let (f', s1) = f s in let (x':'T, s2) = x s1 in (f' x', s2)) : State<'S,'U>

    let eval (State sa) (s:'s)           = fst (sa s)                                                            : 'T
    let exec (State sa : State<'S,'A>) s = snd (sa s)                                                            : 'S

    /// Return the state from the internals of the monad.
    let get   = State (fun s -> (s, s))                                                                          : State<'S,'S>

    /// Replace the state inside the monad.
    let put x = State (fun _ -> ((), x))                                                                         : State<'S,unit>

type State<'s,'t> with
    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member Map   (x, f:'T->_) = State.map f x          : State<'S,'U>
    static member Return a = State (fun s -> (a, s))          : State<'S,'T>
    static member (>>=) (x, f:'T->_) = State.bind f x         : State<'S,'U>
    static member (<*>) (f, x:State<'S,'T>) = State.apply f x : State<'S,'U>
    static member get_Get() = State.get                       : State<'S,'S>
    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member Put x     = State.put x                     : State<'S,unit>

open FSharpPlus.Control
open FSharpPlus

/// Monad Transformer for State<'S, 'T>
[<Struct>]
type StateT<'s,'``monad<'t * 's>``> = StateT of ('s -> '``monad<'t * 's>``)

/// Basic operations on StateT
[<RequireQualifiedAccess>]
module StateT =
    let run (StateT x) = x : 'S -> '``Monad<'T * 'S>``
    let inline map  (f :'T->'U) (StateT (m :_->'``Monad<'T * 'S>``)) = StateT (m >> Map.Invoke (fun (a, s') -> (f a, s')))     : StateT<'S,'``Monad<'U * 'S>``>
    let inline apply (StateT f : StateT<'S,'``Monad<('T -> 'U) * 'S>``>) (StateT a :StateT<'S,'``Monad<'T * 'S>``>) = StateT (fun s -> f s >>= fun (g, t) -> Map.Invoke (fun (z, u) -> (g z, u)) (a t)) : StateT<'S,'``Monad<'U * 'S>``>
    let inline bind (f :'T->StateT<'S,'``Monad<'U * 'S>``>) (StateT m: StateT<'S,'``Monad<'T * 'S>``>) = StateT <| fun s -> m s >>= (fun (a, s') -> run (f a) s')

type StateT<'s,'``monad<'t * 's>``> with
    static member inline Return (x : 'T) = StateT (fun s -> result (x, s))                                                         : StateT<'S,'``Monad<'T * 'S>``>
    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member inline Map    (x : StateT<'S,'``Monad<'T * 'S>``>, f : 'T->'U)                                = StateT.map   f x : StateT<'S,'``Monad<'U * 'S>``>
    static member inline (<*>)  (f : StateT<'S,'``Monad<('T -> 'U) * 'S>``>, x :StateT<'S,'``Monad<'T * 'S>``>) = StateT.apply f x : StateT<'S,'``Monad<'U * 'S>``>
    static member inline (>>=)  (x : StateT<'S,'``Monad<'T * 'S>``>, f : 'T->StateT<'S,'``Monad<'U * 'S>``>)    = StateT.bind  f x

    static member inline get_Empty () = StateT (fun _ -> getEmpty()) : StateT<'S,'``MonadPlus<'T * 'S>``>
    static member inline (<|>) (StateT m, StateT n) = StateT (fun s -> m s <|> n s) : StateT<'S,'``MonadPlus<'T * 'S>``>

    static member inline Lift (m:'``Monad<'T>``) : StateT<'S,'``Monad<'T * 'S>``> = StateT <| fun s -> m >>= fun a -> result (a, s)

    static member inline LiftAsync (x :Async<'T>) = lift (liftAsync x) : '``StateT<'S,'MonadAsync<'T>>``
    
    static member inline get_Get () = StateT (fun s -> result (s , s))  : StateT<'S, '``Monad<'S * 'S>``>
    static member inline Put (x:'S) = StateT (fun _ -> result ((), x))  : StateT<'S, '``Monad<unit * 'S>``>

    static member inline Throw (x :'E) = x |> throw |> lift
    static member inline Catch (m :StateT<'S,'``MonadError<'E1,'T * 'S>``>, h:'E1 -> _) = 
        StateT (fun s -> catch (StateT.run m s) (fun e -> StateT.run (h e) s)) : StateT<'S,'``MonadError<'E2, 'T * 'S>``>

    static member inline Delay (f: unit -> StateT<'S,'``Monad<'T * 'S>``>) =
        StateT (fun s ->
            let d () = StateT.run (f ()) s
            Delay.Invoke d) : StateT<'S,'``Monad<'T * 'S>``>