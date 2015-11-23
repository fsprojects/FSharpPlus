namespace FSharpPlus

open FsControl

type StateT<'s,'``monad<'t * 's>``> = StateT of ('s -> '``monad<'t * 's>``)

[<RequireQualifiedAccess>]
module StateT =
    let run (StateT x) = x : 'S -> '``Monad<'T * 'S>``
    let inline map  (f:'T->'U) (StateT (m:_->'``Monad<'T * 'S>``)) = StateT (m >> Map.Invoke (fun (a, s') -> (f a, s')))     : StateT<'S,'``Monad<'U * 'S>``>
    let inline apply  (StateT f: StateT<'S,'``Monad<('T -> 'U) * 'S>``>) (StateT a: StateT<'S,'``Monad<'T * 'S>``>) = StateT (fun s -> f s >>= fun (g, t) -> Map.Invoke (fun (z, u) -> (g z, u)) (a t)) : StateT<'S,'``Monad<'U * 'S>``>
    let inline bind (f:'T->StateT<'S,'``Monad<'U * 'S>``>) (StateT m: StateT<'S,'``Monad<'T * 'S>``>) = StateT <| fun s -> m s >>= (fun (a, s') -> run (f a) s')

type StateT with
    static member inline Map    (x: StateT<'S,'``Monad<'T * 'S>``>, f:'T->'U     , impl:Map   ) = StateT.map f x                                : StateT<'S,'``Monad<'U * 'S>``>
    static member inline Return (output: StateT<'S,'``Monad<'T * 'S>``>, impl:Return) = fun (a:'T) -> StateT (fun s -> result (a, s))           : StateT<'S,'``Monad<'T * 'S>``>
    static member inline Apply  (f: StateT<'S,'``Monad<('T -> 'U) * 'S>``>, x: StateT<'S,'``Monad<'T * 'S>``>, output: StateT<'S,'``Monad<'U * 'S>``>, impl:Apply ) = StateT.apply f x : StateT<'S,'``Monad<'U * 'S>``>
    static member inline Bind   (x:StateT<'S,'``Monad<'T * 'S>``>, f :'T->StateT<'S,'``Monad<'U * 'S>``>) = StateT.bind f x

    static member inline MZero (output:StateT<'S,'``MonadPlus<'T * 'S>``>, impl:MZero) = StateT (fun _ -> MZero.Invoke()) : StateT<'S,'``MonadPlus<'T * 'S>``>
    static member inline MPlus (  StateT m, StateT n, impl:MPlus) = StateT (fun s -> m s <|> n s   )                      : StateT<'S,'``MonadPlus<'T * 'S>``>

    static member inline Lift (m:'``Monad<'T>``) : StateT<'S,'``Monad<'T * 'S>``> = StateT <| fun s -> m >>= fun a -> result (a, s)

    static member inline LiftAsync (x: Async<'T>) = Lift.Invoke (LiftAsync.Invoke x)
    
    static member inline get_Get()  = StateT (fun s -> result (s , s))  : StateT<'S, '``MonadState<'S * 'S>``>
    static member inline Put (x:'S) = StateT (fun _ -> result ((), x))  : StateT<'S, '``MonadState<unit * 'S>``>

    static member inline ThrowError (x:'E) = x |> ThrowError.Invoke |> Lift.Invoke
    static member inline CatchError (m:StateT<'S,Choice<'T * 'S, 'E1>>, h:'E1 -> _) = 
        StateT (fun s -> CatchError.Invoke (StateT.run m s)   (fun e -> StateT.run (h e) s)) : StateT<'S,Choice<'T * 'S, 'E2>>