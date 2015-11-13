namespace FsControl

open FsControl.Core.Internals.Prelude
open FsControl.Core.Internals.MonadOps

type StateT<'S,'MaS> = StateT of ('S -> 'MaS)

[<RequireQualifiedAccess>]
module StateT =
    let run (StateT x) = x
    let inline map  f (StateT m) = StateT (m >> Map.Invoke (fun (a, s') -> (f a, s')))
    let inline apply  (StateT f) (StateT a) = StateT (fun s -> f s >>= fun (g, t) -> Map.Invoke (fun (z, u) -> (g z, u)) (a t))
    let inline bind f (StateT m) = StateT <| fun s -> m s >>= (fun (a, s') -> run (f a) s')

type StateT<'S,'MaS> with
    static member inline Map    (x, f                  , _:Map   ) = StateT.map f x
    static member inline Return (_:StateT<'s,'ma>      , _:Return) : 'a -> StateT<'s,'ma> = fun a -> StateT <| fun s -> result (a, s)
    static member inline Apply  (f, x, _:StateT<'s,'mb>, _:Apply ) = StateT.apply f x
    static member inline Bind   (x:StateT<'s,'mas>, f :'a -> StateT<'s,'mbs>) : StateT<'s,'mbs> = StateT.bind f x

    static member inline MZero (_:StateT<_,_>       , _:MZero) = StateT <| fun _ -> MZero.Invoke()
    static member inline MPlus (  StateT m, StateT n, _:MPlus) = StateT <| fun s -> m s <|> n s

    static member inline Lift (m:'ma) :StateT<'s,'mas> = StateT <| fun s -> m >>= fun a -> result (a, s)

    static member inline LiftAsync (_:StateT<_,_>) = fun (x: Async<_>) -> Lift.Invoke (LiftAsync.Invoke x)
    
    static member inline get_Get() = StateT (fun s -> result (s , s))
    static member inline Put x     = StateT (fun _ -> result ((), x))    

    static member inline ThrowError (_:StateT<_,_>) = Lift.Invoke << ThrowError.Invoke
    static member inline CatchError ( m:StateT<'T,'U>, _:StateT<'T,'U>) = fun (h:'e -> StateT<'T,'U>) -> 
        StateT (fun s -> CatchError.Invoke (StateT.run m s)   (fun e -> StateT.run (h e) s)):StateT<'T,'U>