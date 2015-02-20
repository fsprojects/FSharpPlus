namespace FsControl.Core.Types

open FsControl.Core.Prelude
open FsControl.Core.TypeMethods
open FsControl.Core.TypeMethods.Monad

type StateT<'S,'MaS> = StateT of ('S -> 'MaS)

[<RequireQualifiedAccess>]
module StateT =
    let run (StateT x) = x
    let inline map  f (StateT m) = StateT (m >> Map.Invoke (fun (a, s') -> (f a, s')))
    let inline apply  (StateT f) (StateT a) = StateT (fun s -> f s >>= fun (g, t) -> Map.Invoke (fun (z, u) -> (g z, u)) (a t))
    let inline bind f (StateT m) = StateT <| fun s -> do'() {
        let! (a, s') = m s
        return! run (f a) s'}

type StateT<'S,'MaS> with
    static member inline Map    (_:Map   , x, _) = fun f -> StateT.map f x
    static member inline Return (_:Return, _:StateT<'s,'ma>) : 'a -> StateT<'s,'ma> = fun a -> StateT <| fun s -> result (a, s)
    static member inline Apply  (_:Apply , f, x, _:StateT<'s,'mb>) = StateT.apply f x
    static member inline Bind   (_:Bind  , x:StateT<'s,'mas>, _:StateT<'s,'mbs>) :('a -> StateT<'s,'mbs>) -> StateT<'s,'mbs> = fun f -> 
        StateT.bind f x

    static member inline Zero (_:Zero, _:StateT<_,_>) =                   StateT <| fun _ -> Zero.Invoke()
    static member inline Plus (_:Plus,   StateT m   ) = fun (StateT n) -> StateT <| fun s -> m s <|> n s

    static member inline Lift (_:Lift, _:StateT<'s,'mas>) = fun (m:'ma) -> (StateT <| fun s -> m >>= fun a -> result (a,s)):StateT<'s,'mas>

    static member inline LiftAsync (_:LiftAsync, _:StateT<_,_>) = fun (x: Async<_>) -> Lift.Invoke (LiftAsync.Invoke x)
    
    static member inline Get (_:Get, _:StateT<_,_>    ) = StateT (fun s -> result (s , s))
    static member inline Put (_:Put, _:StateT<_,_>    ) = fun x  -> StateT (fun _ -> result ((), x))    

    static member inline ThrowError (_:ThrowError, _:StateT<_,_>    ) = Lift.Invoke << ThrowError.Invoke
    static member inline CatchError (_:CatchError,  m:StateT<'T,'U> , _:StateT<'T,'U>) = fun (h:'e -> StateT<'T,'U>) -> 
        StateT (fun s -> CatchError.Invoke (StateT.run m s)   (fun e -> StateT.run (h e) s)):StateT<'T,'U>