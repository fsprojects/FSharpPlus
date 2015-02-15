namespace FsControl.Core.Types

open FsControl.Core.Prelude
open FsControl.Core.TypeMethods
open FsControl.Core.TypeMethods.Functor
open FsControl.Core.TypeMethods.Applicative
open FsControl.Core.TypeMethods.Monad
open FsControl.Core.TypeMethods.MonadTrans
open FsControl.Core.TypeMethods.MonadAsync
open FsControl.Core.TypeMethods.MonadError

type StateT<'S,'MaS> = StateT of ('S -> 'MaS)

[<RequireQualifiedAccess>]
module StateT =
    let run (StateT x) = x
    let inline map  f (StateT m) = StateT (m >> fmap (fun (a, s') -> (f a, s')))
    let inline apply  (StateT f) (StateT a) = StateT (fun s -> f s >>= fun (g, t) -> fmap (fun (z, u) -> (g z, u)) (a t))
    let inline bind f (StateT m) = StateT <| fun s -> do'() {
        let! (a, s') = m s
        return! run (f a) s'}

type StateT<'S,'MaS> with
    static member inline Map    (_:Functor.Map, x, _) = fun f -> StateT.map f x
    static member inline Return (_:Applicative.Return, _:StateT<'s,'ma>) : 'a -> StateT<'s,'ma> = fun a -> StateT <| fun s -> result (a, s)
    static member inline Apply  (_:Applicative.Apply, f, x, _:StateT<'s,'mb>) = fun () -> StateT.apply f x
    static member inline Bind   (_:Monad.Bind, x:StateT<'s,'mas>, _:StateT<'s,'mbs>) :('a -> StateT<'s,'mbs>) -> StateT<'s,'mbs> = fun f -> 
        StateT.bind f x

    static member inline Zero (_:Functor.Zero, _:StateT<_,_>    ) = fun ()         -> StateT <| fun _ -> zero()
    static member inline Plus (_:Functor.Plus,   StateT m,     _) = fun (StateT n) -> StateT <| fun s -> plus (m s) (n s)

    static member inline Lift (_:MonadTrans.Lift, _:StateT<'s,'mas>) = fun (m:'ma) -> (StateT <| fun s -> m >>= fun a -> result (a,s)):StateT<'s,'mas>

    static member inline LiftAsync (_:MonadAsync.LiftAsync, _:StateT<_,_>) = fun (x: Async<_>) -> lift (liftAsync x)
    
    static member inline Get (_:MonadState.Get, _:StateT<_,_>    ) = fun () -> StateT (fun s -> result (s , s))
    static member inline Put (_:MonadState.Put, _:StateT<_,_>    ) = fun x  -> StateT (fun _ -> result ((), x))    

    static member inline ThrowError (_:MonadError.ThrowError, _:StateT<_,_>    ) = lift << throwError
    static member inline CatchError (_:MonadError.CatchError,  m:StateT<'T,'U> , _:StateT<'T,'U>) = fun (h:'e -> StateT<'T,'U>) -> 
        StateT (fun s -> catchError (StateT.run m s)   (fun e -> StateT.run (h e) s)):StateT<'T,'U>