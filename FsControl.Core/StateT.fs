namespace FsControl.Core.Types

open FsControl.Core.Prelude
open FsControl.Core.TypeMethods
open FsControl.Core.TypeMethods.Functor
open FsControl.Core.TypeMethods.Monad
open FsControl.Core.TypeMethods.MonadPlus
open FsControl.Core.TypeMethods.MonadTrans
open FsControl.Core.TypeMethods.MonadAsync
open FsControl.Core.TypeMethods.MonadError

type StateT<'S,'MaS> = StateT of ('S -> 'MaS)

[<RequireQualifiedAccess>]
module StateT =
    let  run   (StateT x) = x
    let  map f (StateT m) = StateT(f << m)

type StateT<'S,'MaS> with
    static member inline instance (Functor.Map, StateT m, _) = fun f -> StateT <| fun s -> do'(){
        let! (x, s') = m s
        return (f x, s')}

    static member inline instance (Applicative.Pure, _:StateT<'s,'ma>                        ) : 'a -> StateT<'s,'ma> = fun a -> StateT <| fun s -> return' (a, s)
    static member inline instance (Monad.Bind  ,   StateT (m:'s->'mas), _:StateT<'s,'mbs>) :('a -> StateT<'s,'mbs>) -> StateT<'s,'mbs> = 
        fun k -> StateT <| fun s -> do'(){
            let! (a, s') = m s
            return! StateT.run (k a) s'}

    static member inline instance (MonadPlus.Mzero, _:StateT<_,_>    ) = fun ()         -> StateT <| fun _ -> mzero()
    static member inline instance (MonadPlus.Mplus,   StateT m,     _) = fun (StateT n) -> StateT <| fun s -> mplus (m s) (n s)

    static member inline instance (MonadTrans.Lift, _:StateT<'s,'mas>) = fun (m:'ma) -> (StateT <| fun s -> m >>= fun a -> return' (a,s)):StateT<'s,'mas>

    static member inline instance (MonadAsync.LiftAsync, _:StateT<_,_>) = fun (x: Async<_>) -> lift (liftAsync x)
    
    static member inline instance (MonadState.Get, _:StateT<_,_>    ) = fun () -> StateT (fun s -> return' (s , s))
    static member inline instance (MonadState.Put, _:StateT<_,_>    ) = fun x  -> StateT (fun _ -> return' ((), x))    

    static member inline instance (MonadError.ThrowError, _:StateT<_,_>    ) = lift << throwError
    static member inline instance (MonadError.CatchError,  m:StateT<'T,'U> , _:StateT<'T,'U>) = fun (h:'e -> StateT<'T,'U>) -> 
        StateT (fun s -> catchError (StateT.run m s)   (fun e -> StateT.run (h e) s)):StateT<'T,'U>