module FsControl.Core.Types.StateT

open FsControl.Core.Prelude
open FsControl.Core.Abstractions
open FsControl.Core.Abstractions.Monad
open FsControl.Core.Abstractions.MonadPlus
open FsControl.Core.Types.MonadTrans
open FsControl.Core.Types.MonadAsync
open FsControl.Core.Types.MonadError
open FsControl.Core.Types.State

type StateT<'S,'MaS> = StateT of ('S -> 'MaS)

module StateT =
    let  runStateT   (StateT x) = x
    let  mapStateT f (StateT m) = StateT(f << m)
    let withStateT f (StateT m) = StateT(m << f)

open StateT

type StateT<'S,'MaS> with
    static member inline instance (Functor.Fmap, StateT m, _) = fun f -> StateT <| fun s -> do'(){
        let! (x, s') = m s
        return (f x, s')}
    static member inline instance (Monad.Return, _:StateT<'s,'ma>                        ) : 'a -> StateT<'s,'ma> = fun a -> StateT <| fun s -> return' (a, s)
    static member inline instance (Monad.Bind  ,   StateT (m:'s->'mas), _:StateT<'s,'mbs>) :('a -> StateT<'s,'mbs>) -> StateT<'s,'mbs> = 
        fun k -> StateT <| fun s -> do'(){
            let! (a, s') = m s
            return! runStateT (k a) s'}

    static member inline instance (MonadPlus.Mzero, _:StateT<_,_>    ) = fun ()         -> StateT <| fun _ -> mzero()
    static member inline instance (MonadPlus.Mplus,   StateT m,     _) = fun (StateT n) -> StateT <| fun s -> mplus (m s) (n s)

    static member inline instance (MonadTrans.Lift, _:StateT<'s,'mas>) = fun (m:'ma) -> (StateT <| fun s -> m >>= fun a -> return' (a,s)):StateT<'s,'mas>
    
    static member inline instance (MonadState.Get, _:StateT<_,_>    ) = fun () -> StateT (fun s -> return' (s , s))
    static member inline instance (MonadState.Put, _:StateT<_,_>    ) = fun x  -> StateT (fun _ -> return' ((), x))

    static member inline instance (MonadAsync.LiftAsync, _:StateT<_,_>) = fun (x: Async<_>) -> lift (liftAsync x)

    static member inline instance (MonadError.ThrowError, _:StateT<_,_>    ) = lift << throwError
    static member inline instance (MonadError.CatchError,  m:StateT<'T,'U> , _:StateT<'T,'U>) = fun (h:'e -> StateT<'T,'U>) -> 
        StateT (fun s -> catchError (runStateT m s)   (fun e -> runStateT (h e) s)):StateT<'T,'U>