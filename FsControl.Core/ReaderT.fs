namespace FsControl.Core.Types

open FsControl.Core.Prelude
open FsControl.Core.TypeMethods
open FsControl.Core.TypeMethods.Functor
open FsControl.Core.TypeMethods.Applicative
open FsControl.Core.TypeMethods.Monad
open FsControl.Core.TypeMethods.MonadPlus
open FsControl.Core.TypeMethods.MonadTrans
open FsControl.Core.TypeMethods.MonadAsync
open FsControl.Core.TypeMethods.MonadError

type ReaderT<'R,'Ma> = ReaderT of ('R -> 'Ma)

[<RequireQualifiedAccess>]
module ReaderT =
    let  run   (ReaderT x) = x
    let inline map f (ReaderT m) = ReaderT(fmap (Reader.map f) m)
    let inline bind f (ReaderT m) = ReaderT <| fun r -> do'(){
            let! a = m r
            return! run (f a) r}
    let inline apply f x = ReaderT(fmap (<*>) f <*> x) :ReaderT<'r,'ma>

type ReaderT<'R,'Ma> with
    static member inline instance (_:Functor.Map  , x, _) = fun f -> ReaderT.map f x

    static member inline instance (Applicative.Pure, _:ReaderT<'r,'ma>            ) :'a  -> ReaderT<'r,'ma> = fun a -> ReaderT <| fun _ -> return' a
    static member inline instance (_:Applicative.Apply, ReaderT f, ReaderT x, _:ReaderT<'r,'mb>) = fun () -> ReaderT.apply f x :ReaderT<'r,'mb>
    static member inline instance (Monad.Bind  , x, _:ReaderT<'r,'m>) :('b -> ReaderT<'r,'m>) -> ReaderT<'r,'m> = fun f -> ReaderT.bind f x

    static member inline instance (MonadPlus.Mzero, _:ReaderT<_,_>        ) = fun ()          -> ReaderT <| fun _ -> mzero()
    static member inline instance (MonadPlus.Mplus,   ReaderT m   ,      _) = fun (ReaderT n) -> ReaderT <| fun r -> mplus (m r) (n r)

    static member inline instance (MonadTrans.Lift, _:ReaderT<'r,'ma>     ) = fun m -> (ReaderT <| fun _ -> m) : ReaderT<'r,'ma>

    static member instance (MonadCont.CallCC , _:ReaderT<'r,Cont<'c,'a>> ) : (('a -> ReaderT<'t,Cont<'c,'u>>) -> ReaderT<'r,Cont<'c,'a>>) -> ReaderT<'r,Cont<'c,'a>> =
        fun f -> ReaderT(fun r -> Cont.callCC <| fun c -> ReaderT.run (f (fun a -> ReaderT <| fun _ -> c a)) r)

    static member inline instance (MonadReader.Ask, _:ReaderT<'r,'a>      ) = fun () -> ReaderT return' :ReaderT<'r,'a>
    static member inline instance (MonadReader.Local, ReaderT m, _:ReaderT<_,_>) = fun f  -> ReaderT(fun r -> m (f r))

    static member inline instance (MonadAsync.LiftAsync,  _:ReaderT<_,_>        ) = fun (x: Async<_>) -> lift (liftAsync x)

    static member inline instance (MonadError.ThrowError, _:ReaderT<_,_>    ) = lift << throwError
    static member inline instance (MonadError.CatchError,  m:ReaderT<'T,'U> , _:ReaderT<'T,'U>) = fun (h:'e -> ReaderT<'T,'U>) -> 
        ReaderT (fun s -> catchError (ReaderT.run m s)   (fun e -> ReaderT.run (h e) s)):ReaderT<'T,'U>

    static member instance (MonadWriter.Tell  , _:ReaderT<'t,'a->Writer<'a,unit>>          ) :        ReaderT<'t,'a->Writer<'a,unit>> = lift Writer.tell
    static member instance (MonadWriter.Listen,   ReaderT m, _:ReaderT<'t,Writer<'a,'b*'a>>) :unit -> ReaderT<'t,Writer<'a,'b*'a>> = fun () -> ReaderT <| fun w -> Writer.listen (m w)  
    static member instance (MonadWriter.Pass  ,   ReaderT m, _:ReaderT<'t,Writer<'a,'b>>   ) :unit -> ReaderT<'t,Writer<'a,'b>>    = fun () -> ReaderT <| fun w -> Writer.pass   (m w)

    static member instance (MonadState.Get    , _:ReaderT<'s,State<'a,'a>>  ) = fun () -> lift (State.get()) :ReaderT<'s,State<'a,'a>>
    static member instance (MonadState.Put    , _:ReaderT<'s,State<'a,unit>>) = lift << State.put : 'a -> ReaderT<'s,State<'a,unit>>