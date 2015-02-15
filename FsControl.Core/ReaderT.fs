namespace FsControl.Core.Types

open FsControl.Core.Prelude
open FsControl.Core.TypeMethods
open FsControl.Core.TypeMethods.Functor
open FsControl.Core.TypeMethods.Applicative
open FsControl.Core.TypeMethods.Monad
open FsControl.Core.TypeMethods.MonadTrans
open FsControl.Core.TypeMethods.MonadAsync
open FsControl.Core.TypeMethods.MonadError

type ReaderT<'R,'Ma> = ReaderT of ('R -> 'Ma)

[<RequireQualifiedAccess>]
module ReaderT =
    let  run (ReaderT x) = x
    let inline map (f:'a->'b) (ReaderT m) = ReaderT (fmap f << m)
    let inline apply (ReaderT f) (ReaderT x) = (ReaderT <| fun r -> f r <*> x r) :ReaderT<'r,'ma>
    let inline bind f (ReaderT m) = ReaderT <| fun r -> do'() {
        let! a = m r
        return! run (f a) r}    

type ReaderT<'R,'Ma> with
    static member inline Map    (_:Functor.Map, x, _) = fun f -> ReaderT.map f x

    static member inline Return (_:Applicative.Return, _:ReaderT<'r,'ma>) :'a  -> ReaderT<'r,'ma> = fun a -> 
        ReaderT <| fun _ -> result a

    static member inline Apply  (_:Applicative.Apply, f, x, _:ReaderT<'r,'mb>) = fun () -> 
        ReaderT.apply f x :ReaderT<'r,'mb>

    static member inline Bind   (_:Monad.Bind  , x, _:ReaderT<'r,'m>) :('b -> ReaderT<'r,'m>) -> ReaderT<'r,'m> = fun f -> 
        ReaderT.bind f x

    static member inline Zero (_:Functor.Zero, _:ReaderT<_,_>   ) = fun ()          -> ReaderT <| fun _ -> zero()
    static member inline Plus (_:Functor.Plus,   ReaderT m   ,_ ) = fun (ReaderT n) -> ReaderT <| fun r -> plus (m r) (n r)

    static member inline Lift (_:MonadTrans.Lift, _:ReaderT<'r,'ma>) = fun m -> (ReaderT <| fun _ -> m) : ReaderT<'r,'ma>

    static member CallCC (_:MonadCont.CallCC , _:ReaderT<'r,Cont<'c,'a>> ) : (('a -> ReaderT<'t,Cont<'c,'u>>) -> ReaderT<'r,Cont<'c,'a>>) -> ReaderT<'r,Cont<'c,'a>> =
        fun f -> ReaderT (fun r -> Cont.callCC <| fun c -> ReaderT.run (f (fun a -> ReaderT <| fun _ -> c a)) r)

    static member inline Ask   (_:MonadReader.Ask, _:ReaderT<'r,'a>      ) = fun () -> ReaderT result :ReaderT<'r,'a>
    static member inline Local (_:MonadReader.Local, ReaderT m, _:ReaderT<_,_>) = fun f  -> ReaderT(fun r -> m (f r))

    static member inline LiftAsync (_:MonadAsync.LiftAsync, _:ReaderT<_,_>) = fun (x: Async<_>) -> lift (liftAsync x)

    static member inline ThrowError (_:MonadError.ThrowError, _:ReaderT<_,_>    ) = lift << throwError
    static member inline CatchError (_:MonadError.CatchError,  m:ReaderT<'T,'U> , _:ReaderT<'T,'U>) = fun (h:'e -> ReaderT<'T,'U>) -> 
        ReaderT (fun s -> catchError (ReaderT.run m s)   (fun e -> ReaderT.run (h e) s)):ReaderT<'T,'U>

    static member Tell   (_:MonadWriter.Tell  , _:ReaderT<'t,'a->Writer<'a,unit>>          ) :        ReaderT<'t,'a->Writer<'a,unit>> = lift Writer.tell
    static member Listen (_:MonadWriter.Listen,   ReaderT m, _:ReaderT<'t,Writer<'a,'b*'a>>) :unit -> ReaderT<'t,Writer<'a,'b*'a>> = fun () -> ReaderT <| fun w -> Writer.listen (m w)  
    static member Pass   (_:MonadWriter.Pass  ,   ReaderT m, _:ReaderT<'t,Writer<'a,'b>>   ) :unit -> ReaderT<'t,Writer<'a,'b>>    = fun () -> ReaderT <| fun w -> Writer.pass   (m w)

    static member Get (_:MonadState.Get    , _:ReaderT<'s,State<'a,'a>>  ) = fun () -> lift (State.get()) :ReaderT<'s,State<'a,'a>>
    static member Put (_:MonadState.Put    , _:ReaderT<'s,State<'a,unit>>) = lift << State.put : 'a -> ReaderT<'s,State<'a,unit>>