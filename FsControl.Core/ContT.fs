namespace FsControl.Core.Types

open FsControl.Core.Prelude
open FsControl.Core.TypeMethods
open FsControl.Core.TypeMethods.Monad
open FsControl.Core.TypeMethods.Functor
open FsControl.Core.TypeMethods.MonadTrans
open FsControl.Core.TypeMethods.MonadAsync

type ContT<'Mr,'A> = ContT of  (('A -> 'Mr) -> 'Mr)    

[<RequireQualifiedAccess>]
module ContT =
    let run   (ContT x) = x
    let inline map f (ContT m) = ContT(fmap (Cont.map f) m)

type ContT<'Mr,'A> with
    static member instance (_:Functor.Map, ContT m, _) = fun f -> ContT(fun c -> m (c << f))

    static member instance (Applicative.Pure, _:ContT<'mr,'a>           ) = fun a -> ContT((|>) a) :ContT<'mr,'a>
    static member instance (Monad.Bind  ,   ContT m, _:ContT<'mr,'b>) = fun k -> ContT(fun c -> m (fun a -> ContT.run(k a) c)) :ContT<'mr,'b>

    static member inline instance (MonadTrans.Lift  , _:ContT<'mr,'a>) = fun (m:'ma) -> ContT((>>=) m) : ContT<'mr,'a>    

    static member inline instance (MonadAsync.LiftAsync   , _:ContT<_,_>   ) = fun (x: Async<_>) -> lift (liftAsync x)

    static member        instance (MonadCont .CallCC, _:ContT<'mr,'b>) = fun f -> ContT(fun k -> ContT.run(f (fun a -> ContT(fun _ -> k a))) k) : ContT<'mr,'b>

    static member instance (MonadReader.Ask, _:ContT<Reader<'a,'b>,'a>) = fun () -> lift (Reader.ask())  :ContT<Reader<'a,'b>,'a>
    static member instance (MonadReader.Local, ContT m, _:ContT<Reader<'a,'b>,'t>) : ('a -> 'b) -> ContT<Reader<'a,'b>,'t> =
        fun f -> ContT <| fun c -> do'(){     
            let! r = Reader.ask()
            return! Reader.local f (m (Reader.local (const' r) << c))}
    
    static member instance (MonadState.Get   , _:ContT<State<'s,'a>,'s>  ) = fun () -> lift (State.get()):ContT<State<'s,'a>,'s>
    static member instance (MonadState.Put   , _:ContT<State<'s,'a>,unit>) = lift << State.put :'s ->     ContT<State<'s,'a>,unit>