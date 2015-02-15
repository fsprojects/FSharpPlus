namespace FsControl.Core.Types

open FsControl.Core.Prelude
open FsControl.Core.TypeMethods
open FsControl.Core.TypeMethods.Functor
open FsControl.Core.TypeMethods.Applicative
open FsControl.Core.TypeMethods.Monad
open FsControl.Core.TypeMethods.MonadTrans
open FsControl.Core.TypeMethods.MonadAsync

type ContT<'Mr,'A> = ContT of  (('A -> 'Mr) -> 'Mr)    

[<RequireQualifiedAccess>]
module ContT =
    let run (ContT m) = m
    let map  f (ContT m) = ContT (fun k -> m (k << f))
    let bind f (ContT m) = ContT (fun k -> m (fun a -> run (f a) k)) :ContT<'mr,'b>
    let apply  (ContT f) (ContT x) = ContT (fun k -> f (fun f' -> x (k << f')))  :ContT<'mr,'a>

type ContT<'Mr,'A> with
    static member Map    (_:Functor.Map, x, _) = fun f -> ContT.map f x
    static member Return (_:Applicative.Return, _:ContT<'mr,'a>       ) = fun a  -> ContT ((|>) a)  :ContT<'mr,'a>
    static member Apply  (_:Applicative.Apply, f, x, _:ContT<'mr,'b>) = fun () -> ContT.apply f x :ContT<'mr,'b>
    static member Bind   (_:Monad.Bind  , x, _:ContT<'mr,'b>) = fun f -> ContT.bind f x :ContT<'mr,'b>

    static member inline Lift (_:MonadTrans.Lift  , _:ContT<'mr,'a>) = fun (m:'ma) -> ContT((>>=) m) : ContT<'mr,'a>    

    static member inline LiftAsync (_:MonadAsync.LiftAsync   , _:ContT<_,_>   ) = fun (x: Async<_>) -> lift (liftAsync x)

    static member        CallCC (_:MonadCont.CallCC, _:ContT<'mr,'b>) = fun f -> 
        ContT (fun k -> ContT.run (f (fun a -> ContT (fun _ -> k a))) k) : ContT<'mr,'b>

    static member Ask   (_:MonadReader.Ask, _:ContT<Reader<'a,'b>,'a>) = fun () -> lift (Reader.ask())  :ContT<Reader<'a,'b>,'a>
    static member Local (_:MonadReader.Local, ContT m, _:ContT<Reader<'a,'b>,'t>) : ('a -> 'b) -> ContT<Reader<'a,'b>,'t> =
        fun f -> ContT <| fun c -> do'(){     
            let! r = Reader.ask()
            return! Reader.local f (m (Reader.local (const' r) << c))}
    
    static member Get (_:MonadState.Get, _:ContT<State<'s,'a>,'s>  ) = fun () -> lift (State.get()):ContT<State<'s,'a>,'s>
    static member Put (_:MonadState.Put, _:ContT<State<'s,'a>,unit>) = lift << State.put :'s ->     ContT<State<'s,'a>,unit>