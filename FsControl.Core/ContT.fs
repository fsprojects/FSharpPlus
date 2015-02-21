namespace FsControl.Core.Types

open FsControl.Core.Prelude
open FsControl.Core.TypeMethods
open FsControl.Core.TypeMethods.Monad

type ContT<'Mr,'A> = ContT of  (('A -> 'Mr) -> 'Mr)    

[<RequireQualifiedAccess>]
module ContT =
    let run (ContT m) = m
    let map  f (ContT m) = ContT (fun k -> m (k << f))
    let bind f (ContT m) = ContT (fun k -> m (fun a -> run (f a) k)) :ContT<'mr,'b>
    let apply  (ContT f) (ContT x) = ContT (fun k -> f (fun f' -> x (k << f')))  :ContT<'mr,'a>

type ContT<'Mr,'A> with
    static member Map    (x, _, _:Map) = fun f -> ContT.map f x
    static member Return (_:ContT<'mr,'a>      , _:Return) = fun a  -> ContT ((|>) a)  :ContT<'mr,'a>
    static member Apply  (f, x, _:ContT<'mr,'b>, _:Apply ) = ContT.apply f x :ContT<'mr,'b>
    static member Bind   (x, _:ContT<'mr,'b>, _:Bind) = fun f -> ContT.bind f x :ContT<'mr,'b>

    static member inline Lift (_:ContT<'mr,'a>) = fun (m:'ma) -> ContT((>>=) m) : ContT<'mr,'a>    

    static member inline LiftAsync (_:ContT<_,_>   ) = fun (x: Async<_>) -> Lift.Invoke (LiftAsync.Invoke x)

    static member        CallCC (_:ContT<'mr,'b>) = fun f -> 
        ContT (fun k -> ContT.run (f (fun a -> ContT (fun _ -> k a))) k) : ContT<'mr,'b>

    static member Ask   (_:ContT<Reader<'a,'b>,'a>) = Lift.Invoke (Reader.ask())  :ContT<Reader<'a,'b>,'a>
    static member Local (ContT m, _:ContT<Reader<'a,'b>,'t>) : ('a -> 'b) -> ContT<Reader<'a,'b>,'t> =
        fun f -> ContT <| fun c -> do'(){     
            let! r = Reader.ask()
            return! Reader.local f (m (Reader.local (const' r) << c))}
    
    static member Get (_:ContT<State<'s,'a>,'s>  ) = Lift.Invoke (State.get()):ContT<State<'s,'a>,'s>
    static member Put (_:ContT<State<'s,'a>,unit>) = Lift.Invoke << State.put :'s ->     ContT<State<'s,'a>,unit>