namespace FsControl

open FsControl.Core.Internals.Prelude
open FsControl.Core.Internals.MonadOps

type ContT<'Mr,'A> = ContT of  (('A -> 'Mr) -> 'Mr)    

[<RequireQualifiedAccess>]
module ContT =
    let run (ContT m) = m
    let map  f (ContT m) = ContT (fun k -> m (k << f))
    let bind f (ContT m) = ContT (fun k -> m (fun a -> run (f a) k)) :ContT<'mr,'b>
    let apply  (ContT f) (ContT x) = ContT (fun k -> f (fun f' -> x (k << f')))  :ContT<'mr,'a>

type ContT<'Mr,'A> with
    static member Map    (x, f, _:Map) = ContT.map f x
    static member Return (_:ContT<'mr,'a>      , _:Return) = fun a  -> ContT ((|>) a)  :ContT<'mr,'a>
    static member Apply  (f, x, _:ContT<'mr,'b>, _:Apply ) = ContT.apply f x :ContT<'mr,'b>
    static member Bind (x, f) = ContT.bind f x :ContT<'mr,'b>

    static member inline Lift (m:'ma) = ContT((>>=) m) : ContT<'mr,'a>    

    static member inline LiftAsync (_:ContT<_,_>   ) = fun (x: Async<_>) -> Lift.Invoke (LiftAsync.Invoke x)

    static member CallCC f = ContT (fun k -> ContT.run (f (fun a -> ContT (fun _ -> k a))) k) : ContT<'mr,'b>

    static member get_Ask() = Lift.Invoke Reader.ask : ContT<Reader<'a,'b>,'a>
    static member Local (ContT m, f : 'a -> 'b)      : ContT<Reader<'a,'b>,'t> =
        ContT <| fun c -> (Reader.ask >>= (fun r -> Reader.local f (m (Reader.local (const' r) << c))))
    
    static member get_Get()  = Lift.Invoke State.get         : ContT<State<'s, 'a>, 's>
    static member Put (x:'s) = x |> State.put |> Lift.Invoke : ContT<State<'s, 'a>, unit>