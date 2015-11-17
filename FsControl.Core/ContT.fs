namespace FsControl

open FsControl.Core.Internals.Prelude
open FsControl.Core.Internals.MonadOps

type ContT<'``monad<'r>``,'t> = ContT of  (('t -> '``monad<'r>``) -> '``monad<'r>``)    

[<RequireQualifiedAccess>]
module ContT =
    let run   (ContT m) = m                                                                 : ('T -> '``Monad<'R>``) ->_
    let map   (f:'T->_) (ContT m) = ContT (fun k -> m (k << f))                             : ContT<'``Monad<'R>``,'U>
    let bind  (f:'T->_) (ContT m) = ContT (fun k -> m (fun a -> run (f a) k))               : ContT<'``Monad<'R>``,'U>
    let apply (ContT f) (ContT x) = ContT (fun k -> f (fun (f':'T->'U) -> x (k << f')))     : ContT<'``Monad<'R>``,'U>

type ContT with
    static member Map    (x, f:'T->'U, _:Map) = ContT.map f x                               : ContT<'``Monad<'R>``,'U>
    static member Return (_: ContT<'``Monad<'R>``,'T>, _:Return) = fun a  -> ContT ((|>) a) : ContT<'``Monad<'R>``,'T>
    static member Apply  (f, x, _:ContT<'``Monad<'R>``,'U>, _:Apply) = ContT.apply f x: ContT<'``Monad<'R>``,'U>
    static member Bind   (x, f:'T->_) = ContT.bind f x                                      : ContT<'``Monad<'R>``,'U>

    static member inline Lift (m:'``Monad<'T>``) = ContT((>>=) m) : ContT<'``Monad<'R>``,'T>    

    static member inline LiftAsync (output:ContT<Async<'R>,'T>) = fun (x: Async<'T>) -> Lift.Invoke (LiftAsync.Invoke x) : ContT<Async<'R>,'T>

    static member CallCC (f:(_->ContT<_,'T>)->_) = ContT (fun k -> ContT.run (f (fun a -> ContT (fun _ -> k a))) k) : ContT<'``Monad<'R>``,'U>

    static member get_Ask() = Lift.Invoke Reader.ask : ContT<Reader<'R,'T>,'R>
    static member Local (ContT m, f : 'R1 -> 'R2)    : ContT<Reader<_ ,'T>,'U> =
        ContT <| fun c -> (Reader.ask >>= (fun r -> Reader.local f (m (Reader.local (const' r) << c))))
    
    static member get_Get()  = Lift.Invoke State.get         : ContT<State<'S, 'T>, 'S>
    static member Put (x:'S) = x |> State.put |> Lift.Invoke : ContT<State<'S, 'T>, unit>