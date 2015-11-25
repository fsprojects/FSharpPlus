namespace FSharpPlus

open FsControl

/// <summary> Computation type: Computations which can be interrupted and resumed.
/// <para/>   Binding strategy: Binding a function to a monadic value creates a new continuation which uses the function as the continuation of the monadic computation.
/// <para/>   Useful for: Complex control structures, error handling, and creating co-routines.</summary>
type Cont<'r,'t> = Cont of (('t->'r)->'r)

[<RequireQualifiedAccess>]
module Cont =
    let run (Cont x) = x      : ('T->'R)->'R  
    
    /// (call-with-current-continuation) calls a function with the current continuation as its argument.
    let callCC (f:('T->Cont<'R,'U>)->_) = Cont (fun k -> run (f (fun a -> Cont(fun _ -> k a))) k)

    let map  (f:'T->_) (Cont x) = Cont (fun c -> x (c << f))                                        : Cont<'R,'U>
    let bind (f:'T->_) (Cont x) = Cont (fun k -> x (fun a -> run(f a) k))                           : Cont<'R,'U>
    let apply (Cont f) (Cont x) = Cont (fun k -> f (fun (f':'T->_) -> x (k << f')))                 : Cont<'R,'U>

type Cont with
    static member Map    (x:Cont<'R,'T>, f) = Cont.map f x              : Cont<'R,'U>
    static member Return n = Cont (fun k -> k n)                        : Cont<'R,'T>
    static member Bind   (x, f:'T->_) = Cont.bind f x                   : Cont<'R,'U>
    static member (<*>)  (f, x:Cont<'R,'T>) = Cont.apply f x            : Cont<'R,'U>
    static member CallCC (f:('T -> Cont<'R,'U>) -> _) = Cont.callCC f   : Cont<'R,'T>


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
    static member Apply  (f, x, _:ContT<'``Monad<'R>``,'U>, _:Apply) = ContT.apply f x      : ContT<'``Monad<'R>``,'U>
    static member Bind   (x, f:'T->_) = ContT.bind f x                                      : ContT<'``Monad<'R>``,'U>

    static member inline Lift (m:'``Monad<'T>``) = ContT((>>=) m) : ContT<'``Monad<'R>``,'T>    

    static member inline LiftAsync (x: Async<'T>) = lift (liftAsync x) : ContT<Async<'R>,'T>

    static member CallCC (f:(_->ContT<_,'T>)->_) = ContT (fun k -> ContT.run (f (fun a -> ContT (fun _ -> k a))) k) : ContT<'``Monad<'R>``,'U>

    static member inline get_Ask() = lift ask               : '``ContT<'MonadReader<'R,'T>,'R>``
    static member inline Local (ContT m, f : 'R1 -> 'R2)    : ContT<_,'``MonadReader<R1,'T>,'U``> =
        ContT <| fun c -> (ask >>= (fun r -> local f (m (local (konst r) << c))))
    
    static member inline get_Get()  = lift get         : '``ContT<'MonadState<'S, 'T>, 'S>``
    static member inline Put (x:'S) = x |> put |> lift : '``ContT<'MonadState<'S, 'T>, unit>``