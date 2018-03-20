namespace FSharpPlus.Data
open System.ComponentModel

/// <summary> Computation type: Computations which can be interrupted and resumed.
/// <para/>   Binding strategy: Binding a function to a monadic value creates a new continuation which uses the function as the continuation of the monadic computation.
/// <para/>   Useful for: Complex control structures, error handling, and creating co-routines.</summary>
[<Struct>]
type Cont<'r,'t> = Cont of (('t->'r)->'r)

/// Basic operations on Cont
[<RequireQualifiedAccess>]
module Cont =
    let run (Cont x) = x      : ('T->'R)->'R  
    
    /// (call-with-current-continuation) calls a function with the current continuation as its argument.
    let callCC (f:('T->Cont<'R,'U>)->_) = Cont (fun k -> run (f (fun a -> Cont(fun _ -> k a))) k)

    let map  (f:'T->_) (Cont x) = Cont (fun c -> x (c << f))                        : Cont<'R,'U>
    let bind (f:'T->_) (Cont x) = Cont (fun k -> x (fun a -> run (f a) k))          : Cont<'R,'U>
    let apply (Cont f) (Cont x) = Cont (fun k -> f (fun (f':'T->_) -> x (k << f'))) : Cont<'R,'U>

open FSharpPlus

/// Monad Transformer for Cont<'R,'T>
type ContT<'r,'t> = Cont<'r,'t>

type Cont<'r,'t> with
    static member Return n = Cont (fun k -> k n)                        : Cont<'R,'T>
    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member Map    (x : Cont<'R,'T>, f) = Cont.map f x            : Cont<'R,'U>
    static member (<*>)  (f, x : Cont<'R,'T>) = Cont.apply f x          : Cont<'R,'U>
    static member (>>=)  (x, f : 'T->_)       = Cont.bind f x           : Cont<'R,'U>
    static member Delay f = Cont (fun k -> Cont.run (f()) k)            : Cont<'R,'T>
    static member TryWith    (Cont c, h) = Cont(fun k -> try (c k) with e -> Cont.run (h e) k) : Cont<'R,'T>
    static member TryFinally (Cont c, h) = Cont(fun k -> try (c k) finally h())                : Cont<'R,'T>

    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member CallCC (f: ('T -> Cont<'R,'U>) -> _) = Cont.callCC f  : Cont<'R,'T>

    static member inline Lift (m:'``Monad<'T>``) = Cont ((>>=) m) : ContT<'``Monad<'R>``,'T>    

    static member inline LiftAsync (x: Async<'T>) = lift (liftAsync x) : ContT<Async<'R>,'T>

    static member inline get_Ask () = lift ask              : '``ContT<'MonadReader<'R,'T>,'R>``
    static member inline Local (Cont m, f : 'R1 -> 'R2)     : ContT<_,'``MonadReader<R1,'T>,'U``> =
        Cont <| fun c -> (ask >>= (fun r -> local f (m (local (konst r) << c))))
    
    static member inline get_Get () = lift get         : '``ContT<'MonadState<'S, 'T>, 'S>``
    static member inline Put (x:'S) = x |> put |> lift : '``ContT<'MonadState<'S, 'T>, unit>``

/// Basic operations on ContT
module ContT =
    let run (Cont x) = x      : ('T->'R)->'R