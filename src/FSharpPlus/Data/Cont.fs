namespace FSharpPlus.Data

/// <namespacedoc>
/// <summary>
/// Data contains types and modules that represent data structures designed to be used with F#+ abstractions.
/// </summary>
/// </namespacedoc>

#nowarn "1125"

open System.ComponentModel
open FSharpPlus


/// <summary> Computation type: Computations which can be interrupted and resumed.
/// <para/>   Binding strategy: Binding a function to a monadic value creates a new continuation which uses the function as the continuation of the monadic computation.
/// <para/>   Useful for: Complex control structures, error handling, and creating co-routines.</summary>
[<Struct>]
type Cont<'r,'t> = Cont of (('t->'r)->'r)

/// Basic operations on Cont
[<RequireQualifiedAccess>]
module Cont =

    /// The result of running a CPS computation with a given final continuation.
    let run (Cont x) (continuation: 'T->'R) = x continuation : 'R
    
    /// The result of running a CPS computation with the identity function as the final continuation.
    let eval (Cont x) = x id : 'R

    /// (call-with-current-continuation) calls a function with the current continuation as its argument.
    let callCC (f: ('T->Cont<'R,'U>)->_) = Cont (fun k -> run (f (fun a -> Cont(fun _ -> k a))) k)

    let map  (f: 'T->_) (Cont x) = Cont (fun c -> x (c << f))                         : Cont<'R,'U>
    let bind (f: 'T->_) (Cont x) = Cont (fun k -> x (fun a -> run (f a) k))           : Cont<'R,'U>
    let apply  (Cont f) (Cont x) = Cont (fun k -> f (fun (f': 'T->_) -> x (k << f'))) : Cont<'R,'U>

    let map2 (f: 'T -> 'U -> 'V) (Cont x) (Cont y)                = Cont (fun k -> x (f >> fun k' -> y (k' >> k)))                       : Cont<'R, 'V>
    let map3 (f: 'T -> 'U -> 'V -> 'W) (Cont x) (Cont y) (Cont z) = Cont (fun k -> x (f >> fun k' -> y (k' >> fun k'' -> z (k'' >> k)))) : Cont<'R, 'W>


/// Monad Transformer for Cont<'R,'T>
type ContT<'r,'t> = Cont<'r,'t>

type Cont<'r,'t> with
    static member Return n = Cont (fun k -> k n)              : Cont<'R,'T>

    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member Map   (x: Cont<'R,'T>, f) = Cont.map f x    : Cont<'R,'U>

    /// <summary>Lifts a function into a Cont. Same as map.
    /// To be used in Applicative Style expressions, combined with &lt;*&gt;
    /// </summary>
    /// <category index="1">Functor</category>
    static member (<!>) (f: 'T->'U, x: Cont<'R, 'T>) : Cont<'R, 'U> = Cont.map f x

    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member inline Lift2 (f: 'T->'U->'V, x: Cont<'R,'T>, y: Cont<'R,'U>) : Cont<'R,'V> = Cont.map2 f x y

    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member inline Lift3 (f: 'T->'U->'V->'W, x: Cont<'R,'T>, y: Cont<'R,'U>, z: Cont<'R,'V>) : Cont<'R,'W> = Cont.map3 f x y z

    static member (<*>) (f, x: Cont<'R,'T>) = Cont.apply f x  : Cont<'R,'U>

    /// <summary>
    /// Sequences two Conts left-to-right, discarding the value of the first argument.
    /// </summary>
    /// <category index="2">Applicative</category>
    static member ( *>) (x: Cont<'R, 'T>, y: Cont<'R, 'U>) : Cont<'R, 'U> = ((fun (_: 'T) (k: 'U) -> k) </Cont.map/>  x : Cont<'R, 'U->'U>) </Cont.apply/> y
    
    /// <summary>
    /// Sequences two Conts left-to-right, discarding the value of the second argument.
    /// </summary>
    /// <category index="2">Applicative</category>
    static member (<*  ) (x: Cont<'R, 'U>, y: Cont<'R, 'T>) : Cont<'R, 'U> = ((fun (k: 'U) (_: 'T) -> k ) </Cont.map/> x : Cont<'R, 'T->'U>) </Cont.apply/> y

    static member (>>=) (x, f: 'T->_)       = Cont.bind f x   : Cont<'R,'U>
    
    /// <summary>
    /// Composes left-to-right two Cont functions (Kleisli composition).
    /// </summary>
    /// <category index="2">Monad</category>
    static member (>=>) (f, (g: 'U -> _)) : 'T -> Cont<'R, 'V> = fun x -> Cont.bind g (f x)

    static member Delay f = Cont (fun k -> Cont.run (f ()) k) : Cont<'R,'T>
    static member TryWith    (Cont c, h) = Cont (fun k -> try (c k) with e -> Cont.run (h e) k) : Cont<'R,'T>
    static member TryFinally (Cont c, h) = Cont (fun k -> try (c k) finally h ())               : Cont<'R,'T>
    static member Using (resource, f: _ -> Cont<'R,'T>) = Cont.TryFinally (f resource, fun () -> dispose resource)

    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member CallCC (f: ('T -> Cont<'R,'U>) -> _) = Cont.callCC f : Cont<'R,'T>

#if !FABLE_COMPILER || FABLE_COMPILER_3 || FABLE_COMPILER_4

    static member inline Lift (m: '``Monad<'T>``) = Cont ((>>=) m) : ContT<'``Monad<'R>``,'T>

    static member inline LiftAsync (x: Async<'T>) = lift (liftAsync x) : ContT<Async<'R>,'T>
    
    static member inline get_Ask () = lift ask             : '``ContT<'MonadReader<'R,'T>,'R>``
    static member inline Local (Cont m, f: 'R1 -> 'R2)     : ContT<_,'``MonadReader<R1,'T>,'U``> =
        Cont <| fun c -> (ask >>= (fun r -> local f (m (local (konst r) << c))))
    
    static member inline get_Get () = lift get          : '``ContT<'MonadState<'S, 'T>, 'S>``
    static member inline Put (x: 'S) = x |> put |> lift : '``ContT<'MonadState<'S, 'T>, unit>``

#endif


/// Basic operations on ContT
module ContT =

    /// The result of running a CPS computation with the identity function as the final continuation.
    let run (Cont x: ContT<'MR, 'T>) (continuation: 'T->'MR) = x continuation : 'MR

#if !FABLE_COMPILER

    /// The result of running a CPS computation with its inner monad's 'Return' function as the final continuation.
    let inline eval (Cont x: ContT<'MR, 'T>) = x result :' MR

#endif
