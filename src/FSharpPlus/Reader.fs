namespace FSharpPlus.Data

open FSharpPlus
open FSharpPlus.Control
open System.ComponentModel
/// <summary> Computation type: Computations which read values from a shared environment.
/// <para/>   Binding strategy: Monad values are functions from the environment to a value. The bound function is applied to the bound value, and both have access to the shared environment.
/// <para/>   Useful for: Maintaining variable bindings, or other shared environment.</summary>
[<Struct>]
type Reader<'r,'t> = Reader of ('r->'t)

/// Basic operations on Reader
[<RequireQualifiedAccess>]
module Reader =
    let run (Reader x) = x                                                 : 'R->'T
    let map  (f: 'T->_ ) (Reader m) = Reader (f << m)                      : Reader<'R,'U>
    let bind (f: 'T->_ ) (Reader m) = Reader (fun r -> run (f (m r)) r)    : Reader<'R,'U>
    let apply (Reader f) (Reader x) = Reader (fun a -> f a ((x: _->'T) a)) : Reader<'R,'U>

    /// Retrieves the monad environment.
    let ask = Reader id                                                    : Reader<'R,'R>

    /// <summary> Executes a computation in a modified environment. </summary>
    /// <param name="f"> The function to modify the environment.    </param>
    /// <param name="m"> Reader to run in the modified environment. </param>
    let local (f: 'R1->'R2) m = let (Reader m) = m in Reader (m << f) : Reader<'R1,'T>

type Reader<'r,'t> with

    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member Map   (x: Reader<'R,'T>, f) = Reader.map f x   : Reader<'R,'U>

    static member Return x = Reader (fun _ -> x)                 : Reader<'R,'T>
    static member (>>=) (x: Reader<'R,'T>, f) = Reader.bind f x  : Reader<'R,'U>
    static member (<*>) (f, x: Reader<'R,'T>) = Reader.apply f x : Reader<'R,'U>

    static member get_Ask ()    = Reader.ask                     : Reader<'R,'R>

    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member Local (m, f: 'R1->'R2) = Reader.local f m      : Reader<'R1,'T>

    static member inline Extract (Reader (f : 'Monoid -> 'T)) = f (Zero.Invoke ()) : 'T
    static member inline (=>>)   (Reader (g : 'Monoid -> 'T), f : Reader<'Monoid,'T> -> 'U) = Reader (fun a -> f (Reader (fun b -> (g (Plus.Invoke a b))))) : Reader<'Monoid,'U>


/// Monad Transformer for Reader<'R, 'T>
[<Struct>]
type ReaderT<'r,'``monad<'t>``> = ReaderT of ('r -> '``monad<'t>``)

/// Basic operations on Reader
[<RequireQualifiedAccess>]
module ReaderT =
    let  run (ReaderT x) = x : 'R -> '``Monad<'T>``
    #if !FABLE_COMPILER
    let inline hoist (x: Reader<'R, 'T>) = (ReaderT << (fun a -> result << a) << Reader.run) x : ReaderT<'R, '``Monad<'T>``>
    #endif
    let inline map   (f: 'T->'U) (ReaderT m: ReaderT<'R, '``Monad<'T>``>) = ReaderT (map f << m)                                : ReaderT<'R, '``Monad<'U>``>
    let inline apply (ReaderT (f: _ -> '``Monad<'T -> 'U>``)) (ReaderT (x: _->'``Monad<'T>``)) = ReaderT (fun r -> f r <*> x r) : ReaderT<'R, '``Monad<'U>``>
    let inline bind  (f: 'T->_) (ReaderT (m: _->'``Monad<'T>``)) = ReaderT (fun r -> m r >>= (fun a -> run (f a) r))            : ReaderT<'R, '``Monad<'U>``>

type ReaderT<'r,'``monad<'t>``> with
    #if !FABLE_COMPILER
    static member inline Return (x: 'T) = ReaderT (fun _ -> result x)                                                 : ReaderT<'R, '``Monad<'T>``>
    #endif
    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member inline Map   (x: ReaderT<'R, '``Monad<'T>``>, f: 'T->'U)                        = ReaderT.map   f x : ReaderT<'R, '``Monad<'U>``>

    static member inline (<*>) (f: ReaderT<_,'``Monad<'T -> 'U>``>, x: ReaderT<_,'``Monad<'T>``>) = ReaderT.apply f x : ReaderT<'R, '``Monad<'U>``>
    static member inline (>>=) (x: ReaderT<_,'``Monad<'T>``>, f: 'T->ReaderT<'R,'``Monad<'U>``>)  = ReaderT.bind  f x : ReaderT<'R, '``Monad<'U>``>
    
    static member inline get_Empty () = ReaderT (fun _ -> getEmpty ()) : ReaderT<'R, '``MonadPlus<'T>``>
    static member inline (<|>) (ReaderT m, ReaderT n) = ReaderT (fun r -> m r <|> n r) : ReaderT<'R, '``MonadPlus<'T>``>

    static member inline TryWith (source: ReaderT<'R,'``Monad<'T>``>, f: exn -> ReaderT<'R,'``Monad<'T>``>) = ReaderT (fun s -> TryWith.Invoke (ReaderT.run source s) (fun x -> ReaderT.run (f x) s))
    static member inline TryFinally (computation: ReaderT<'R,'``Monad<'T>``>, f) = ReaderT (fun s -> TryFinally.Invoke     (ReaderT.run computation s) f)
    static member inline Using (resource, f: _ -> ReaderT<'R,'``Monad<'T>``>)    = ReaderT (fun s -> Using.Invoke resource (fun x -> ReaderT.run (f x) s))
    static member inline Delay (body : unit   ->  ReaderT<'R,'``Monad<'T>``>)    = ReaderT (fun s -> Delay.Invoke (fun _ -> ReaderT.run (body ()) s))

    static member        Lift m = ReaderT (fun _ -> m)                                  : ReaderT<'R,'``Monad<'T>``>

    static member inline LiftAsync (x: Async<'T>) = (lift (liftAsync x)                 : ReaderT<'R,'``MonadAsync<'T>``>)

    static member inline CallCC (f: ('T -> ReaderT<'R, Cont<_,'U>>) -> _)               : ReaderT<'R,'``MonadCont<'C,'T>``> =
        ReaderT (fun r -> callCC <| fun c -> ReaderT.run (f (fun a -> ReaderT <| fun _ -> c a)) r)
          
    #if !FABLE_COMPILER  
    static member inline get_Ask () = ReaderT result                                    : ReaderT<'R,'``Monad<'T>``>
    #endif
    static member        Local (ReaderT m, f: _->'R2) = ReaderT (fun r -> m (f r))      : ReaderT<'R1,'``Monad<'T>``>

    static member inline Throw (x: 'E) = x |> throw |> lift : ReaderT<'R,'``MonadError<'E,'T>``>
    static member inline Catch (m: ReaderT<'R,'``MonadError<'E1,'T>``>, h: 'E1 -> _) =
        ReaderT (fun s -> catch (ReaderT.run m s)   (fun e -> ReaderT.run (h e) s))     : ReaderT<'R,'``MonadError<'E2,'T>``>

    static member inline Tell   w           = w |> tell |> lift                         : ReaderT<'R, '``MonadWriter<'Monoid,unit>``>
    static member inline Listen (ReaderT m) = ReaderT (fun w -> listen (m w))           : ReaderT<'R, '``MonadWriter<'Monoid,'T*'Monoid>``>
    static member inline Pass   (ReaderT m) = ReaderT (fun w -> pass   (m w))           : ReaderT<'R, '``MonadWriter<'Monoid,'T>``>   

    static member inline get_Get () = lift get         : ReaderT<'R, '``MonadState<'S, 'S>``>
    static member inline Put x      = x |> put |> lift : ReaderT<'R, '``MonadState<'S, unit>``>