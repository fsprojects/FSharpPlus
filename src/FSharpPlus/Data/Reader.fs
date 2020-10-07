namespace FSharpPlus.Data

#nowarn "1125"

open System.ComponentModel
open FSharpPlus
open FSharpPlus.Control


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

    /// Combines two Readers into one by applying a mapping function.
    let map2 (mapping: 'T->'U->'V) (Reader x) (Reader y) = Reader (fun r -> mapping (x r) (y r)) : Reader<'R,'V>
    
    /// Zips two Readers into one.
    let zip (x: Reader<'R,'T>) (y: Reader<'R,'U>) = map2 tuple2 x y        : Reader<'R, 'T * 'U>

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

    #if !FABLE_COMPILER
    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member Zip (x, y) = Reader.zip x y    

    static member inline Extract (Reader (f : 'Monoid -> 'T)) = f (Zero.Invoke ()) : 'T
    static member inline (=>>)   (Reader (g : 'Monoid -> 'T), f : Reader<'Monoid,'T> -> 'U) = Reader (fun a -> f (Reader (fun b -> (g (Plus.Invoke a b))))) : Reader<'Monoid,'U>

    #endif

    static member TryWith (Reader computation, h)    = Reader (fun s -> try computation s with e -> Reader.run (h e) s) : Reader<'R,'T>
    static member TryFinally (Reader computation, f) = Reader (fun s -> try computation s finally f ())
    static member Using (resource, f: _ -> Reader<'R,'T>) = Reader.TryFinally (f resource, fun () -> dispose resource)
    static member Delay (body: unit->Reader<'R,'T>)  = Reader (fun s -> Reader.run (body ()) s) : Reader<'R,'T>


#if !FABLE_COMPILER

/// Monad Transformer for Reader<'R, 'T>
[<Struct>]
type ReaderT<'r,'``monad<'t>``> = ReaderT of ('r -> '``monad<'t>``)

/// Basic operations on Reader
[<RequireQualifiedAccess>]
module ReaderT =
    let  run (ReaderT x) = x : 'R -> '``Monad<'T>``

    let inline hoist (x: Reader<'R, 'T>) = (ReaderT << (fun a -> result << a) << Reader.run) x : ReaderT<'R, '``Monad<'T>``>

    let inline map   (f: 'T->'U) (ReaderT m: ReaderT<'R, '``Monad<'T>``>) = ReaderT (map f << m)                                : ReaderT<'R, '``Monad<'U>``>

    /// Combines two ReaderTs into one by applying a mapping function.
    let inline map2 (f: 'T->'U->'V) (ReaderT x: ReaderT<'R,'``Monad<'T>``>) (ReaderT y: ReaderT<'R,'``Monad<'U>``>) = ReaderT (fun a -> lift2 f (x a) (y a)) : ReaderT<'R,'``Monad<'V>``>

    let inline apply (ReaderT (f: _ -> '``Monad<'T -> 'U>``)) (ReaderT (x: _->'``Monad<'T>``)) = ReaderT (fun r -> f r <*> x r) : ReaderT<'R, '``Monad<'U>``>

    /// Zips two ReaderTs into one.
    let inline zip (x: ReaderT<'S,'``Monad<'T>``>) (y: ReaderT<'S,'``Monad<'U>``>) = apply (map tuple2 x) y : ReaderT<'S,'``Monad<'T * 'U>``>

    let inline bind  (f: 'T->_) (ReaderT (m: _->'``Monad<'T>``)) = ReaderT (fun r -> m r >>= (fun a -> run (f a) r))            : ReaderT<'R, '``Monad<'U>``>

    /// Embed a Monad<'T> into an ReaderT<'R, 'Monad<'T>>
    let lift m = ReaderT (fun _ -> m)                                                                                           : ReaderT<'R, '``Monad<'T>``>

type ReaderT<'r,'``monad<'t>``> with

    static member inline Return (x: 'T) = ReaderT (fun _ -> result x)                                                 : ReaderT<'R, '``Monad<'T>``>

    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member inline Map   (x: ReaderT<'R, '``Monad<'T>``>, f: 'T->'U)                        = ReaderT.map   f x : ReaderT<'R, '``Monad<'U>``>

    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member inline Lift2 (f: 'T->'U->'V, x: ReaderT<'S,'``Monad<'T>``>, y: ReaderT<'S,'``Monad<'U>``>) : ReaderT<'S,'``Monad<'V>``> = ReaderT.map2 f x y

    static member inline (<*>) (f: ReaderT<_,'``Monad<'T -> 'U>``>, x: ReaderT<_,'``Monad<'T>``>) = ReaderT.apply f x : ReaderT<'R, '``Monad<'U>``>
    static member inline (>>=) (x: ReaderT<_,'``Monad<'T>``>, f: 'T->ReaderT<'R,'``Monad<'U>``>)  = ReaderT.bind  f x : ReaderT<'R, '``Monad<'U>``>
    
    static member inline get_Empty () = ReaderT (fun _ -> getEmpty ()) : ReaderT<'R, '``MonadPlus<'T>``>
    static member inline (<|>) (ReaderT m, ReaderT n) = ReaderT (fun r -> m r <|> n r) : ReaderT<'R, '``MonadPlus<'T>``>

    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member inline Zip (x: ReaderT<'S,'``Monad<'T>``>, y: ReaderT<'S,'``Monad<'U>``>) = ReaderT.zip x y

    static member inline TryWith (source: ReaderT<'R,'``Monad<'T>``>, f: exn -> ReaderT<'R,'``Monad<'T>``>) = ReaderT (fun s -> TryWith.InvokeForStrict (fun () -> ReaderT.run source s) (fun x -> ReaderT.run (f x) s))
    static member inline TryFinally (computation: ReaderT<'R,'``Monad<'T>``>, f) = ReaderT (fun s -> TryFinally.InvokeForStrict (fun () -> ReaderT.run computation s) f)
    static member inline Using (resource, f: _ -> ReaderT<'R,'``Monad<'T>``>)    = ReaderT (fun s -> Using.Invoke resource (fun x -> ReaderT.run (f x) s))
    static member inline Delay (body : unit   ->  ReaderT<'R,'``Monad<'T>``>)    = ReaderT (fun s -> Delay.Invoke (fun _ -> ReaderT.run (body ()) s))

    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member        Lift m = ReaderT (fun _ -> m)                                  : ReaderT<'R,'``Monad<'T>``>

    static member inline LiftAsync (x: Async<'T>) = (ReaderT.lift (liftAsync x)         : ReaderT<'R,'``MonadAsync<'T>``>)

    static member inline CallCC (f: ('T -> ReaderT<'R, Cont<_,'U>>) -> _)               : ReaderT<'R,'``MonadCont<'C,'T>``> =
        ReaderT (fun r -> callCC <| fun c -> ReaderT.run (f (fun a -> ReaderT <| fun _ -> c a)) r)
          
    static member inline get_Ask () = ReaderT result                                    : ReaderT<'R,'``Monad<'T>``>
    static member        Local (ReaderT m, f: _->'R2) = ReaderT (fun r -> m (f r))      : ReaderT<'R1,'``Monad<'T>``>

    static member inline Throw (x: 'E) = x |> throw |> ReaderT.lift                     : ReaderT<'R,'``MonadError<'E,'T>``>
    static member inline Catch (m: ReaderT<'R,'``MonadError<'E1,'T>``>, h: 'E1 -> _) =
        ReaderT (fun s -> catch (ReaderT.run m s)   (fun e -> ReaderT.run (h e) s))     : ReaderT<'R,'``MonadError<'E2,'T>``>

    static member inline Tell   w           = w |> tell |> ReaderT.lift                 : ReaderT<'R, '``MonadWriter<'Monoid,unit>``>
    static member inline Listen (ReaderT m) = ReaderT (fun w -> listen (m w))           : ReaderT<'R, '``MonadWriter<'Monoid,'T*'Monoid>``>
    static member inline Pass   (ReaderT m) = ReaderT (fun w -> pass   (m w))           : ReaderT<'R, '``MonadWriter<'Monoid,'T>``>   

    static member inline get_Get () = ReaderT.lift get         : ReaderT<'R, '``MonadState<'S, 'S>``>
    static member inline Put x      = x |> put |> ReaderT.lift : ReaderT<'R, '``MonadState<'S, unit>``>

#endif
