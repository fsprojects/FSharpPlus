namespace FSharpPlus

/// <summary> Computation type: Computations which read values from a shared environment.
/// <para/>   Binding strategy: Monad values are functions from the environment to a value. The bound function is applied to the bound value, and both have access to the shared environment.
/// <para/>   Useful for: Maintaining variable bindings, or other shared environment.</summary>
type Reader<'r,'t> = Reader of ('r->'t)

[<RequireQualifiedAccess>]
module Reader =
    let run (Reader x) = x                                                  : 'R->'T
    let map   (f:'T->_ ) (Reader m) = Reader (f << m)                       : Reader<'R,'U>
    let bind  (f:'T->_ ) (Reader m) = Reader (fun r -> run (f (m r)) r)     : Reader<'R,'U>
    let apply (Reader f) (Reader x) = Reader (fun a -> f a ((x:_->'T) a))   : Reader<'R,'U>

    /// Retrieves the monad environment.
    let ask = Reader id                                                     : Reader<'R,'R>

    /// <summary> Executes a computation in a modified environment. </summary>
    /// <param name="f"> The function to modify the environment.    </param>
    /// <param name="m"> Reader to run in the modified environment. </param>
    let local (f:'R1->'R2) m = let (Reader m) = m in Reader (m << f)        : Reader<'R1,'T>

type Reader with
    static member Map   (x:Reader<'R,'T>, f) = Reader.map f x   : Reader<'R,'U>
    static member Return x = Reader (fun _ -> x)                : Reader<'R,'T>
    static member Bind  (x:Reader<'R,'T>, f) = Reader.bind f x  : Reader<'R,'U>
    static member (<*>) (f, x:Reader<'R,'T>) = Reader.apply f x : Reader<'R,'U>
    static member get_Ask()    = Reader.ask                     : Reader<'R,'R>
    static member Local (m, f:'R1->'R2) = Reader.local f m      : Reader<'R1,'T>

open FsControl

type ReaderT<'r,'``monad<'t>``> = ReaderT of ('r -> '``monad<'t>``)

[<RequireQualifiedAccess>]
module ReaderT =
    let  run (ReaderT x) = x    : 'R -> '``Monad<'T>``
    let inline map   (f:'T->'U) (ReaderT m : ReaderT<'R, '``Monad<'T>``>) = ReaderT (map f << m)                                    : ReaderT<'R, '``Monad<'U>``>
    let inline apply (ReaderT (f: _ -> '``Monad<'T -> 'U>``)) (ReaderT (x:_->'``Monad<'T>``)) = ReaderT (fun r -> f r <*> x r)      : ReaderT<'R, '``Monad<'U>``>
    let inline bind  (f:'T->_) (ReaderT (m:_->'``Monad<'T>``)) = ReaderT (fun r -> m r >>= (fun a -> run (f a) r))                  : ReaderT<'R, '``Monad<'U>``>

type ReaderT with
    static member inline Return (x : 'T) = ReaderT (fun _ -> result x)                                                      : ReaderT<'R, '``Monad<'T>``> 
    static member inline Map    (x : ReaderT<'R, '``Monad<'T>``>, f : 'T->'U)                        = ReaderT.map f x      : ReaderT<'R, '``Monad<'U>``>
    static member inline Apply  (f : ReaderT<_,'``Monad<'T -> 'U>``>, x : ReaderT<_,'``Monad<'T>``>) = ReaderT.apply f x    : ReaderT<'R, '``Monad<'U>``>
    static member inline Bind   (x : ReaderT<_,'``Monad<'T>``>, f : 'T->ReaderT<'R,'``Monad<'U>``>)  = ReaderT.bind f x     : ReaderT<'R, '``Monad<'U>``>
    
    static member inline MZero (output: ReaderT<'R, '``MonadPlus<'T>``>, impl:MZero) = ReaderT (fun _ -> getMZero())        : ReaderT<'R, '``MonadPlus<'T>``>
    static member inline MPlus (ReaderT m, ReaderT n, impl:MPlus)                    = ReaderT (fun r -> m r <|> n r)       : ReaderT<'R, '``MonadPlus<'T>``>

    static member        Lift m = ReaderT (fun _ -> m)                                      : ReaderT<'R,'``Monad<'T>``>

    static member inline LiftAsync (x: Async<'T>) = (lift (liftAsync x)                     : ReaderT<'R,'``MonadAsync<'T>``>)

    static member inline CallCC (f : ('T -> ReaderT<'R, Cont<_,'U>>) -> _)                  : ReaderT<'R,'``MonadCont<'C,'T>``> =
        ReaderT (fun r -> callCC <| fun c -> ReaderT.run (f (fun a -> ReaderT <| fun _ -> c a)) r)
            
    static member inline get_Ask() = ReaderT result                                         : ReaderT<'R,'``Monad<'T>``>
    static member        Local (ReaderT m, f:_->'R2) = ReaderT(fun r -> m (f r))            : ReaderT<'R1,'``Monad<'T>``>

    static member inline Throw (x:'E) = x |> throw |> lift : ReaderT<'R,'``MonadError<'E,'T>``>
    static member inline Catch (m:ReaderT<'R,'``MonadError<'E1,'T>``>, h:'E1 -> _) = 
        ReaderT (fun s -> catch (ReaderT.run m s)   (fun e -> ReaderT.run (h e) s))         : ReaderT<'R,'``MonadError<'E2,'T>``>

    static member inline Tell   w           = w |> tell |> lift                             : ReaderT<'R, '``MonadWriter<'Monoid,unit>``>
    static member inline Listen (ReaderT m) = ReaderT (fun w -> listen (m w))               : ReaderT<'R, '``MonadWriter<'Monoid,'T*'Monoid>``>
    static member inline Pass   (ReaderT m) = ReaderT (fun w -> pass   (m w))               : ReaderT<'R, '``MonadWriter<'Monoid,'T>``>   

    static member inline get_Get() = lift get         :ReaderT<'R, '``MonadState<'S, 'S>``>
    static member inline Put x     = x |> put |> lift :ReaderT<'R, '``MonadState<'S, unit>``>