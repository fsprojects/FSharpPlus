namespace FSharpPlus

open FsControl

[<RequireQualifiedAccess>]
module internal Option =
    let inline apply f x =
        match (f,x) with 
        | Some f, Some x -> Some (f x) 
        | _              -> None

open FsControl

type OptionT<'``monad<option<'t>>``> = OptionT of '``monad<option<'t>>``

[<RequireQualifiedAccess>]
module OptionT =
    let run   (OptionT m) = m : '``Monad<option<'T>>``
    let inline bind (f:'T-> OptionT<'``Monad<option<'U>``>) (OptionT m : OptionT<'``Monad<option<'T>``>)               = (OptionT <| (m  >>= (fun maybe_value -> match maybe_value with Some value -> run (f value) | _ -> result None)))
    let inline apply (OptionT f : OptionT<'``Monad<option<('T -> 'U)>``>) (OptionT x : OptionT<'``Monad<option<'T>``>) = OptionT (map Option.apply f <*> x)  : OptionT<'``Monad<option<'U>``>
    let inline map  (f:'T->'U) (OptionT m : OptionT<'``Monad<option<'T>``>)                                            = OptionT (map (Option.map f) m) : OptionT<'``Monad<option<'U>``>

type OptionT with
    static member inline Map    (x : OptionT<'``Monad<option<'T>``>, f : 'T->'U , impl:Map)                                                       = OptionT.map f x           : OptionT<'``Monad<option<'U>``>
    static member inline Return (output : OptionT<'``Monad<option<'T>``>, impl:Return)                                                            = OptionT << result << Some : 'T -> OptionT<'``Monad<option<'T>``>
    static member inline Apply  (f : OptionT<'``Monad<option<('T -> 'U)>``>, x : OptionT<'``Monad<option<'T>``>, output:OptionT<'r>, impl:Apply ) = OptionT.apply f x         : OptionT<'``Monad<option<'U>``>
    static member inline Bind   (x  : OptionT<'``Monad<option<'T>``>, f: 'T -> OptionT<'``Monad<option<'U>``>)                                    = OptionT.bind f x

    static member inline MZero (output: OptionT<'``MonadPlus<option<'T>``>, impl:MZero) = OptionT <| result None                                                                   : OptionT<'``MonadPlus<option<'T>``>
    static member inline MPlus (OptionT x, OptionT y, impl:MPlus)                       = OptionT <| (x  >>= (fun maybe_value -> match maybe_value with Some value -> x | _ -> y)) : OptionT<'``MonadPlus<option<'T>``>

    static member inline Lift (x:'``Monad<'T>``) = x |> liftM Some |> OptionT : OptionT<'``Monad<option<'T>>``>

    static member inline LiftAsync (x : Async<'T>) = lift (liftAsync x)  : '``OptionT<'MonadAsync<'T>>``

    static member inline Throw (x:'E) = x |> throw |> lift
    static member inline Catch (m:OptionT<'``MonadError<'E1,'T>``>, h:'E1 -> OptionT<'``MonadError<'E2,'T>``>) = OptionT ((fun v h -> catch v h) (OptionT.run m) (OptionT.run << h)) : OptionT<'``MonadError<'E2,'T>``>

    static member inline CallCC (f:(('T -> OptionT<'``MonadCont<'R,option<'U>>``>) -> _)) = OptionT(callCC <| fun c -> OptionT.run(f (OptionT << c << Some)))  : OptionT<'``MonadCont<'R,option<'T>>``>

    static member inline get_Get() = lift get           : '``OptionT<'MonadState<'S,'S>>``
    static member inline Put (x:'T) = x |> put |> lift  : '``OptionT<'MonadState<unit,'S>>``

    static member inline get_Ask() = lift ask                                                   :'``OptionT<'MonadReader<'R,option<'R>>>``
    static member inline Local (OptionT (m:'``MonadReader<'R2,'T>``), f:'R1->'R2) = OptionT (local f m)

    static member inline Tell (w:'Monoid) = w |> tell |> lift        : '``OptionT<'MonadWriter<'Monoid, unit>>``
    static member inline Listen (m )                                 : OptionT<'``'MonadWriter<'Monoid, option<'T>>``> =
        let liftMaybe (m, w) = Option.map (fun x -> (x, w)) m
        OptionT (listen (OptionT.run m) >>= (result << liftMaybe))
    static member inline Pass m : OptionT<'``MonadWriter<'Monoid, option<'T>>``> = OptionT (OptionT.run m >>= option (result None) (map Some << pass << result))