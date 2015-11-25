namespace FSharpPlus

open FsControl

type OptionT<'``monad<option<'t>>``> = OptionT of '``monad<option<'t>>``

[<RequireQualifiedAccess>]
module OptionT =
    let run   (OptionT m) = m : '``Monad<option<'T>>``
    let inline map  (f:'T->'U) (OptionT m : OptionT<'``Monad<option<'T>``>)                                            = OptionT (map (Option.map f) m) : OptionT<'``Monad<option<'U>``>
    let inline bind (f:'T-> OptionT<'``Monad<option<'U>``>) (OptionT m : OptionT<'``Monad<option<'T>``>)               = (OptionT <| (m  >>= (fun maybe_value -> match maybe_value with Some value -> run (f value) | _ -> result None)))
    let inline apply (OptionT f : OptionT<'``Monad<option<('T -> 'U)>``>) (OptionT x : OptionT<'``Monad<option<'T>``>) = OptionT (Map.Invoke Option.apply f <*> x)  : OptionT<'``Monad<option<'U>``>

type OptionT with
    static member inline Map    (x : OptionT<'``Monad<option<'T>``>, f : 'T->'U , impl:Map)                                                       = OptionT.map f x                                                                             : OptionT<'``Monad<option<'U>``>
    static member inline Return (output : OptionT<'``Monad<option<'T>``>, impl:Return)                                                            = OptionT << result << Some                                                                   : 'T -> OptionT<'``Monad<option<'T>``>
    static member inline Apply  (f : OptionT<'``Monad<option<('T -> 'U)>``>, x : OptionT<'``Monad<option<'T>``>, output:OptionT<'r>, impl:Apply ) = OptionT.apply f x                                                                           : OptionT<'``Monad<option<'U>``>
    static member inline Bind   (x  : OptionT<'``Monad<option<'T>``>, f: 'T -> OptionT<'``Monad<option<'U>``>)                                    = OptionT.bind f x

    static member inline MZero (output: OptionT<'``MonadPlus<option<'T>``>, impl:MZero)                                                           = OptionT <| result None                                                                      : OptionT<'``MonadPlus<option<'T>``>
    static member inline MPlus (OptionT x, OptionT y, impl:MPlus)                                                                                 = OptionT <| (x  >>= (fun maybe_value -> match maybe_value with Some value -> x | _ -> y))    : OptionT<'``MonadPlus<option<'T>``>

    static member inline Lift (x:'``Monad<'T>``) = x |> (Map.FromMonad Some)           |> OptionT : OptionT<'``Monad<option<'T>>``>

    static member inline LiftAsync (x : Async<'T>) = lift (liftAsync x)

    static member inline ThrowError (x:'E) = x |> throw |> lift
    static member inline CatchError (m:OptionT<'``MonadError<'E1,'T>``>, h:'E1 -> OptionT<'``MonadError<'E2,'T>``>) = OptionT ((fun v h -> catch v h) (OptionT.run m) (OptionT.run << h)) : OptionT<'``MonadError<'E2,'T>``>

    static member inline CallCC (f:(('T -> OptionT<Cont<'R,'U>>) -> _)) = OptionT(callCC <| fun c -> OptionT.run(f (OptionT << c << Some)))      : OptionT<'``MonadCont<'R,option<'T>>``>

    static member inline get_Get() = lift get
    static member inline Put (x:'T) = x |> put |> lift

    static member inline get_Ask() = lift ask
    static member inline Local (OptionT (m:Reader<'R2,'T>), f:'R1->'R2) = OptionT (local f m)

    static member inline Tell (w:'Monoid) = w |> tell |> lift
    static member inline Listen (m ) =
        let liftMaybe (m, w) = Option.map (fun x -> (x, w)) m
        OptionT (listen (OptionT.run m) >>= (result << liftMaybe))
    static member inline Pass m : OptionT<'``MonadWriter<'Monoid, option<'T>>``> = OptionT (OptionT.run m >>= option (result None) (map Some << pass << result))

