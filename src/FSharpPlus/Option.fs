namespace FSharpPlus.Data

open FsControl
open FSharpPlus

/// Additional operations on Option
[<RequireQualifiedAccess>]
module Option =
    let inline traverse f = function Some x -> Map.Invoke Some (f x) | _ -> result None   


/// Monad Transformer for Option<'T>
type OptionT<'``monad<option<'t>>``> = OptionT of '``monad<option<'t>>``

/// Basic operations on OptionT
[<RequireQualifiedAccess>]
module OptionT =
    let run   (OptionT m) = m : '``Monad<option<'T>>``
    let inline bind (f:'T-> OptionT<'``Monad<option<'U>``>) (OptionT m : OptionT<'``Monad<option<'T>``>)               = (OptionT <| (m  >>= (fun maybe_value -> match maybe_value with Some value -> run (f value) | _ -> result None)))
    let inline apply (OptionT f : OptionT<'``Monad<option<('T -> 'U)>``>) (OptionT x : OptionT<'``Monad<option<'T>``>) = OptionT (map Option.apply f <*> x)  : OptionT<'``Monad<option<'U>``>
    let inline map  (f:'T->'U) (OptionT m : OptionT<'``Monad<option<'T>``>)                                            = OptionT (map (Option.map f) m) : OptionT<'``Monad<option<'U>``>

type OptionT<'``monad<option<'t>>``> with
    static member inline Return (x : 'T) = Some x |> result |> OptionT                                                          : OptionT<'``Monad<seq<'T>``>
    static member inline Map    (x : OptionT<'``Monad<seq<'T>``>, f : 'T->'U) = OptionT.map f x                                 : OptionT<'``Monad<seq<'U>``>
    static member inline (<*>)  (f : OptionT<'``Monad<seq<('T -> 'U)>``>, x : OptionT<'``Monad<seq<'T>``>) = OptionT.apply f x  : OptionT<'``Monad<seq<'U>``>
    static member inline Bind   (x : OptionT<'``Monad<seq<'T>``>, f : 'T -> OptionT<'``Monad<seq<'U>``>)   = OptionT.bind f x

    static member inline get_Empty () = OptionT <| result None : OptionT<'``MonadPlus<option<'T>``>
    static member inline Append (OptionT x, OptionT y) = OptionT <| (x  >>= (fun maybe_value -> match maybe_value with Some _ -> x | _ -> y)) : OptionT<'``MonadPlus<option<'T>``>

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