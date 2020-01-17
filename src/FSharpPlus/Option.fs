namespace FSharpPlus.Data

open FSharpPlus.Control
open FSharpPlus
open System.ComponentModel

#if !FABLE_COMPILER
/// Additional operations on Option
[<RequireQualifiedAccess>]
module Option =
    let inline traverse f = function Some x -> Map.Invoke Some (f x) | _ -> result None   
#endif

/// Monad Transformer for Option<'T>
[<Struct>]
type OptionT<'``monad<option<'t>>``> = OptionT of '``monad<option<'t>>``

/// Basic operations on OptionT
[<RequireQualifiedAccess>]
module OptionT =
    let run   (OptionT m) = m : '``Monad<option<'T>>``

    #if !FABLE_COMPILER

    /// Embed a Monad<'T> into an OptionT<'Monad<option<'T>>>
    let inline lift (x: '``Monad<'T>``) : OptionT<'``Monad<option<'T>>``> =
        if FSharpPlus.Internals.Helpers.alwaysFalse<bool> then x |> liftM Some |> OptionT
        else x |> map Some |> OptionT

    /// Transform an option<'T,'Error> to an OptionT<'Monad<option<'T,'Error>>>
    let inline hoist (x: option<'T>) = OptionT (result x) : OptionT<'``Monad<option<'T>>``>

    let inline bind (f: 'T-> OptionT<'``Monad<option<'U>``>) (OptionT m: OptionT<'``Monad<option<'T>``>)             = (OptionT <| (m  >>= (fun maybe_value -> match maybe_value with Some value -> run (f value) | _ -> result None)))
    let inline apply (OptionT f: OptionT<'``Monad<option<('T -> 'U)>``>) (OptionT x: OptionT<'``Monad<option<'T>``>) = OptionT (map Option.apply f <*> x) : OptionT<'``Monad<option<'U>``>
    let inline map  (f: 'T->'U) (OptionT m: OptionT<'``Monad<option<'T>``>)                                          = OptionT (map (Option.map f) m) : OptionT<'``Monad<option<'U>``>
    #endif

type OptionT<'``monad<option<'t>>``> with
    #if !FABLE_COMPILER
    static member inline Return (x: 'T) = Some x |> result |> OptionT                                                        : OptionT<'``Monad<seq<'T>``>

    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member inline Map    (x: OptionT<'``Monad<seq<'T>``>, f: 'T->'U) = OptionT.map f x                                : OptionT<'``Monad<seq<'U>``>

    static member inline (<*>)  (f: OptionT<'``Monad<seq<('T -> 'U)>``>, x: OptionT<'``Monad<seq<'T>``>) = OptionT.apply f x : OptionT<'``Monad<seq<'U>``>
    static member inline (>>=)  (x: OptionT<'``Monad<seq<'T>``>, f: 'T -> OptionT<'``Monad<seq<'U>``>)   = OptionT.bind  f x

    static member inline get_Empty () = OptionT <| result None : OptionT<'``MonadPlus<option<'T>``>
    static member inline (<|>) (OptionT x, OptionT y) = OptionT <| (x  >>= (fun maybe_value -> match maybe_value with Some value -> result (Some value) | _ -> y)) : OptionT<'``MonadPlus<option<'T>``>
    #endif

    static member inline TryWith (source: OptionT<'``Monad<option<'T>>``>, f: exn -> OptionT<'``Monad<option<'T>>``>) = OptionT (TryWith.Invoke (OptionT.run source) (OptionT.run << f))
    static member inline TryFinally (computation: OptionT<'``Monad<option<'T>>``>, f) = OptionT (TryFinally.Invoke     (OptionT.run computation) f)
    static member inline Using (resource, f: _ -> OptionT<'``Monad<option<'T>>``>)    = OptionT (Using.Invoke resource (OptionT.run << f))
    static member inline Delay (body : unit   ->  OptionT<'``Monad<option<'T>>``>)    = OptionT (Delay.Invoke (fun _ -> OptionT.run (body ()))) : OptionT<'``Monad<option<'T>>``>

    #if !FABLE_COMPILER
    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member inline Lift (x: '``Monad<'T>``) : OptionT<'``Monad<option<'T>>``> = OptionT.lift x
    #endif

    static member inline LiftAsync (x : Async<'T>) = OptionT.lift (liftAsync x) : OptionT<'``MonadAsync<'T>``>

    static member inline Throw (x: 'E) = x |> throw |> OptionT.lift
    static member inline Catch (m: OptionT<'``MonadError<'E1,'T>``>, h: 'E1 -> OptionT<'``MonadError<'E2,'T>``>) = OptionT ((fun v h -> catch v h) (OptionT.run m) (OptionT.run << h)) : OptionT<'``MonadError<'E2,'T>``>

    static member inline CallCC (f: (('T -> OptionT<'``MonadCont<'R,option<'U>>``>) -> _)) = OptionT (callCC <| fun c -> OptionT.run (f (OptionT << c << Some))) : OptionT<'``MonadCont<'R,option<'T>>``>

    static member inline get_Get () = OptionT.lift get           : OptionT<'``MonadState<'S,'S>``>
    static member inline Put (x: 'T) = x |> put |> OptionT.lift  : OptionT<'``MonadState<unit,'S>``>

    static member inline get_Ask () = OptionT.lift ask           : OptionT<'``MonadReader<'R,option<'R>>``>
    static member inline Local (OptionT (m: '``MonadReader<'R2,'T>``), f: 'R1->'R2) = OptionT (local f m)

    static member inline Tell (w: 'Monoid) = w |> tell |> OptionT.lift : OptionT<'``MonadWriter<'Monoid, unit>``>

    #if !FABLE_COMPILER
    static member inline Listen m                              : OptionT<'``'MonadWriter<'Monoid, option<'T>>``> =
        let liftMaybe (m, w) = Option.map (fun x -> (x, w)) m
        OptionT (listen (OptionT.run m) >>= (result << liftMaybe))

    static member inline Pass m : OptionT<'``MonadWriter<'Monoid, option<'T>>``> = OptionT (OptionT.run m >>= option (map Some << pass << result) (result None))
    #endif