namespace FSharpPlus.Data

open System.ComponentModel
open FSharpPlus
open FSharpPlus.Internals.Prelude
open FSharpPlus.Control


#if !FABLE_COMPILER || FABLE_COMPILER_3

/// Additional operations on Option
[<RequireQualifiedAccess>]
module Option =
    let inline traverse f = function Some x -> Map.Invoke Some (f x) | _ -> result None

/// Monad Transformer for Option<'T>
[<Struct>]
type OptionT<'``monad<option<'t>>``> = OptionT of '``monad<option<'t>>``

/// Basic operations on OptionT
[<RequireQualifiedAccess>]
module OptionT =
    let run   (OptionT m) = m : '``Monad<option<'T>>``

    /// Embed a Monad<'T> into an OptionT<'Monad<option<'T>>>
    let inline lift (x: '``Monad<'T>``) : OptionT<'``Monad<option<'T>>``> =
        if opaqueId false then x |> liftM Some |> OptionT
        else x |> map Some |> OptionT

    /// Transform an option<'T,'Error> to an OptionT<'Monad<option<'T,'Error>>>
    let inline hoist (x: option<'T>) = OptionT (result x) : OptionT<'``Monad<option<'T>>``>

    let inline bind (f: 'T-> OptionT<'``Monad<option<'U>``>) (OptionT m: OptionT<'``Monad<option<'T>``>)             = OptionT <| (m >>= (fun maybe_value -> match maybe_value with Some value -> run (f value) | _ -> result None))
    let inline apply (OptionT f: OptionT<'``Monad<option<('T -> 'U)>``>) (OptionT x: OptionT<'``Monad<option<'T>``>) = OptionT (map Option.apply f <*> x) : OptionT<'``Monad<option<'U>``>    
    let inline map  (f: 'T->'U) (OptionT m: OptionT<'``Monad<option<'T>``>)                                          = OptionT (map (Option.map f) m) : OptionT<'``Monad<option<'U>``>
    let inline map2 (f: 'T->'U->'V) (OptionT x: OptionT<'``Monad<option<'T>>``>) (OptionT y: OptionT<'``Monad<option<'U>>``>) = OptionT (lift2 (Option.map2 f) x y) : OptionT<'``Monad<option<'V>>``>
    let inline map3 (f: 'T->'U->'V->'W) (OptionT x: OptionT<'``Monad<option<'T>>``>) (OptionT y: OptionT<'``Monad<option<'U>>``>) (OptionT z: OptionT<'``Monad<option<'V>>``>) = OptionT (lift3 (Option.map3 f) x y z) : OptionT<'``Monad<option<'W>>``>

type OptionT<'``monad<option<'t>>``> with
    
    static member inline Return (x: 'T) = Some x |> result |> OptionT                                                        : OptionT<'``Monad<option<'T>``>

    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member inline Map    (x: OptionT<'``Monad<option<'T>``>, f: 'T->'U) = OptionT.map f x                                : OptionT<'``Monad<option<'U>``>

    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member inline Lift2 (f: 'T->'U->'V, x: OptionT<'``Monad<option<'T>``>, y: OptionT<'``Monad<option<'U>``>) = OptionT.map2 f x y  : OptionT<'``Monad<option<'V>``>

    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member inline Lift3 (f: 'T->'U->'V->'W, x: OptionT<'``Monad<option<'T>``>, y: OptionT<'``Monad<option<'U>``>, z: OptionT<'``Monad<option<'W>``>) = OptionT.map3 f x y z : OptionT<'``Monad<option<'W>``>

    static member inline (<*>) (f: OptionT<'``Monad<option<('T -> 'U)>``>, x: OptionT<'``Monad<option<'T>``>) = OptionT.apply f x : OptionT<'``Monad<option<'U>``>
    static member inline (>>=) (x: OptionT<'``Monad<option<'T>``>, f: 'T -> OptionT<'``Monad<option<'U>``>)   = OptionT.bind  f x

    /// <summary>
    /// Composes left-to-right two Option functions (Kleisli composition).
    /// </summary>
    /// <category index="2">Monad</category>
    static member inline (>=>) (f: 'T -> OptionT<'``Monad<option<'U>``>, g: 'U -> OptionT<'``Monad<option<'V>``>) : 'T -> OptionT<'``Monad<option<'V>``> = fun x -> OptionT.bind g (f x)

    static member inline get_Zero () : OptionT<'``MonadPlus<option<'T>``> = OptionT <| result None
    static member inline (+) (OptionT x, OptionT y) : OptionT<'``MonadPlus<option<'T>``> =
        OptionT <| (x >>= function
            | None -> y
            | Some x -> y >>= function
                | None -> result (Some x)
                | Some y -> result (Some (x ++ y)))
    
    static member inline get_Empty () : OptionT<'``MonadPlus<option<'T>``> = OptionT <| result None
    static member inline (<|>) (OptionT x, OptionT y) : OptionT<'``MonadPlus<option<'T>``> = OptionT <| (x >>= function Some value -> result (Some value) | _ -> y)

    static member inline TryWith (source: unit -> OptionT<'``Monad<option<'T>>``>, f: exn -> OptionT<'``Monad<option<'T>>``>) = OptionT (TryWith.Invoke  (fun () -> OptionT.run (source ())) (OptionT.run << f))
    static member inline TryFinally (computation: unit -> OptionT<'``Monad<option<'T>>``>, f) = OptionT (TryFinally.Invoke (fun () -> OptionT.run (computation ())) f)
    static member inline Using (resource, f: _ -> OptionT<'``Monad<option<'T>>``>)    = OptionT (Using.Invoke resource (OptionT.run << f))
    static member inline Delay (body : unit   ->  OptionT<'``Monad<option<'T>>``>)    = OptionT (Delay.Invoke (fun _ -> OptionT.run (body ()))) : OptionT<'``Monad<option<'T>>``>

    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member inline Lift (x: '``Monad<'T>``) : OptionT<'``Monad<option<'T>>``> = OptionT.lift x

    static member inline LiftAsync (x : Async<'T>) = OptionT.lift (liftAsync x) : OptionT<'``MonadAsync<'T>``>

    static member inline Throw (x: 'E) = x |> throw |> OptionT.lift
    static member inline Catch (m: OptionT<'``MonadError<'E1,'T>``>, h: 'E1 -> OptionT<'``MonadError<'E2,'T>``>) = OptionT ((fun v h -> catch v h) (OptionT.run m) (OptionT.run << h)) : OptionT<'``MonadError<'E2,'T>``>

    static member inline CallCC (f: (('T -> OptionT<'``MonadCont<'R,option<'U>>``>) -> _)) = OptionT (callCC <| fun c -> OptionT.run (f (OptionT << c << Some))) : OptionT<'``MonadCont<'R,option<'T>>``>

    static member inline get_Get () = OptionT.lift get           : OptionT<'``MonadState<'S,'S>``>
    static member inline Put (x: 'S) = x |> put |> OptionT.lift  : OptionT<'``MonadState<unit,'S>``>

    static member inline get_Ask () = OptionT.lift ask           : OptionT<'``MonadReader<'R,option<'R>>``>
    static member inline Local (OptionT (m: '``MonadReader<'R2,'T>``), f: 'R1->'R2) = OptionT (local f m)

    static member inline Tell (w: 'Monoid) = w |> tell |> OptionT.lift : OptionT<'``MonadWriter<'Monoid, unit>``>

    static member inline Listen m                              : OptionT<'``'MonadWriter<'Monoid, option<'T>>``> =
        let liftMaybe (m, w) = Option.map (fun x -> (x, w)) m
        OptionT (listen (OptionT.run m) >>= (result << liftMaybe))

    static member inline Pass m : OptionT<'``MonadWriter<'Monoid, option<'T>>``> = OptionT (OptionT.run m >>= option (map Some << pass << result) (result None))

#endif
