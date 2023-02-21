namespace FSharpPlus.Data

open System.ComponentModel
open FSharpPlus
open FSharpPlus.Internals.Prelude
open FSharpPlus.Control


#if !FABLE_COMPILER || FABLE_COMPILER_3

/// Additional operations on ValueOption
[<RequireQualifiedAccess>]
module ValueOption =
    let inline traverse f = function ValueSome x -> Map.Invoke ValueSome (f x) | _ -> result ValueNone

/// Monad Transformer for ValueOption<'T>
[<Struct>]
type ValueOptionT<'``monad<voption<'t>>``> = ValueOptionT of '``monad<voption<'t>>``

/// Basic operations on ValueOptionT
[<RequireQualifiedAccess>]
module ValueOptionT =
    let run   (ValueOptionT m) = m : '``Monad<voption<'T>>``

    /// Embed a Monad<'T> into an ValueOptionT<'Monad<voption<'T>>>
    let inline lift (x: '``Monad<'T>``) : ValueOptionT<'``Monad<voption<'T>>``> =
        if opaqueId false then x |> liftM ValueSome |> ValueOptionT
        else x |> map ValueSome |> ValueOptionT

    /// Transform an voption<'T,'Error> to an ValueOptionT<'Monad<voption<'T,'Error>>>
    let inline hoist (x: voption<'T>) = ValueOptionT (result x) : ValueOptionT<'``Monad<voption<'T>>``>

    let inline bind (f: 'T-> ValueOptionT<'``Monad<voption<'U>``>) (ValueOptionT m: ValueOptionT<'``Monad<voption<'T>``>)             = ValueOptionT <| (m >>= (fun maybe_value -> match maybe_value with ValueSome value -> run (f value) | _ -> result ValueNone))
    let inline apply (ValueOptionT f: ValueOptionT<'``Monad<voption<('T -> 'U)>``>) (ValueOptionT x: ValueOptionT<'``Monad<voption<'T>``>) = ValueOptionT (map ValueOption.apply f <*> x) : ValueOptionT<'``Monad<voption<'U>``>    
    let inline map  (f: 'T->'U) (ValueOptionT m: ValueOptionT<'``Monad<voption<'T>``>)                                          = ValueOptionT (map (ValueOption.map f) m) : ValueOptionT<'``Monad<voption<'U>``>
    let inline map2 (f: 'T->'U->'V) (ValueOptionT x: ValueOptionT<'``Monad<voption<'T>>``>) (ValueOptionT y: ValueOptionT<'``Monad<voption<'U>>``>) = ValueOptionT (lift2 (ValueOption.map2 f) x y) : ValueOptionT<'``Monad<voption<'V>>``>
    let inline map3 (f: 'T->'U->'V->'W) (ValueOptionT x: ValueOptionT<'``Monad<voption<'T>>``>) (ValueOptionT y: ValueOptionT<'``Monad<voption<'U>>``>) (ValueOptionT z: ValueOptionT<'``Monad<voption<'V>>``>) = ValueOptionT (lift3 (ValueOption.map3 f) x y z) : ValueOptionT<'``Monad<voption<'W>>``>

type ValueOptionT<'``monad<voption<'t>>``> with
    
    static member inline Return (x: 'T) = ValueSome x |> result |> ValueOptionT                                                        : ValueOptionT<'``Monad<voption<'T>``>

    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member inline Map    (x: ValueOptionT<'``Monad<voption<'T>``>, f: 'T->'U) = ValueOptionT.map f x                                : ValueOptionT<'``Monad<voption<'U>``>

    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member inline Lift2 (f: 'T->'U->'V, x: ValueOptionT<'``Monad<voption<'T>``>, y: ValueOptionT<'``Monad<voption<'U>``>) = ValueOptionT.map2 f x y  : ValueOptionT<'``Monad<voption<'V>``>

    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member inline Lift3 (f: 'T->'U->'V->'W, x: ValueOptionT<'``Monad<voption<'T>``>, y: ValueOptionT<'``Monad<voption<'U>``>, z: ValueOptionT<'``Monad<voption<'W>``>) = ValueOptionT.map3 f x y z : ValueOptionT<'``Monad<voption<'W>``>

    static member inline (<*>) (f: ValueOptionT<'``Monad<voption<('T -> 'U)>``>, x: ValueOptionT<'``Monad<voption<'T>``>) = ValueOptionT.apply f x : ValueOptionT<'``Monad<voption<'U>``>
    static member inline (>>=) (x: ValueOptionT<'``Monad<voption<'T>``>, f: 'T -> ValueOptionT<'``Monad<voption<'U>``>)   = ValueOptionT.bind  f x

    static member inline get_Zero () : ValueOptionT<'``MonadPlus<voption<'T>``> = ValueOptionT <| result ValueNone
    static member inline (+) (ValueOptionT x, ValueOptionT y) : ValueOptionT<'``MonadPlus<voption<'T>``> =
        ValueOptionT <| (x >>= function
            | ValueNone -> y
            | ValueSome x -> y >>= function
                | ValueNone -> result (ValueSome x)
                | ValueSome y -> result (ValueSome (x ++ y)))
    
    static member inline get_Empty () : ValueOptionT<'``MonadPlus<voption<'T>``> = ValueOptionT <| result ValueNone
    static member inline (<|>) (ValueOptionT x, ValueOptionT y) : ValueOptionT<'``MonadPlus<voption<'T>``> = ValueOptionT <| (x >>= function ValueSome value -> result (ValueSome value) | _ -> y)

    static member inline TryWith (source: ValueOptionT<'``Monad<voption<'T>>``>, f: exn -> ValueOptionT<'``Monad<voption<'T>>``>) = ValueOptionT (TryWith.Invoke (ValueOptionT.run source) (ValueOptionT.run << f))
    static member inline TryFinally (computation: ValueOptionT<'``Monad<voption<'T>>``>, f) = ValueOptionT (TryFinally.Invoke     (ValueOptionT.run computation) f)
    static member inline Using (resource, f: _ -> ValueOptionT<'``Monad<voption<'T>>``>)    = ValueOptionT (Using.Invoke resource (ValueOptionT.run << f))
    static member inline Delay (body : unit   ->  ValueOptionT<'``Monad<voption<'T>>``>)    = ValueOptionT (Delay.Invoke (fun _ -> ValueOptionT.run (body ()))) : ValueOptionT<'``Monad<voption<'T>>``>

    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member inline Lift (x: '``Monad<'T>``) : ValueOptionT<'``Monad<voption<'T>>``> = ValueOptionT.lift x

    static member inline LiftAsync (x : Async<'T>) = ValueOptionT.lift (liftAsync x) : ValueOptionT<'``MonadAsync<'T>``>

    static member inline Throw (x: 'E) = x |> throw |> ValueOptionT.lift
    static member inline Catch (m: ValueOptionT<'``MonadError<'E1,'T>``>, h: 'E1 -> ValueOptionT<'``MonadError<'E2,'T>``>) = ValueOptionT ((fun v h -> catch v h) (ValueOptionT.run m) (ValueOptionT.run << h)) : ValueOptionT<'``MonadError<'E2,'T>``>

    static member inline CallCC (f: (('T -> ValueOptionT<'``MonadCont<'R,voption<'U>>``>) -> _)) = ValueOptionT (callCC <| fun c -> ValueOptionT.run (f (ValueOptionT << c << ValueSome))) : ValueOptionT<'``MonadCont<'R,voption<'T>>``>

    static member inline get_Get () = ValueOptionT.lift get           : ValueOptionT<'``MonadState<'S,'S>``>
    static member inline Put (x: 'S) = x |> put |> ValueOptionT.lift  : ValueOptionT<'``MonadState<unit,'S>``>

    static member inline get_Ask () = ValueOptionT.lift ask           : ValueOptionT<'``MonadReader<'R,voption<'R>>``>
    static member inline Local (ValueOptionT (m: '``MonadReader<'R2,'T>``), f: 'R1->'R2) = ValueOptionT (local f m)

    static member inline Tell (w: 'Monoid) = w |> tell |> ValueOptionT.lift : ValueOptionT<'``MonadWriter<'Monoid, unit>``>

    static member inline Listen m                              : ValueOptionT<'``'MonadWriter<'Monoid, voption<'T>>``> =
        let liftMaybe (m, w) = ValueOption.map (fun x -> (x, w)) m
        ValueOptionT (listen (ValueOptionT.run m) >>= (result << liftMaybe))

    static member inline Pass m : ValueOptionT<'``MonadWriter<'Monoid, voption<'T>>``> = ValueOptionT (ValueOptionT.run m >>= option (map ValueSome << pass << result) (result ValueNone))

#endif
