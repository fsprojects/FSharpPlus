namespace FSharpPlus.Data

#nowarn "686"
#if !FABLE_COMPILER

open System.ComponentModel
open FSharpPlus
open FSharpPlus.Control
open FSharpPlus.Internals.Prelude


[<NoComparison>]
type Free<'functor, 't> = Pure of 't | Roll of obj

[<AutoOpen>]
module FreePrimitives =
    let inline Roll (f: '``Functor<Free<'Functor, 'T>>``) : Free<'Functor, 'T> =
        if opaqueId false then
            let (_: 'Functor) = Map.Invoke (fun (_: Free<'Functor, 'T>) -> Unchecked.defaultof<__>) f
            ()
        Free<'Functor, 'T>.Roll f
    let (|Pure|Roll|) x = match x with Choice1Of2 x -> Pure x | Choice2Of2 x -> Roll x

/// Basic operations on Free Monads
[<RequireQualifiedAccess>]
module Free =

    let inline run (f: Free<'Functor, 'T>) : Choice<_, '``Functor<Free<'Functor, 'T>>``> =
        if opaqueId false then
            let (_: ^``Functor<Free<'Functor, 'T>>``) = Map.Invoke (fun (_: __) -> Unchecked.defaultof<Free<'Functor, 'T>>) Unchecked.defaultof<'Functor>
            ()
        match f with
        | Free.Pure x -> Choice1Of2 x
        | Free.Roll x -> let x = unbox x in Choice2Of2 x

    let inline map f x =
        let rec loop (f: 'T->'U) (x: Free<'Functor, 'T>) : Free<'Functor, 'U> =
            match run x with
            | Pure x -> Pure (f x)
            | Roll (x: ^``Functor<Free<'Functor, 'T>>``) -> Roll (Map.Invoke (loop f : Free<'Functor, 'T> -> _) x: ^``Functor<Free<'Functor, 'U>>``)
        loop f x

    let inline bind (f: 'T -> Free<'Functor, 'U>) (x: Free<'Functor, 'T>) : Free<'Functor, 'U> =
        let rec loop f (x: Free<_,_>) =
            match run x with
            | Pure r -> f r
            | Roll (x: ^``Functor<Free<'Functor, 'T>>``) -> Roll (Map.Invoke (loop f : Free<'Functor, 'T> -> _) x: ^``Functor<Free<'Functor, 'U>>``) : Free<'Functor, 'U>
        loop f x

    let inline apply (f: Free<'Functor, 'T->'U>) (x: Free<'Functor, 'T>) : Free<'Functor, 'U> =
        let rec loop (x: Free<_,_>) (f: Free<_,_>) =
            match run f with
            | Pure f -> map<'T, 'U, 'Functor, '``Functor<Free<'Functor, 'T>>``, '``Functor<Free<'Functor, 'U>>``> f x : Free<'Functor, 'U>
            | Roll (f: ^``Functor<Free<'Functor, ('T -> 'U)>>``) -> Roll (Map.Invoke (loop x: Free<'Functor, ('T -> 'U)> -> _) f: '``Functor<Free<'Functor, 'U>>``)
        loop x f

    let inline map2 (f: 'T->'U->'V) (x: Free<'Functor, 'T>) (y: Free<'Functor, 'U>) : Free<'Functor, 'V> =
        let rec loop (y: Free<_,_>) (x: Free<_,_>) =
            match run x with
            | Pure x -> map<'U, 'V, 'Functor, '``Functor<Free<'Functor, 'U>>``, '``Functor<Free<'Functor, 'V>>``> (f x) y : Free<'Functor, 'V>
            | Roll (x: ^``Functor<Free<'Functor, 'T>>``) -> Roll (Map.Invoke (loop y: Free<'Functor, 'T> -> _) x: '``Functor<Free<'Functor, 'V>>``)
        loop y x

    let inline map3 (f: 'T->'U->'V->'W) (x: Free<'Functor, 'T>) (y: Free<'Functor, 'U>) (z: Free<'Functor, 'V>) : Free<'Functor, 'W> =
        let rec loop (y: Free<_,_>) (x: Free<_,_>) (z: Free<_,_>) =
            match run x with
            | Pure x -> map2<'U, 'V, 'W, 'Functor, '``Functor<Free<'Functor, 'U>>``, '``Functor<Free<'Functor, 'V>>``, '``Functor<Free<'Functor, 'W>>``> (f x) y z : Free<'Functor, 'W>
            | Roll (x: ^``Functor<Free<'Functor, 'T>>``) -> Roll (Map.Invoke (loop y: Free<'Functor, 'T> -> _) x: '``Functor<Free<'Functor, 'W>>``)
        loop y x z

    /// Folds the Free structure into a Monad
    let inline fold (f: '``Functor<'T>`` -> '``Monad<'T>``) (x: Free<'Functor, 'U>) : '``Monad<'U>`` =
        let rec loop f x =
            match run x with
            | Pure a -> Return.Invoke a
            | Roll x -> f x >>= loop f
        loop f x

    /// Tear down a Free monad using iteration.
    let inline iterM (f: '``Functor<'Monad<'T>>`` -> '``Monad<'T>``) (x: Free<'Functor, 'T>) : '``Monad<'T>`` =
        let rec loop f x =
            match run x with
            | Pure  x -> Return.Invoke x
            | Roll (x: ^``Functor<Free<'Functor, 'T>>``) -> f (loop f <!> x)
        loop f x

    /// Lift any Functor into a Free structure
    let inline liftF (x: '``Functor<'T>``) : Free<'Functor, 'T> = Roll (Map.Invoke (Pure: 'T -> Free<'Functor, 'T>) x : '``Functor<Free<'Functor, 'T>>``)

    /// Lift a natural transformation from functor F to functor G into a natural transformation from Free of F to Free of G.
    let inline hoist (f: ^``F<Free<'F<'T>, 'T>>`` -> ^``G<Free<'F<'T>, 'T>>``) (x: Free<'``F<'T>``, 'T>) : Free<'``G<'T>``, 'T> =
        let rec loop f x =
            if opaqueId false then
                let _: '``G<Free<'F<'T>, 'T>>`` = Map.Invoke Unchecked.defaultof<Free<'``G<'T>``, 'T> -> Free<'``F<'T>``, 'T>> Unchecked.defaultof<'``G<Free<'G<'T>, 'T>>``>
                let _: '``G<Free<'G<'T>, 'T>>`` = Map.Invoke Unchecked.defaultof<'T -> Free<'``G<'T>``, 'T>> Unchecked.defaultof<'``G<'T>``>
                ()
            match run x with
            | Pure x -> Pure x
            | Roll (x: ^``F<Free<'F<'T>, 'T>>``) -> Roll (Map.Invoke (loop f: _ -> _) (f x) : ^``G<Free<'G<'T>, 'T>>``)
        loop f x


type Free<'functor, 't> with

    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member inline Map (x: Free<'Functor, 'T>, f: 'T -> 'U) = Free.map f x : Free<'Functor, 'U>

    static member Return x = Pure x
    static member inline (>>=) (x: Free<'Functor, 'T>, f: 'T -> Free<'Functor, 'U>)   = Free.bind  f x : Free<'Functor, 'U>
    static member inline (<*>) (f: Free<'Functor, ('T -> 'U)>, x: Free<'Functor, 'T>) = Free.apply f x : Free<'Functor, 'U>
    
    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member inline Lift2 (f, x: Free<'Functor, 'T>, y: Free<'Functor, 'U>) = Free.map2 f x y: Free<'Functor, 'V>

    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member inline Lift3 (f, x: Free<'Functor, 'T>, y: Free<'Functor, 'U>, z: Free<'Functor, 'V>) = Free.map3 f x y z: Free<'Functor, 'W>

    static member        Delay (x: unit -> Free<'Functor, 'T>) = x ()

#endif
