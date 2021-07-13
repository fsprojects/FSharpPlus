namespace FSharpPlus.Data

open System.ComponentModel
open FSharpPlus.Operators


/// Array with an Applicative functor based on zipping and parallel execution.
type ParallelArray<'t> =
    | Infinite of 't
    | Bounded  of 't array


/// Basic operations on ParallelArray
module ParallelArray =
    let run = function
        | Bounded a -> a
        | _         -> invalidOp "Resulting array would be infinite."

    #if !FABLE_COMPILER
    let map f = function
        | Infinite s -> Infinite (f s)
        | Bounded  a -> Bounded (Array.Parallel.map f a)

    let ap f x =
        match f, x with
        | Infinite f, Infinite x -> Infinite (f x)
        | Infinite f, Bounded  x -> Bounded (Array.Parallel.map f x)
        | Bounded  f, Infinite x -> Bounded (Array.Parallel.map ((|>) x) f)
        | Bounded  f, Bounded  x ->
            if f.LongLength < x.LongLength then Bounded (Array.Parallel.mapi (fun i f -> f x.[i]) f)
            else                                Bounded (Array.Parallel.mapi (fun i x -> f.[i] x) x)

    let map2 f x y =
        match x, y with
        | Infinite x, Infinite y -> Infinite (f x y)
        | Infinite x, Bounded  y -> Bounded (Array.Parallel.map (f x) y)
        | Bounded  x, Infinite y -> Bounded (Array.Parallel.map (fun x -> f x y) x)
        | Bounded  x, Bounded  y ->
            if x.LongLength < y.LongLength then Bounded (Array.Parallel.mapi (fun i x -> f x y.[i]) x)
            else                                Bounded (Array.Parallel.mapi (fun i y -> f x.[i] y) y)

    let map3 f x y z =
        match x, y, z with
        | Infinite x, Infinite y, Infinite z -> Infinite (f x y z)
        | Infinite x, Bounded  _, Bounded  _ -> map2 (f x) y z
        | Bounded  _, Infinite y, Bounded  _ -> map2 (fun x z -> f x y z) x z
        | Bounded  _, Bounded  _, Infinite z -> map2 (fun x y -> f x y z) x y
        | Bounded  x, Infinite y, Infinite z -> Bounded (Array.Parallel.map (fun x -> f x y z) x)
        | Infinite x, Bounded  y, Infinite z -> Bounded (Array.Parallel.map (fun y -> f x y z) y)
        | Infinite x, Infinite y, Bounded  z -> Bounded (Array.Parallel.map (f x y) z)
        | Bounded  x, Bounded  y, Bounded  z ->
            if   x.LongLength < y.LongLength && x.LongLength <= z.LongLength then Bounded (Array.Parallel.mapi (fun i x -> f x y.[i] z.[i]) x)
            elif y.LongLength < x.LongLength && y.LongLength <= z.LongLength then Bounded (Array.Parallel.mapi (fun i y -> f x.[i] y z.[i]) y)
            else                                                                  Bounded (Array.Parallel.mapi (fun i z -> f x.[i] y.[i] z) z)
    #endif

/// A type alias for ParallelArray<'T>
type parray<'t> = ParallelArray<'t>

[<AutoOpen>]
module ParallelArrayOperators =
    /// Creates a parallel array from a normal array.
    let parray s = Bounded s

type ParallelArray<'t> with

    #if !FABLE_COMPILER
    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member Map (x: parray<_>, f) = ParallelArray.map f x
    #endif

    static member Return (x: 'a) = Infinite x
    #if !FABLE_COMPILER
    static member (<*>) (f: parray<'a->'b>, x: parray<_>) = ParallelArray.ap f x : parray<'b>

    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member Lift2 (f, x: parray<'T>, y: parray<'U>) = ParallelArray.map2 f x y : parray<'V>

    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member Lift3 (f, x: parray<'T>, y: parray<'U>, z: parray<'V>) = ParallelArray.map3 f x y z : parray<'W>

    static member inline get_Zero () = Bounded (getZero ()) : parray<'m>
    static member inline (+) (x: parray<'m>, y: parray<'m>) = lift2 plus x y : parray<'m>
    #endif