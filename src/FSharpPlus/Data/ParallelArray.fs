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
    static member Lift2 (f, x: parray<'T>, y: parray<'U>) = ParallelArray.map2 f x y : parray<'V>
    static member inline get_Zero () = Bounded (getZero ()) : parray<'m>
    static member inline (+) (x: parray<'m>, y: parray<'m>) = lift2 plus x y : parray<'m>
    #endif