namespace FSharpPlus.Data

open FSharpPlus.Control
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

    let map f = function
        | Infinite s -> Infinite   (f s)
        | Bounded  a -> Bounded (Array.Parallel.map f a)

    let ap f x = 
        match (f, x) with
        | Infinite f, Infinite x -> Infinite (f x)
        | Infinite f, Bounded  x -> Bounded (Array.Parallel.map f x)
        | Bounded  f, Infinite x -> Bounded (Array.Parallel.map ((|>) x) f)
        | Bounded  f, Bounded  x -> 
            if f.LongLength < x.LongLength then Bounded (Array.Parallel.mapi (fun i f -> f x.[i]) f)
            else                                Bounded (Array.Parallel.mapi (fun i x -> f.[i] x) x)

    //let inline append (a:ParallelArray<'m>) (b:ParallelArray<'m>) = liftA2 mappend a b :ParallelArray<'m>

/// A type alias for ParallelArray<'T>
type parray<'t> = ParallelArray<'t>

[<AutoOpen>]
module ParallelArrayOperators =
    /// Creates a parallel array from a normal array.
    let parray s = Bounded s

type ParallelArray<'t> with
    static member Map (x:parray<_>, f) = ParallelArray.map f x
    static member Return (x:'a) = Infinite x
    static member (<*>) (f:parray<'a->'b>, x:parray<_>) = ParallelArray.ap f x :parray<'b>
    static member inline get_Zero() = Bounded (getZero()) : parray<'m>
    static member inline (+) (x:parray<'m>, y:parray<'m>) = liftA2 plus x y:parray<'m>