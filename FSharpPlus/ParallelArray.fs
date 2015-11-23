namespace FSharpPlus

open FsControl
open FSharpPlus.Operators

type ParallelArray<'t> =
    | Const   of 't
    | Bounded of 't array


module ParallelArray =
    let run = function
        | Bounded a -> a
        | _         -> invalidOp "Resulting array would be infinite."

    let map f = function
        | Const   s -> Const   (f s)
        | Bounded a -> Bounded (Array.Parallel.map f a)

    let ap f x = 
        match (f, x) with
        | Const   f, Const   x -> Const   (f x)
        | Const   f, Bounded x -> Bounded (Array.Parallel.map f x)
        | Bounded f, Const   x -> Bounded (Array.Parallel.map ((|>) x) f)
        | Bounded f, Bounded x -> 
            if f.LongLength < x.LongLength then Bounded (Array.Parallel.mapi (fun i f -> f x.[i]) f)
            else                                Bounded (Array.Parallel.mapi (fun i x -> f.[i] x) x)

    //let inline append (a:ParallelArray<'m>) (b:ParallelArray<'m>) = liftA2 mappend a b :ParallelArray<'m>

type parray<'t> = ParallelArray<'t>

[<AutoOpen>]
module ParallelArrayOperators =
    let parray s = Bounded s

type ParallelArray with
    static member Map (x:parray<_>, f) = ParallelArray.map f x
    static member Return (x:'a) = Const x
    static member (<*>) (f:parray<'a->'b>, x:parray<_>) = ParallelArray.ap f x :parray<'b>
    static member inline get_Empty() = Bounded (getEmpty()) : parray<'m>
    static member inline Append (x:parray<'m>, y:parray<'m>) = liftA2 append x y:parray<'m>