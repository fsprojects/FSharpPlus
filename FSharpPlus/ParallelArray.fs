namespace FSharpPlus

open FsControl.Core.Abstractions
open FSharpPlus.Prelude

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
        match (f,x) with
        | Const   f, Const   x -> Const   (f x)
        | Const   f, Bounded x -> Bounded (Array.Parallel.map f x)
        | Bounded f, Const   x -> Bounded (Array.Parallel.map ((|>) x) f)
        | Bounded f, Bounded x -> 
            if f.LongLength < x.LongLength then Bounded (Array.Parallel.mapi (fun i f -> f x.[i]) f)
            else                                Bounded (Array.Parallel.mapi (fun i x -> f.[i] x) x)

    let apend a b = 
        match (a,b) with
        | Bounded a, Bounded b -> Bounded (Array.append a b)
        | _                    -> invalidOp "Resulting array would be infinite."

type parray<'t> = ParallelArray<'t>

[<AutoOpen>]
module ParallelArrayOperators =
    let parray s = Bounded s

type ParallelArray with
    static member instance (_:Functor.Map, x:parray<_>       , _) = fun f -> ParallelArray.map f x
    static member instance (_:Applicative.Pure , _:parray<'a>   ) = fun (x:'a) -> Const x
    static member instance (_:Applicative.Apply, f:parray<'a->'b>, x:parray<_> ,_:parray<'b>) = fun () -> ParallelArray.ap f x :parray<'b>
    static member instance (_:Monoid.Mempty   , _:parray<'a>   ) = fun () -> Bounded Array.empty : parray<'a>
    static member instance (_:Monoid.Mappend  , x:parray<'a>, _) = fun y  -> ParallelArray.apend x y