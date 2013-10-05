namespace FsControl.Core.Abstractions

// Functor class ----------------------------------------------------------

open System
open Applicative
open Monad
module Functor =
    type DefaultImpl =         
        static member inline FmapFromApplicative f x = pure' f <*> x
        static member inline FmapFromMonad f x = x >>= (return' << f)

    type Fmap = Fmap with
        static member instance (Fmap, x:option<_>    , _) = fun f -> Option.map  f x
        static member instance (Fmap, x:List<_>      , _:List<'b>) = fun f -> List.map f x :List<'b>
        static member instance (Fmap, g:_->_         , _) = (>>) g
        static member instance (Fmap, (m,a)          , _) = fun f -> (m, f a)
        static member instance (Fmap, x:array<_>     , _) = fun f -> Array.map   f x
        static member instance (Fmap, x:_ [,]        , _) = fun f -> Array2D.map f x
        static member instance (Fmap, x:_ [,,]       , _) = fun f -> Array3D.map f x
        static member instance (Fmap, x:_ [,,,]      , _) = fun f ->
            Array4D.init (x.GetLength 0) (x.GetLength 1) (x.GetLength 2) (x.GetLength 3) (fun a b c d -> f x.[a,b,c,d])
        static member instance (Fmap, x:Async<_>   , _) = fun f -> DefaultImpl.FmapFromMonad f x
        static member instance (Fmap, x:Nullable<_>, _) = fun f -> if x.HasValue then Nullable(f x.Value) else Nullable()
        static member instance (Fmap, x:Choice<_,_>, _) = fun f -> match x with Choice2Of2 x -> Choice2Of2(f x) | x -> x

    let inline internal fmap   f x = Inline.instance (Fmap, x) f