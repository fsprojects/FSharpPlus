namespace FsControl.Core.Abstractions

// Functor class ----------------------------------------------------------

open System
open Applicative
open Monad

module Functor =
    type DefaultImpl =         
        static member inline MapFromApplicative f x = pure' f <*> x
        static member inline MapFromMonad f x = x >>= (return' << f)

    type Map = Map with
        static member instance (Map, x:option<_>    , _) = fun f -> Option.map  f x
        static member instance (Map, x:List<_>      , _:List<'b>) = fun f -> List.map f x :List<'b>
        static member instance (Map, g:_->_         , _) = (>>) g
        static member instance (Map, (m,a)          , _) = fun f -> (m, f a)
        static member instance (Map, x:array<_>     , _) = fun f -> Array.map   f x
        static member instance (Map, x:_ [,]        , _) = fun f -> Array2D.map f x
        static member instance (Map, x:_ [,,]       , _) = fun f -> Array3D.map f x
        static member instance (Map, x:_ [,,,]      , _) = fun f ->
            Array4D.init (x.GetLength 0) (x.GetLength 1) (x.GetLength 2) (x.GetLength 3) (fun a b c d -> f x.[a,b,c,d])
        static member instance (Map, x:Async<_>   , _) = fun f -> DefaultImpl.MapFromMonad f x
        static member instance (Map, x:Nullable<_>, _) = fun f -> if x.HasValue then Nullable(f x.Value) else Nullable()
        static member instance (Map, x:Choice<_,_>, _) = fun f -> match x with Choice1Of2 x -> Choice1Of2(f x) | Choice2Of2 x -> Choice2Of2 x

    let inline internal fmap   f x = Inline.instance (Map, x) f