namespace FsControl.Core.Abstractions

open System
open FsControl.Core.Prelude
open Monoid

// Monad class ------------------------------------------------------------
module Monad =
    type Bind = Bind with
        static member        instance (Bind, x:option<_>   , _:option<'b> ) = fun (f:_->option<'b>) -> Option.bind  f x
        static member        instance (Bind, x:List<_>     , _:List<'b>   ) = fun (f:_->List<'b>  ) -> List.collect f x
        static member        instance (Bind, f:'r->'a      , _:'r->'b     ) = fun (k:_->_->'b) r    -> k (f r) r
        static member inline instance (Bind, (w, a):'m * 'a, _:'m * 'b    ) = fun (k:_->'m * 'b   ) -> let (w', b) = k a in (mappend w w', b)
        static member        instance (Bind, x:Async<'a>   , _:'b Async   ) = fun (f:_->Async<'b> ) -> async.Bind(x,f)
        static member        instance (Bind, x:Choice<'a,'e>, _:Choice<'b,'e>) = fun (k:'a->Choice<'b,'e>) -> 
            match x with Choice1Of2 r -> k r | Choice2Of2 e -> Choice2Of2 e

        //Restricted Monad
        static member instance (Bind, x:Nullable<_> , _:'b Nullable) = fun f -> if x.HasValue then f x.Value else Nullable() : Nullable<'b>

    let inline internal (>>=) x (f:_->'R) : 'R = Inline.instance (Bind, x) f


open Monad

module Applicative =
    type Pure = Pure with
        static member        instance (Pure, _:option<'a>   ) = fun x -> Some x      :option<'a>
        static member        instance (Pure, _:List<'a>     ) = fun x -> [x]         :List<'a>
        static member        instance (Pure, _:'r -> 'a     ) = const':'a  -> 'r -> _
        static member inline instance (Pure, _: 'm * 'a     ) = fun (x:'a) -> (mempty(), x)
        static member        instance (Pure, _:'a Async     ) = fun (x:'a) -> async.Return x        
        static member        instance (Pure, _:Choice<'a,'e>) = fun x -> Choice1Of2 x :Choice<'a,'e>

        //Restricted
        static member instance (Pure, _:'a Nullable  ) = fun (x:'a) -> Nullable x

    let inline internal pure' x   = Inline.instance Pure x

    type DefaultImpl =         
        static member inline ApFromMonad f x = f >>= fun x1 -> x >>= fun x2 -> pure'(x1 x2)

    type Ap = Ap with
        static member        instance (Ap, f:option<_>   , x:option<'a>   , _:option<'b>   ) = fun () -> DefaultImpl.ApFromMonad f x :option<'b>
        static member        instance (Ap, f:List<_>     , x:List<'a>     , _:List<'b>     ) = fun () -> DefaultImpl.ApFromMonad f x :List<'b>
        static member        instance (Ap, f:'r -> _     , g: _ -> 'a     , _: 'r -> 'b    ) = fun () -> fun x -> f x (g x) :'b
        static member inline instance (Ap, f:'m * _      , x:'m * 'a      , _:'m * 'b      ) = fun () -> DefaultImpl.ApFromMonad f x :'m *'b
        static member        instance (Ap, f:Async<_>    , x:Async<'a>    , _:Async<'b>    ) = fun () -> DefaultImpl.ApFromMonad f x :Async<'b>
        static member        instance (Ap, f:Choice<_,'e>, x:Choice<'a,'e>, _:Choice<'b,'e>) = fun () ->
            match (f,x) with
            | (Choice1Of2 a, Choice1Of2 b) -> Choice1Of2 (a b)
            | (Choice2Of2 a, _)            -> Choice2Of2 a
            | (_, Choice2Of2 b)            -> Choice2Of2 b :Choice<'b,'e>

    
    let inline internal (<*>) x y = Inline.instance (Ap, x, y) ()

open Applicative

module Alternative =
    type Empty = Empty with
        static member instance (_Alternative:Empty, _:option<'a>) = fun () -> None
        static member instance (_Alternative:Empty, _:List<'a>  ) = fun () -> []

    type Append = Append with   
        static member instance (_Alternative:Append, x:option<_>, _) = fun y -> match x with | None -> y | xs -> xs
        static member instance (_Alternative:Append, x:List<_>  , _) = fun y -> x @ y


// Functor class ----------------------------------------------------------

module Functor =
    type DefaultImpl =         
        static member inline MapFromApplicative f x = pure' f <*> x
        static member inline MapFromMonad f x = x >>= (pure' << f)

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

    
    let inline internal sequence ms =
        let k m m' = m >>= fun (x:'a) -> m' >>= fun xs -> (pure' :list<'a> -> 'M) (List.Cons(x,xs))
        List.foldBack k ms ((pure' :list<'a> -> 'M) [])

    let inline internal mapM f as' = sequence (List.map f as')

    let inline internal liftM  f m1    = m1 >>= (pure' << f)
    let inline internal liftM2 f m1 m2 = m1 >>= fun x1 -> m2 >>= fun x2 -> pure' (f x1 x2)
    let inline internal ap     x y     = liftM2 id x y

    let inline internal (>=>)  f g x   = f x >>= g

    // Do notation ------------------------------------------------------------

    type DoNotationBuilder() =
        member inline b.Return(x)    = pure' x
        member inline b.Bind(p,rest) = p >>= rest
        member        b.Let (p,rest) = rest p
        member    b.ReturnFrom(expr) = expr
    let inline  internal do'() = new DoNotationBuilder()

    let inline internal return' x = Inline.instance Pure x


open Functor

module Comonad =

    type Extract = Extract with
        static member        instance (Extract, (w:'w,a:'a) ,_) = fun () -> a
        static member inline instance (Extract, f:'m->'t ,_:'t) = fun () -> f (mempty())

    let inline internal extract x = Inline.instance (Extract, x) ()


    type Duplicate = Duplicate with
        static member        instance (Duplicate, (w:'w, a:'a), _:'w * ('w*'a)) = fun () -> (w,(w,a))
        static member inline instance (Duplicate, f:'m->'a, _:'m->'m->'a) = fun () a b -> f (mappend a b)

    let inline internal duplicate x = Inline.instance (Duplicate, x) ()


    let inline internal extend g s = fmap g (duplicate s)
    let inline internal (=>>)  s g = fmap g (duplicate s)


// MonadPlus class ------------------------------------------------------------
module MonadPlus =
    type Mzero = Mzero with
        static member instance (Mzero, _:option<'a>) = fun () -> None
        static member instance (Mzero, _:List<'a>  ) = fun () -> []

    type Mplus = Mplus with
        static member instance (Mplus, x:option<_>, _) = fun y -> match x with None -> y | xs -> xs
        static member instance (Mplus, x:List<_>  , _) = fun y -> x @ y

    let inline internal mzero () = Inline.instance Mzero ()
    let inline internal mplus (x:'a) (y:'a) : 'a = Inline.instance (Mplus, x) y