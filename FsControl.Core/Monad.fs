namespace FsControl.Core.Abstractions

//open FsControl.Core.Prelude
open System

// Monad class ------------------------------------------------------------
module Monad =
    type Return = Return with
        static member instance (Return, _:option<'a>   ) = fun x -> Some x      :option<'a>
        static member instance (Return, _:List<'a>     ) = fun x -> [x]         :List<'a>
        static member instance (Return, _: 'r -> 'a    ) = fun (x:'a) (_:'r) -> x
        static member instance (Return, _:'a Async     ) = fun (x:'a) -> async.Return x        
        static member instance (Return, _:Choice<'a,'e>) = fun x -> Choice1Of2 x :Choice<'a,'e>

        //Restricted Monad
        static member instance (Return, _:'a Nullable  ) = fun (x:'a) -> Nullable x

    type Bind = Bind with
        static member instance (Bind, x:option<_>   , _:option<'b> ) = fun (f:_->option<'b>) -> Option.bind  f x
        static member instance (Bind, x:List<_>     , _:List<'b>   ) = fun (f:_->List<'b>  ) -> List.collect f x
        static member instance (Bind, f:'r->'a      , _:'r->'b     ) = fun (k:_->_->'b) r    -> k (f r) r    
        static member instance (Bind, x:Async<'a>   , _:'b Async   ) = fun (f:_->Async<'b> ) -> async.Bind(x,f)
        static member instance (Bind, x:Choice<_,'e>, _:Choice<'b,'e>) = fun (k:_->Choice<'b,_>) -> 
            match x with Choice1Of2 r -> k r | x -> x

        //Restricted Monad
        static member instance (Bind, x:Nullable<_> , _:'b Nullable) = fun f ->
            if x.HasValue then f x.Value else Nullable() : Nullable<'b>

    let inline internal return' x = Inline.instance Return x
    let inline internal (>>=) x (f:_->'R) : 'R = Inline.instance (Bind, x) f
    let inline internal (=<<) (f:_->'R) x : 'R = Inline.instance (Bind, x) f

    let inline internal sequence ms =
        let k m m' = m >>= fun (x:'a) -> m' >>= fun xs -> (return' :list<'a> -> 'M) (List.Cons(x,xs))
        List.foldBack k ms ((return' :list<'a> -> 'M) [])

    let inline internal mapM f as' = sequence (List.map f as')

    let inline internal liftM  f m1    = m1 >>= (return' << f)
    let inline internal liftM2 f m1 m2 = m1 >>= fun x1 -> m2 >>= fun x2 -> return' (f x1 x2)
    let inline internal when'  p s     = if p then s else return' ()
    let inline internal unless p s     = when' (not p) s
    let inline internal ap     x y     = liftM2 id x y

    let inline internal (>=>)  f g x   = f x >>= g
    let inline internal (<=<)  g f x   = f x >>= g

// Do notation ------------------------------------------------------------

    type DoNotationBuilder() =
        member inline b.Return(x)    = return' x
        member inline b.Bind(p,rest) = p >>= rest
        member        b.Let (p,rest) = rest p
        member    b.ReturnFrom(expr) = expr
    let inline  internal do'() = new DoNotationBuilder()