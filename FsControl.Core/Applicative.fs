namespace FsControl.Core.Abstractions

open FsControl.Core.Prelude

module Applicative =
    open Monad
    type DefaultImpl =         
        static member inline PureFromMonad x = return' x
        static member inline ApFromMonad f x = f >>= fun x1 -> x >>= fun x2 -> return'(x1 x2)

    type Pure = Pure with
        static member instance (Pure, _:option<'a>   ) = fun (x:'a) -> DefaultImpl.PureFromMonad x :option<'a>
        static member instance (Pure, _:List<'a>     ) = fun (x:'a) -> DefaultImpl.PureFromMonad x :List<'a>
        static member instance (Pure, _:'r -> 'a     ) = const':'a  -> 'r -> _
        static member instance (Pure, _:Async<'a>    ) = fun (x:'a) -> DefaultImpl.PureFromMonad x :Async<'a>
        static member instance (Pure, _:Choice<'a,'e>) = fun (x:'a) -> DefaultImpl.PureFromMonad x :Choice<_,'e>

    type Ap = Ap with
        static member instance (Ap, f:option<_>   , x:option<'a>   , _:option<'b>   ) = fun () -> DefaultImpl.ApFromMonad f x :option<'b>
        static member instance (Ap, f:List<_>     , x:List<'a>     , _:List<'b>     ) = fun () -> DefaultImpl.ApFromMonad f x :List<'b>
        static member instance (Ap, f:'r -> _     , g: _ -> 'a     , _: 'r -> 'b    ) = fun () -> fun x -> f x (g x) :'b
        static member instance (Ap, f:Async<_>    , x:Async<'a>    , _:Async<'b>    ) = fun () -> DefaultImpl.ApFromMonad f x :Async<'b>
        static member instance (Ap, f:Choice<_,'e>, x:Choice<'a,'e>, _:Choice<'b,'e>) = fun () ->
            match (f,x) with
            | (Choice1Of2 a, Choice1Of2 b) -> Choice1Of2 (a b)
            | (Choice2Of2 a, _)            -> Choice2Of2 a
            | (_, Choice2Of2 b)            -> Choice2Of2 b :Choice<'b,'e>

    let inline internal pure' x   = Inline.instance Pure x
    let inline internal (<*>) x y = Inline.instance (Ap, x, y) ()

module Alternative =
    open Applicative

    type Empty = Empty with
        static member instance (_Alternative:Empty, _:option<'a>) = fun () -> None
        static member instance (_Alternative:Empty, _:List<'a>  ) = fun () -> []

    type Append = Append with   
        static member instance (_Alternative:Append, x:option<_>, _) = fun y -> match x with | None -> y | xs -> xs
        static member instance (_Alternative:Append, x:List<_>  , _) = fun y -> x @ y