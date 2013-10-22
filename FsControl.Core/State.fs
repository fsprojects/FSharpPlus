namespace FsControl.Core.Types

open FsControl.Core.Abstractions
open FsControl.Core.Abstractions.Applicative

type State<'S,'A> = State of ('S->('A * 'S))

module State =
    let runState (State x) = x :'s->_
    let mapState  f (State m)  = State(f << m) :State<'s,_>
    let withState f (State m)  = State(m << f) :State<'s,_>
    let evalState (State sa) (s:'s) = fst(sa s)
    let execState (State sa) (s:'s) = snd(sa s)
    let get() = State (fun s -> (s , s))       :State<'s,_>
    let put x = State (fun _ -> ((), x))       :State<'s,_>

open State

type State<'S,'A> with
    static member instance (Functor.Map  , State m,              _) = fun f -> State(fun s -> let (a, s') = m s in (f a, s')) :State<'s,_>
    static member instance (Monad.Return, _:State<'s,'a>           ) = fun a -> State(fun s -> (a, s))                                :State<'s,'a>
    static member instance (Monad.Bind  ,   State m, _:State<'s,'b>) = fun k -> State(fun s -> let (a, s') = m s in runState(k a) s') :State<'s,'b>
    static member instance (Applicative.Pure, _:State<'s,'a>) = fun (x:'a) -> DefaultImpl.PureFromMonad x :State<'s,_>
    static member instance (Applicative.Ap, f:State<'s,_>, x:State<'s,'a>, _:State<'s,'b>) = fun () -> DefaultImpl.ApFromMonad f x :State<'s,'b>