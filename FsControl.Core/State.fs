namespace FsControl.Core.Types

open FsControl.Core.TypeMethods
open FsControl.Core.TypeMethods.Applicative

type State<'S,'A> = State of ('S->('A * 'S))

[<RequireQualifiedAccess>]
module State =
    let run (State x) = x :'s->_
    let map  f (State m)  = State(f << m) :State<'s,_>
    let eval (State sa) (s:'s) = fst(sa s)
    let exec (State sa) (s:'s) = snd(sa s)
    let get() = State (fun s -> (s , s))       :State<'s,_>
    let put x = State (fun _ -> ((), x))       :State<'s,_>

type State<'S,'A> with
    static member instance (Functor.Map  , State m,               _) = fun f -> State(fun s -> let (a, s') = m s in (f a, s')) :State<'s,_>
    static member instance (Applicative.Pure, _:State<'s,'a>           ) = fun a -> State(fun s -> (a, s))                                 :State<'s,'a>
    static member instance (Monad.Bind  ,   State m, _:State<'s,'b>) = fun k -> State(fun s -> let (a, s') = m s in State.run(k a) s') :State<'s,'b>
    static member instance (Applicative.Apply, f:State<'s,_>, x:State<'s,'a>, _:State<'s,'b>) = fun () -> DefaultImpl.ApplyFromMonad f x :State<'s,'b>