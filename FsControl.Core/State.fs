namespace FsControl.Core.Types

open FsControl.Core.TypeMethods
open FsControl.Core.TypeMethods.Applicative

type State<'S,'A> = State of ('S->('A * 'S))

[<RequireQualifiedAccess>]
module State =
    let run (State x) = x :'s->_
    let map  f (State m)  = State (fun s -> let (a, s') = m s in ( f a, s')) :State<'s,_>
    let bind f (State m) = State(fun s -> let (a, s') = m s in run (f a) s') :State<'s,'b>
    let apply f x = State (fun s -> let f, s1 = run f s in let x, s2 = run x s1 in f x, s2)

    let eval (State sa) (s:'s) = fst(sa s)
    let exec (State sa) (s:'s) = snd(sa s)
    let get() = State (fun s -> (s , s))  :State<'s,_>
    let put x = State (fun _ -> ((), x))  :State<'s,_>

type State<'S,'A> with
    static member instance (_:Functor.Map   , x, _) = fun f -> State.map f x :State<'s,_>
    static member instance (Applicative.Pure, _:State<'s,'a>           ) = fun a -> State(fun s -> (a, s))                                 :State<'s,'a>
    static member instance (Monad.Bind  ,   x, _:State<'s,'b>) = fun f -> State.bind f x  :State<'s,'b>
    static member instance (_:Applicative.Apply, f:State<'s,_>, x:State<'s,'a>, _:State<'s,'b>) = fun () -> State.apply f x :State<'s,'b>