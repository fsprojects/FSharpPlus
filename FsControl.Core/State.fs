namespace FsControl.Core.Types

open FsControl.Core.TypeMethods
open FsControl.Core.TypeMethods.Applicative

type State<'S,'A> = State of ('S->('A * 'S))

[<RequireQualifiedAccess>]
module State =
    let run (State x) = x :'s->_
    let map  f (State m)  = State(f << m) :State<'s,_>
    let bind f (State m) = State(fun s -> let (a, s') = m s in run (f a) s') :State<'s,'b>
    let eval (State sa) (s:'s) = fst(sa s)
    let exec (State sa) (s:'s) = snd(sa s)
    let get() = State (fun s -> (s , s))  :State<'s,_>
    let put x = State (fun _ -> ((), x))  :State<'s,_>

type State<'S,'A> with
    static member instance (_:Functor.Map   , x, _) = fun f -> State.map f x :State<'s,_>
    static member instance (Applicative.Pure, _:State<'s,'a>           ) = fun a -> State(fun s -> (a, s))                                 :State<'s,'a>
    static member instance (Monad.Bind  ,   State m, _:State<'s,'b>) = fun k -> State(fun s -> let (a, s') = m s in State.run(k a) s')     :State<'s,'b>
    static member instance (_:Applicative.Apply, f:State<'s,_>, x:State<'s,'a>, _:State<'s,'b>) = fun () -> 
        State(fun st -> 
            let (f, st1) = State.run f st
            let (x, st2) = State.run x st1
            (f x, st2))   :State<'s,'b>
