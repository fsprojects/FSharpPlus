namespace FsControl.Core.Types

type State<'S,'A> = State of ('S->('A * 'S))

[<RequireQualifiedAccess>]
module State =
    let run (State x) = x :'s->_
    let map   f (State m) = State (fun s -> let (a, s') = m s in (f a, s'))    :State<'s,_>
    let bind  f (State m) = State (fun s -> let (a, s') = m s in run (f a) s') :State<'s,'b>
    let apply (State f) (State x) = State (fun s -> let (f', s1) = f s in let (x', s2) = x s1 in (f' x', s2))

    let eval (State sa) (s:'s) = fst (sa s)
    let exec (State sa) (s:'s) = snd (sa s)
    let get() = State (fun s -> (s , s)) :State<'s,_>
    let put x = State (fun _ -> ((), x)) :State<'s,_>

type State<'S,'A> with
    static member Map   (x, f) = State.map f x                :State<'s,_>
    static member Return a = State (fun s -> (a, s))          :State<'s,'a>
    static member Bind  (x, f) = State.bind f x               :State<'s,'b>
    static member (<*>) (f, x:State<'s,'a>) = State.apply f x :State<'s,'b>