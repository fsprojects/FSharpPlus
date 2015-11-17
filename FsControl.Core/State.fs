namespace FsControl

/// <summary> Computation type: Computations which maintain state.
/// <para/>   Binding strategy: Threads a state parameter through the sequence of bound functions so that the same state value is never used twice, giving the illusion of in-place update.
/// <para/>   Useful for: Building computations from sequences of operations that require a shared state. </summary>
type State<'s,'t> = State of ('s->('t * 's))

[<RequireQualifiedAccess>]
module State =
    let run (State x) = x                                                                                           : 'S->('T * 'S)
    let map   f (State m) = State (fun s -> let (a:'T, s') = m s in (f a, s'))                                      : State<'S,'U>
    let bind  f (State m) = State (fun s -> let (a:'T, s') = m s in run (f a) s')                                   : State<'S,'U>
    let apply (State f) (State x) = State (fun s -> let (f', s1) = f s in let (x':'T, s2) = x s1 in (f' x', s2))    : State<'S,'U>

    let eval (State sa) (s:'s)           = fst (sa s)                                                               : 'T
    let exec (State sa : State<'S,'A>) s = snd (sa s)                                                               : 'S

    /// Return the state from the internals of the monad.
    let get   = State (fun s -> (s, s))                                                                             : State<'S,'S>

    /// Replace the state inside the monad.
    let put x = State (fun _ -> ((), x))                                                                            : State<'S,unit>

type State with
    static member Map   (x, f:'T->_) = State.map f x          : State<'S,'U>
    static member Return a = State (fun s -> (a, s))          : State<'S,'T>
    static member Bind  (x, f:'T->_) = State.bind f x         : State<'S,'U>
    static member (<*>) (f, x:State<'S,'T>) = State.apply f x : State<'S,'U>
    static member get_Get() = State.get                       : State<'S,'S>
    static member Put x     = State.put x                     : State<'S,unit>