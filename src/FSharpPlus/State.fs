namespace FSharpPlus.Data

open System.ComponentModel

/// <summary> Computation type: Computations which maintain state.
/// <para/>   Binding strategy: Threads a state parameter through the sequence of bound functions so that the same state value is never used twice, giving the illusion of in-place update.
/// <para/>   Useful for: Building computations from sequences of operations that require a shared state. </summary>
[<Struct>]
type State<'s,'t> = State of ('s->('t * 's))

/// Basic operations on State
[<RequireQualifiedAccess>]
module State =
    let run (State x) = x                                                                                         : 'S->('T * 'S)
    let map   f (State m) = State (fun s -> let (a: 'T, s') = m s in (f a, s'))                                   : State<'S,'U>
    let bind  f (State m) = State (fun s -> let (a: 'T, s') = m s in run (f a) s')                                : State<'S,'U>
    let apply (State f) (State x) = State (fun s -> let (f', s1) = f s in let (x': 'T, s2) = x s1 in (f' x', s2)) : State<'S,'U>

    let eval (State sa) (s: 's)         = fst (sa s) : 'T
    let exec (State sa: State<'S,'A>) s = snd (sa s) : 'S

    /// Return the state from the internals of the monad.
    let get = State (fun s -> (s, s))                                                                             : State<'S,'S>

    /// Get a value which depends on the current state.
    let gets f = State (fun s -> (f s, s))                                                                        : State<'S,'T>

    /// Replace the state inside the monad.
    let put x = State (fun _ -> ((), x))                                                                          : State<'S,unit>

    /// Modify the state inside the monad by applying a function.
    let modify f = State (fun s -> ((), f s))                                                                     : State<'S,unit>

    /// Zips two States into one.
    let zip (x: State<'S,'T>) (y: State<'S,'U>) = apply (map (fun a b -> (a, b)) x) y : State<'S, ('T * 'U)>

type State<'s,'t> with

    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member Map   (x, f: 'T->_) = State.map f x          : State<'S,'U>

    static member Return a = State (fun s -> (a, s))           : State<'S,'T>
    static member (>>=) (x, f: 'T->_) = State.bind f x         : State<'S,'U>
    static member (<*>) (f, x: State<'S,'T>) = State.apply f x : State<'S,'U>
    static member get_Get () = State.get                       : State<'S,'S>

    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member Put x     = State.put x                      : State<'S,unit>

    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member Zip (x, y) = State.zip x y