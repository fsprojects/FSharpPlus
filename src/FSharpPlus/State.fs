namespace FSharpPlus.Data

open System.ComponentModel
open FSharpPlus.Internals.Prelude

/// <summary> Computation type: Computations which maintain state.
/// <para/>   Binding strategy: Threads a state parameter through the sequence of bound functions so that the same state value is never used twice, giving the illusion of in-place update.
/// <para/>   Useful for: Building computations from sequences of operations that require a shared state. </summary>
[<MeasureAnnotatedAbbreviation>]
type State<'s,'t> = 's->('t * 's)

module StateFunctions =
    let toState<'S,'T> (f: ('S -> 'T * 'S)) : State<'S,'T> = retype f
    let ofState<'S,'T> (f: State<'S,'T>) : ('S -> 'T * 'S) = retype f

open StateFunctions

[<AutoOpen>]
module StateAutoOpens =
    let state x = toState x
    let (|State|) x = ofState x

/// Basic operations on State
[<RequireQualifiedAccess>]
module State =
    let run (State x) = x                                                                                         : 'S->('T * 'S)
    let map   f (State m) = state (fun s -> let (a: 'T, s') = m s in (f a, s'))                                   : State<'S,'U>
    let bind  f (State m) = state (fun s -> let (a: 'T, s') = m s in run (f a) s')                                : State<'S,'U>
    let apply (State f) (State x) = state (fun s -> let (f', s1) = f s in let (x': 'T, s2) = x s1 in (f' x', s2)) : State<'S,'U>

    let eval (State sa) (s: 's)         = fst (sa s) : 'T
    let exec (State sa: State<'S,'A>) s = snd (sa s) : 'S

    /// Return the state from the internals of the monad.
    let get<'S> = state (fun (s: 'S) -> (s, s))                                                                   : State<'S,'S>

    /// Get a value which depends on the current state.
    let gets f = state (fun s -> (f s, s))                                                                        : State<'S,'T>

    /// Replace the state inside the monad.
    let put x = state (fun _ -> ((), x))                                                                          : State<'S,unit>

    /// Modify the state inside the monad by applying a function.
    let modify f = state (fun s -> ((), f s))                                                                     : State<'S,unit>

    /// Zips two States into one.
    let zip (x: State<'S,'T>) (y: State<'S,'U>) = apply (map (fun a b -> (a, b)) x) y : State<'S, ('T * 'U)>