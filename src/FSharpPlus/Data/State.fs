namespace FSharpPlus.Data

#nowarn "1125"

open System.ComponentModel
open FSharpPlus


/// <summary> Computation type: Computations which maintain state.
/// <para>   Binding strategy: Threads a state parameter through the sequence of bound functions so that the same state value is never used twice, giving the illusion of in-place update.</para>
/// <para>   Useful for: Building computations from sequences of operations that require a shared state.</para>
/// The <typeparamref name="'s"/> indicates the computation state, while <typeparamref name="'t"/> indicates the result.</summary>
[<Struct>]
type State<'s,'t> = State of ('s->('t * 's))

/// Basic operations on State
[<RequireQualifiedAccess>]
module State =
    /// Runs the state with an inital state to get back the result and the new state.
    let run (State x) = x                                                                                         : 'S->('T * 'S)

    let map   f (State m) = State (fun s -> let (a: 'T, s') = m s in (f a, s'))                                   : State<'S,'U>

    /// Combines two States into one by applying a mapping function.
    let map2 (f: 'T->'U->_) (State x) (State y) = State (fun s -> let (g, s1) = Tuple2.mapItem1 f (x s) in Tuple2.mapItem1 g (y s1)) : State<'S,'V>

    /// Combines three States into one by applying a mapping function.
    let map3 (f: 'T->'U->'V->_) (State x) (State y) (State z) = State (fun s -> let (g, s1) = Tuple2.mapItem1 f (x s) in let (h, s2) = Tuple2.mapItem1 g (y s1) in Tuple2.mapItem1 h (z s2)) : State<'S,'W>

    let bind  f (State m) = State (fun s -> let (a: 'T, s') = m s in run (f a) s')                                : State<'S,'U>
    let apply (State f) (State x) = State (fun s -> let (f', s1) = f s in let (x': 'T, s2) = x s1 in (f' x', s2)) : State<'S,'U>
    /// Evaluates a <paramref name="sa">state computation</paramref> with the <paramref name="s">initial value</paramref> and return only the result value of the computation. Ignore the final state.
    let eval (State sa) (s: 's)         = fst (sa s) : 'T
    /// Evaluates a <paramref name="sa">state computation</paramref> with the <paramref name="s">initial value</paramref> and return only the final state of the computation. Ignore the result value.
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
    let zip (x: State<'S,'T>) (y: State<'S,'U>) = map2 tuple2 x y : State<'S, ('T * 'U)>

type State<'s,'t> with

    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member Map   (x, f: 'T->_) = State.map f x          : State<'S,'U>

    /// <summary>Lifts a function into a State. Same as map.
    /// To be used in Applicative Style expressions, combined with &lt;*&gt;
    /// </summary>
    /// <category index="1">Functor</category>
    static member (<!>) (f: 'T->'U, x: State<'S, 'T>) : State<'S, 'U> = State.map f x

    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member Lift2 (f: 'T->'U->_, x, y) = State.map2 f x y : State<'S, 'V>

    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member Lift3 (f: 'T->'U->'V->_, x, y, z) = State.map3 f x y z : State<'S, 'W>

    static member Return a = State (fun s -> (a, s))           : State<'S,'T>
    static member (>>=) (x, f: 'T->_) = State.bind f x         : State<'S,'U>
    static member (<*>) (f, x: State<'S,'T>) = State.apply f x : State<'S,'U>

    /// <summary>
    /// Sequences two States left-to-right, discarding the value of the first argument.
    /// </summary>
    /// <category index="2">Applicative</category>
    static member ( *>) (x: State<'S, 'T>, y: State<'S, 'U>) : State<'S, 'U> = ((fun (_: 'T) (k: 'U) -> k) </State.map/> x : State<'S, 'U->'U>) </State.apply/> y

    /// <summary>
    /// Sequences two States left-to-right, discarding the value of the second argument.
    /// </summary>
    /// <category index="2">Applicative</category>
    static member (<* ) (x: State<'S, 'U>, y: State<'S, 'T>) : State<'S, 'U> = ((fun (k: 'U) (_: 'T) -> k ) </State.map/> x : State<'S, 'T->'U>) </State.apply/> y

    static member get_Get () = State.get                       : State<'S,'S>

    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member Put x     = State.put x                      : State<'S,unit>

    #if !FABLE_COMPILER || FABLE_COMPILER_3
    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member Zip (x, y) = State.zip x y
    #endif

    static member TryWith (State computation, h)    = State (fun s -> try computation s with e -> State.run (h e) s) : State<'S,'T>
    static member TryFinally (State computation, f) = State (fun s -> try computation s finally f ()) : State<'S,'T>
    static member Using (resource, f: _ -> State<'S,'T>) = State.TryFinally (f resource, fun () -> dispose resource)
    static member Delay (body: unit->State<'S,'T>)  = State (fun s -> State.run (body ()) s) : State<'S,'T>


#if !FABLE_COMPILER || FABLE_COMPILER_3

open FSharpPlus.Control
open FSharpPlus.Internals.Prelude

/// Monad Transformer for State<'S, 'T>
[<Struct>]
type StateT<'s,'``monad<'t * 's>``> = StateT of ('s -> '``monad<'t * 's>``)

/// Basic operations on StateT
[<RequireQualifiedAccess>]
module StateT =
    /// Runs the state with an inital state to get back the result and the new state wrapped in an inner monad.
    let run (StateT x) = x : 'S -> '``Monad<'T * 'S>``

    /// Embed a Monad<'T> into a StateT<'S,'``Monad<'T * 'S>``>
    let inline lift (m: '``Monad<'T>``) : StateT<'S,'``Monad<'T * 'S>``> =
        if opaqueId false then StateT <| fun s -> (m |> liftM (fun a -> (a, s)))
        else StateT <| fun s -> (m |> map (fun a -> (a, s)))

    /// Transform a State<'S, 'T> to a StateT<'S, '``Monad<'T * 'S>``>
    let inline hoist (x: State<'S, 'T>) = (StateT << (fun a -> result << a) << State.run) x : StateT<'S, '``Monad<'T * 'S>``>

    let inline map (f: 'T->'U) (StateT (m :_->'``Monad<'T * 'S>``)) = StateT (m >> Map.Invoke (fun (a, s') -> (f a, s'))) : StateT<'S,'``Monad<'U * 'S>``>
    
    /// Combines two StateTs into one by applying a mapping function.
    let inline map2 (f: 'T->'U->'V) (StateT x: StateT<'S,'``Monad<'T * 'S>``>) (StateT y: StateT<'S,'``Monad<'U * 'S>``>) : StateT<'S,'``Monad<'V * 'S>``> = StateT (fun s -> x s >>= fun (g, s1) -> y s1 >>= fun (h, s2) -> result (f g h, s2)) : StateT<'S,'``Monad<'V * 'S>``>

    /// Combines three StateTs into one by applying a mapping function.
    let inline map3 (f: 'T->'U->'V->'W) (StateT x: StateT<'S,'``Monad<'T * 'S>``>) (StateT y: StateT<'S,'``Monad<'U * 'S>``>) (StateT z: StateT<'S,'``Monad<'V * 'S>``>) : StateT<'S,'``Monad<'W * 'S>``> =
        StateT (fun s -> x s >>= fun (g, s1) -> y s1 >>= fun (h, s2) -> z s2 >>= fun (i, s3) -> result (f g h i, s3))

    let inline apply (StateT f: StateT<'S,'``Monad<('T -> 'U) * 'S>``>) (StateT a: StateT<'S,'``Monad<'T * 'S>``>) = StateT (fun s -> f s >>= fun (g, t) -> Map.Invoke (fun (z: 'T, u: 'S) -> ((g z: 'U), u)) (a t)) : StateT<'S,'``Monad<'U * 'S>``>

    /// Zips two StateTs into one.
    let inline zip (x: StateT<'S,'``Monad<'T * 'S>``>) (y: StateT<'S,'``Monad<'U * 'S>``>) = apply (map tuple2 x) y : StateT<'S,'``Monad<('T * 'U) * 'S>``>

    let inline bind (f: 'T->StateT<'S,'``Monad<'U * 'S>``>) (StateT m: StateT<'S,'``Monad<'T * 'S>``>) = StateT <| fun s -> m s >>= (fun (a, s') -> run (f a) s')

type StateT<'s,'``monad<'t * 's>``> with

    static member inline Return (x: 'T) = StateT (fun s -> result (x, s)) : StateT<'S,'``Monad<'T * 'S>``>

    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member inline Map    (x: StateT<'S,'``Monad<'T * 'S>``>, f : 'T->'U)                                = StateT.map   f x : StateT<'S,'``Monad<'U * 'S>``>

    /// <summary>Lifts a function into a StateT. Same as map.
    /// To be used in Applicative Style expressions, combined with &lt;*&gt;
    /// </summary>
    /// <category index="1">Functor</category>
    static member inline (<!>) (f: 'T -> 'U, x: StateT<'S, '``Monad<'T * 'S>``>) : StateT<'S, '``Monad<'U * 'S>``> = StateT.map f x

    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member inline Lift2 (f: 'T->'U->'V, x: StateT<'S,'``Monad<'T * 'S>``>, y: StateT<'S,'``Monad<'U * 'S>``>) : StateT<'S,'``Monad<'V * 'S>``> = StateT.map2 f x y

    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member inline Lift3 (f: 'T->'U->'V->'W, x: StateT<'S,'``Monad<'T * 'S>``>, y: StateT<'S,'``Monad<'U * 'S>``>, z : StateT<'S,'``Monad<'V * 'S>``>) : StateT<'S,'``Monad<'W * 'S>``> = StateT.map3 f x y z

    static member inline (<*>)  (f: StateT<'S,'``Monad<('T -> 'U) * 'S>``>, x: StateT<'S,'``Monad<'T * 'S>``>) = StateT.apply f x : StateT<'S,'``Monad<'U * 'S>``>
    
    /// <summary>
    /// Sequences two States left-to-right, discarding the value of the first argument.
    /// </summary>
    /// <category index="2">Applicative</category>
    static member inline ( *>) (x: StateT<'S, '``Monad<'T * 'S>``>, y: StateT<'S, '``Monad<'U * 'S>``>) : StateT<'S, '``Monad<'U * 'S>``> = ((fun (_: 'T) (k: 'U) -> k) </StateT.map/> x : StateT<'S, '``Monad<('U->'U) * 'S>``>) </StateT.apply/> y

    /// <summary>
    /// Sequences two States left-to-right, discarding the value of the second argument.
    /// </summary>
    /// <category index="2">Applicative</category>
    static member inline (<* ) (x: StateT<'S, '``Monad<'U * 'S>``>, y: StateT<'S, '``Monad<'T * 'S>``>) : StateT<'S, '``Monad<'U * 'S>``> = ((fun (k: 'U) (_: 'T) -> k ) </StateT.map/> x : StateT<'S, '``Monad<('T->'U) * 'S>``>) </StateT.apply/> y
    
    static member inline (>>=)  (x: StateT<'S,'``Monad<'T * 'S>``>, f: 'T->StateT<'S,'``Monad<'U * 'S>``>) = StateT.bind  f x

    static member inline get_Empty () = StateT (fun _ -> getEmpty ()) : StateT<'S,'``MonadPlus<'T * 'S>``>
    static member inline (<|>) (StateT m, StateT n) = StateT (fun s -> m s <|> n s) : StateT<'S,'``MonadPlus<'T * 'S>``>

    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member inline Zip (x: StateT<'S,'``Monad<'T * 'S>``>, y: StateT<'S,'``Monad<'U * 'S>``>) = StateT.zip x y

    static member inline TryWith (source: StateT<'S,'``Monad<'T * 'S>``>, f: exn -> StateT<'S,'``Monad<'T * 'S>``>) = StateT (fun s -> TryWith.InvokeForStrict (fun () -> StateT.run source s) (fun x -> StateT.run (f x) s))
    static member inline TryFinally (computation: StateT<'S,'``Monad<'T * 'S>``>, f) = StateT (fun s -> TryFinally.InvokeForStrict (fun () -> StateT.run computation s) f)
    static member inline Using (resource, f: _ -> StateT<'S,'``Monad<'T * 'S>``>)    = StateT (fun s -> Using.Invoke resource (fun x -> StateT.run (f x) s))
    static member inline Delay (body : unit   ->  StateT<'S,'``Monad<'T * 'S>``>)    = StateT (fun s -> Delay.Invoke (fun _ -> StateT.run (body ()) s)) : StateT<'S,'``Monad<'T * 'S>``>

    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member inline Lift (m: '``Monad<'T>``) : StateT<'S,'``Monad<'T * 'S>``> = StateT.lift m

    static member inline LiftAsync (x :Async<'T>) = StateT.lift (liftAsync x) : StateT<'S,'``MonadAsync<'T>``>
    
    static member inline get_Get ()  = StateT (fun s -> result (s , s)) : StateT<'S, '``Monad<'S * 'S>``>
    static member inline Put (x: 'S) = StateT (fun _ -> result ((), x)) : StateT<'S, '``Monad<unit * 'S>``>

    static member inline Throw (x: 'E) = x |> throw |> StateT.lift
    static member inline Catch (m: StateT<'S,'``MonadError<'E1,'T * 'S>``>, h: 'E1 -> _) =
        StateT (fun s -> catch (StateT.run m s) (fun e -> StateT.run (h e) s)) : StateT<'S,'``MonadError<'E2, 'T * 'S>``>

#endif
