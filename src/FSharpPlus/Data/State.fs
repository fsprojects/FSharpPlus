namespace FSharpPlus.Data

#nowarn "0193"
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

    static member TryWith (computation: unit -> State<_, _>, h)    = State (fun s -> try (State.run (computation ())) s with e -> State.run (h e) s) : State<'S, 'T>
    static member TryFinally (computation: unit -> State<_, _>, f) = State (fun s -> try (State.run (computation ())) s finally f ()) : State<'S, 'T>
    static member Using (resource, f: _ -> State<'S,'T>) = State.TryFinally ((fun () -> f resource), fun () -> dispose resource)
    static member Delay (body: unit -> State<'S,'T>)  = State (fun s -> State.run (body ()) s) : State<'S, 'T>


#if !FABLE_COMPILER || FABLE_COMPILER_3

open FSharpPlus.Control
open FSharpPlus.Internals.Prelude

/// Monad Transformer for State<'S, 'T>
[<Struct>]
type StateT<'s, 'monad, 't> =
    /// Represented as 'monad<'t * 's>
    Value of ('s -> obj)

type [<AutoOpen>]StateTOperations =
    [<GeneralizableValue>]
    static member inline StateT< ^``monad<'t * 's>``, ^monad, 's, 't when (Map or  ^``monad<'t * 's>`` or  ^monad) : (static member Map: ( ^``monad<'t * 's>`` * ('t * 's -> __)) * Map ->  ^monad)
                                                                     and  (Map or  ^monad or  ^``monad<'t * 's>``) : (static member Map: ( ^monad * (__ -> 't * 's)) * Map ->  ^``monad<'t * 's>``)
                                                                        > (f: 's -> '``monad<'t * 's>``) : StateT<'s,'monad,'t> =
        if opaqueId false then
            let _: 'monad = Unchecked.defaultof<'``monad<'t * 's>``> |> map (fun (_: 't * 's) -> Unchecked.defaultof<__>)
            let _: '``monad<'t * 's>`` = Unchecked.defaultof<'monad> |> map (fun (_: __) -> Unchecked.defaultof<'t * 's>)
            ()
        Value (f >> box)

module [<AutoOpen>]StateTOperations =
    let inline stateT (x: 's -> '``monad<'t * 's>``) : StateT<'s, 'monad, 't> = StateT x
    let inline (|StateT|) (Value x: StateT<'S,'Monad,'T>) =
        if opaqueId false then
            let _: '``Monad<'T * 'S>`` = map (fun (_: __) -> Unchecked.defaultof<'T * 'S>) Unchecked.defaultof<'Monad>
            ()
        x >> unbox : 'S -> '``Monad<'T * 'S>``

/// Basic operations on StateT
[<RequireQualifiedAccess>]
module StateT =

    open FSharpPlus.Control

    /// Runs the state with an inital state to get back the result and the new state wrapped in an inner monad.
    let inline run (StateT (x : 'S -> '``Monad<'T * 'S>``) : StateT<'S, 'Monad, 'T>) = x

    /// Embed a Monad<'T> into a StateT<'S, 'Monad, 'T>
    let inline lift<'T, 'S, .. > (m: '``Monad<'T>``) : StateT<'S, 'Monad, 'T> =
        StateT <| fun s -> ((m |> (if opaqueId false then liftM else map) (fun (a: 'T) -> (a, s))) : '``Monad<'T * 'S>``)
    
    /// Transform a State<'S, 'T> to a StateT<'S, '``Monad<'T * 'S>``>
    let inline hoist (x: State<'S, 'T>) =
        let _: '``Monad<'T * 'S>`` = 
            if opaqueId false then
                map (fun _ -> Unchecked.defaultof<'T * 'S>) Unchecked.defaultof<'Monad>
            else Unchecked.defaultof<_>
        (StateT << (fun a -> (result: ('T * 'S) -> '``Monad<'T * 'S>``) << a) << State.run) x : StateT<'S, 'Monad, 'T>

    let inline map<'T, 'U, 'S, .. > (f: 'T -> 'U) (StateT (m: _ -> '``Monad<'T * 'S>``) : StateT<'S, 'Monad, 'T>) =
        StateT (m >> (Map.Invoke (fun (a, s': 'S) -> (f a, s')) : _ -> '``Monad<'U * 'S>``)) : StateT<'S, 'Monad, 'U>

    /// Combines two StateTs into one by applying a mapping function.
    let inline map2<'T, 'U, 'V, 'S, .. > (f: 'T -> 'U -> 'V) (StateT (x: 'S -> '``Monad<'T * 'S>``) : StateT<'S, 'Monad, 'T>) (StateT (y: 'S -> '``Monad<'U * 'S>``) : StateT<'S, 'Monad, 'U>) : StateT<'S, 'Monad, 'V> =
        StateT (fun s -> (x s: '``Monad<'T * 'S>``) >>= fun (g, s1) -> (y s1: '``Monad<'U * 'S>``) >>= fun (h, s2: 'S) -> (result (f g h, s2) : '``Monad<'V * 'S>``))

    /// Combines three StateTs into one by applying a mapping function.
    let inline map3<'T, 'U, 'V, 'W, 'S, .. > (f: 'T -> 'U -> 'V -> 'W) (StateT (x: 'S -> '``Monad<'T * 'S>``) : StateT<'S, 'Monad, 'T>) (StateT (y: 'S -> '``Monad<'U * 'S>``) : StateT<'S, 'Monad, 'U>) (StateT (z: 'S -> '``Monad<'V * 'S>``): StateT<'S, 'Monad, 'V>) : StateT<'S, 'Monad, 'W> =
        StateT (fun s -> (x s: '``Monad<'T * 'S>``) >>= fun (g, s1) -> (y s1: '``Monad<'U * 'S>``) >>= fun (h, s2) -> (z s2: '``Monad<'V * 'S>``) >>= fun (i, s3: 'S) -> (result (f g h i, s3) : '``Monad<'W * 'S>``))
    
    let inline apply<'T, 'U, 'S, .. > (StateT (f: 'S -> '``Monad<('T -> 'U) * 'S>``) : StateT<'S, 'Monad, ('T -> 'U)>) (StateT a: StateT<'S, 'Monad,'T>) : StateT<'S, 'Monad, 'U> =
        StateT (fun s -> f s >>= fun (g, t) -> (Map.Invoke (fun (z: 'T, u: 'S) -> ((g z: 'U), u)) (a t: '``Monad<'T * 'S>``) : '``Monad<'U * 'S>``))

    // /// Zips two StateTs into one.
    let inline zip (x: StateT<'S, 'Monad, 'T>) (y: StateT<'S, 'Monad, 'U>) = apply (map tuple2 x) y : StateT<'S, 'Monad, ('T * 'U)>

    let inline bind<'T, 'U, 'S, .. > (f: 'T -> StateT<'S, 'Monad, 'U>) (StateT m: StateT<'S, 'Monad, 'T>) : StateT<'S ,'Monad, 'U> =
        StateT (fun s -> (m s: '``Monad<'T * 'S>``) >>= (fun (a, s') -> run (f a) s') : '``Monad<'U * 'S>``)        

type StateT<'s, 'monad, 't> with

    static member inline Return (x: 'T) =
        let _: '``Monad<'T * 'S>`` = 
            if opaqueId false then
                result Unchecked.defaultof<'T * 'S>
            else Unchecked.defaultof<_>
        let _: '``Monad<'T * 'S>`` = 
            if opaqueId false then
                map (fun (_: __) -> Unchecked.defaultof<'T * 'S>) Unchecked.defaultof<'Monad>
            else Unchecked.defaultof<_>
        Value (fun s -> box (result (x, s) : '``Monad<'T * 'S>``)) : StateT<'S, 'Monad, 'T>

    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member inline Map (x: StateT<'S, 'Monad, 'T>, f : 'T -> 'U) : StateT<'S, 'Monad, 'U> = StateT.map<_, _, _, 'Monad, '``Monad<'T * 'S>``, '``Monad<'U * 'S>``> f x

    /// <summary>Lifts a function into a StateT. Same as map.
    /// To be used in Applicative Style expressions, combined with &lt;*&gt;
    /// </summary>
    /// <category index="1">Functor</category>
    static member inline (<!>) (f: 'T -> 'U, x: StateT<'S, 'Monad, 'T>) : StateT<'S, 'Monad, 'U> = StateT.map<_, _, _, 'Monad, '``Monad<'T * 'S>``, '``Monad<'U * 'S>``> f x
    
    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member inline Lift2 (f: 'T -> 'U -> 'V, x: StateT<'S, 'Monad, 'T>, y: StateT<'S, 'Monad, 'U>) : StateT<'S, 'Monad, 'V> =
        StateT.map2<'T, 'U, 'V, 'S, 'Monad, '``Monad<'T * 'S>``, '``Monad<'U * 'S>``, '``Monad<'V * 'S>``> f x y
    
    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member inline Lift3 (f: 'T -> 'U -> 'V -> 'W, x: StateT<'S, 'Monad, 'T>, y: StateT<'S, 'Monad, 'U>, z : StateT<'S, 'Monad, 'V>) : StateT<'S, 'Monad, 'W> =
        StateT.map3<'T, 'U, 'V, 'W, 'S, 'Monad, '``Monad<'T * 'S>``, '``Monad<'U * 'S>``, '``Monad<'V * 'S>``, '``Monad<'W * 'S>``> f x y z
    
    static member inline (<*>)  (f: StateT<'S, 'Monad, ('T -> 'U)>, x: StateT<'S, 'Monad, 'T>) =
        StateT.apply<_, _, _, 'Monad, '``Monad<'(T -> 'U) * 'S>``, '``Monad<'U * 'S>``, '``Monad<'T * 'S>``> f x : StateT<'S, 'Monad, 'U>
    
    /// <summary>
    /// Sequences two States left-to-right, discarding the value of the first argument.
    /// </summary>
    /// <category index="2">Applicative</category>
    static member inline ( *>) (x: StateT<'S, 'Monad, 'T>, y: StateT<'S, 'Monad, 'U>) : StateT<'S, 'Monad, 'U> =
        let (<!>) = StateT.map<_, _, _, 'Monad, '``Monad<'T * 'S>``, '``Monad<('U -> 'U) * 'S>``>
        let (<*>) = StateT.apply<_, _, _, 'Monad, '``Monad<'(U -> 'U) * 'S>``, '``Monad<'U * 'S>``, '``Monad<'U * 'S>``>
        ((fun (_: 'T) (k: 'U) -> k) <!> x: StateT<'S, 'Monad, ('U -> 'U)>) <*> y
    
    /// <summary>
    /// Sequences two States left-to-right, discarding the value of the second argument.
    /// </summary>
    /// <category index="2">Applicative</category>
    static member inline (<* ) (x: StateT<'S, 'Monad, 'U>, y: StateT<'S, 'Monad, 'T>) : StateT<'S, 'Monad, 'U> =
        let (<!>) = StateT.map<_, _, _, 'Monad, '``Monad<'U * 'S>``, '``Monad<('T -> 'U) * 'S>``>
        let (<*>) = StateT.apply<_, _, _, 'Monad, '``Monad<'(T -> 'U) * 'S>``, '``Monad<'U * 'S>``, '``Monad<'T * 'S>``>
        ((fun (k: 'U) (_: 'T) -> k) <!> x: StateT<'S, 'Monad, ('T -> 'U)>) <*> y

    static member inline (>>=) (x: StateT<'S, 'Monad, 'T>, f: 'T -> StateT<'S, 'Monad, 'U>) : StateT<'S, 'Monad, 'U> =
        StateT.bind<_, _, _, 'Monad, '``Monad<'T>``, '``Monad<'U>``> f x

    static member inline get_Empty () =
        StateTOperations.StateT (fun _ -> getEmpty () : '``MonadPlus<'T * 'S>``) : StateT<'S, 'MonadPlus, 'T>

    static member inline (<|>) (StateT (m: 'S -> '``MonadPlus<'T * 'S>``) : StateT<'S, 'MonadPlus, 'T>, StateT (n: 'S -> '``MonadPlus<'T * 'S>``) : StateT<'S, 'MonadPlus, 'T>) : StateT<'S, 'MonadPlus, 'T> =
        StateTOperations.StateT (fun s -> m s <|> n s)
    
    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member inline Zip (x: StateT<'S, 'Monad, 'T>, y: StateT<'S, 'Monad, 'U>) = StateT.zip x y
    
    static member inline TryWith (source: unit -> StateT<'S, 'Monad, 'T>, f: exn -> StateT<'S, 'Monad, 'T>) =
        StateTOperations.StateT< '``Monad<'T * 'S>``, 'Monad, 'S, 'T> (fun s -> TryWithS.InvokeFromOtherMonad (fun () -> (StateT.run (source ()) s : '``Monad<'T * 'S>`` )) (fun x -> StateT.run (f x) s))

    static member inline TryFinally (computation: unit -> StateT<'S,'Monad,'T>, f) =
        StateTOperations.StateT< '``Monad<'T * 'S>``, 'Monad, 'S, 'T> (fun s -> TryFinallyS.Invoke (fun () -> StateT.run (computation ()) s) f)

    static member inline Using (resource: 'S, f: _ -> StateT<'S,'Monad,'T>) =
        StateTOperations.StateT< '``Monad<'T * 'S>``, 'Monad, 'S, 'T> (fun s -> Using.Invoke resource (fun x -> StateT.run (f x) s))
    
    static member inline Delay (body: unit -> StateT<'S, 'Monad, 'T>) : StateT<'S, 'Monad, 'T> =
        Value ((fun s -> Delay.Invoke (fun () -> (StateT.run (body ()) s: '``Monad<'T * 'S>``))) >> box<'``Monad<'T * 'S>``>)

    
    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member inline Lift (m: '``Monad<'T>``) : StateT<'S, 'Monad, 'T> = StateT.lift<_, _, _, '``Monad<'T * 'S>``, _> m
  
    static member inline LiftAsync (x: Async<'T>) =
        StateT.lift<_, _, _, '``MonadAsync<'T * 'S>``, _> (liftAsync x: '``MonadAsync<'T>``) : StateT<'S, 'MonadAsync, 'T>
         
    static member inline get_Get ()  =
        StateTOperations.StateT (fun s -> result (s , s) : '``Monad<'S * 'S>``) : StateT<'S, 'Monad, 'S>

    static member inline Put (x: 'S) =
        StateTOperations.StateT (fun _ -> (result ((), x) : '``Monad<unit * 'S>``)) : StateT<'S, 'Monad, unit>

    static member inline Throw (x: 'E) : StateT<'S, '``MonadError<'E>``, 'T> =
        x |> (throw: 'E -> '``MonadError<'E, 'T>``) |> StateT.lift

    static member inline Catch (m: StateT<'S, '``MonadError<'E1>`` ,'T>, h: 'E1 -> StateT<'S, '``MonadError<'E2>``, 'T>) =
        StateTOperations.StateT (fun s -> catch (StateT.run m s: '``MonadError<'E1, ('T * 'S)>``) (fun e -> StateT.run (h e) s: '``MonadError<'E2, ('T * 'S)>``)) : StateT<'S, '``MonadError<'E2>``, 'T>

    static member inline get_Ask () : StateT<'S, '``MonadReader<'R>``, 'R> = StateT.lift<'R, 'S, '``MonadReader<'R, 'R>``, '``MonadReader<'R, ('R * 'S)>``, '``MonadReader<'R>``> ask
    static member inline Local (StateT (m: 'S -> '``MonadReader<'R2, ('T * 'S)>``) : StateT<'S, '``MonadReader<'`R2>``, 'T>, f: 'R1 -> 'R2) : StateT<'S, '``MonadReader<'R1>``, 'T> = StateTOperations.StateT (local f << m: 'S -> '``MonadReader<'R1, ('T * 'S)>``)

#endif
