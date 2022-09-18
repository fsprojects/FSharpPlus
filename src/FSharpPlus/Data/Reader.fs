namespace FSharpPlus.Data

#nowarn "0193"
#nowarn "1125"

open System.ComponentModel
open FSharpPlus
open FSharpPlus.Internals.Prelude
open FSharpPlus.Control


/// <summary> Computation type: Computations which read values from a shared environment.
/// <para/>   Binding strategy: Monad values are functions from the environment to a value. The bound function is applied to the bound value, and both have access to the shared environment.
/// <para/>   Useful for: Maintaining variable bindings, or other shared environment.</summary>
[<Struct>]
type Reader<'r,'t> = Reader of ('r->'t)

/// Basic operations on Reader
[<RequireQualifiedAccess>]
module Reader =
    let run (Reader x) = x                                                 : 'R->'T
    let map  (f: 'T->_ ) (Reader m) = Reader (f << m)                      : Reader<'R,'U>
    let bind (f: 'T->_ ) (Reader m) = Reader (fun r -> run (f (m r)) r)    : Reader<'R,'U>
    let apply (Reader f) (Reader x) = Reader (fun a -> f a ((x: _->'T) a)) : Reader<'R,'U>

    /// Combines two Readers into one by applying a mapping function.
    let map2 (mapping: 'T->'U->'V) (Reader x) (Reader y) = Reader (fun r -> mapping (x r) (y r)) : Reader<'R,'V>

    /// Combines three Readers into one by applying a mapping function.
    let map3 (mapping: 'T->'U->'V->'W) (Reader x) (Reader y) (Reader z) = Reader (fun r -> mapping (x r) (y r) (z r)) : Reader<'R,'W>
    
    /// Zips two Readers into one.
    let zip (x: Reader<'R,'T>) (y: Reader<'R,'U>) = map2 tuple2 x y        : Reader<'R, 'T * 'U>

    /// Retrieves the monad environment.
    let ask = Reader id                                                    : Reader<'R,'R>

    /// <summary> Executes a computation in a modified environment. </summary>
    /// <param name="f"> The function to modify the environment.    </param>
    /// <param name="m"> Reader to run in the modified environment. </param>
    let local (f: 'R1->'R2) m = let (Reader m) = m in Reader (m << f) : Reader<'R1,'T>

type Reader<'r,'t> with

    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member Map   (x: Reader<'R,'T>, f) = Reader.map f x : Reader<'R,'U>

    /// <summary>Lifts a function into a Reader. Same as map.
    /// To be used in Applicative Style expressions, combined with &lt;*&gt;
    /// </summary>
    /// <category index="1">Functor</category>
    static member (<!>) (f: 'T->'U, x: Reader<'R, 'T>) : Reader<'R, 'U> = Reader.map f x

    static member Return x = Reader (fun _ -> x)                 : Reader<'R,'T>
    static member (>>=) (x: Reader<'R,'T>, f) = Reader.bind f x  : Reader<'R,'U>
    static member (<*>) (f, x: Reader<'R,'T>) = Reader.apply f x : Reader<'R,'U>

    /// <summary>
    /// Sequences two Readers left-to-right, discarding the value of the first argument.
    /// </summary>
    /// <category index="2">Applicative</category>
    static member ( *>) (x: Reader<'R, 'T>, y: Reader<'R, 'U>) : Reader<'R, 'U> = ((fun (_: 'T) (k: 'U) -> k) </Reader.map/> x : Reader<'R, 'U -> 'U>) </Reader.apply/> y

    /// <summary>
    /// Sequences two Readers left-to-right, discarding the value of the second argument.
    /// </summary>
    /// <category index="2">Applicative</category>
    static member (<* ) (x: Reader<'R, 'U>, y: Reader<'R, 'T>) : Reader<'R, 'U> = ((fun (k: 'U) (_: 'T) -> k ) </Reader.map/> x : Reader<'R, 'T -> 'U>) </Reader.apply/> y

    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member Lift2 (f, x: Reader<'R, 'T>, y: Reader<'R, 'U>) = Reader.map2 f x y : Reader<'R, 'V>

    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member Lift3 (f, x: Reader<'R, 'T>, y: Reader<'R, 'U>, z: Reader<'R,'V>) = Reader.map3 f x y z : Reader<'R, 'W>

    static member get_Ask () : Reader<'R, 'R> = Reader.ask

    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member Local (m, f: 'R1 -> 'R2) : Reader<'R1, 'T> = Reader.local f m

    #if !FABLE_COMPILER || FABLE_COMPILER_3
    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member Zip (x, y) = Reader.zip x y

    static member inline Extract (Reader (f : 'Monoid -> 'T)) = f (Zero.Invoke ()) : 'T
    static member inline (=>>)   (Reader (g : 'Monoid -> 'T), f : Reader<'Monoid, 'T> -> 'U) : Reader<'Monoid, 'U> = Reader (fun a -> f (Reader (fun b -> (g (Plus.Invoke a b)))))

    #endif

    static member TryWith    (computation: unit -> Reader<_, _>, h) : Reader<'R, 'T> = Reader (fun s -> try (Reader.run (computation ())) s with e -> Reader.run (h e) s)
    static member TryFinally (computation: unit -> Reader<_, _>, f) = Reader (fun s -> try (Reader.run (computation ())) s finally f ())
    static member Using (resource, f: _ -> Reader<'R, 'T>) = Reader.TryFinally ((fun () -> f resource), fun () -> dispose resource)
    static member Delay (body: unit->Reader<'R, 'T>) : Reader<'R, 'T> = Reader (fun s -> Reader.run (body ()) s)


#if !FABLE_COMPILER || FABLE_COMPILER_3

/// Monad Transformer for Reader<'R, 'T>
[<Struct>]
type ReaderT<'r, 'monad, 't> =
    /// Represented as 'r -> 'monad<'t>
    Value of ('r -> obj)

type [<AutoOpen>]ReaderTOperations =
    [<GeneralizableValue>]
    static member inline ReaderT< ^``monad<'t>``, ^monad, 'r, 't when (Map or ^``monad<'t>`` or  ^monad) : (static member Map: (^``monad<'t>`` * ('t -> __)) * Map -> ^monad)
                                                                 and  (Map or ^monad or  ^``monad<'t>``) : (static member Map: (^monad * (__ -> 't)) * Map -> ^``monad<'t>``)
                                                                        > (f: 'r -> '``monad<'t>``) : ReaderT<'r, 'monad, 't> =
        if opaqueId false then
            let _: 'monad = Unchecked.defaultof<'``monad<'t>``> |> map (fun (_: 't) -> Unchecked.defaultof<__>)
            let _: '``monad<'t>`` = Unchecked.defaultof<'monad> |> map (fun (_: __) -> Unchecked.defaultof<'t>)
            ()
        Value (f >> box)

module [<AutoOpen>]ReaderTOperations =
    let inline readerT (x: 'r -> '``monad<'t>``) : ReaderT<'r, 'monad, 't> = ReaderT x
    let inline (|ReaderT|) (Value x: ReaderT<'R, 'Monad, 'T>) =
        if opaqueId false then
            let _: '``Monad<'T>`` = map (fun (_: __) -> Unchecked.defaultof<'T>) Unchecked.defaultof<'Monad>
            ()
        x >> unbox : 'R -> '``Monad<'T>``



/// Basic operations on Reader
[<RequireQualifiedAccess>]
module ReaderT =
    let inline run (ReaderT (x : 'R -> '``Monad<'T>``) : ReaderT<'R, 'Monad, 'T>) = x    
   
    /// Transform a Reader<'R, 'T> to a ReaderT<'R, 'Monad, 'T>
    let inline hoist (x: Reader<'R, 'T>) =
        let _: '``Monad<'T>`` = 
            if opaqueId false then
                map (fun _ -> Unchecked.defaultof<'T>) Unchecked.defaultof<'Monad>
            else Unchecked.defaultof<_>
        (ReaderT << (fun a -> (result: _ -> '``Monad<'T>``) << a) << Reader.run) x : ReaderT<'R, 'Monad, 'T>

    let inline map<'T, 'U, 'R, .. > (f: 'T -> 'U) (ReaderT (m: _ -> '``Monad<'T>``) : ReaderT<'R, 'Monad, 'T>) =
        ReaderT (map f << m : _ -> '``Monad<'U>``) : ReaderT<'R, 'Monad, 'U>

    /// Combines two ReaderTs into one by applying a mapping function.
    let inline map2<'T, 'U, 'V, 'R, .. > (f: 'T -> 'U -> 'V) (ReaderT (x: 'R -> '``Monad<'T>``) : ReaderT<'R, 'Monad, 'T>) (ReaderT (y: 'R -> '``Monad<'U>``) : ReaderT<'R, 'Monad, 'U>) : ReaderT<'R, 'Monad, 'V> =
        ReaderT ((fun a -> lift2 f (x a) (y a)) : 'R -> '``Monad<'V>``)
    
    /// Combines three ReaderTs into one by applying a mapping function.
    let inline map3<'T, 'U, 'V, 'W, 'R, .. > (f: 'T -> 'U -> 'V -> 'W) (ReaderT (x: 'R -> '``Monad<'T>``) : ReaderT<'R, 'Monad, 'T>) (ReaderT (y: 'R -> '``Monad<'U>``) : ReaderT<'R, 'Monad, 'U>) (ReaderT (z: 'R -> '``Monad<'V>``) : ReaderT<'R, 'Monad, 'V>) : ReaderT<'R, 'Monad, 'W> =
        ReaderT ((fun a -> lift3 f (x a) (y a) (z a)) : 'R -> '``Monad<'W>``)
    
    let inline apply<'T, 'U, 'R, .. > (ReaderT (f: 'R -> '``Monad<'T -> 'U>``) : ReaderT<'R, 'Monad, ('T -> 'U)>) (ReaderT x : ReaderT<'R, 'Monad, 'T>) : ReaderT<'R, 'Monad, 'U> =
        ReaderT (fun r -> (f r <*> (x r : '``Monad<'T>``) : '``Monad<'U>``))
    
    /// Zips two ReaderTs into one.
    let inline zip (x: ReaderT<'R, 'Monad, 'T>) (y: ReaderT<'R, 'Monad, 'U>) : ReaderT<'R, 'Monad, ('T * 'U)> = apply (map tuple2 x) y

    let inline bind<'T, 'U, 'R, .. > (f: 'T -> ReaderT<'R, 'Monad, 'U>) (ReaderT m: ReaderT<'R, 'Monad, 'T>) : ReaderT<'R, 'Monad, 'U> =
        ReaderT (fun r -> (m r: '``Monad<'T>``) >>= (fun a -> run (f a) r) : '``Monad<'U>``)

    /// Embed a Monad<'T> into an ReaderT<'R, 'Monad, 'T>
    let inline lift<'T, 'R, .. > (m: '``Monad<'T>``) = ReaderT (fun _ -> m) : ReaderT<'R, 'Monad, 'T>

type ReaderT<'r, 'monad, 't> with

    static member inline Return (x: 'T) =
        let _: '``Monad<'T>`` = 
            if opaqueId false then
                result Unchecked.defaultof<'T>
            else Unchecked.defaultof<_>
        let _: '``Monad<'T>`` = 
            if opaqueId false then
                map (fun (_: __) -> Unchecked.defaultof<'T>) Unchecked.defaultof<'Monad>
            else Unchecked.defaultof<_>
        Value (fun _ -> box (result x : '``Monad<'T>``)) : ReaderT<'R, 'Monad, 'T>
 
    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member inline Map (x: ReaderT<'R, 'Monad, 'T>, f: 'T -> 'U) : ReaderT<'R, 'Monad, 'U> = ReaderT.map f x
 
    /// <summary>Lifts a function into a ReaderT. Same as map.
    /// To be used in Applicative Style expressions, combined with &lt;*&gt;
    /// </summary>
    /// <category index="1">Functor</category>
    static member inline (<!>) (f: 'T -> 'U, x: ReaderT<'R, 'Monad, 'T>) : ReaderT<'R, 'Monad, 'U> = ReaderT.map<_, _, _, 'Monad, '``Monad<'T>``, '``Monad<'U>``> f x
 
    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member inline Lift2 (f: 'T -> 'U -> 'V, x: ReaderT<'R, 'Monad, 'T>, y: ReaderT<'R, 'Monad, 'U>) : ReaderT<'R, 'Monad, 'V> =
        ReaderT.map2<'T, 'U, 'V, 'R, 'Monad, '``Monad<'T>``, '``Monad<'U>``, '``Monad<'V>``> f x y
 
    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member inline Lift3 (f: 'T -> 'U -> 'V -> 'W, x: ReaderT<'R, 'Monad, 'T>, y: ReaderT<'R, 'Monad, 'U>, z: ReaderT<'R, 'Monad, 'V>) : ReaderT<'R, 'Monad, 'W> =
        ReaderT.map3<'T, 'U, 'V, 'W, 'R, 'Monad, '``Monad<'T>``, '``Monad<'U>``, '``Monad<'V>``, '``Monad<'W>``> f x y z
 
    static member inline (<*>) (f: ReaderT<_, 'Monad, ('T -> 'U)>, x: ReaderT<_, 'Monad, 'T>) : ReaderT<'R, 'Monad, 'U> =
        ReaderT.apply<_, _, _, 'Monad, '``Monad<'(T -> 'U)>``, '``Monad<'T>``, '``Monad<'U>``> f x
 
    /// <summary>
    /// Sequences two Readers left-to-right, discarding the value of the first argument.
    /// </summary>
    /// <category index="2">Applicative</category>
    static member inline ( *>) (x: ReaderT<'R, 'Monad, 'T>, y: ReaderT<'R, 'Monad, 'U>) : ReaderT<'R, 'Monad, 'U> =
        let (<!>) = ReaderT.map<_, _, _, 'Monad, '``Monad<'T>``, '``Monad<('U -> 'U)>``>
        let (<*>) = ReaderT.apply<_, _, _, 'Monad, '``Monad<'(U -> 'U)>``, '``Monad<'U>``, '``Monad<'U>``>
        ((fun (_: 'T) (k: 'U) -> k) <!> x: ReaderT<'R, 'Monad, ('U -> 'U)>) <*> y
 
    /// <summary>
    /// Sequences two Readers left-to-right, discarding the value of the second argument.
    /// </summary>
    /// <category index="2">Applicative</category>
    static member inline (<* ) (x: ReaderT<'R, 'Monad, 'U>, y: ReaderT<'R, 'Monad, 'T>) : ReaderT<'R, 'Monad, 'U> =
        let (<!>) = ReaderT.map<_, _, _, 'Monad, '``Monad<'U>``, '``Monad<('T -> 'U)>``>
        let (<*>) = ReaderT.apply<_, _, _, 'Monad, '``Monad<'(T -> 'U)>``, '``Monad<'T>``, '``Monad<'U>``>
        ((fun (k: 'U) (_: 'T) -> k) <!> x: ReaderT<'R, 'Monad, ('T -> 'U)>) <*> y
 
    static member inline (>>=) (x: ReaderT<_, 'Monad, 'T>, f: 'T -> ReaderT<'R, 'Monad, 'U>) : ReaderT<'R, 'Monad, 'U> =
        ReaderT.bind<_, _, _, 'Monad, '``Monad<'T>``, '``Monad<'U>``> f x
     
    static member inline get_Empty () = ReaderTOperations.ReaderT (fun _ -> getEmpty () : '``MonadPlus<'T>``) : ReaderT<'R, 'MonadPlus, 'T>
    static member inline (<|>) (ReaderT (m: 'R -> '``MonadPlus<'T>``) : ReaderT<'R, 'MonadPlus, 'T>, ReaderT (n: 'R -> '``MonadPlus<'T>``) : ReaderT<'R, 'MonadPlus, 'T>) : ReaderT<'R, 'MonadPlus, 'T> =
        ReaderTOperations.ReaderT (fun r -> m r <|> n r)
 
    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member inline Zip (x: ReaderT<'S, 'Monad, 'T>, y: ReaderT<'S, 'Monad, 'U>) = ReaderT.zip x y
 
    static member inline TryWith (source: unit -> ReaderT<'R, 'Monad, 'T>, f: exn -> ReaderT<'R, 'Monad, 'T>) =
        ReaderTOperations.ReaderT<'``Monad<'T>``, 'Monad, 'R, 'T> (fun s -> TryWith.Invoke  (fun () -> ((ReaderT.run (source ()) s: '``Monad<'T>``))) (fun x -> ReaderT.run (f x) s))

    static member inline TryFinally (computation: unit -> ReaderT<'R, 'Monad, 'T>, f) =
        ReaderTOperations.ReaderT<'``Monad<'T>``, 'Monad, 'R, 'T> (fun s -> TryFinally.Invoke (fun () -> ReaderT.run (computation ()) s) f)

    static member inline Using (resource, f: _ -> ReaderT<'R, 'Monad, 'T>) =
        ReaderTOperations.ReaderT<'``Monad<'T>``, 'Monad, 'R, 'T> (fun s -> Using.Invoke resource (fun x -> ReaderT.run (f x) s))

    static member inline Delay (body: unit -> ReaderT<'R, 'Monad, 'T>) : ReaderT<'R, 'Monad, 'T> =
        Value ((fun s -> Delay.Invoke (fun () -> (ReaderT.run (body ()) s : '``Monad<'T>``))) >> box<'``Monad<'T>``>)

    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member inline Lift (m: '``Monad<'T>``) : ReaderT<'R, 'Monad, 'T> = ReaderT.lift m
 
    static member inline LiftAsync (x: Async<'T>) : ReaderT<'R, 'MonadAsync, 'T> = ReaderT.lift (liftAsync x : '``MonadAsync<'T>``)
 
    static member inline CallCC (f: ('T -> ReaderT<'R, '``MonadCont<'C>``, 'U>) -> ReaderT<'R, '``MonadCont<'C>``, 'T>) : ReaderT<'R, '``MonadCont<'C>``, 'T> =
        ReaderTOperations.ReaderT (fun r -> callCC <| fun (c: _ -> '``MonadCont<'C, 'U>``) -> ReaderT.run (f (fun a -> ReaderTOperations.ReaderT (fun _ -> c a))) r: '``MonadCont<'C, 'T>``)
           
    static member inline get_Ask () : ReaderT<'R, 'Monad, 'T> = ReaderTOperations.ReaderT (result: 'R -> '``Monad<'R>``)
    static member inline Local (ReaderT (m: 'R2 -> '``Monad<'T>``) : ReaderT<'R2, 'Monad, 'T>, f: 'R1 -> 'R2) : ReaderT<'R1, 'Monad, 'T> = ReaderTOperations.ReaderT (fun r -> m (f r))
 
    static member inline Throw (x: 'E) : ReaderT<'R, '``MonadError<'E>``, 'T> =
        x |> (throw: 'E -> '``MonadError<'E, 'T>``) |> ReaderT.lift

    static member inline Catch (m: ReaderT<'R, '``MonadError<'E1>``, 'T>, h: 'E1 -> ReaderT<'R, '``MonadError<'E2>``, 'T>) : ReaderT<'R, '``MonadError<'E2>``, 'T> =
        ReaderTOperations.ReaderT (fun s -> catch (ReaderT.run m s : '``MonadError<'E1, 'T>``) (fun e -> ReaderT.run (h e) s : '``MonadError<'E2, 'T>``))
 
     
    static member inline Tell  (w: 'Monoid) : ReaderT<'R, '``MonadWriter<'Monoid>``, unit> =
        ReaderT.lift (tell w: '``MonadWriter<'Monoid, unit>``)

    static member inline Listen (ReaderT m: ReaderT<'R, '``MonadWriter<'Monoid>``, 'T>) : ReaderT<'R, '``MonadWriter<'Monoid>``, ('T * 'Monoid)> =
        ReaderTOperations.ReaderT<'``MonadWriter<'Monoid, ('T * 'Monoid)>``, _, _, _> (fun w -> listen (m w: '``MonadWriter<'Monoid, 'T>``))

    static member inline Pass (ReaderT m: ReaderT<'R, '``MonadWriter<'Monoid>``, ('T * ('Monoid -> 'Monoid))>) : ReaderT<'R, '``MonadWriter<'Monoid>``, 'T> =
        ReaderTOperations.ReaderT (fun w -> pass (m w: '``MonadWriter<'Monoid, ('T * ('Monoid -> 'Monoid))>``) : '``MonadWriter<'Monoid, 'T>``)
 
    static member inline get_Get () : ReaderT<'R, '``MonadState<'S>``, 'S> = ReaderT.lift (get: '``MonadState<'S, 'S>``)
    static member inline Put (x: 'S) : ReaderT<'R, '``MonadState<'S>``, unit> = ReaderT.lift (put x: '``MonadState<'S, unit>``)

#endif
