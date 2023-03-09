namespace FSharpPlus.Data

#nowarn "0193"
#nowarn "0193"

open System.ComponentModel
open FSharpPlus
open FSharpPlus.Internals.Prelude
open FSharpPlus.Control


/// <summary> Computation type: Computations which produce a stream of data in addition to the computed values.
/// <para/>   Binding strategy: Combines the outputs of the subcomputations using <c>mappend</c>.
/// <para/>   Useful for: Logging, or other computations that produce output "on the side". </summary>
[<Struct>]
type Writer<'monoid,'t> = Writer of ('t * 'monoid)

/// Basic operations on Writer
[<RequireQualifiedAccess>]
module Writer =

    /// Unwraps a writer computation as a (result, output) pair. (The inverse of Writer.)
    let run (Writer x) = x : 'T * 'Monoid

    let map f (Writer (a: 'T, w)) = Writer (f a, w)                                            : Writer<'Monoid,'U>


    #if !FABLE_COMPILER || FABLE_COMPILER_3

    /// Combines two Writers into one by applying a mapping function.
    let inline map2 f (Writer (a: 'T, w1)) (Writer (b: 'U, w2)) = Writer (f a b, w1 ++ w2) : Writer<'Monoid,'V>

    /// Combines three Writers into one by applying a mapping function.
    let inline map3 f (Writer (a: 'T, w1)) (Writer (b: 'U, w2)) (Writer (c: 'V, w3)) = Writer (f a b c, w1 ++ w2 ++ w3) : Writer<'Monoid,'W>

    let inline bind f (Writer (a: 'T, w)) = Writer (let (b, w') = run (f a) in (b, plus w w')) : Writer<'Monoid,'U>
    let inline apply  (Writer (f, a)) (Writer (x: 'T, b))       = Writer (f x, plus a b)       : Writer<'Monoid,'U>

    #else

    /// Combines two Writers into one by applying a mapping function.
    let inline map2 f (Writer (a: 'T, w1)) (Writer (b: 'U, w2)) = Writer (f a b, w1 + w2)   : Writer<'Monoid,'V>

    /// Combines three Writers into one by applying a mapping function.
    let inline map3 f (Writer (a: 'T, w1)) (Writer (b: 'U, w2)) (Writer (c: 'V, w3)) = Writer (f a b c, w1 + w2 + w3) : Writer<'Monoid, 'W>

    let inline bind f (Writer (a: 'T, w)) = Writer (let (b, w') = run (f a) in (b, w + w')) : Writer<'Monoid,'U>
    let inline apply  (Writer (f, a)) (Writer (x: 'T, b))       = Writer (f x, a + b)       : Writer<'Monoid,'U>

    #endif

    /// Extract the output from a writer computation.
    let exec (Writer m:Writer<'Monoid,'T>) = snd m : 'Monoid

    /// Embeds a simple writer action.
    let tell w = Writer((), w) : Writer<'Monoid,unit>

    /// <summary> An action that executes the action <paramref name="m"/> and adds its output
    /// to the value of the computation. </summary>
    /// <param name="m">The action to be executed.</param>
    let listen m = let (Writer (a, w)) = m in Writer((a, w), w) : Writer<'Monoid,('T * 'Monoid)>
    
    /// Action that executes the action m, which returns a value and a function, and returns the value, applying the function to the output.
    let pass m = let (Writer((a, f), w: 'Monoid)) = m in Writer(a, f w) : Writer<'Monoid,'T>

type Writer<'monoid,'t> with

    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member        Map   (x, f: 'T->_) = Writer.map f x           : Writer<'Monoid,'U>

    /// <summary>Lifts a function into a Writer. Same as map.
    /// To be used in Applicative Style expressions, combined with &lt;*&gt;
    /// </summary>
    /// <category index="1">Functor</category>
    static member (<!>) (f: 'T -> _, x) : Writer<'Monoid, 'U> = Writer.map f x

    #if !FABLE_COMPILER
    static member inline Return x = Writer (x, getZero ())              : Writer<'Monoid,'T>
    #else
    static member inline Return x = Writer (x, LanguagePrimitives.GenericZero) : Writer<'Monoid,'T>
    #endif

    static member inline (>>=) (x, f: 'T->_) = Writer.bind f x          : Writer<'Monoid,'U>

    /// <summary>
    /// Composes left-to-right two Writer functions (Kleisli composition).
    /// </summary>
    /// <category index="2">Monad</category>
    static member inline (>=>) (f, (g: 'U -> _)) : 'T -> Writer<'Monoid, 'V> = fun x -> Writer.bind g (f x)

    static member inline (<*>) (f, x: Writer<_,'T>) = Writer.apply f x  : Writer<'Monoid,'U>

    /// <summary>
    /// Sequences two Writers left-to-right, discarding the value of the first argument.
    /// </summary>
    /// <category index="2">Applicative</category>
    static member inline ( *>) (x: Writer<'Monoid, 'T>, y: Writer<'Monoid, 'U>) : Writer<'Monoid, 'U> = ((fun (_: 'T) (k: 'U) -> k) </Writer.map/> x : Writer<'Monoid, 'U -> 'U>) </Writer.apply/> y

    /// <summary>
    /// Sequences two Writers left-to-right, discarding the value of the second argument.
    /// </summary>
    /// <category index="2">Applicative</category>
    static member inline (<* ) (x: Writer<'Monoid, 'U>, y: Writer<'Monoid, 'T>) : Writer<'Monoid, 'U> = ((fun (k: 'U) (_: 'T) -> k ) </Writer.map/> x : Writer<'Monoid, 'T -> 'U>) </Writer.apply/> y

    static member        Tell   w = Writer.tell w                       : Writer<'Monoid,unit>
    static member        Listen m = Writer.listen m                     : Writer<'Monoid,('T * 'Monoid)>
    static member        Pass   m = Writer.pass m                       : Writer<'Monoid,'T>

    static member        Extract (Writer (_: 'W, a: 'T)) = a
    static member        (=>>)   (Writer (w: 'W, _: 'T) as g, f : Writer<_,_> -> 'U) = Writer (w, f g)

#if !FABLE_COMPILER || FABLE_COMPILER_3


/// Monad Transformer for Writer<'Monoid, 'T>
[<Struct>]
type WriterT<'monoid, 'monad, 't> =
    /// Rerepsenmted as 'monad<'t * 'monoid>
    Value of obj

type [<AutoOpen>]WriterTOperations =
    [<GeneralizableValue>]
    static member inline WriterT< ^``monad<'t * 'monoid>``, ^monad, 'monoid, 't when (Map or  ^``monad<'t * 'monoid>`` or  ^monad) : (static member Map: ( ^``monad<'t * 'monoid>`` * ('t * 'monoid -> __)) * Map ->  ^monad)
                                                                     and  (Map or  ^monad or  ^``monad<'t * 'monoid>``) : (static member Map: ( ^monad * (__ -> 't * 'monoid)) * Map ->  ^``monad<'t * 'monoid>``)
                                                                        > (f: '``monad<'t * 'monoid>``) : WriterT<'monoid,'monad,'t> =
        if opaqueId false then
            let _: 'monad = Unchecked.defaultof<'``monad<'t * 'monoid>``> |> map (fun (_: 't * 'monoid) -> Unchecked.defaultof<__>)
            let _: '``monad<'t * 'monoid>`` = Unchecked.defaultof<'monad> |> map (fun (_: __) -> Unchecked.defaultof<'t * 'monoid>)
            ()
        Value (f |> box)

module [<AutoOpen>]WriterTOperations =
    let inline writerT (x: '``monad<'t * 'monoid>``) : WriterT<'monoid, 'monad, 't> = WriterT x
    let inline (|WriterT|) (Value x: WriterT<'Monoid, 'Monad, 'T>) =
        if opaqueId false then
            let _: '``Monad<'T * 'Monoid>`` = map (fun (_: __) -> Unchecked.defaultof<'T * 'Monoid>) Unchecked.defaultof<'Monad>
            ()
        x |> unbox : '``Monad<'T * 'Monoid>``

/// Basic operations on WriterT
[<RequireQualifiedAccess>]
module WriterT =

    let inline run (WriterT (x : '``Monad<'T * 'Monoid>``) : WriterT<'Monoid, 'Monad, 'T>) = x

    /// Embed a Monad<'T> into a WriterT<'Monad<'T * 'Monoid>>
    let inline lift<'T, .. > (m: '``Monad<'T>``) : WriterT<'Monoid, 'Monad, 'T> =
        WriterT <| (m |> (if opaqueId false then liftM else map) (fun a -> (a, getZero () : 'T * 'Monoid)) : '``Monad<'T * 'Monoid>``)

    let inline map<'T, 'U, .. > (f: 'T -> 'U) (WriterT (m: '``Monad<'T * 'Monoid>``) : WriterT<'Monoid, 'Monad, 'T>) : WriterT<'Monoid, 'Monad, 'U> =
        let mapWriter f (a, m) = (f a, m)
        WriterT (map (mapWriter f: _ -> 'U * 'Monoid) m: '``Monad<'U * 'Monoid>``)

    /// Combines two WriterTs into one by applying a mapping function.
    let inline map2<'T, 'U, 'V, .. > (f: 'T -> 'U -> 'V) (WriterT (x: '``Monad<'T * 'Monoid>``): WriterT<'Monoid, 'Monad, 'T>) (WriterT (y: '``Monad<'U * 'Monoid>``) : WriterT<'Monoid, 'Monad, 'U>) : WriterT<'Monoid, 'Monad, 'V> =
        WriterT (lift2 (fun (x, a: 'Monoid) (y, b: 'Monoid) -> f x y, Plus.Invoke a b) x y : '``Monad<'V * 'Monoid>``)

    /// Combines three WriterTs into one by applying a mapping function.
    let inline map3<'T, 'U, 'V, 'W, .. > (f: 'T -> 'U -> 'V -> 'W) (WriterT (x: '``Monad<'T * 'Monoid>``) : WriterT<'Monoid, 'Monad, 'T>) (WriterT (y: '``Monad<'U * 'Monoid>``) : WriterT<'Monoid, 'Monad, 'U>) (WriterT (z: '``Monad<'V * 'Monoid>``) : WriterT<'Monoid, 'Monad, 'V>) : WriterT<'Monoid, 'Monad, 'W> =
        WriterT (lift3 (fun (x, a: 'Monoid) (y, b: 'Monoid) (z, c: 'Monoid) -> f x y z, a ++ b ++ c) x y z : '``Monad<'W * 'Monoid>``)

    let inline apply<'T, 'U, .. > (WriterT (f: '``Monad<('T -> 'U) * 'Monoid>``) : WriterT<'Monoid, 'Monad, 'T -> 'U>) (WriterT x: WriterT<'Monoid, 'Monad, 'T>) : WriterT<'Monoid, 'Monad, 'U> =
        WriterT ((f >>= fun ((a: 'T -> 'U), w) -> (Map.Invoke (fun (b: 'T, w': 'Monoid) -> ((a b), plus w w')) (x: '``Monad<'T * 'Monoid>``))) : '``Monad<'U * 'Monoid>``)
        
    let inline bind<'T, 'U, .. > (f: 'T -> WriterT<'Monoid, 'Monad, 'U>) (WriterT (m: '``Monad<'T * 'Monoid>``) : WriterT<'Monoid, 'Monad, 'T>) : WriterT<'Monoid, 'Monad, 'U> =
        WriterT (m >>= (fun (a, w) -> (run (f a) : '``Monad<'U * 'Monoid>``) >>= (fun (b, w') -> (result ((b: 'U), (plus w w': 'Monoid)) : '``Monad<'U * 'Monoid>``) )))

type WriterT<'monoid, 'monad, 't> with

    static member inline Return (x: 'T) =
        let _:'``Monad<'T * 'Monoid>`` = 
            if opaqueId false then
                result Unchecked.defaultof<'T * 'Monoid>
            else Unchecked.defaultof<_>
        let _: '``Monad<'T * 'Monoid>`` = 
            if opaqueId false then
                map (fun (_: __) -> Unchecked.defaultof<'T * 'Monoid>) Unchecked.defaultof<'Monad>
            else Unchecked.defaultof<_>
        Value (result (x, getZero ()) : '``Monad<'T * 'Monoid>``) : WriterT<'Monoid,'Monad,'T>

    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member inline Map (x: WriterT<'Monoid, 'Monad, 'T>, f: 'T -> 'U) = WriterT.map f x : WriterT<'Monoid, 'Monad, 'U>

    /// <summary>Lifts a function into a WriterT. Same as map.
    /// To be used in Applicative Style expressions, combined with &lt;*&gt;
    /// </summary>
    /// <category index="1">Functor</category>
    static member inline (<!>) (f: 'T -> 'U, x: WriterT<'Monoid, 'Monad, 'T>) : WriterT<'Monoid, 'Monad, 'U> = WriterT.map<_, _, _, 'Monad, '``Monad<'T * 'Monoid>``, '``Monad<'U * 'Monoid>``> f x

    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member inline Lift2 (f: 'T -> 'U -> 'V, x: WriterT<'Monoid, 'Monad, 'T>, y: WriterT<'Monoid, 'Monad, 'U>) : WriterT<'Monoid, 'Monad, 'V> =
        WriterT.map2<'T, 'U, 'V, 'Monoid, 'Monad, '``Monad<'T * 'Monoid>``, '``Monad<'U * 'Monoid>``, '``Monad<'V * 'Monoid>``> f x y

    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member inline Lift3 (f: 'T -> 'U -> 'V -> 'W, x: WriterT<'Monoid, 'Monad, 'T>, y: WriterT<'Monoid, 'Monad, 'U>, z: WriterT<'Monoid, 'Monad, 'V>) : WriterT<'Monoid, 'Monad, 'W> =
        WriterT.map3<'T, 'U, 'V, 'W, 'Monoid, 'Monad, '``Monad<'T * 'Monoid>``, '``Monad<'U * 'Monoid>``, '``Monad<'V * 'Monoid>``, '``Monad<'W * 'Monoid>``> f x y z

    static member inline (<*>) (f: WriterT<'Monoid, 'Monad, 'T -> 'U>, x: WriterT<'Monoid, 'Monad, 'T>) =
        WriterT.apply<_, _, _, 'Monad, '``Monad<'(T -> 'U) * 'Monoid>``, '``Monad<'U * 'Monoid>``, '``Monad<'T * 'Monoid>``> f x : WriterT<'Monoid, 'Monad, 'U>
    
    /// <summary>
    /// Sequences two Writers left-to-right, discarding the value of the first argument.
    /// </summary>
    /// <category index="2">Applicative</category>
    static member inline ( *>) (x: WriterT<'Monoid, 'Monad, 'T>, y: WriterT<'Monoid, 'Monad, 'U>) : WriterT<'Monoid, 'Monad, 'U> =
        let (<!>) = WriterT.map<_, _, _, 'Monad, '``Monad<'T * 'Monoid>``, '``Monad<('U -> 'U) * 'Monoid>``>
        let (<*>) = WriterT.apply<_, _, _, 'Monad, '``Monad<'(U -> 'U) * 'Monoid>``, '``Monad<'U * 'Monoid>``, '``Monad<'U * 'Monoid>``>
        ((fun (_: 'T) (k: 'U) -> k) <!> x: WriterT<'Monoid, 'Monad, ('U -> 'U)>) <*> y
    
    /// <summary>
    /// Sequences two Writers left-to-right, discarding the value of the second argument.
    /// </summary>
    /// <category index="2">Applicative</category>
    static member inline (<* ) (x: WriterT<'Monoid, 'Monad, 'U>, y: WriterT<'Monoid, 'Monad, 'T>) : WriterT<'Monoid, 'Monad, 'U> =
        let (<!>) = WriterT.map<_, _, _, 'Monad, '``Monad<'U * 'Monoid>``, '``Monad<('T -> 'U) * 'Monoid>``>
        let (<*>) = WriterT.apply<_, _, _, 'Monad, '``Monad<'(T -> 'U) * 'Monoid>``, '``Monad<'U * 'Monoid>``, '``Monad<'T * 'Monoid>``>
        ((fun (k: 'U) (_: 'T) -> k) <!> x: WriterT<'Monoid, 'Monad, ('T -> 'U)>) <*> y
    
    static member inline (>>=) (x: WriterT<'Monoid, 'Monad, 'T>, f: 'T -> _) : WriterT<'Monoid, 'Monad, 'U> =
        WriterT.bind<'T, 'U, 'Monoid, ' Monad, '``Monad<'T * 'Monoid>``, '``Monad<'U * 'Monoid>``> f x

    static member inline get_Empty () : WriterT<'Monoid, 'MonadPlus, 'T> =
        WriterTOperations.WriterT (getEmpty () : '``MonadPlus<'T * 'Monoid>``)

    static member inline (<|>) (WriterT (m: '``MonadPlus<'T * 'S>``), WriterT (n: '``MonadPlus<'T * 'S>``)) : WriterT<'Monoid, 'MonadPlus, 'T> =
        WriterTOperations.WriterT (m <|> n)

    static member inline TryWith (source: unit -> WriterT<'Monoid, 'Monad, 'T>, f: exn -> WriterT<'Monoid, 'Monad, 'T>) =
        WriterTOperations.WriterT< '``Monad<'T * 'Monoid>``, 'Monad, 'Monoid, 'T> (TryWith.Invoke  (fun () -> WriterT.run (source ())) (WriterT.run << f))

    static member inline TryFinally (computation: unit -> WriterT<'Monoid, 'Monad, 'T>, f) = WriterTOperations.WriterT<'``Monad<'T * 'Monoid>``, 'Monad, 'Monoid, 'T> (TryFinally.Invoke (fun () -> WriterT.run (computation ())) f)
    static member inline Using (resource, f: _ -> WriterT<'Monoid, 'Monad, 'T>)    = WriterTOperations.WriterT<'``Monad<'T * 'Monoid>``, 'Monad, 'Monoid, 'T> (Using.Invoke resource (WriterT.run << f))
    static member inline Delay (body: unit ->  WriterT<'Monoid, 'Monad, 'T>) : WriterT<'Monoid, 'Monad, 'T> =
        Value ((Delay.Invoke (fun () -> WriterT.run (body ()) : '``Monad<'T * 'S>``)) |> box<'``Monad<'T * 'S>``>)


    static member inline Tell (w: 'Monoid) : WriterT<'Monoid, 'Monad, unit> =
        WriterTOperations.WriterT (result ((), w) : '``Monad<unit * 'Monoid>``)

    static member inline Listen (WriterT (m: '``Monad<'T * 'Monoid>``) : WriterT<'Monoid, 'Monad, 'T> ) : WriterT<'Monoid, 'Monad, 'T * 'Monoid> =
        WriterTOperations.WriterT ((m >>= (fun ((a: 'T), w: 'Monoid) -> result ((a, w), w))) : '``Monad<('T * 'Monoid) * 'Monoid>``)

    static member inline Pass   (WriterT (m: '``Monad<('T * ('Monoid' -> 'Monoid)) * 'Monoid>``) : WriterT<'Monoid, 'Monad, 'T * ('Monoid' -> 'Monoid)>) : WriterT<'Monoid, 'Monad, 'T> =
        WriterTOperations.WriterT ((m >>= (fun ((a, f), w: 'Monoid) -> result ((a: 'T) , (f w: 'Monoid)))) : '``Monad<'T * 'Monoid>``)

    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member inline Lift (m: '``Monad<'T>``) : WriterT<'Monoid, 'Monad, 'T> = WriterT.lift<_, _, '``Monad<'T * 'Monoid>``, _, _> m
    
    static member inline LiftAsync (x: Async<'T>) : WriterT<'Monoid, 'MonadAsync, 'T> = WriterT.lift<_, _, '``MonadAsync<'T * 'Monoid>``, _, _> (liftAsync x: '``MonadAsync<'T>``)

    static member inline Throw (x: 'E) : WriterT<'Monoid, '``MonadError<'E>``, 'T> =
        WriterT.lift<'T, '``MonadError<'E, 'T>``, '``MonadError<'E, 'T *  ^Monoid>``, '``MonadError<'E>``, 'Monoid> (throw x : '``MonadError<'E, 'T>``)

    static member inline Catch (m: WriterT<'Monoid, '``MonadError<'E1>``, 'T>, h: 'E1 -> WriterT<'Monoid, '``MonadError<'E2>``, 'T>) : WriterT<'Monoid, '``MonadError<'E2>``, 'T> =
        WriterTOperations.WriterT (catch (WriterT.run m: '``MonadError<'E1, ('T * 'Monoid)>``) (WriterT.run << h) : '``MonadError<'E2, ('T * 'Monoid)>``)
    
    // 'Monad : MonadCont<'R, 'Monad>
    static member inline CallCC (f: ('T -> WriterT<'Monoid, 'Monad, 'U>) -> WriterT<'Monoid, 'Monad, 'T>) : WriterT<'Monoid, 'Monad, 'T> =
        WriterTOperations.WriterT (callCC <| fun (c: ('T * 'Monoid) -> '``Monad<'U * 'Monoid>``) -> (WriterT.run (f (fun a -> WriterTOperations.WriterT <| c (a, (getZero () : 'Monoid)))) : '``Monad<'T * 'Monoid>``))
       
    // 'Monad : MonadReader<'R, 'Monad>
    static member inline get_Ask () : WriterT<'Monoid, '``MonadReader<'R>``, 'R> = WriterT.lift<_, '``MonadReader<'R, 'R>``, '``MonadReader<'R, ('R * 'Monoid)>``, '``MonadReader<'R>``, _> ask
    static member inline Local (WriterT m : WriterT<'Monoid, '``MonadReader<'R2>``, 'T>, f: 'R1 -> 'R2) : WriterT<'Monoid, '``MonadReader<'R1>``, 'T> =
        WriterTOperations.WriterT (local f (m: '``MonadReader<'R2, 'T * 'Monoid>``) : '``MonadReader<'R1, 'T * 'Monoid>``)
    
    static member inline get_Get () : WriterT<'Monoid, '``MonadState<'S>``, 'S> =
        WriterT.lift<_, '``MonadState<'S, 'S>``, '``MonadState<'S, 'S *  'Monoid>``, '``MonadState<'S>``, _> get

    static member inline Put (x: 'S) : WriterT<'Monoid, '``MonadState<'S>``, unit> =
        x |> put |> WriterT.lift<_, '``MonadState<'S, unit>``, '``MonadState<'S, (unit * 'Monoid)>``, '``MonadState<'S>``, _>

#endif