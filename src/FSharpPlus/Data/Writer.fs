namespace FSharpPlus.Data

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


    #if !FABLE_COMPILER

    /// Combines two Writers into one by applying a mapping function.
    let inline map2 f (Writer (a: 'T, w1)) (Writer (b: 'U, w2)) = Writer (f a b, w1 plus w2)   : Writer<'Monoid,'V>

    let inline bind f (Writer (a: 'T, w)) = Writer (let (b, w') = run (f a) in (b, plus w w')) : Writer<'Monoid,'U>
    let inline apply  (Writer (f, a)) (Writer (x: 'T, b))       = Writer (f x, plus a b)       : Writer<'Monoid,'U>

    #else

    /// Combines two Writers into one by applying a mapping function.
    let inline map2 f (Writer (a: 'T, w1)) (Writer (b: 'U, w2)) = Writer (f a b, w1 + w2)   : Writer<'Monoid,'V>

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

    #if !FABLE_COMPILER
    static member inline Return x = Writer (x, getZero ())              : Writer<'Monoid,'T>
    #else
    static member inline Return x = Writer (x, LanguagePrimitives.GenericZero) : Writer<'Monoid,'T>
    #endif

    static member inline (>>=) (x, f: 'T->_) = Writer.bind f x          : Writer<'Monoid,'U>
    static member inline (<*>) (f, x: Writer<_,'T>) = Writer.apply f x  : Writer<'Monoid,'U>

    static member        Tell   w = Writer.tell w                       : Writer<'Monoid,unit>
    static member        Listen m = Writer.listen m                     : Writer<'Monoid,('T * 'Monoid)>
    static member        Pass   m = Writer.pass m                       : Writer<'Monoid,'T>

    static member        Extract (Writer (_: 'W, a: 'T)) = a
    static member        (=>>)   (Writer (w: 'W, _: 'T) as g, f : Writer<_,_> -> 'U) = Writer (w, f g)

#if !FABLE_COMPILER


/// Monad Transformer for Writer<'Monoid, 'T>
[<Struct>]
type WriterT<'``monad<'t * 'monoid>``> = WriterT of '``monad<'t * 'monoid>``

/// Basic operations on WriterT
[<RequireQualifiedAccess>]
module WriterT =

    let run (WriterT x) = x : '``Monad<'T * 'Monoid>``

    /// Embed a Monad<'T> into a WriterT<'Monad<'T * 'Monoid>>
    let inline lift (m: '``Monad<'T>``) : WriterT<'``Monad<'T * 'Monoid>``> =
        if opaqueId false then m |> liftM (fun a -> (a, getZero ())) |> WriterT
        else m |> map (fun a -> (a, getZero ())) |> WriterT

    let inline map (f: 'T->'U) (WriterT m:WriterT<'``Monad<'T * 'Monoid>``>) =
        let mapWriter f (a, m) = (f a, m)
        WriterT (map (mapWriter f) m) : WriterT<'``Monad<'U * 'Monoid>``>

    /// Combines two WriterTs into one by applying a mapping function.
    let inline map2 (f: 'T->'U->'V) (WriterT x: WriterT<'``Monad<option<'T>``>) (WriterT y: WriterT<'``Monad<option<'U>``>) : WriterT<'``Monad<option<'V>``> = WriterT (lift2 (fun (a, x) (b, y) -> Plus.Invoke a b, f x y) x y)

    let inline apply (WriterT f : WriterT<'``Monad<('T -> 'U) * 'Monoid>``>) (WriterT x : WriterT<'``Monad<'T * 'Monoid>``>) =
        let applyWriter (a, w) (b, w') = (a b, plus w w')
        WriterT (result applyWriter <*> f <*> x) : WriterT<'``Monad<'U * 'Monoid>``>
        
    let inline bind (f: 'T->WriterT<'``Monad<'U * 'Monoid>``>) (WriterT (m: '``Monad<'T * 'Monoid>``)) = 
        WriterT (m >>= (fun (a, w) -> run (f a) >>= (fun (b, w') -> result (b, plus w w'))))  : WriterT<'``Monad<'U * 'Monoid>``>

type WriterT<'``monad<'t * 'monoid>``> with

    static member inline Return (x: 'T) = WriterT (result (x, getZero ())) : WriterT<'``Monad<'T * 'Monoid>``>

    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member inline Map   (x: WriterT<'``Monad<'T * 'Monoid>``>, f: 'T -> 'U)                                   = WriterT.map   f x : WriterT<'``Monad<'U * 'Monoid>``>

    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member inline Lift2 (f: 'T->'U->'V, x: WriterT<'``Monad<'T * 'Monoid>``>, y: WriterT<'``Monad<'U * 'Monoid>``>) : WriterT<'``Monad<'V * 'Monoid>``> = WriterT.map2 f x y

    static member inline (<*>) (f: WriterT<'``Monad<('T -> 'U) * 'Monoid>``>, x: WriterT<'``Monad<'T * 'Monoid>``>)  = WriterT.apply f x : WriterT<'``Monad<'U * 'Monoid>``>
    static member inline (>>=) (x: WriterT<'``Monad<'T * 'Monoid>``>, f: 'T -> _)                                    = WriterT.bind  f x : WriterT<'``Monad<'U * 'Monoid>``>

    static member inline get_Empty () = WriterT (getEmpty ()) : WriterT<'``MonadPlus<'T * 'Monoid>``>
    static member inline (<|>) (WriterT m, WriterT n) = WriterT (m <|> n) : WriterT<'``MonadPlus<'T * 'Monoid>``>

    static member inline TryWith (source: WriterT<'``Monad<'T * 'Monoid>``>, f: exn -> WriterT<'``Monad<'T * 'Monoid>``>) = WriterT (TryWith.Invoke (WriterT.run source) (WriterT.run << f))
    static member inline TryFinally (computation: WriterT<'``Monad<'T * 'Monoid>``>, f) = WriterT (TryFinally.Invoke     (WriterT.run computation) f)
    static member inline Using (resource, f: _ -> WriterT<'``Monad<'T * 'Monoid>``>)    = WriterT (Using.Invoke resource (WriterT.run << f))
    static member inline Delay (body : unit   ->  WriterT<'``Monad<'T * 'Monoid>``>)    = WriterT (Delay.Invoke (fun _ -> WriterT.run (body ()))) : WriterT<'``Monad<'T * 'Monoid>``>

    static member inline Tell   (w: 'Monoid) = WriterT (result ((), w))                                                                                        : WriterT<'``Monad<unit * 'Monoid>``>
    static member inline Listen (WriterT m: WriterT<'``Monad<('T * ('Monoid'T -> 'Monoid)) * 'Monoid>``>) = WriterT (m >>= (fun (a, w) -> result ((a, w), w))) : WriterT<'``Monad<('T * 'Monoid) * 'Monoid>``>
    static member inline Pass   (WriterT m: WriterT<'``Monad<'T * 'Monoid>``>) = WriterT (m >>= (fun ((a, f), w) -> result (a, f w)))                          : WriterT<'``Monad<'T * 'Monoid>``>

    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member inline Lift (m: '``Monad<'T>``) : WriterT<'``Monad<'T * 'Monoid>``> = WriterT.lift m
    
    static member inline LiftAsync (x: Async<'T>) = WriterT.lift (liftAsync x) : WriterT<'``MonadAsync<'T>``>

    static member inline Throw (x: 'E) = x |> throw |> WriterT.lift
    static member inline Catch (m: WriterT<'``MonadError<'E2, 'T * 'Monoid>``>, h: 'E2 -> _) =
            WriterT (catch (WriterT.run m) (WriterT.run << h)) : WriterT<'``MonadChoice<'T * 'Monoid, 'E2>``>

    static member inline CallCC (f: ('a->WriterT<Cont<'r,'t>>)->_) : WriterT<'``MonadCont<'r,'a*'b>``> =
        WriterT (callCC <| fun c -> WriterT.run (f (fun a -> WriterT <| c (a, getZero ()))))
       
    static member inline get_Ask ()                     = WriterT.lift ask    : WriterT<'``MonadReader<'R,'R*'Monoid>``>
    static member inline Local (WriterT m, f: 'R1->'R2) = WriterT (local f m) : WriterT<'``MonadReader<'R1,'T*'Monoid>``>

    static member inline get_Get () = WriterT.lift get          : WriterT<'``MonadState<'S,'S*'Monoid>``>
    static member inline Put (x: 'S) = x |> put |> WriterT.lift : WriterT<'``MonadState<'S,unit*'Monoid>``>

#endif