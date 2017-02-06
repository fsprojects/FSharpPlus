namespace FSharpPlus.Data

/// <summary> Computation type: Computations which produce a stream of data in addition to the computed values.
/// <para/>   Binding strategy: Combines the outputs of the subcomputations using <c>mappend</c>.
/// <para/>   Useful for: Logging, or other computations that produce output "on the side". </summary>
type Writer<'monoid,'t> = Writer of ('t * 'monoid)

open FSharpPlus

/// Basic operations on Writer
[<RequireQualifiedAccess>]
module Writer =

    /// Unwraps a writer computation as a (result, output) pair. (The inverse of Writer.)
    let run (Writer x) = x                                                                      : 'T * 'Monoid

    let map f (Writer (a:'T, w)) = Writer (f a, w)                                              : Writer<'Monoid,'U>
    let inline bind f (Writer (a:'T, w)) = Writer (let (b, w') = run (f a) in (b, append w w')) : Writer<'Monoid,'U>
    let inline apply  (Writer (f, a)) (Writer (x:'T, b))       = Writer (f x, append a b)       : Writer<'Monoid,'U>

    /// Extract the output from a writer computation.
    let exec (Writer m:Writer<_,'T>) = snd m                                                    : Writer<'Monoid,'U>

    /// Embeds a simple writer action.
    let tell w = Writer((), w)                                                                  : Writer<'Monoid,unit>

    /// <summary> An action that executes the action <paramref name="m"/> and adds its output
    /// to the value of the computation. </summary>
    /// <param name="m">The action to be executed.</param>
    let listen m = let (Writer (a, w)) = m in Writer((a, w), w)                                 : Writer<'Monoid,('T * 'Monoid)>
    
    /// Action that executes the action m, which returns a value and a function, and returns the value, applying the function to the output.
    let pass m = let (Writer((a, f), w:'Monoid)) = m in Writer(a, f w)                          : Writer<'Monoid,'T>

type Writer with
    static member        Map   (x, f:'T->_) = Writer.map f x            : Writer<'Monoid,'U>
    static member inline Return x = Writer (x, getEmpty())              : Writer<'Monoid,'T>
    static member inline Bind  (x, f:'T->_) = Writer.bind f x           : Writer<'Monoid,'U>
    static member inline (<*>) (f, x:Writer<_,'T>) = Writer.apply f x   : Writer<'Monoid,'U>

    static member        Tell   w = Writer.tell w                       : Writer<'Monoid,unit>
    static member        Listen m = Writer.listen m                     : Writer<'Monoid,('T * 'Monoid)>
    static member        Pass   m = Writer.pass m                       : Writer<'Monoid,'T>

    static member        Extract (Writer (_ : 'W, a : 'T)) = a
    static member        Extend  (Writer (w : 'W, _ : 'T) as g, f : Writer<_,_> -> 'U) = Writer (w, f g)

open FsControl

/// Monad Transformer for Writer<'Monoid, 'T>
type WriterT<'``monad<'t * 'monoid>``> = WriterT of '``monad<'t * 'monoid>``

/// Basic operations on WriterT
[<RequireQualifiedAccess>]
module WriterT =

    let run (WriterT x) = x : '``Monad<'T * 'Monoid>``

    let inline map (f:'T->'U) (WriterT m:WriterT<'``Monad<'T * 'Monoid>``>) =
        let mapWriter f (a, m) = (f a, m)
        WriterT (map (mapWriter f) m) : WriterT<'``Monad<'U * 'Monoid>``>

    let inline apply (WriterT f : WriterT<'``Monad<('T -> 'U) * 'Monoid>``>) (WriterT x : WriterT<'``Monad<'T * 'Monoid>``>) =
        let applyWriter (a, w) (b, w') = (a b, append w w')
        WriterT (result applyWriter <*> f <*> x) : WriterT<'``Monad<'U * 'Monoid>``>
        
    let inline bind (f:'T->WriterT<'``Monad<'U * 'Monoid>``>) (WriterT (m:'``Monad<'T * 'Monoid>``)) = 
        WriterT (m >>= (fun (a, w) -> run (f a) >>= (fun (b, w') -> result (b, append w w'))))  : WriterT<'``Monad<'U * 'Monoid>``>
    

type WriterT with

    static member inline Return (x : 'T) = WriterT (result (x, getEmpty()))                                                                 : WriterT<'``Monad<'T * 'Monoid>``>
    static member inline Map    (x : WriterT<'``Monad<'T * 'Monoid>``>, f : 'T -> 'U)                                   = WriterT.map f x   : WriterT<'``Monad<'U * 'Monoid>``>
    static member inline (<*>)  (f : WriterT<'``Monad<('T -> 'U) * 'Monoid>``>, x : WriterT<'``Monad<'T * 'Monoid>``>)  = WriterT.apply f x : WriterT<'``Monad<'U * 'Monoid>``>
    static member inline Bind   (x : WriterT<'``Monad<'T * 'Monoid>``>, f :'T -> _)                                     = WriterT.bind f x  : WriterT<'``Monad<'U * 'Monoid>``>

    static member inline get_MZero () = WriterT (getMZero()) : WriterT<'``MonadPlus<'T * 'Monoid>``>
    static member inline MPlus (WriterT m, WriterT n) = WriterT (m <|> n) : WriterT<'``MonadPlus<'T * 'Monoid>``>

    static member inline Tell   (w:'Monoid) = WriterT (result ((), w))                                                                                          : WriterT<'``Monad<unit * 'Monoid>``>
    static member inline Listen (WriterT m: WriterT<'``Monad<('T * ('Monoid'T -> 'Monoid)) * 'Monoid>``>) = WriterT (m >>= (fun (a, w) -> result ((a, w), w)))  : WriterT<'``Monad<('T * 'Monoid) * 'Monoid>``>
    static member inline Pass   (WriterT m: WriterT<'``Monad<'T * 'Monoid>``>) = WriterT (m >>= (fun ((a, f), w) -> result (a, f w)))                           : WriterT<'``Monad<'T * 'Monoid>``>

    static member inline Lift (m:'``Monad<'T>``) : WriterT<'``Monad<'T * 'Monoid>``> = WriterT (m >>= (fun a -> result (a, getEmpty())))
    
    static member inline LiftAsync (x: Async<'T>) = lift (liftAsync x) : '``WriterT<'MonadAsync<'T>>``

    static member inline Throw (x: 'E) = x |> throw |> lift
    static member inline Catch (m:WriterT<'``MonadError<'E2, 'T * 'Monoid>``> , h:'E2 -> _) = 
            WriterT (catch (WriterT.run m) (WriterT.run << h)) : WriterT<'``MonadChoice<'T * 'Monoid, 'E2>``>

    static member inline CallCC (f : ('a->WriterT<Cont<'r,'t>>)->_)  : WriterT<'``MonadCont<'r,'a*'b>``> = 
        WriterT (callCC <| fun c -> WriterT.run (f (fun a -> WriterT <| c (a, getEmpty()))))
       
    static member inline get_Ask()                     = lift ask               : '``WriterT<'MonadReader<'R,'R*'Monoid>>``
    static member inline Local (WriterT m, f:'R1->'R2) = WriterT (local f m)    : WriterT<'``MonadReader<'R1,'T*'Monoid>``>

    static member inline get_Get()  = lift get           : '``WriterT<'MonadState<'S,'S*'Monoid>>``  
    static member inline Put (x:'S) = x |> put |> lift   : '``WriterT<'MonadState<'S,unit*'Monoid>>``