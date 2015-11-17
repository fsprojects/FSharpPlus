namespace FsControl

/// <summary> Computation type: Computations which produce a stream of data in addition to the computed values.
/// <para/>   Binding strategy: Combines the outputs of the subcomputations using <c>mappend</c>.
/// <para/>   Useful for: Logging, or other computations that produce output "on the side". </summary>
type Writer<'monoid,'t> = Writer of ('t * 'monoid)

[<RequireQualifiedAccess>]
module Writer =

    /// Unwraps a writer computation as a (result, output) pair. (The inverse of Writer.)
    let run (Writer x) = x                                                                                  : 'T * 'Monoid

    let map f (Writer (a:'T, w)) = Writer (f a, w)                                                          : Writer<'Monoid,'U>
    let inline bind f (Writer (a:'T, w)) = Writer (let (b, w') = run (f a) in (b, MAppend.Invoke w w'))     : Writer<'Monoid,'U>
    let inline apply  (Writer (f, a)) (Writer (x:'T, b))       = Writer (f x, MAppend.Invoke a b)           : Writer<'Monoid,'U>

    /// Extract the output from a writer computation.
    let exec (Writer m:Writer<_,'T>) = snd m                                                                : Writer<'Monoid,'U>

    /// Embeds a simple writer action.
    let tell w = Writer((), w)                                                                              : Writer<'Monoid,unit>

    /// <summary> An action that executes the action <paramref name="m"/> and adds its output to the value of the computation. </summary>
    /// <param name="m">The action to be executed.</param>
    let listen m = let (Writer (a, w)) = m in Writer((a, w), w)                                             : Writer<'Monoid,('T * 'Monoid)>
    
    /// Action that executes the action m, which returns a value and a function, and returns the value, applying the function to the output.
    let pass m = let (Writer((a, f), w:'Monoid)) = m in Writer(a, f w)                                      : Writer<'Monoid,'T>

type Writer with
    static member        Map   (x, f:'T->_) = Writer.map f x            : Writer<'Monoid,'U>
    static member inline Return x = Writer (x, MEmpty.Invoke())         : Writer<'Monoid,'T>
    static member inline Bind  (x, f:'T->_) = Writer.bind f x           : Writer<'Monoid,'U>
    static member inline (<*>) (f, x:Writer<_,'T>) = Writer.apply f x   : Writer<'Monoid,'U>
    static member        Tell   w = Writer.tell w                       : Writer<'Monoid,unit>
    static member        Listen m = Writer.listen m                     : Writer<'Monoid,('T * 'Monoid)>
    static member        Pass   m = Writer.pass m                       : Writer<'Monoid,'T>