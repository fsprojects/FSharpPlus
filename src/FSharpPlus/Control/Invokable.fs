namespace FSharpPlus.Control

#if (!FABLE_COMPILER || FABLE_COMPILER_3) && !FABLE_COMPILER_4

open System
open FSharpPlus.Internals


// Invokable class --------------------------------------------------------

type Invoke =
    inherit Default1

    static member inline Invoke (_: ^t when ^t : null and ^t : struct, _, _output: ^O, _mthd: Default1) = id
    static member inline Invoke (_: 'T, x, _output: ^O, _mthd: Default1) = (^T : (static member Invoke : _ -> _) x)

    static member        Invoke (g:  'T -> 'U  , x: 'T, _output: 'U, _mthd: Invoke) = g x        : 'U
    static member        Invoke (g: Func<'T,'U>, x: 'T, _output: 'U, _mthd: Invoke) = g.Invoke x : 'U

    // No return type check
    static member inline InvokeNRTC (f: '``Category<'T,'U>``, x : 'T) =
        let inline call (_: ^I, x: 'TT) = ((^I or ^TT) : (static member Invoke : _-> _) x)
        call (f, x)  

    static member inline Invoke (f: '``Category<'T,'U>``, x: 'T) : 'U =
        let inline call (mthd: ^M, f: ^I, output: ^R, x: 'TT) = ((^M or ^TT) : (static member Invoke : _*_*_*_ -> _) f, x, output, mthd)
        call (Unchecked.defaultof<Invoke>, f, Unchecked.defaultof<'U>, x)

type ComposedStaticInvokable< ^F, ^G>  =
    static member inline Invoke x =
        let i  =  Invoke.Invoke (Unchecked.defaultof<'G>, x)
        Invoke.Invoke (Unchecked.defaultof<'F>, i)

#endif