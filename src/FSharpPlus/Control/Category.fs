namespace FSharpPlus.Control

#nowarn "0077"
#if (!FABLE_COMPILER || FABLE_COMPILER_3) && !FABLE_COMPILER_4

open System
open System.Runtime.InteropServices
open FSharpPlus.Internals


// Category class ---------------------------------------------------------

type Id =
    inherit Default1
    static member Id ([<Optional>]_output:  'T -> 'T  , [<Optional>]_mthd: Id) = id             : 'T -> 'T
    static member Id ([<Optional>]_output: Func<'T,'T>, [<Optional>]_mthd: Id) = Func<'T,'T> id : Func<'T,'T>

    static member inline Invoke () : '``Category<'T,'T>`` =
        let inline call (mthd: ^M, output: ^R) = ((^M or ^R) : (static member Id : _*_ -> _) output, mthd)
        call (Unchecked.defaultof<Id>, Unchecked.defaultof<'``Category<'T,'T>``>)

    static member inline InvokeOnInstance () : '``Category<'T,'T>`` = (^``Category<'T,'T>`` : (static member Id : _) ())

type Id with
    static member inline Id (_output:  '``Category<'T,'T>``        , _mthd: Default1) = Id.InvokeOnInstance () : '``Category<'T,'T>``
    static member inline Id (_output: ^t when ^t:null and ^t:struct, _mthd: Default1) = id                       


type Comp =
    inherit Default1
    static member ``<<<`` (f:  'U -> 'V  , g:  'T -> 'U  , [<Optional>]_output (*: 'T -> 'V   *), [<Optional>]_mthd: Comp) = g >> f : 'T -> 'V
    static member ``<<<`` (f: Func<'U,'V>, g: Func<'T,'U>, [<Optional>]_output (*: Func<'T,'V>*), [<Optional>]_mthd: Comp) = Func<'T,'V> (g.Invoke >> f.Invoke)

    static member inline Invoke (f : '``Category<'U,'V>``) (g : '``Category<'T,'U>``) : '``Category<'T,'V>`` =
        let inline call (mthd: ^M, f: ^I, output: ^R) = ((^M or ^I or ^R) : (static member ``<<<`` : _*_*_*_ -> _) f, g, output, mthd)
        call (Unchecked.defaultof<Comp>, f, Unchecked.defaultof<Comp>)  //Unchecked.defaultof<'``Category<'T,'V>``>)

    static member inline InvokeOnInstance  (f: '``Category<'U,'V>``) (g: '``Category<'T,'U>``) : '``Category<'T,'V>`` = ( ^``Category<'T,'V>`` : (static member (<<<) : _*_ -> _) f, g)
    static member inline InvokeOnInstance' (f: '``Category<'U,'V>``) (g: '``Category<'T,'U>``) : '``Category<'T,'V>`` = ((^``Category<'U,'V>`` or ^``Category<'T,'U>``) : (static member (<<<) : _*_ -> _) f, g) : '``Category<'T,'V>``

type Comp with
    static member inline ``<<<`` (f: '``Category<'U,'V>``, g: '``Category<'T,'U>``, _output (* : '``Category<'T,'V>``   *) , _mthd : Default1) = Comp.InvokeOnInstance' f g : '``Category<'T,'V>``
    static member inline ``<<<`` (f: 'F, g: 'G, _, _mthd: Default1) =         
        let inline ivk (f: 'T) (x: 'U)  = (^T : (static member Invoke : _*_ -> _) f, x)
        let inline h f g x = 
            let i = ivk f x
            ivk g i
        let _ = h f g
        Unchecked.defaultof<ComposedStaticInvokable<'F, 'G>>

#endif