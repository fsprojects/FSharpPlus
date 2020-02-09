namespace FSharpPlus.Control

open System
open System.Runtime.CompilerServices
open System.Runtime.InteropServices
open System.Text
open System.Collections.Generic
open System.Threading.Tasks
open Microsoft.FSharp.Quotations

open FSharpPlus.Internals
open FSharpPlus.Internals.Prelude
open FSharpPlus

// ArrowChoice class ------------------------------------------------------

#nowarn "0077"

type Fanin =
    inherit Default1
    static member ``|||`` (f:  'T -> 'V  , g: 'U -> 'V   , [<Optional>]_output: Choice<'U,'T> -> 'V   , [<Optional>]_mthd: Fanin) = Choice.either g f                                        : Choice<'U,'T> -> 'V
    static member ``|||`` (f: Func<'T,'V>, g: Func<'U,'V>, [<Optional>]_output: Func<Choice<'U,'T>,'V>, [<Optional>]_mthd: Fanin) = Func<Choice<'U,'T>,'V> (Choice.either g.Invoke f.Invoke) : Func<Choice<'U,'T>,'V>

    static member inline Invoke (f: '``ArrowChoice<'T,'V>``) (g: '``ArrowChoice<'U,'V>``) : '``ArrowChoice<Choice<'U,'T>,'V>`` =
        let inline call (mthd: ^M, output: ^R) = ((^M or ^R) : (static member ``|||`` : _*_*_*_ -> _) f, g, output, mthd)
        call (Unchecked.defaultof<Fanin>, Unchecked.defaultof<'``ArrowChoice<Choice<'U,'T>,'V>``>)

    static member inline InvokeOnInstance (f: '``ArrowChoice<'T,'V>``) (g: '``ArrowChoice<'U,'V>``) : '``ArrowChoice<Choice<'U,'T>,'V>`` = (^``ArrowChoice<Choice<'U,'T>,'V>`` : (static member (|||) : _*_ -> _) f, g)

#if !FABLE_COMPILER
type Fanin with
    static member inline ``|||`` (f: '``ArrowChoice<'T,'V>``, g: '``ArrowChoice<'U,'V>``, _output: '``ArrowChoice<Choice<'U,'T>,'V>``, _mthd: Default1) = Fanin.InvokeOnInstance f g : '``ArrowChoice<Choice<'U,'T>,'V>``
    static member inline ``|||`` (_: '``ArrowChoice<'T,'V>``, _: '``ArrowChoice<'U,'V>``, _output: ^t when ^t : null and ^t : struct , _mthd: Default1) = id
#endif


type AcMerge =
    inherit Default1
    static member ``+++`` (f: 'T1 -> 'U1   , g: 'T2 -> 'U2   , [<Optional>]_output:  Choice<'T2,'T1> ->  Choice<'U2,'U1> , [<Optional>]_mthd: AcMerge) = Fanin.Invoke (Choice2Of2 << f) (Choice1Of2 << g)                                       : Choice<'T2,'T1> ->  Choice<'U2,'U1>
    static member ``+++`` (f: Func<'T1,'U1>, g: Func<'T2,'U2>, [<Optional>]_output: Func<Choice<'T2,'T1>,Choice<'U2,'U1>>, [<Optional>]_mthd: AcMerge) = Fanin.Invoke (Func<_,_> (Choice2Of2 << f.Invoke)) (Func<_,_> (Choice1Of2 << g.Invoke)) : Func<Choice<'T2,'T1>,Choice<'U2,'U1>>

    static member inline Invoke (f: '``ArrowChoice<'T1,'U1>``) (g: '``ArrowChoice<'T2,'U2>``) : '``ArrowChoice<Choice<'T2,'T1>,Choice<'U2,'U1>>`` =
        let inline call (mthd: ^M, output: ^R) = ((^M or ^R) : (static member ``+++`` : _*_*_*_ -> _) f, g, output, mthd)
        call (Unchecked.defaultof<AcMerge>, Unchecked.defaultof<'``ArrowChoice<Choice<'T2,'T1>,Choice<'U2,'U1>>``>)

    static member inline InvokeOnInstance (f: '``ArrowChoice<'T1,'U1>``) (g: '``ArrowChoice<'T2,'U2>``) : '``ArrowChoice<Choice<'T2,'T1>,Choice<'U2,'U1>>`` = (^``ArrowChoice<Choice<'T2,'T1>,Choice<'U2,'U1>>`` : (static member (+++) : _*_ -> _) f, g)

#if !FABLE_COMPILER
type AcMerge with
    static member inline ``+++`` (f: '``ArrowChoice<'T1,'U1>``, g: '``ArrowChoice<'T2,'U2>``, _output: '``ArrowChoice<Choice<'T2,'T1>,Choice<'U2,'U1>>``, _mthd: Default1) = AcMerge.InvokeOnInstance f g : '``ArrowChoice<Choice<'T2,'T1>,Choice<'U2,'U1>>``
    static member inline ``+++`` (_: '``ArrowChoice<'T1,'U1>``, _: '``ArrowChoice<'T2,'U2>``, _output: ^t when ^t : null and ^t : struct                , _mthd: Default1) = id
#endif


type AcLeft =
    inherit Default1
    static member inline Left (f:  'T -> 'U   , [<Optional>]_output:   Choice<'V,'T> -> Choice<'V,'U> , [<Optional>]_mthd: AcLeft) = AcMerge.Invoke f id : Choice<'V,'T> -> Choice<'V,'U>
    static member inline Left (f: Func<'T,'U> , [<Optional>]_output: Func<Choice<'V,'T>,Choice<'V,'U>>, [<Optional>]_mthd: AcLeft) = AcMerge.Invoke f (Func<'V,_> id)

    static member inline Invoke (f: '``ArrowChoice<'T,'U>``) : '``ArrowChoice<Choice<'V,'T>,Choice<'V,'U>>`` =
        let inline call (mthd: ^M, source: ^I, output: ^R) = ((^M or ^I or ^R) : (static member Left : _*_*_ -> _) source, output, mthd)
        call (Unchecked.defaultof<AcLeft>, f, Unchecked.defaultof<'``ArrowChoice<Choice<'V,'T>,Choice<'V,'U>>``>)

    static member inline InvokeOnInstance (f: '``ArrowChoice<'T,'U>``) : '``ArrowChoice<Choice<'V,'T>,Choice<'V,'U>>`` = ((^``ArrowChoice<'T,'U>`` or ^``ArrowChoice<Choice<'V,'T>,Choice<'V,'U>>``) : (static member Left : _ -> _) f)

#if !FABLE_COMPILER
type AcLeft with
    static member inline Left (f: '``ArrowChoice<'T,'U>``, _output: '``ArrowChoice<Choice<'V,'T>,Choice<'V,'U>>``, _mthd: Default1) = AcLeft.InvokeOnInstance f: '``ArrowChoice<Choice<'V,'T>,Choice<'V,'U>>``
    static member inline Left (_: '``ArrowChoice<'T,'U>``, _output: ^t when ^t : null and ^t : struct            , _mthd: Default1) = id
#endif

type AcRight =
    inherit Default1
    static member inline Right (f:  'T -> 'U   , [<Optional>]_output:   Choice<'T,'V> -> Choice<'U,'V> , [<Optional>]_mthd: AcRight) = AcMerge.Invoke id f : Choice<'T,'V> -> Choice<'U,'V>
    static member inline Right (f: Func<'T,'U> , [<Optional>]_output: Func<Choice<'T,'V>,Choice<'U,'V>>, [<Optional>]_mthd: AcRight) = AcMerge.Invoke (Func<_,'V> id) f

    static member inline Invoke (f: '``ArrowChoice<'T,'U>``) : '``ArrowChoice<Choice<'T,'V>,Choice<'U,'V>>``   =
        let inline call (mthd: ^M, source: ^I, output: ^R) = ((^M or ^I or ^R) : (static member Right : _*_*_ -> _) source, output, mthd)
        call (Unchecked.defaultof<AcRight>, f, Unchecked.defaultof<'``ArrowChoice<Choice<'T,'V>,Choice<'U,'V>>``>)

    static member inline InvokeOnInstance (f: '``ArrowChoice<'T,'U>``) : '``ArrowChoice<Choice<'V,'T>,Choice<'U,'V>>`` = ((^``ArrowChoice<'T,'U>`` or ^``ArrowChoice<Choice<'V,'T>,Choice<'U,'V>>``) : (static member Right : _ -> _) f)

#if !FABLE_COMPILER
type AcRight with
    static member inline Right (f: '``ArrowChoice<'T,'U>``, _output: '``ArrowChoice<Choice<'V,'T>,Choice<'U,'V>>``, _mthd: Default1) = AcRight.InvokeOnInstance f : '``ArrowChoice<Choice<'V,'T>,Choice<'U,'V>>``
    static member inline Right (_: '``ArrowChoice<'T,'U>``, _output: ^t when ^t : null and ^t : struct            , _mthd: Default1) = id
#endif