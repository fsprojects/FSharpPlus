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

#if !FABLE_COMPILER

// Arrow class ------------------------------------------------------------

#nowarn "0077"

type Arr =
    inherit Default1
    static member Arr (f: 'T -> 'U, [<Optional>]_output:  'T-> 'U   , [<Optional>]_mthd: Arr) = f
    static member Arr (f: 'T -> 'U, [<Optional>]_output: Func<'T,'U>, [<Optional>]_mthd: Arr) = Func<'T,'U> f

    static member inline Invoke (f: 'T -> 'U) : '``Arrow<'T,'U>`` = 
        let inline call (mthd: ^M, output: ^R) = ((^M or ^R) : (static member Arr: _*_*_ -> _) f, output, mthd)
        call (Unchecked.defaultof<Arr>, Unchecked.defaultof<'``Arrow<'T,'U>``>)

    static member inline InvokeOnInstance (f: 'T -> 'U) : '``Arrow<'T,'U>`` = (^``Arrow<'T,'U>`` : (static member Arr: _ -> _) f)

type Arr with
    static member inline Arr (f: 'T -> 'U, _output: '``Arrow<'T,'U>``                , _mthd: Default1) = Arr.InvokeOnInstance f : '``Arrow<'T,'U>``
    static member inline Arr (_: 'T -> 'U, _output: ^t when ^t : null and ^t : struct, _mthd: Default1) = id


type ArrFirst =
    inherit Default1
    static member First (f: 'T -> 'U   , [<Optional>]_output:   'T*'V -> 'U*'V  , [<Optional>]_mthd: ArrFirst) =            fun (x, y) -> (f x       , y)  : 'U*'V
    static member First (f: Func<'T,'U>, [<Optional>]_output: Func<'T*'V,'U*'V> , [<Optional>]_mthd: ArrFirst) = Func<_,_> (fun (x, y) -> (f.Invoke x, y)) : Func<'T*'V,'U*'V>

    static member inline Invoke (f: '``Arrow<'T,'U>``) : '``Arrow<('T * 'V),('U * 'V)>`` =
        let inline call (mthd: ^M, source: ^I, output: ^R) = ((^M or ^I or ^R) : (static member First : _*_*_ -> _) source, output, mthd)
        call (Unchecked.defaultof<ArrFirst>, f, Unchecked.defaultof<'``Arrow<('T * 'V),('U * 'V)>``>)

    static member inline InvokeOnInstance (f: '``Arrow<'T,'U>``) : '``Arrow<('T * 'V),('U * 'V)>`` = ((^``Arrow<'T,'U>`` or ^``Arrow<('T * 'V),('U * 'V)>``) : (static member First : _ -> _) f)

type ArrFirst with
    static member inline First (f: '``Arrow<'T,'U>``, _output: '``Arrow<('T * 'V),('U * 'V)>``, _mthd: Default1) = ArrFirst.InvokeOnInstance f  : '``Arrow<('T * 'V),('U * 'V)>``
    static member inline First (_: ^t when ^t : null and ^t : struct  , _output               , _mthd: Default1) = id


type ArrSecond =
    inherit Default1
    static member Second (f: 'T -> 'U   , [<Optional>]_output:   'V*'T -> 'V*'U  , [<Optional>]_mthd: ArrSecond) =            fun (x, y) -> (x,        f y)  : 'V*'U
    static member Second (f: Func<'T,'U>, [<Optional>]_output: Func<'V*'T,'V*'U> , [<Optional>]_mthd: ArrSecond) = Func<_,_> (fun (x, y) -> (x, f.Invoke y)) : Func<'V*'T,'V*'U>

    static member inline Invoke (f: '``Arrow<'T,'U>``) : '``Arrow<('V * 'T),('V * 'U)>`` =
        let inline call (mthd: ^M, source: ^I, output: ^R) = ((^M or ^I or ^R) : (static member Second : _*_*_ -> _) source, output, mthd)
        call (Unchecked.defaultof<ArrSecond>, f, Unchecked.defaultof<'``Arrow<('V * 'T),('V * 'U)>``>)

    static member inline InvokeOnInstance (f: '``Arrow<'T,'U>``) : '``Arrow<('V * 'T),('V * 'U)>`` = ((^``Arrow<'T,'U>`` or ^``Arrow<('V * 'T),('V * 'U)>``) : (static member Second : _ -> _) f)

type ArrSecond with
    static member inline Second (f: '``Arrow<'T,'U>``, _output: '``Arrow<('V * 'T),('V * 'U)>``, _mthd: Default2 ) : '``Arrow<('V * 'T),('V * 'U)>`` = 
        let arrSwap = Arr.InvokeOnInstance (fun (x, y) -> (y, x))
        Comp.InvokeOnInstance arrSwap (Comp.InvokeOnInstance (ArrFirst.InvokeOnInstance f) arrSwap)

    static member inline Second (f: '``Arrow<'T,'U>``, _output: '``Arrow<('V * 'T),('V * 'U)>``, _mthd: Default1) = ArrSecond.InvokeOnInstance f : '``Arrow<('V * 'T),('V * 'U)>``
    static member inline Second (_: ^t when ^t : null and ^t : struct  , _output               , _mthd: Default1) = id


type ArrCombine =
    inherit Default1
    static member ``***`` (f: 'T1 -> 'U1   , g: 'T2 -> 'U2   , [<Optional>]_output: 'T1*'T2 -> 'U1*'U2   , [<Optional>]_mthd: ArrCombine) =           (fun (x, y) -> (f x       , g y       )) : 'T1*'T2 -> 'U1*'U2
    static member ``***`` (f: Func<'T1,'U1>, g: Func<'T2,'U2>, [<Optional>]_output: Func<'T1*'T2,'U1*'U2>, [<Optional>]_mthd: ArrCombine) = Func<_,_> (fun (x, y) -> (f.Invoke x, g.Invoke y)) : Func<'T1*'T2,'U1*'U2>

    static member inline Invoke (f: '``Arrow<'T1,'U1>``) (g: '``Arrow<'T2,'U2>``) : '``Arrow<('T1 * 'T2),('U1 * 'U2)>`` =
        let inline call (mthd: ^M, output: ^R) = ((^M or ^R) : (static member ``***`` : _*_*_*_ -> _) f, g, output, mthd)
        call (Unchecked.defaultof<ArrCombine>, Unchecked.defaultof<'``Arrow<('T1 * 'T2),('U1 * 'U2)>``>)

    static member inline InvokeOnInstance (f: '``Arrow<'T1,'U1>``) (g: '``Arrow<'T2,'U2>``) : '``Arrow<('T1 * 'T2),('U1 * 'U2)>`` = (^``Arrow<('T1 * 'T2),('U1 * 'U2)>`` : (static member ``***`` : _*_ -> _) f, g)

type ArrCombine with
    static member inline ``***`` (f: '``Arrow<'T1,'U1>``, g: '``Arrow<'T2,'U2>``, _output: '``Arrow<('T1 * 'T2),('U1 * 'U2)>``, _mthd: Default2) = Comp.InvokeOnInstance (ArrSecond.InvokeOnInstance g) (ArrFirst.InvokeOnInstance f) : '``Arrow<('T1 * 'T2),('U1 * 'U2)>``

    static member inline ``***`` (f: '``Arrow<'T1,'U1>``, g: '``Arrow<'T2,'U2>``, _output: '``Arrow<('T1 * 'T2),('U1 * 'U2)>``, _mthd: Default1) = ArrCombine.InvokeOnInstance f g                                                    : '``Arrow<('T1 * 'T2),('U1 * 'U2)>``
    static member inline ``***`` (_: '``Arrow<'T1,'U1>``, _: '``Arrow<'T2,'U2>``, _output: ^t when ^t : null and ^t : struct  , _mthd: Default1) = id


type Fanout =
    inherit Default1
    static member ``&&&`` (f: 'T -> 'U1   , g: 'T -> 'U2   , [<Optional>]_output: 'T -> 'U1*'U2   , [<Optional>]_mthd: Fanout) =            (fun (x, y) -> (f x       , g y       )) << (fun b -> (b, b))  : 'T -> 'U1*'U2
    static member ``&&&`` (f: Func<'T,'U1>, g: Func<'T,'U2>, [<Optional>]_output: Func<'T,'U1*'U2>, [<Optional>]_mthd: Fanout) = Func<_,_> ((fun (x, y) -> (f.Invoke x, g.Invoke y)) << (fun b -> (b, b))) : Func<'T,'U1*'U2>

    static member inline Invoke (f: '``Arrow<'T,'U1>``) (g: '``Arrow<'T,'U2>``) : '``Arrow<'T,('U1 * 'U2)>`` =
        let inline call (mthd: ^M, output: ^R) = ((^M or ^R) : (static member ``&&&`` : _*_*_*_ -> _) f, g, output, mthd)
        call (Unchecked.defaultof<Fanout>, Unchecked.defaultof<'``Arrow<'T,('U1 * 'U2)>``>)

    static member inline InvokeOnInstance (f: '``Arrow<'T,'U1>``) (g: '``Arrow<'T,'U2>``) : '``Arrow<'T,('U1 * 'U2)>`` = (^``Arrow<'T,('U1 * 'U2)>`` : (static member (&&&) : _*_ -> _) f, g)

type Fanout with
    static member inline ``&&&`` (f: '``Arrow<'T,'U1>``, g: '``Arrow<'T,'U2>``, _output: '``Arrow<'T,('U1 * 'U2)>``,    _mthd: Default3) = Comp.InvokeOnInstance (Comp.InvokeOnInstance (ArrSecond.InvokeOnInstance g) (ArrFirst.InvokeOnInstance f)) (Arr.InvokeOnInstance (fun b -> (b, b))) : '``Arrow<'T,('U1 * 'U2)>``
    static member inline ``&&&`` (f: '``Arrow<'T,'U1>``, g: '``Arrow<'T,'U2>``, _output: '``Arrow<'T,('U1 * 'U2)>``,    _mthd: Default2) = Comp.InvokeOnInstance (ArrCombine.InvokeOnInstance f g) (Arr.InvokeOnInstance (fun b -> (b, b)))                                                    : '``Arrow<'T,('U1 * 'U2)>``

    static member inline ``&&&`` (f: '``Arrow<'T,'U1>``, g: '``Arrow<'T,'U2>``, _output: '``Arrow<'T,('U1 * 'U2)>``,    _mthd: Default1) = Fanout.InvokeOnInstance f g                                                                                                                         : '``Arrow<'T,('U1 * 'U2)>``
    static member inline ``&&&`` (_: '``Arrow<'T,'U1>``, _: '``Arrow<'T,'U2>``, _output: ^t when ^t:null and ^t:struct, _mthd: Default1) = id

#endif