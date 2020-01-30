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

// ArrowApply class -------------------------------------------------------

type App =
    inherit Default1
    static member App ([<Optional>]_output:  ('T -> 'U)     * 'T -> 'U, [<Optional>]_mthd: App) =            (fun (f           , x) -> f x)        : ('T -> 'U)     * 'T -> 'U
    static member App ([<Optional>]_output: Func<Func<'T,'U> * 'T, 'U>, [<Optional>]_mthd: App) = Func<_, _> (fun (f: Func<_,_>, x) -> f.Invoke x) : Func<Func<'T,'U> * 'T, 'U>

    static member inline Invoke () : '``ArrowApply<('ArrowApply<'T,'U> * 'T)>,'U)>`` =
        let inline call (mthd: ^M, output: ^R) = ((^M or ^R) : (static member App : _*_ -> _) output, mthd)
        call (Unchecked.defaultof<App>, Unchecked.defaultof<'``ArrowApply<('ArrowApply<'T,'U> * 'T)>,'U)>``>)

    static member inline InvokeOnInstance () : '``ArrowApply<('ArrowApply<'T,'U> * 'T)>,'U)>`` = (^``ArrowApply<('ArrowApply<'T,'U> * 'T)>,'U)>`` : (static member App : _) ())

#if !FABLE_COMPILER
type App with
    static member inline App (_output: '``ArrowApply<('ArrowApply<'T,'U> * 'T)>,'U)>``, _mthd: Default1) = App.InvokeOnInstance () : '``ArrowApply<('ArrowApply<'T,'U> * 'T)>,'U)>``
    static member inline App (_output: ^t when ^t : null and ^t : struct              , _mthd: Default1) = id
#endif
