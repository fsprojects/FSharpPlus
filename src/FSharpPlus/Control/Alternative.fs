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

type Empty =
    inherit Default1
    static member        Empty ([<Optional>]_output: seq<'T>             , [<Optional>]_mthd: Default2) = Seq.empty    : seq<'T>
    #if !FABLE_COMPILER
    static member inline Empty ([<Optional>]_output: '``Alternative<'T>``, [<Optional>]_mthd: Default1) = (^``Alternative<'T>`` : (static member Empty : ^``Alternative<'T>``) ()) : '``Alternative<'T>``
    static member inline Empty (_output: ^t when ^t: null and ^t: struct ,             _mthd: Default1) = id    
    #endif
    static member        Empty ([<Optional>]_output: option<'T>          , [<Optional>]_mthd: Empty   ) = None         : option<'T>
    static member        Empty ([<Optional>]_output: list<'T>            , [<Optional>]_mthd: Empty   ) = [  ]         : list<'T>
    static member        Empty ([<Optional>]_output: 'T []               , [<Optional>]_mthd: Empty   ) = [||]         : 'T []    

    static member inline Invoke () : '``Alternative<'T>`` =
        let inline call (mthd: ^M, output: ^R) = ((^M or ^R) : (static member Empty : _*_ -> _) output, mthd)
        call (Unchecked.defaultof<Empty>, Unchecked.defaultof<'``Alternative<'T>``> )


type Append =
    inherit Default1
    static member        ``<|>`` (x: 'T seq              , y              , [<Optional>]_mthd: Default2) = Seq.append   x y
    #if !FABLE_COMPILER
    static member inline ``<|>`` (x: '``Alt<'T>``        , y: '``Alt<'T>``, [<Optional>]_mthd: Default1) = (^``Alt<'T>`` :  (static member (<|>) : _*_ -> _) x, y) : '``Alt<'T>``
    static member inline ``<|>`` (_: ^t when ^t: null and ^t: struct   , _,             _mthd: Default1) = ()
    #endif
    static member inline ``<|>`` (x: Result<_,_>         , y              , [<Optional>]_mthd: Append  ) = match x, y with Ok _        , _ -> x | Error x     , Error y      -> Error      (Plus.Invoke x y) | _, _ -> y
    static member inline ``<|>`` (x: Choice<_,_>         , y              , [<Optional>]_mthd: Append  ) = match x, y with Choice1Of2 _, _ -> x | Choice2Of2 x, Choice2Of2 y -> Choice2Of2 (Plus.Invoke x y) | _, _ -> y
    static member inline ``<|>`` (x: Either<_,_>         , y              , [<Optional>]_mthd: Append  ) = match x with Left _ -> y | xs -> xs
    static member        ``<|>`` (x: 'T option           , y              , [<Optional>]_mthd: Append  ) = match x with None   -> y | xs -> xs
    static member        ``<|>`` (x: 'T list             , y              , [<Optional>]_mthd: Append  ) = x @ y
    static member        ``<|>`` (x: 'T []               , y              , [<Optional>]_mthd: Append  ) = Array.append x y

    static member inline Invoke (x: '``Alt<'T>``) (y: '``Alt<'T>``) : '``Alt<'T>`` =
        let inline call (mthd: ^M, input1: ^I, input2: ^I) = ((^M or ^I) : (static member ``<|>`` : _*_*_ -> _) input1, input2, mthd)
        call (Unchecked.defaultof<Append>, x, y)

