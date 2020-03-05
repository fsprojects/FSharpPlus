namespace FSharpPlus.Control

#if !FABLE_COMPILER

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

    static member inline Empty ([<Optional>]_output: '``Alternative<'T>``, [<Optional>]_mthd: Default1) = (^``Alternative<'T>`` : (static member Empty : ^``Alternative<'T>``) ()) : '``Alternative<'T>``
    static member inline Empty (_output: ^t when ^t: null and ^t: struct ,             _mthd: Default1) = id    

    static member        Empty ([<Optional>]_output: option<'T>          , [<Optional>]_mthd: Empty   ) = None         : option<'T>
    static member        Empty ([<Optional>]_output: list<'T>            , [<Optional>]_mthd: Empty   ) = [  ]         : list<'T>
    static member        Empty ([<Optional>]_output: 'T []               , [<Optional>]_mthd: Empty   ) = [||]         : 'T []    

    static member inline Invoke () : '``Alternative<'T>`` =
        let inline call (mthd: ^M, output: ^R) = ((^M or ^R) : (static member Empty : _*_ -> _) output, mthd)
        call (Unchecked.defaultof<Empty>, Unchecked.defaultof<'``Alternative<'T>``> )

    static member inline InvokeOnInstance () : '``Alternative<'T>`` = (^``Alternative<'T>`` : (static member Empty : ^``Alternative<'T>``) ()) : '``Alternative<'T>``


type Append =
    inherit Default1
    static member        ``<|>`` (x: 'T seq              , y              , [<Optional>]_mthd: Default2) = Seq.append   x y

    static member inline ``<|>`` (x: '``Alt<'T>``        , y: '``Alt<'T>``, [<Optional>]_mthd: Default1) = (^``Alt<'T>`` :  (static member (<|>) : _*_ -> _) x, y) : '``Alt<'T>``
    static member inline ``<|>`` (_: ^t when ^t: null and ^t: struct   , _,             _mthd: Default1) = ()

    static member inline ``<|>`` (x: Result<_,_>         , y              , [<Optional>]_mthd: Append  ) = match x, y with Ok _        , _ -> x | Error x     , Error y      -> Error      (Plus.Invoke x y) | _, _ -> y
    static member inline ``<|>`` (x: Choice<_,_>         , y              , [<Optional>]_mthd: Append  ) = match x, y with Choice1Of2 _, _ -> x | Choice2Of2 x, Choice2Of2 y -> Choice2Of2 (Plus.Invoke x y) | _, _ -> y
    static member inline ``<|>`` (x: Either<_,_>         , y              , [<Optional>]_mthd: Append  ) = match x with Left _ -> y | xs -> xs
    static member        ``<|>`` (x: 'T option           , y              , [<Optional>]_mthd: Append  ) = match x with None   -> y | xs -> xs
    static member        ``<|>`` (x: 'T list             , y              , [<Optional>]_mthd: Append  ) = x @ y
    static member        ``<|>`` (x: 'T []               , y              , [<Optional>]_mthd: Append  ) = Array.append x y

    static member inline Invoke (x: '``Alt<'T>``) (y: '``Alt<'T>``) : '``Alt<'T>`` =
        let inline call (mthd: ^M, input1: ^I, input2: ^I) = ((^M or ^I) : (static member ``<|>`` : _*_*_ -> _) input1, input2, mthd)
        call (Unchecked.defaultof<Append>, x, y)



type IsLeftZeroForAppend =
    inherit Default1

    static member inline IsLeftZeroForAppend (_: ref<'T>   when 'T : struct    , _mthd: Default3) = false
    static member inline IsLeftZeroForAppend (_: ref<'T>   when 'T : not struct, _mthd: Default2) = false

    static member inline IsLeftZeroForAppend (t: ref<'``Applicative<'T>``>             , _mthd: Default1) = (^``Applicative<'T>`` : (static member IsLeftZeroForAppend : _ -> _) t.Value)
    static member inline IsLeftZeroForAppend (_: ref< ^t> when ^t: null and ^t: struct , _mthd: Default1) = ()

    static member        IsLeftZeroForAppend (t: ref<option<_>  > , _mthd: IsLeftZeroForAppend) = Option.isSome t.Value
    static member        IsLeftZeroForAppend (t: ref<Result<_,_>> , _mthd: IsLeftZeroForAppend) = match t.Value with (Ok _        ) -> true | _ -> false
    static member        IsLeftZeroForAppend (t: ref<Choice<_,_>> , _mthd: IsLeftZeroForAppend) = match t.Value with (Choice1Of2 _) -> true | _ -> false

    static member inline Invoke (x: '``Applicative<'T>``) : bool =
        let inline call (mthd : ^M, input: ^I) =
            ((^M or ^I) : (static member IsLeftZeroForAppend : _*_ -> _) (ref input), mthd)
        call(Unchecked.defaultof<IsLeftZeroForAppend>, x)

    static member inline InvokeOnInstance (x: '``Applicative<'T>``) : bool = (^``Applicative<'T>`` : (static member IsLeftZeroForAppend : _ -> _) x)


type Choice =
    inherit Default1

    static member inline Choice (x: ref<'``Foldable<'Alternative<'T>>``>, _mthd: Default4) =
        use e = (ToSeq.Invoke x.Value).GetEnumerator ()
        let mutable res = Empty.Invoke ()
        while e.MoveNext() && not (IsLeftZeroForAppend.Invoke res) do
            res <- Append.Invoke res e.Current
        res

    static member inline Choice (x: ref<'``Reducible<'Alt<'T>>``>, _mthd: Default2) =
        let inline _f f = Reduce.Invoke f x.Value : '``Alt<'T>>``
        let t = ToSeq.Invoke x.Value
        use e = t.GetEnumerator ()
        e.MoveNext() |> ignore
        let mutable res = e.Current
        while e.MoveNext() && not (IsLeftZeroForAppend.Invoke res) do
            res <- Append.Invoke res e.Current
        res

    static member inline Choice (x: ref<'``Foldable<'Alternative<'T>>``> , _mthd: Default1) = (^``Foldable<'Alternative<'T>>`` :  (static member Choice : _ -> _) x.Value) : '``Alternative<'T>``
    static member inline Choice (_: ref< ^t> when ^t: null and ^t: struct, _mthd: Default1) = ()

    static member inline Choice (x: ref<seq<'``Alternative<'T>``>>, _mthd: Choice) =
        use e = x.Value.GetEnumerator ()
        let mutable res = Empty.Invoke ()
        while e.MoveNext() && not (IsLeftZeroForAppend.Invoke res) do
            res <- Append.Invoke res e.Current
        res

    static member inline Choice (x: ref<list<'``Alternative<'T>``>>, _mthd: Choice) =
        use e = (List.toSeq x.Value ).GetEnumerator ()
        let mutable res = Empty.Invoke ()
        while e.MoveNext() && not (IsLeftZeroForAppend.Invoke res) do
            res <- Append.Invoke res e.Current
        res

    static member inline Choice (x: ref< array<'``Alternative<'T>``>>, _mthd: Choice) =
        let arr = x.Value
        let mutable i = 0
        let mutable res = Empty.Invoke ()
        let last = Array.length arr - 1
        while i < last && not (IsLeftZeroForAppend.Invoke res) do
            i <- i + 1
            res <- Append.Invoke res arr.[i]
        res

    static member inline Invoke (x: '``Foldable<'Alternative<'T>>``) : '``Alternative<'T>>`` =
        let inline call (mthd: ^M, input1: ^I) = ((^M or ^I) : (static member Choice : _*_ -> _) (ref input1, mthd))
        call (Unchecked.defaultof<Choice>, x)

#endif