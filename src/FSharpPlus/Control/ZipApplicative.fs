namespace FSharpPlus.Control

open System
open System.Text
open System.Runtime.InteropServices
open System.Collections.Generic
open System.Threading.Tasks
open Microsoft.FSharp.Quotations

open FSharpPlus.Internals
open FSharpPlus.Internals.Prelude
open FSharpPlus
open FSharpPlus.Data


[<System.ComponentModel.EditorBrowsable(System.ComponentModel.EditorBrowsableState.Never)>]
module ZipApplivativeConts =
    let [<Literal>]MessagePure = "'Pure' operation is not defined for "
    let [<Literal>]Code = 10707

open ZipApplivativeConts

type Pure =
    inherit Default1
    static member inline InvokeOnInstance (x: 'T) = (^``ZipApplicative<'T>`` : (static member Pure : ^T -> ^``ZipApplicative<'T>``) x)

#if (!FABLE_COMPILER || FABLE_COMPILER_3) && !FABLE_COMPILER_4

    static member inline Invoke (x: 'T) : '``ZipApplicative<'T>`` =
        let inline call (mthd: ^M, output: ^R) = ((^M or ^R) : (static member Pure : _*_ -> _) output, mthd)
        call (Unchecked.defaultof<Pure>, Unchecked.defaultof<'``ZipApplicative<'T>``>) x

    static member        Pure (_: seq<'a>         , _: Default2 ) = fun x -> Seq.initInfinite (fun _ -> x)         : seq<'a>
    static member        Pure (_: NonEmptySeq<'a> , _: Default2 ) = fun x -> NonEmptySeq.initInfinite (fun _ -> x) : NonEmptySeq<'a>
    static member        Pure (_: IEnumerator<'a> , _: Default2 ) = fun x -> Enumerator.upto None (fun _ -> x)     : IEnumerator<'a>
    static member inline Pure (_: 'R              , _: Default1 ) = fun (x: 'T) -> Pure.InvokeOnInstance x    : 'R
    static member        Pure (x: Lazy<'a>        , _: Pure) = Return.Return (x, Unchecked.defaultof<Return>) : _ -> Lazy<'a>
    #if !FABLE_COMPILER
    static member        Pure (_: 'T Task         , _: Pure) = fun x -> Task.FromResult x                     : 'T Task
    #endif
    #if !NET45 && !NETSTANDARD2_0 && !FABLE_COMPILER
    static member        Pure (_: 'T ValueTask    , _: Pure) = fun (x: 'T) -> ValueTask<'T> x                 : 'T ValueTask
    #endif
    static member        Pure (x: option<'a>      , _: Pure) = Return.Return (x, Unchecked.defaultof<Return>)
    static member        Pure (x: voption<'a>     , _: Pure) = Return.Return (x, Unchecked.defaultof<Return>)
    static member        Pure (_: list<'a>        , _: Pure) = fun x -> List.cycle [x]                        : list<'a>
    
    [<CompilerMessage(MessagePure + "'t [].", Code, IsError = true)>]
    static member        Pure (x: 'a []           , _: Pure) = Return.Return (x, Unchecked.defaultof<Return>)

    static member        Pure (x: 'r -> 'a        , _: Pure) = Return.Return (x, Unchecked.defaultof<Return>)
    static member inline Pure (x:  'm * 'a        , _: Pure) = Return.Return (x, Unchecked.defaultof<Return>)
    static member inline Pure (x: struct ('m * 'a), _: Pure) = Return.Return (x, Unchecked.defaultof<Return>)
    static member        Pure (_: 'a Async        , _: Pure) = fun (x: 'a) -> async.Return x
    static member inline Pure (_: Result<'t, 'e>  , _: Pure) = fun x -> if opaqueId false then Error      (Plus.Invoke Unchecked.defaultof<'e> Unchecked.defaultof<'e>) else Ok x         : Result<'t, 'e>
    static member inline Pure (_: Choice<'t, 'e>  , _: Pure) = fun x -> if opaqueId false then Choice2Of2 (Plus.Invoke Unchecked.defaultof<'e> Unchecked.defaultof<'e>) else Choice1Of2 x : Choice<'t, 'e>
    #if !FABLE_COMPILER
    static member        Pure (x: Expr<'a>        , _: Pure) = Return.Return (x, Unchecked.defaultof<Return>)
    #endif
    
    [<CompilerMessage(MessagePure + "ResizeArray<'t>.", Code, IsError = true)>]
    static member        Pure (x: ResizeArray<'a>, _: Pure  ) = Return.Return (x, Unchecked.defaultof<Return>)

    [<CompilerMessage(MessagePure + "string.", Code, IsError = true)>]
    static member        Pure (_: string         , _: Pure  ) = fun (x: char) -> string x : string
    [<CompilerMessage(MessagePure + "StringBuilder.", Code, IsError = true)>]
    static member        Pure (_: StringBuilder  , _: Pure  ) = fun (x: char) -> new StringBuilder (string x) : StringBuilder
    [<CompilerMessage(MessagePure + "Set.", Code, IsError = true)>]
    static member        Pure (_: 'a Set         , _: Pure  ) = fun (x: 'a  ) -> Set.singleton x
    [<CompilerMessage(MessagePure + "HashSet.", Code, IsError = true)>]
    static member        Pure (_: 'a HashSet     , _: Pure  ) = fun (x: 'a  ) -> HashSet.singleton x

#endif

type ZipApply =
    inherit Default1
 
#if (!FABLE_COMPILER || FABLE_COMPILER_3) && !FABLE_COMPILER_4

    static member        ``<.>`` (struct (f: Lazy<'T->'U>        , x: Lazy<'T>             ), [<Optional>]_output: Lazy<'U>             , [<Optional>]_mthd: ZipApply) = Apply.``<*>`` (struct (f, x), _output, Unchecked.defaultof<Apply>)
    static member        ``<.>`` (struct (f: seq<_>              , x: seq<'T>              ), [<Optional>]_output: seq<'U>              , [<Optional>]_mthd: ZipApply) = Seq.map2 (<|) f x
    static member        ``<.>`` (struct (f: NonEmptySeq<_>      , x: NonEmptySeq<'T>      ), [<Optional>]_output: NonEmptySeq<'U>      , [<Optional>]_mthd: ZipApply) = NonEmptySeq.map2 (<|) f x
    static member        ``<.>`` (struct (f: IEnumerator<_>      , x: IEnumerator<'T>      ), [<Optional>]_output: IEnumerator<'U>      , [<Optional>]_mthd: ZipApply) = Apply.``<*>`` (struct (f, x), _output, Unchecked.defaultof<Apply>)
    static member        ``<.>`` (struct (f: list<_>             , x: list<'T>             ), [<Optional>]_output: list<'U>             , [<Optional>]_mthd: ZipApply) = List.map2Shortest (<|) f x
    static member        ``<.>`` (struct (f: _ []                , x: 'T []                ), [<Optional>]_output: 'U []                , [<Optional>]_mthd: ZipApply) = Array.map2Shortest (<|) f x
    static member        ``<.>`` (struct (f: 'r -> _             , x: _ -> 'T              ), [<Optional>]_output:  'r -> 'U            , [<Optional>]_mthd: ZipApply) = Apply.``<*>`` (struct (f, x), _output, Unchecked.defaultof<Apply>)
    static member inline ``<.>`` (struct (f: 'Monoid * _         , x: ('Monoid * 'T)       ), [<Optional>]_output: 'Monoid * 'U         , [<Optional>]_mthd: ZipApply) = Apply.``<*>`` (struct (f, x), _output, Unchecked.defaultof<Apply>)
    static member inline ``<.>`` (struct (f: struct ('Monoid * _), x: struct ('Monoid * 'T)), [<Optional>]_output: struct ('Monoid * 'U), [<Optional>]_mthd: ZipApply) = Apply.``<*>`` (struct (f, x), _output, Unchecked.defaultof<Apply>)
    #if !FABLE_COMPILER && !NET45
    static member        ``<.>`` (struct (f: Task<_>             , x: Task<'T>             ), [<Optional>]_output: Task<'U>             , [<Optional>]_mthd: ZipApply) = Task.map2 (<|) f x
    #endif
    #if !NET45 && !NETSTANDARD2_0 && !FABLE_COMPILER
    static member        ``<.>`` (struct (f: ValueTask<_>        , x: ValueTask<'T>        ), [<Optional>]_output: ValueTask<'U>        , [<Optional>]_mthd: ZipApply) = ValueTask.map2 (<|) f x
    #endif
    static member        ``<.>`` (struct (f: Async<_>            , x: Async<'T>            ), [<Optional>]_output: Async<'U>            , [<Optional>]_mthd: ZipApply) : Async<'U>            = Async.map2 (<|) f x
    static member        ``<.>`` (struct (f: option<_>           , x: option<'T>           ), [<Optional>]_output: option<'U>           , [<Optional>]_mthd: ZipApply)                        = Apply.``<*>`` (struct (f, x), _output, Unchecked.defaultof<Apply>)
    static member        ``<.>`` (struct (f: voption<_>          , x: voption<'T>          ), [<Optional>]_output: voption<'U>          , [<Optional>]_mthd: ZipApply)                        = Apply.``<*>`` (struct (f, x), _output, Unchecked.defaultof<Apply>)
    static member inline ``<.>`` (struct (f: Result<_,'E>        , x: Result<'T,'E>        ), [<Optional>]_output: Result<'b,'E>        , [<Optional>]_mthd: ZipApply) : Result<'U, 'E>       = Result.apply2With Plus.Invoke (<|) f x
    static member inline ``<.>`` (struct (f: Choice<_,'E>        , x: Choice<'T,'E>        ), [<Optional>]_output: Choice<'b,'E>        , [<Optional>]_mthd: ZipApply) : Choice<'U, 'E>       = Choice.apply2With Plus.Invoke (<|) f x
    static member inline ``<.>`` (struct (f: KeyValuePair<'Key,_>, x: KeyValuePair<'Key,'T>), [<Optional>]_output: KeyValuePair<'Key,'U>, [<Optional>]_mthd: Default2)                        = Apply.``<*>`` (struct (f, x), _output, Unchecked.defaultof<Apply>)
    static member inline ``<.>`` (struct (f: KeyValuePair2<_,_>  , x: KeyValuePair2<_,'T> ) ,             _output: KeyValuePair2<_,'U>  ,             _mthd: Default2) : KeyValuePair2<'Key,'U> =
        let a, b = f.Key, x.Key
        let f, x = f.Value, x.Value
        KeyValuePair2 (Plus.Invoke a b, f x)


    static member        ``<.>`` (struct (f: Map<'Key,_>         , x: Map<'Key,'T>         ), [<Optional>]_output: Map<'Key,'U>         , [<Optional>]_mthd: ZipApply) : Map<'Key,'U>         = Apply.``<*>`` (struct (f, x), _output, Unchecked.defaultof<Apply>)
    static member        ``<.>`` (struct (f: Dictionary<'Key,_>  , x: Dictionary<'Key,'T>  ), [<Optional>]_output: Dictionary<'Key,'U>  , [<Optional>]_mthd: ZipApply) : Dictionary<'Key,'U>  = Apply.``<*>`` (struct (f, x), _output, Unchecked.defaultof<Apply>)
    static member        ``<.>`` (struct (f: IDictionary<'Key,_> , x: IDictionary<'Key,'T> ), [<Optional>]_output: IDictionary<'Key,'U> , [<Optional>]_mthd: ZipApply) : IDictionary<'Key,'U> = Apply.``<*>`` (struct (f, x), _output, Unchecked.defaultof<Apply>)
    static member        ``<.>`` (struct (f: IReadOnlyDictionary<'Key,_>, x: IReadOnlyDictionary<'Key,'T> ), [<Optional>]_output: IReadOnlyDictionary<'Key,'U>, [<Optional>]_mthd: ZipApply) : IReadOnlyDictionary<'Key,'U> = Apply.``<*>`` (struct (f, x), _output, Unchecked.defaultof<Apply>)

    #if !FABLE_COMPILER
    static member        ``<.>`` (struct (f: Expr<'T->'U>, x: Expr<'T>), [<Optional>]_output: Expr<'U>, [<Optional>]_mthd: ZipApply) = Apply.``<*>`` (struct (f, x), _output, Unchecked.defaultof<Apply>)
    #endif
    static member        ``<.>`` (struct (f: ('T->'U) ResizeArray, x: 'T ResizeArray), [<Optional>]_output: 'U ResizeArray, [<Optional>]_mthd: ZipApply) = ResizeArray.map2Shortest (<|) f x

    static member inline Invoke (f: '``ZipApplicative<'T -> 'U>``) (x: '``ZipApplicative<'T>``) : '``ZipApplicative<'U>`` =
        let inline call (mthd: ^M, input1: ^I1, input2: ^I2, output: ^R) =
            ((^M or ^I1 or ^I2 or ^R) : (static member ``<.>`` : struct (_*_) * _ * _ -> _) (struct (input1, input2)), output, mthd)
        call(Unchecked.defaultof<ZipApply>, f, x, Unchecked.defaultof<'``ZipApplicative<'U>``>)

#endif

    static member inline InvokeOnInstance (f: '``ZipApplicative<'T->'U>``) (x: '``ZipApplicative<'T>``) : '``ZipApplicative<'U>`` =
        ((^``ZipApplicative<'T->'U>`` or ^``ZipApplicative<'T>`` or ^``ZipApplicative<'U>``) : (static member (<.>) : _*_ -> _) (f, x))

#if (!FABLE_COMPILER || FABLE_COMPILER_3) && !FABLE_COMPILER_4

type ZipApply with
    static member inline ``<.>`` (struct (_: ^t when ^t : null and ^t: struct, _: ^u when ^u : null and ^u: struct), _output: ^r when ^r : null and ^r: struct, _mthd: Default1) = id
    static member inline ``<.>`` (struct (f: '``Applicative<'T->'U>``, x: '``Applicative<'T>``), _output: '``Applicative<'U>``, [<Optional>]_mthd: Default1) : '``Applicative<'U>`` =
        ((^``Applicative<'T->'U>`` or ^``Applicative<'T>`` or ^``Applicative<'U>``) : (static member (<.>) : _*_ -> _) f, x)


type Map2 =
    inherit Default1

    static member        Map2 (f, (x: Lazy<_>            , y: Lazy<_>            ), _mthd: Map2) = Lift2.Lift2 (f, (x, y), Unchecked.defaultof<Lift2>)
    static member        Map2 (f, (x: seq<_>             , y: seq<_>             ), _mthd: Map2) = Seq.map2 f x y
    static member        Map2 (f, (x: NonEmptySeq<_>     , y: NonEmptySeq<_>     ), _mthd: Map2) = NonEmptySeq.map2 f x y
    static member        Map2 (f, (x: IEnumerator<_>     , y: IEnumerator<_>     ), _mthd: Map2) = Lift2.Lift2 (f, (x, y), Unchecked.defaultof<Lift2>)
    static member        Map2 (f, (x                     , y                     ), _mthd: Map2) = List.map2Shortest f x y
    static member        Map2 (f, (x: _ []               , y: _ []               ), _mthd: Map2) = Array.map2Shortest f x y
    static member        Map2 (f, (x: 'R -> 'T           , y: 'R -> 'U           ), _mthd: Map2) = Lift2.Lift2 (f, (x, y), Unchecked.defaultof<Lift2>)
    static member inline Map2 (f, (x: 'Monoid * 'T       , y: 'Monoid * 'U       ), _mthd: Map2) = Lift2.Lift2 (f, (x, y), Unchecked.defaultof<Lift2>)
    static member inline Map2 (f, (x: struct ('Monoid*'T), y: struct ('Monoid*'U)), _mthd: Map2) = Lift2.Lift2 (f, (x, y), Unchecked.defaultof<Lift2>)
    #if !FABLE_COMPILER && !NET45
    static member        Map2 (f, (x: Task<'T>           , y: Task<'U>           ), _mthd: Map2) = Task.map2 f x y
    #endif
    #if !NET45 && !NETSTANDARD2_0 && !FABLE_COMPILER
    static member        Map2 (f, (x: ValueTask<'T>      , y: ValueTask<'U>      ), _mthd: Map2) = ValueTask.map2 f x y
    #endif
    static member        Map2 (f, (x                     , y                     ), _mthd: Map2) = Async.map2 f x y
    static member        Map2 (f, (x: option<_>          , y: option<_>          ), _mthd: Map2) = Lift2.Lift2 (f, (x, y), Unchecked.defaultof<Lift2>)
    
    #if !FABLE_COMPILER
    static member        Map2 (f, (x: voption<_>         , y: voption<_>         ), _mthd: Map2) = Lift2.Lift2 (f, (x, y), Unchecked.defaultof<Lift2>)
    #endif
    static member inline Map2 (f, (x: Result<'T,'Error>  , y: Result<'U,'Error>  ), _mthd: Map2) = Result.apply2With Plus.Invoke f x y
    static member inline Map2 (f, (x: Choice<'T,'Error>  , y: Choice<'U,'Error>  ), _mthd: Map2) = Choice.map2 f x y
    static member        Map2 (f, (x: Map<'Key,'T>       , y : Map<'Key,'U>      ), _mthd: Map2) = Lift2.Lift2 (f, (x, y), Unchecked.defaultof<Lift2>)
    static member        Map2 (f, (x: Dictionary<'Key,'T>, y: Dictionary<'Key,'U>), _mthd: Map2) = Lift2.Lift2 (f, (x, y), Unchecked.defaultof<Lift2>)
    #if !FABLE_COMPILER
    static member        Map2 (f, (x: Expr<'T>           , y: Expr<'U>           ), _mthd: Map2) = Lift2.Lift2 (f, (x, y), Unchecked.defaultof<Lift2>)
    #endif
    static member        Map2 (f, (x: ResizeArray<'T>    , y: ResizeArray<'U>    ), _mthd: Map2) = ResizeArray.map2Shortest f x y

    static member inline Invoke (f: 'T -> 'U -> 'V) (x: '``ZipApplicative<'T>``) (y: '``ZipApplicative<'U>``) : '``ZipApplicative<'V>`` =
        let inline call (mthd : ^M, input1: ^I1, input2: ^I2, _output: ^R) =
            ((^M or ^I1 or ^I2 or ^R) : (static member Map2 : _*(_*_)*_ -> _) f, (input1, input2), mthd)
        call (Unchecked.defaultof<Map2>, x, y, Unchecked.defaultof<'``ZipApplicative<'V>``>)

    static member inline InvokeOnInstance (f: 'T -> 'U -> 'V) (x: '``ZipApplicative<'T>``) (y: '``ZipApplicative<'U>``) =
        ((^``ZipApplicative<'T>`` or ^``ZipApplicative<'U>``) : (static member Map2 : _*_*_ -> _) f, x, y)

type Map2 with
    static member inline Map2 (f, (x, y), _mthd: Default2) = (((Pure.InvokeOnInstance f, x) ||> ZipApply.InvokeOnInstance), y) ||> ZipApply.InvokeOnInstance

    static member inline Map2 (_, (_:'t when 't: null and 't: struct, _: ^u when ^u : null and ^u: struct), _mthd: Default1) = id
    static member inline Map2 (f: 'T -> 'U -> 'V, (x: '``ZipApplicative<'T>``, y: '``ZipApplicative<'U>``), _mthd: Default1) = ((^``ZipApplicative<'T>`` or ^``ZipApplicative<'U>`` ) : (static member Map2 : _*_*_ -> _) f, x, y)

type Map3 =
    inherit Default1

    static member        Map3 (f, (x: Lazy<_>            , y: Lazy<_>            , z: Lazy<_>             ), _mthd: Map3) = Lift3.Lift3 (f, (x, y, z), Unchecked.defaultof<Lift3>)
    static member        Map3 (f, (x: seq<_>             , y: seq<_>             , z: seq<_>              ), _mthd: Map3) = Seq.map3 f x y z
    static member        Map3 (f, (x: NonEmptySeq<_>     , y: NonEmptySeq<_>     , z: NonEmptySeq<_>      ), _mthd: Map3) = NonEmptySeq.map3 f x y z
    static member        Map3 (f, (x: IEnumerator<_>     , y: IEnumerator<_>     , z: IEnumerator<_>      ), _mthd: Map3) = Lift3.Lift3 (f, (x, y, z), Unchecked.defaultof<Lift3>)
    static member        Map3 (f, (x                     , y                     , z                      ), _mthd: Map3) = List.map3Shortest f x y z
    static member        Map3 (f, (x: _ []               , y: _ []               , z: _ []                ), _mthd: Map3) = Array.map3Shortest f x y z
    static member        Map3 (f, (x: 'R -> 'T           , y: 'R -> 'U           , z: 'R -> 'V            ), _mthd: Map3) = Lift3.Lift3 (f, (x, y, z), Unchecked.defaultof<Lift3>)
    static member inline Map3 (f, (x: 'Monoid * 'T       , y: 'Monoid * 'U       , z: 'Monoid * 'V        ), _mthd: Map3) = Lift3.Lift3 (f, (x, y, z), Unchecked.defaultof<Lift3>)
    static member inline Map3 (f, (x: struct ('Monoid*'T), y: struct ('Monoid*'U), z: struct ('Monoid* 'T)), _mthd: Map3) = Lift3.Lift3 (f, (x, y, z), Unchecked.defaultof<Lift3>)
    #if !FABLE_COMPILER && !NET45
    static member        Map3 (f, (x: Task<'T>           , y: Task<'U>           , z: Task<'V>            ), _mthd: Map3) = Task.map3 f x y z
    #endif
    #if !NET45 && !NETSTANDARD2_0 && !FABLE_COMPILER
    static member        Map3 (f, (x: ValueTask<'T>      , y: ValueTask<'U>      , z: ValueTask<'V>       ), _mthd: Map3) = ValueTask.map3 f x y z
    #endif
    static member        Map3 (f, (x                     , y                     , z                      ), _mthd: Map3) = Async.map3  f x y z
    static member        Map3 (f, (x: option<_>          , y: option<_>          , z: option<_>           ), _mthd: Map3) = Lift3.Lift3 (f, (x, y, z), Unchecked.defaultof<Lift3>)
    
    #if !FABLE_COMPILER && !NET45
    static member        Map3 (f, (x: voption<_>         , y: voption<_>         , z: voption<_>          ), _mthd: Map3) = Lift3.Lift3 (f, (x, y, z), Unchecked.defaultof<Lift3>)
    #endif
    static member inline Map3 (f, (x: Result<'T,'Error>  , y: Result<'U,'Error>  , z: Result<'V, 'Error>  ), _mthd: Map3) = Result.apply3With Plus.Invoke f x y z
    static member inline Map3 (f, (x: Choice<'T,'Error>  , y: Choice<'U,'Error>  , z: Choice<'V, 'Error>  ), _mthd: Map3) = Choice.apply3With Plus.Invoke f x y z
    static member        Map3 (f, (x: Map<'Key,'T>       , y: Map<'Key,'U>       , z: Map<'Key, 'V>       ), _mthd: Map3) = Lift3.Lift3 (f, (x, y, z), Unchecked.defaultof<Lift3>)
    static member        Map3 (f, (x: Dictionary<'Key,'T>, y: Dictionary<'Key,'U>, z: Dictionary<'Key, 'V>), _mthd: Map3) = Lift3.Lift3 (f, (x, y, z), Unchecked.defaultof<Lift3>)
    #if !FABLE_COMPILER
    static member        Map3 (f, (x: Expr<'T>           , y: Expr<'U>           , z: Expr<'V>            ), _mthd: Map3) = Lift3.Lift3 (f, (x, y, z), Unchecked.defaultof<Lift3>)
    #endif
    static member        Map3 (f, (x: ResizeArray<'T>    , y: ResizeArray<'U>    , z: ResizeArray<'V>     ), _mthd: Map3) = ResizeArray.map3Shortest f x y z

    static member inline Invoke (f: 'T -> 'U -> 'V -> 'W) (x: '``ZipApplicative<'T>``) (y: '``ZipApplicative<'U>``) (z: '``ZipApplicative<'V>``) : '``ZipApplicative<'W>`` =
        let inline call (mthd: ^M, input1: ^I1, input2: ^I2, input3: ^I3, _output: ^R) =
            ((^M or ^I1 or ^I2 or ^I3 or ^R) : (static member Map3 : _*(_*_*_)*_ -> _) f, (input1, input2, input3), mthd)
        call (Unchecked.defaultof<Map3>, x, y, z, Unchecked.defaultof<'``ZipApplicative<'W>``>)

    static member inline InvokeOnInstance (f: 'T -> 'U -> 'V -> 'W) (x: '``ZipApplicative<'T>``) (y: '``ZipApplicative<'U>``) (z: '``ZipApplicative<'V>``)=
        ((^``ZipApplicative<'T>`` or ^``ZipApplicative<'U>`` or ^``ZipApplicative<'V>``) : (static member Map3 : _*_*_*_ -> _) f, x, y, z)

type Map3 with
    static member inline Map3 (f, (x, y, z), _mthd: Default3) = ((((Pure.InvokeOnInstance f, x) ||> ZipApply.InvokeOnInstance), y) ||> ZipApply.InvokeOnInstance, z) ||> ZipApply.InvokeOnInstance
    static member inline Map3 (_, (_:'t when 't: null and 't: struct, _: ^u when ^u : null and ^u: struct, _: ^v when ^v : null and ^v: struct), _mthd: Default1) = id
    static member inline Map3 (f: 'T -> 'U -> 'V -> 'W, (x: '``ZipApplicative<'T>``, y: '``ZipApplicative<'U>``, z: '``ZipApplicative<'V>``)   , _mthd: Default1) = ((^``ZipApplicative<'T>`` or ^``ZipApplicative<'U>`` or ^``ZipApplicative<'V>`` ) : (static member Map3 : _*_*_*_ -> _) f, x, y, z)

type IsZipLeftZero =
    inherit Default1

    static member IsZipLeftZero (t: ref<seq<_>>        , _mthd: IsZipLeftZero) = Seq.isEmpty t.Value
    static member IsZipLeftZero (_: ref<NonEmptySeq<_>>, _mthd: IsZipLeftZero) = false
    static member IsZipLeftZero (t: ref<list<_>>       , _mthd: IsZipLeftZero) = List.isEmpty t.Value
    static member IsZipLeftZero (t: ref<array<_>>      , _mthd: IsZipLeftZero) = Array.isEmpty t.Value
    static member IsZipLeftZero (t: ref<option<_>>     , _mthd: IsZipLeftZero) = IsLeftZero.IsLeftZero (t, Unchecked.defaultof<IsLeftZero>)
    #if !FABLE_COMPILER
    static member IsZipLeftZero (t: ref<voption<_>>    , _mthd: IsZipLeftZero) = IsLeftZero.IsLeftZero (t, Unchecked.defaultof<IsLeftZero>)
    #endif
    static member IsZipLeftZero (_: ref<Result<_,_>>   , _mthd: IsZipLeftZero) = false
    static member IsZipLeftZero (_: ref<Choice<_,_>>   , _mthd: IsZipLeftZero) = false

    static member inline Invoke (x: '``ZipApplicative<'T>``) : bool =
        let inline call (mthd: ^M, input: ^I) =
            ((^M or ^I) : (static member IsZipLeftZero : _*_ -> _) ref input, mthd)
        call(Unchecked.defaultof<IsZipLeftZero>, x)

    static member inline InvokeOnInstance (x: '``ZipApplicative<'T>``) : bool =
        ((^``ZipApplicative<'T>``) : (static member IsZipLeftZero : _ -> _) x)

type IsZipLeftZero with

    static member inline IsZipLeftZero (_: ref<'T>   when 'T : struct    , _mthd: Default4) = false
    static member inline IsZipLeftZero (_: ref<'T>   when 'T : not struct, _mthd: Default3) = false

    // empty <.> f = empty  ==> empty is left zero for <.>
    static member inline IsZipLeftZero (t: ref<'``Alternative<'T>``>     , _mthd: Default2) = (t.Value = Empty.InvokeOnInstance ())

    static member inline IsZipLeftZero (t: ref<'``ZipApplicative<'T>``>  , _mthd: Default1) = (^``ZipApplicative<'T>`` : (static member IsZipLeftZero : _ -> _) t.Value)
    static member inline IsZipLeftZero (_: ref< ^t> when ^t: null and ^t: struct, _: Default1) = ()

#endif
