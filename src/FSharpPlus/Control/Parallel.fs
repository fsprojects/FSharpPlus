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


type ParReturn =
    inherit Default1
    static member inline InvokeOnInstance (x: 'T) = (^``ParApplicative<'T>`` : (static member ParReturn : ^T -> ^``ParApplicative<'T>``) x)

#if (!FABLE_COMPILER || FABLE_COMPILER_3) && !FABLE_COMPILER_4

    static member inline Invoke (x: 'T) : '``ParApplicative<'T>`` =
        let inline call (mthd: ^M, output: ^R) = ((^M or ^R) : (static member ParReturn : _*_ -> _) output, mthd)
        call (Unchecked.defaultof<ParReturn>, Unchecked.defaultof<'``ParApplicative<'T>``>) x

    static member        ParReturn (_: seq<'a>         , _: Default2 ) = fun x -> Seq.initInfinite (fun _ -> x)         : seq<'a>
    static member        ParReturn (_: NonEmptySeq<'a> , _: Default2 ) = fun x -> NonEmptySeq.initInfinite (fun _ -> x) : NonEmptySeq<'a>
    static member        ParReturn (_: IEnumerator<'a> , _: Default2 ) = fun x -> Enumerator.upto None (fun _ -> x)     : IEnumerator<'a>
    static member inline ParReturn (_: 'R              , _: Default1 ) = fun (x: 'T) -> ParReturn.InvokeOnInstance x    : 'R
    static member        ParReturn (x: Lazy<'a>        , _: ParReturn) = Return.Return (x, Unchecked.defaultof<Return>) : _ -> Lazy<'a>
    #if !FABLE_COMPILER
    static member        ParReturn (_: 'T Task         , _: ParReturn) = fun x -> Task.FromResult x                     : 'T Task
    #endif
    #if NETSTANDARD2_1 && !FABLE_COMPILER
    static member        ParReturn (_: 'T ValueTask    , _: ParReturn) = fun (x: 'T) -> ValueTask<'T> x                 : 'T ValueTask
    #endif
    static member        ParReturn (x: option<'a>      , _: ParReturn) = Return.Return (x, Unchecked.defaultof<Return>)
    static member        ParReturn (x: voption<'a>     , _: ParReturn) = Return.Return (x, Unchecked.defaultof<Return>)
    static member        ParReturn (_: list<'a>        , _: ParReturn) = fun x -> List.cycle [x]                        : list<'a>
    
    [<CompilerMessage("No parallel applicative Return operation for 't []", 10720, IsError = true)>]
    static member        ParReturn (x: 'a []           , _: ParReturn) = Return.Return (x, Unchecked.defaultof<Return>)

    static member        ParReturn (x: 'r -> 'a        , _: ParReturn) = Return.Return (x, Unchecked.defaultof<Return>)
    static member inline ParReturn (x:  'm * 'a        , _: ParReturn) = Return.Return (x, Unchecked.defaultof<Return>)
    static member inline ParReturn (x: struct ('m * 'a), _: ParReturn) = Return.Return (x, Unchecked.defaultof<Return>)
    static member        ParReturn (_: 'a Async        , _: ParReturn) = fun (x: 'a) -> async.Return x
    static member inline ParReturn (_: Result<'t, 'e>  , _: ParReturn) = fun x -> if opaqueId false then Error      (Plus.Invoke Unchecked.defaultof<'e> Unchecked.defaultof<'e>) else Ok x         : Result<'t, 'e>
    static member inline ParReturn (_: Choice<'t, 'e>  , _: ParReturn) = fun x -> if opaqueId false then Choice2Of2 (Plus.Invoke Unchecked.defaultof<'e> Unchecked.defaultof<'e>) else Choice1Of2 x : Choice<'t, 'e>
    #if !FABLE_COMPILER
    static member        ParReturn (x: Expr<'a>        , _: ParReturn) = Return.Return (x, Unchecked.defaultof<Return>)
    #endif
    
    [<CompilerMessage("No parallel applicative Return operation for ResizeArray<'t>", 10720, IsError = true)>]
    static member        ParReturn (x: ResizeArray<'a>, _: ParReturn  ) = Return.Return (x, Unchecked.defaultof<Return>)

    //Restricted
    [<CompilerMessage("No parallel applicative Return operation for string", 10720, IsError = true)>]
    static member        ParReturn (_: string         , _: ParReturn  ) = fun (x: char) -> string x : string
    [<CompilerMessage("No parallel applicative Return operation for StringBuilder", 10720, IsError = true)>]
    static member        ParReturn (_: StringBuilder  , _: ParReturn  ) = fun (x: char) -> new StringBuilder (string x) : StringBuilder
    [<CompilerMessage("No parallel applicative Return operation for Set", 10720, IsError = true)>]
    static member        ParReturn (_: 'a Set         , _: ParReturn  ) = fun (x: 'a  ) -> Set.singleton x
    static member        ParReturn (_: 'a Set2        , _: ParReturn  ) = fun (_: 'a  ) -> Set2() : 'a Set2

#endif

type ParApply =
    inherit Default1
 
#if (!FABLE_COMPILER || FABLE_COMPILER_3) && !FABLE_COMPILER_4

    static member        ``</>`` (struct (f: Lazy<'T->'U>        , x: Lazy<'T>             ), [<Optional>]_output: Lazy<'U>             , [<Optional>]_mthd: ParApply) = Apply.``<*>`` (struct (f, x), _output, Unchecked.defaultof<Apply>)
    static member        ``</>`` (struct (f: seq<_>              , x: seq<'T>              ), [<Optional>]_output: seq<'U>              , [<Optional>]_mthd: ParApply) = Seq.map2 (<|) f x
    static member        ``</>`` (struct (f: NonEmptySeq<_>      , x: NonEmptySeq<'T>      ), [<Optional>]_output: NonEmptySeq<'U>      , [<Optional>]_mthd: ParApply) = NonEmptySeq.map2 (<|) f x
    static member        ``</>`` (struct (f: IEnumerator<_>      , x: IEnumerator<'T>      ), [<Optional>]_output: IEnumerator<'U>      , [<Optional>]_mthd: ParApply) = Apply.``<*>`` (struct (f, x), _output, Unchecked.defaultof<Apply>)
    static member        ``</>`` (struct (f: list<_>             , x: list<'T>             ), [<Optional>]_output: list<'U>             , [<Optional>]_mthd: ParApply) = List.map2Shortest (<|) f x
    static member        ``</>`` (struct (f: _ []                , x: 'T []                ), [<Optional>]_output: 'U []                , [<Optional>]_mthd: ParApply) = Array.map2Shortest (<|) f x
    static member        ``</>`` (struct (f: 'r -> _             , x: _ -> 'T              ), [<Optional>]_output:  'r -> 'U            , [<Optional>]_mthd: ParApply) = Apply.``<*>`` (struct (f, x), _output, Unchecked.defaultof<Apply>)
    static member inline ``</>`` (struct (f: 'Monoid * _         , x: ('Monoid * 'T)       ), [<Optional>]_output: 'Monoid * 'U         , [<Optional>]_mthd: ParApply) = Apply.``<*>`` (struct (f, x), _output, Unchecked.defaultof<Apply>)
    static member inline ``</>`` (struct (f: struct ('Monoid * _), x: struct ('Monoid * 'T)), [<Optional>]_output: struct ('Monoid * 'U), [<Optional>]_mthd: ParApply) = Apply.``<*>`` (struct (f, x), _output, Unchecked.defaultof<Apply>)
    #if !FABLE_COMPILER
    static member        ``</>`` (struct (f: Task<_>             , x: Task<'T>             ), [<Optional>]_output: Task<'U>             , [<Optional>]_mthd: ParApply) = Task.pmap2 (<|) f x
    #endif
    #if !NET45 && !NETSTANDARD2_0 && !FABLE_COMPILER
    static member        ``</>`` (struct (f: ValueTask<_>        , x: ValueTask<'T>        ), [<Optional>]_output: ValueTask<'U>        , [<Optional>]_mthd: ParApply) = ValueTask.pmap2 (<|) f x
    #endif
    static member        ``</>`` (struct (f: Async<_>            , x: Async<'T>            ), [<Optional>]_output: Async<'U>            , [<Optional>]_mthd: ParApply) : Async<'U>            = Async.pmap2 (<|) f x
    static member        ``</>`` (struct (f: option<_>           , x: option<'T>           ), [<Optional>]_output: option<'U>           , [<Optional>]_mthd: ParApply)                        = Apply.``<*>`` (struct (f, x), _output, Unchecked.defaultof<Apply>)
    static member        ``</>`` (struct (f: voption<_>          , x: voption<'T>          ), [<Optional>]_output: voption<'U>          , [<Optional>]_mthd: ParApply)                        = Apply.``<*>`` (struct (f, x), _output, Unchecked.defaultof<Apply>)
    static member inline ``</>`` (struct (f: Result<_,'E>        , x: Result<'T,'E>        ), [<Optional>]_output: Result<'b,'E>        , [<Optional>]_mthd: ParApply) : Result<'U, 'E>       = Result.apply2With Plus.Invoke (<|) f x
    static member inline ``</>`` (struct (f: Choice<_,'E>        , x: Choice<'T,'E>        ), [<Optional>]_output: Choice<'b,'E>        , [<Optional>]_mthd: ParApply) : Choice<'U, 'E>       = Choice.apply2With Plus.Invoke (<|) f x
    static member inline ``</>`` (struct (f: KeyValuePair<'Key,_>, x: KeyValuePair<'Key,'T>), [<Optional>]_output: KeyValuePair<'Key,'U>, [<Optional>]_mthd: Default2)                        = Apply.``<*>`` (struct (f, x), _output, Unchecked.defaultof<Apply>)
    static member inline ``</>`` (struct (f: KeyValuePair2<_,_>, x: KeyValuePair2<_,'T> )   ,             _output: KeyValuePair2<_,'U>  ,             _mthd: Default2) : KeyValuePair2<'Key,'U> =
        let a, b = f.Key, x.Key
        let f, x = f.Value, x.Value
        KeyValuePair2 (Plus.Invoke a b, f x)


    static member        ``</>`` (struct (f: Map<'Key,_>         , x: Map<'Key,'T>         ), [<Optional>]_output: Map<'Key,'U>         , [<Optional>]_mthd: ParApply) : Map<'Key,'U>         = Apply.``<*>`` (struct (f, x), _output, Unchecked.defaultof<Apply>)
    static member        ``</>`` (struct (f: Dictionary<'Key,_>  , x: Dictionary<'Key,'T>  ), [<Optional>]_output: Dictionary<'Key,'U>  , [<Optional>]_mthd: ParApply) : Dictionary<'Key,'U>  = Apply.``<*>`` (struct (f, x), _output, Unchecked.defaultof<Apply>)
    static member        ``</>`` (struct (f: IDictionary<'Key,_> , x: IDictionary<'Key,'T> ), [<Optional>]_output: IDictionary<'Key,'U> , [<Optional>]_mthd: ParApply) : IDictionary<'Key,'U> = Apply.``<*>`` (struct (f, x), _output, Unchecked.defaultof<Apply>)
    static member        ``</>`` (struct (f: IReadOnlyDictionary<'Key,_>, x: IReadOnlyDictionary<'Key,'T> ), [<Optional>]_output: IReadOnlyDictionary<'Key,'U>, [<Optional>]_mthd: ParApply) : IReadOnlyDictionary<'Key,'U> = Apply.``<*>`` (struct (f, x), _output, Unchecked.defaultof<Apply>)

    #if !FABLE_COMPILER
    static member        ``</>`` (struct (f: Expr<'T->'U>, x: Expr<'T>), [<Optional>]_output: Expr<'U>, [<Optional>]_mthd: ParApply) = Apply.``<*>`` (struct (f, x), _output, Unchecked.defaultof<Apply>)
    #endif
    static member        ``</>`` (struct (f: ('T->'U) ResizeArray, x: 'T ResizeArray), [<Optional>]_output: 'U ResizeArray, [<Optional>]_mthd: ParApply) = ResizeArray.map2Shortest (<|) f x

    static member inline Invoke (f: '``ParApplicative<'T -> 'U>``) (x: '``ParApplicative<'T>``) : '``ParApplicative<'U>`` =
        let inline call (mthd: ^M, input1: ^I1, input2: ^I2, output: ^R) =
            ((^M or ^I1 or ^I2 or ^R) : (static member ``</>`` : struct (_*_) * _ * _ -> _) (struct (input1, input2)), output, mthd)
        call(Unchecked.defaultof<ParApply>, f, x, Unchecked.defaultof<'``ParApplicative<'U>``>)

#endif

    static member inline InvokeOnInstance (f: '``ParApplicative<'T->'U>``) (x: '``ParApplicative<'T>``) : '``ParApplicative<'U>`` =
        ((^``ParApplicative<'T->'U>`` or ^``ParApplicative<'T>`` or ^``ParApplicative<'U>``) : (static member (</>) : _*_ -> _) (f, x))

#if (!FABLE_COMPILER || FABLE_COMPILER_3) && !FABLE_COMPILER_4

type ParApply with
    static member inline ``</>`` (struct (_: ^t when ^t : null and ^t: struct, _: ^u when ^u : null and ^u: struct), _output: ^r when ^r : null and ^r: struct, _mthd: Default1) = id
    static member inline ``</>`` (struct (f: '``Applicative<'T->'U>``, x: '``Applicative<'T>``), _output: '``Applicative<'U>``, [<Optional>]_mthd: Default1) : '``Applicative<'U>`` =
        ((^``Applicative<'T->'U>`` or ^``Applicative<'T>`` or ^``Applicative<'U>``) : (static member (</>) : _*_ -> _) f, x)


type ParLift2 =
    inherit Default1

    static member        ParLift2 (f, (x: Lazy<_>            , y: Lazy<_>            ), _mthd: ParLift2) = Lift2.Lift2 (f, (x, y), Unchecked.defaultof<Lift2>)
    static member        ParLift2 (f, (x: seq<_>             , y: seq<_>             ), _mthd: ParLift2) = Seq.map2 f x y
    static member        ParLift2 (f, (x: NonEmptySeq<_>     , y: NonEmptySeq<_>     ), _mthd: ParLift2) = NonEmptySeq.map2 f x y
    static member        ParLift2 (f, (x: IEnumerator<_>     , y: IEnumerator<_>     ), _mthd: ParLift2) = Lift2.Lift2 (f, (x, y), Unchecked.defaultof<Lift2>)
    static member        ParLift2 (f, (x                     , y                     ), _mthd: ParLift2) = List.map2Shortest f x y
    static member        ParLift2 (f, (x: _ []               , y: _ []               ), _mthd: ParLift2) = Array.map2Shortest f x y
    static member        ParLift2 (f, (x: 'R -> 'T           , y: 'R -> 'U           ), _mthd: ParLift2) = Lift2.Lift2 (f, (x, y), Unchecked.defaultof<Lift2>)
    static member inline ParLift2 (f, (x: 'Monoid * 'T       , y: 'Monoid * 'U       ), _mthd: ParLift2) = Lift2.Lift2 (f, (x, y), Unchecked.defaultof<Lift2>)
    static member inline ParLift2 (f, (x: struct ('Monoid*'T), y: struct ('Monoid*'U)), _mthd: ParLift2) = Lift2.Lift2 (f, (x, y), Unchecked.defaultof<Lift2>)
    #if !FABLE_COMPILER
    static member        ParLift2 (f, (x: Task<'T>           , y: Task<'U>           ), _mthd: ParLift2) = Task.pmap2 f x y
    #endif
    #if NETSTANDARD2_1 && !FABLE_COMPILER
    static member        ParLift2 (f, (x: ValueTask<'T>      , y: ValueTask<'U>      ), _mthd: ParLift2) = ValueTask.pmap2 f x y
    #endif
    static member        ParLift2 (f, (x                     , y                     ), _mthd: ParLift2) = Async.pmap2 f x y
    static member        ParLift2 (f, (x: option<_>          , y: option<_>          ), _mthd: ParLift2) = Lift2.Lift2 (f, (x, y), Unchecked.defaultof<Lift2>)
    
    #if !FABLE_COMPILER
    static member        ParLift2 (f, (x: voption<_>         , y: voption<_>         ), _mthd: ParLift2) = Lift2.Lift2 (f, (x, y), Unchecked.defaultof<Lift2>)
    #endif
    static member inline ParLift2 (f, (x: Result<'T,'Error>  , y: Result<'U,'Error>  ), _mthd: ParLift2) = Result.apply2With Plus.Invoke f x y
    static member inline ParLift2 (f, (x: Choice<'T,'Error>  , y: Choice<'U,'Error>  ), _mthd: ParLift2) = Choice.map2 f x y
    static member        ParLift2 (f, (x: Map<'Key,'T>       , y : Map<'Key,'U>      ), _mthd: ParLift2) = Lift2.Lift2 (f, (x, y), Unchecked.defaultof<Lift2>)
    static member        ParLift2 (f, (x: Dictionary<'Key,'T>, y: Dictionary<'Key,'U>), _mthd: ParLift2) = Lift2.Lift2 (f, (x, y), Unchecked.defaultof<Lift2>)
    #if !FABLE_COMPILER
    static member        ParLift2 (f, (x: Expr<'T>           , y: Expr<'U>           ), _mthd: ParLift2) = Lift2.Lift2 (f, (x, y), Unchecked.defaultof<Lift2>)
    #endif
    static member        ParLift2 (f, (x: ResizeArray<'T>    , y: ResizeArray<'U>    ), _mthd: ParLift2) = ResizeArray.map2Shortest f x y

    static member inline Invoke (f: 'T -> 'U -> 'V) (x: '``ParApplicative<'T>``) (y: '``ParApplicative<'U>``) : '``ParApplicative<'V>`` =
        let inline call (mthd : ^M, input1: ^I1, input2: ^I2, _output: ^R) =
            ((^M or ^I1 or ^I2 or ^R) : (static member ParLift2 : _*(_*_)*_ -> _) f, (input1, input2), mthd)
        call (Unchecked.defaultof<ParLift2>, x, y, Unchecked.defaultof<'``ParApplicative<'V>``>)

    static member inline InvokeOnInstance (f: 'T -> 'U -> 'V) (x: '``ParApplicative<'T>``) (y: '``ParApplicative<'U>``) =
        ((^``ParApplicative<'T>`` or ^``ParApplicative<'U>``) : (static member ParLift2 : _*_*_ -> _) f, x, y)

type ParLift2 with
    static member inline ParLift2 (f, (x, y), _mthd: Default2) = (((ParReturn.InvokeOnInstance f, x) ||> ParApply.InvokeOnInstance), y) ||> ParApply.InvokeOnInstance

    static member inline ParLift2 (_, (_:'t when 't: null and 't: struct, _: ^u when ^u : null and ^u: struct), _mthd: Default1) = id
    static member inline ParLift2 (f: 'T -> 'U -> 'V, (x: '``ParApplicative<'T>``, y: '``ParApplicative<'U>``), _mthd: Default1) = ((^``ParApplicative<'T>`` or ^``ParApplicative<'U>`` ) : (static member ParLift2 : _*_*_ -> _) f, x, y)

type ParLift3 =
    inherit Default1

    static member        ParLift3 (f, (x: Lazy<_>            , y: Lazy<_>            , z: Lazy<_>             ), _mthd: ParLift3) = Lift3.Lift3 (f, (x, y, z), Unchecked.defaultof<Lift3>)
    static member        ParLift3 (f, (x: seq<_>             , y: seq<_>             , z: seq<_>              ), _mthd: ParLift3) = Seq.map3 f x y z
    static member        ParLift3 (f, (x: NonEmptySeq<_>     , y: NonEmptySeq<_>     , z: NonEmptySeq<_>      ), _mthd: ParLift3) = NonEmptySeq.map3 f x y z
    static member        ParLift3 (f, (x: IEnumerator<_>     , y: IEnumerator<_>     , z: IEnumerator<_>      ), _mthd: ParLift3) = Lift3.Lift3 (f, (x, y, z), Unchecked.defaultof<Lift3>)
    static member        ParLift3 (f, (x                     , y                     , z                      ), _mthd: ParLift3) = List.map3Shortest f x y z
    static member        ParLift3 (f, (x: _ []               , y: _ []               , z: _ []                ), _mthd: ParLift3) = Array.map3Shortest f x y z
    static member        ParLift3 (f, (x: 'R -> 'T           , y: 'R -> 'U           , z: 'R -> 'V            ), _mthd: ParLift3) = Lift3.Lift3 (f, (x, y, z), Unchecked.defaultof<Lift3>)
    static member inline ParLift3 (f, (x: 'Monoid * 'T       , y: 'Monoid * 'U       , z: 'Monoid * 'V        ), _mthd: ParLift3) = Lift3.Lift3 (f, (x, y, z), Unchecked.defaultof<Lift3>)
    static member inline ParLift3 (f, (x: struct ('Monoid*'T), y: struct ('Monoid*'U), z: struct ('Monoid* 'T)), _mthd: ParLift3) = Lift3.Lift3 (f, (x, y, z), Unchecked.defaultof<Lift3>)
    #if !FABLE_COMPILER
    static member        ParLift3 (f, (x: Task<'T>           , y: Task<'U>           , z: Task<'V>            ), _mthd: ParLift3) = Task.pmap3 f x y z
    #endif
    #if NETSTANDARD2_1 && !FABLE_COMPILER
    static member        ParLift3 (f, (x: ValueTask<'T>      , y: ValueTask<'U>      , z: ValueTask<'V>       ), _mthd: ParLift3) = ValueTask.pmap3 f x y z
    #endif
    static member        ParLift3 (f, (x                     , y                     , z                      ), _mthd: ParLift3) = Async.pmap3  f x y z
    static member        ParLift3 (f, (x: option<_>          , y: option<_>          , z: option<_>           ), _mthd: ParLift3) = Lift3.Lift3 (f, (x, y, z), Unchecked.defaultof<Lift3>)
    
    #if !FABLE_COMPILER
    static member        ParLift3 (f, (x: voption<_>         , y: voption<_>         , z: voption<_>          ), _mthd: ParLift3) = Lift3.Lift3 (f, (x, y, z), Unchecked.defaultof<Lift3>)
    #endif
    static member inline ParLift3 (f, (x: Result<'T,'Error>  , y: Result<'U,'Error>  , z: Result<'V, 'Error>  ), _mthd: ParLift3) = Result.apply3With Plus.Invoke f x y z
    static member inline ParLift3 (f, (x: Choice<'T,'Error>  , y: Choice<'U,'Error>  , z: Choice<'V, 'Error>  ), _mthd: ParLift3) = Choice.apply3With Plus.Invoke f x y z
    static member        ParLift3 (f, (x: Map<'Key,'T>       , y: Map<'Key,'U>       , z: Map<'Key, 'V>       ), _mthd: ParLift3) = Lift3.Lift3 (f, (x, y, z), Unchecked.defaultof<Lift3>)
    static member        ParLift3 (f, (x: Dictionary<'Key,'T>, y: Dictionary<'Key,'U>, z: Dictionary<'Key, 'V>), _mthd: ParLift3) = Lift3.Lift3 (f, (x, y, z), Unchecked.defaultof<Lift3>)
    #if !FABLE_COMPILER
    static member        ParLift3 (f, (x: Expr<'T>           , y: Expr<'U>           , z: Expr<'V>            ), _mthd: ParLift3) = Lift3.Lift3 (f, (x, y, z), Unchecked.defaultof<Lift3>)
    #endif
    static member        ParLift3 (f, (x: ResizeArray<'T>    , y: ResizeArray<'U>    , z: ResizeArray<'V>     ), _mthd: ParLift3) = ResizeArray.map3Shortest f x y z

    static member inline Invoke (f: 'T -> 'U -> 'V -> 'W) (x: '``ParApplicative<'T>``) (y: '``ParApplicative<'U>``) (z: '``ParApplicative<'V>``) : '``ParApplicative<'W>`` =
        let inline call (mthd: ^M, input1: ^I1, input2: ^I2, input3: ^I3, _output: ^R) =
            ((^M or ^I1 or ^I2 or ^I3 or ^R) : (static member ParLift3 : _*(_*_*_)*_ -> _) f, (input1, input2, input3), mthd)
        call (Unchecked.defaultof<ParLift3>, x, y, z, Unchecked.defaultof<'``ParApplicative<'W>``>)

    static member inline InvokeOnInstance (f: 'T -> 'U -> 'V -> 'W) (x: '``ParApplicative<'T>``) (y: '``ParApplicative<'U>``) (z: '``ParApplicative<'V>``)=
        ((^``ParApplicative<'T>`` or ^``ParApplicative<'U>`` or ^``ParApplicative<'V>``) : (static member ParLift3 : _*_*_*_ -> _) f, x, y, z)

type ParLift3 with
    static member inline ParLift3 (f, (x, y, z), _mthd: Default3) = ((((ParReturn.InvokeOnInstance f, x) ||> ParApply.InvokeOnInstance), y) ||> ParApply.InvokeOnInstance, z) ||> ParApply.InvokeOnInstance
    static member inline ParLift3 (_, (_:'t when 't: null and 't: struct, _: ^u when ^u : null and ^u: struct, _: ^v when ^v : null and ^v: struct), _mthd: Default1) = id
    static member inline ParLift3 (f: 'T -> 'U -> 'V -> 'W, (x: '``ParApplicative<'T>``, y: '``ParApplicative<'U>``, z: '``ParApplicative<'V>``)   , _mthd: Default1) = ((^``ParApplicative<'T>`` or ^``ParApplicative<'U>`` or ^``ParApplicative<'V>`` ) : (static member ParLift3 : _*_*_*_ -> _) f, x, y, z)

type IsParLeftZero =
    inherit Default1

    static member IsParLeftZero (t: ref<seq<_>>        , _mthd: IsParLeftZero) = Seq.isEmpty t.Value
    static member IsParLeftZero (_: ref<NonEmptySeq<_>>, _mthd: IsParLeftZero) = false
    static member IsParLeftZero (t: ref<list<_>>       , _mthd: IsParLeftZero) = List.isEmpty t.Value
    static member IsParLeftZero (t: ref<array<_>>      , _mthd: IsParLeftZero) = Array.isEmpty t.Value
    static member IsParLeftZero (t: ref<option<_>>     , _mthd: IsParLeftZero) = IsLeftZero.IsLeftZero (t, Unchecked.defaultof<IsLeftZero>)
    #if !FABLE_COMPILER
    static member IsParLeftZero (t: ref<voption<_>>    , _mthd: IsParLeftZero) = IsLeftZero.IsLeftZero (t, Unchecked.defaultof<IsLeftZero>)
    #endif
    static member IsParLeftZero (_: ref<Result<_,_>>   , _mthd: IsParLeftZero) = false
    static member IsParLeftZero (_: ref<Choice<_,_>>   , _mthd: IsParLeftZero) = false

    static member inline Invoke (x: '``ParApplicative<'T>``) : bool =
        let inline call (mthd: ^M, input: ^I) =
            ((^M or ^I) : (static member IsParLeftZero : _*_ -> _) ref input, mthd)
        call(Unchecked.defaultof<IsParLeftZero>, x)

    static member inline InvokeOnInstance (x: '``ParApplicative<'T>``) : bool =
        ((^``ParApplicative<'T>``) : (static member IsParLeftZero : _ -> _) x)

type IsParLeftZero with

    static member inline IsParLeftZero (_: ref<'T>   when 'T : struct    , _mthd: Default4) = false
    static member inline IsParLeftZero (_: ref<'T>   when 'T : not struct, _mthd: Default3) = false

    // empty </> f = empty  ==> empty is left zero for </>
    static member inline IsParLeftZero (t: ref<'``Alternative<'T>``>     , _mthd: Default2) = (t.Value = Empty.InvokeOnInstance ())

    static member inline IsParLeftZero (t: ref<'``ParApplicative<'T>``>  , _mthd: Default1) = (^``ParApplicative<'T>`` : (static member IsParLeftZero : _ -> _) t.Value)
    static member inline IsParLeftZero (_: ref< ^t> when ^t: null and ^t: struct, _: Default1) = ()

#endif
