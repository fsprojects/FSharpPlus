﻿namespace FSharpPlus.Control

open System
open System.Runtime.InteropServices
open System.Collections.Generic
open System.Threading.Tasks
open Microsoft.FSharp.Quotations

open FSharpPlus.Internals
open FSharpPlus
open FSharpPlus.Data


type ParReturn =
    inherit Default1
    static member inline InvokeOnInstance (x: 'T) = (^``ParApplicative<'T>`` : (static member ParReturn : ^T -> ^``ParApplicative<'T>``) x)

#if (!FABLE_COMPILER || FABLE_COMPILER_3) && !FABLE_COMPILER_4

    static member inline Invoke (x: 'T) : '``ParApplicative<'T>`` =
        let inline call (mthd: ^M, output: ^R) = ((^M or ^R) : (static member ParReturn : _*_ -> _) output, mthd)
        call (Unchecked.defaultof<ParReturn>, Unchecked.defaultof<'``ParApplicative<'T>``>) x
    
    

    static member        ParReturn (_: seq<'a>        , _: Default2) = fun  x      -> Seq.initInfinite (fun _ -> x) : seq<'a>
    static member        ParReturn (_: NonEmptySeq<'a>, _: Default2) = fun  x      -> NonEmptySeq.initInfinite (fun _ -> x) : NonEmptySeq<'a>
    static member        ParReturn (_: IEnumerator<'a>, _: Default2) = fun  x      -> Enumerator.upto None (fun _ -> x) : IEnumerator<'a>
    static member inline ParReturn (_: 'R             , _: Default1) = fun (x: 'T) -> ParReturn.InvokeOnInstance x         : 'R
    static member        ParReturn (_: Lazy<'a>       , _: ParReturn  ) = fun x -> Lazy<_>.CreateFromValue x : Lazy<'a>
    #if !FABLE_COMPILER
    static member        ParReturn (_: 'T Task        , _: ParReturn  ) = fun x -> Task.FromResult x                    : 'T Task
    #endif
    #if NETSTANDARD2_1 && !FABLE_COMPILER
    static member        ParReturn (_: 'T ValueTask   , _: ParReturn  ) = fun (x: 'T) -> ValueTask<'T> x                : 'T ValueTask
    #endif
    static member inline ParReturn (_: option<'a>     , _: ParReturn  ) = fun x -> Some x                               : option<'a>
    static member inline ParReturn (_: voption<'a>    , _: ParReturn  ) = fun x -> ValueSome x                          : voption<'a>
    static member        ParReturn (_: list<'a>       , _: ParReturn  ) = fun x -> List.cycle [x]                       : list<'a>
    static member        ParReturn (_: 'a []          , _: ParReturn  ) = fun x -> [|x|]                                : 'a []
    static member        ParReturn (_: 'r -> 'a       , _: ParReturn  ) = Prelude.const': 'a -> 'r -> _
    static member inline ParReturn (_:  'm * 'a       , _: ParReturn  ) = fun (x: 'a) -> (Zero.Invoke (): 'm), x
    static member inline ParReturn (_: struct ('m * 'a), _: ParReturn ) = fun (x: 'a) -> struct ((Zero.Invoke (): 'm), x)
    static member        ParReturn (_: 'a Async       , _: ParReturn  ) = fun (x: 'a) -> async.Return x
    static member inline ParReturn (_: Result<'t,'e>  , _: ParReturn  ) = fun x -> if false then Error (Zero.Invoke (): 'e) else Ok x              : Result<'t,'e>
    static member inline ParReturn (_: Choice<'t,'e>  , _: ParReturn  ) = fun x -> if false then Choice2Of2 (Zero.Invoke (): 'e) else Choice1Of2 x : Choice<'t,'e>
    #if !FABLE_COMPILER
    static member        ParReturn (_: Expr<'a>       , _: ParReturn  ) = fun x -> Expr.Cast<'a> (Expr.Value (x: 'a))
    #endif
    static member        ParReturn (_: ResizeArray<'a>, _: ParReturn  ) = fun x -> ResizeArray<'a> (Seq.singleton x)

#endif

type ParApply =
    inherit Default1
 
#if (!FABLE_COMPILER || FABLE_COMPILER_3) && !FABLE_COMPILER_4

    static member inline ``</>`` (f: '``Monad<'T->'U>``      , x: '``Monad<'T>``      , [<Optional>]_output: '``Monad<'U>``      , [<Optional>]_mthd:Default2) : '``Monad<'U>``       = Bind.InvokeOnInstance f (fun (x1: 'T->'U) -> Bind.InvokeOnInstance x (fun x2 -> ParReturn.InvokeOnInstance (x1 x2)))
    static member inline ``</>`` (f: '``ParApplicative<'T->'U>``, x: '``ParApplicative<'T>``, [<Optional>]_output: '``ParApplicative<'U>``, [<Optional>]_mthd:Default1) : '``ParApplicative<'U>`` = ((^``ParApplicative<'T->'U>`` or ^``ParApplicative<'T>`` or ^``ParApplicative<'U>``) : (static member (<*>) : _*_ -> _) f, x)

    static member        ``</>`` (f: Lazy<'T->'U>     , x: Lazy<'T>             , [<Optional>]_output: Lazy<'U>             , [<Optional>]_mthd: ParApply) = Lazy.apply f x                               : Lazy<'U>
    static member        ``</>`` (f: seq<_>           , x: seq<'T>              , [<Optional>]_output: seq<'U>              , [<Optional>]_mthd: ParApply) = Seq.map2 (<|) f x                               : seq<'U>
    static member        ``</>`` (f: NonEmptySeq<_>   , x: NonEmptySeq<'T>      , [<Optional>]_output: NonEmptySeq<'U>      , [<Optional>]_mthd: ParApply) = NonEmptySeq.map2 (<|) f x                       : NonEmptySeq<'U>
    static member        ``</>`` (f: IEnumerator<_>   , x: IEnumerator<'T>      , [<Optional>]_output: IEnumerator<'U>      , [<Optional>]_mthd: ParApply) = Enumerator.map2 id f x : IEnumerator<'U>
    static member        ``</>`` (f: list<_>          , x: list<'T>             , [<Optional>]_output: list<'U>             , [<Optional>]_mthd: ParApply) = List.map2Shortest (<|) f x                               : list<'U>
    static member        ``</>`` (f: _ []             , x: 'T []                , [<Optional>]_output: 'U []                , [<Optional>]_mthd: ParApply) = Array.apply f x                              : 'U []
    static member        ``</>`` (f: 'r -> _          , g: _ -> 'T              , [<Optional>]_output:  'r -> 'U            , [<Optional>]_mthd: ParApply) = fun x -> let f' = f x in f' (g x)            : 'U
    static member inline ``</>`` ((a: 'Monoid, f)     , (b: 'Monoid, x: 'T)     , [<Optional>]_output: 'Monoid * 'U         , [<Optional>]_mthd: ParApply) = (Plus.Invoke a b, f x)                       : 'Monoid *'U
    static member inline ``</>`` (struct (a: 'Monoid, f), struct (b: 'Monoid, x: 'T), [<Optional>]_output: struct ('Monoid * 'U), [<Optional>]_mthd: ParApply) = struct (Plus.Invoke a b, f x)            : struct ('Monoid * 'U)
    #if !FABLE_COMPILER
    static member        ``</>`` (f: Task<_>          , x: Task<'T>             , [<Optional>]_output: Task<'U>             , [<Optional>]_mthd: ParApply) = Task.apply   f x : Task<'U>
    #endif
    #if NETSTANDARD2_1 && !FABLE_COMPILER
    static member        ``</>`` (f: ValueTask<_>     , x: ValueTask<'T>        , [<Optional>]_output: ValueTask<'U>        , [<Optional>]_mthd: ParApply) = ValueTask.apply   f x : ValueTask<'U>
    #endif
    static member        ``</>`` (f: Async<_>         , x: Async<'T>            , [<Optional>]_output: Async<'U>            , [<Optional>]_mthd: ParApply) = Async.parMap2 (<|) f x : Async<'U>
    static member        ``</>`` (f: option<_>        , x: option<'T>           , [<Optional>]_output: option<'U>           , [<Optional>]_mthd: ParApply) = Option.apply f x : option<'U>
    static member        ``</>`` (f: voption<_>       , x: voption<'T>          , [<Optional>]_output: voption<'U>          , [<Optional>]_mthd: ParApply) = ValueOption.apply f x : voption<'U>
    static member inline ``</>`` (f: Result<_,'E>     , x: Result<'T,'E>        , [<Optional>]_output: Result<'b,'E>        , [<Optional>]_mthd: ParApply) = Result.apply2With Plus.Invoke (<|) f x : Result<'U, 'E>
    static member inline ``</>`` (f: Choice<_,'E>     , x: Choice<'T,'E>        , [<Optional>]_output: Choice<'b,'E>        , [<Optional>]_mthd: ParApply) = Choice.apply2With Plus.Invoke (<|) f x : Choice<'U, 'E>
    static member inline ``</>`` (KeyValue(a: 'Key, f), KeyValue(b: 'Key, x: 'T), [<Optional>]_output: KeyValuePair<'Key,'U>, [<Optional>]_mthd: ParApply) : KeyValuePair<'Key,'U> = KeyValuePair (Plus.Invoke a b, f x)

    static member        ``</>`` (f: Map<'Key,_>      , x: Map<'Key,'T>         , [<Optional>]_output: Map<'Key,'U>         , [<Optional>]_mthd: ParApply) : Map<'Key,'U> = Map (seq {
       for KeyValue(k, vf) in f do
           match Map.tryFind k x with
           | Some vx -> yield k, vf vx
           | _       -> () })

    static member        ``</>`` (f: Dictionary<'Key,_>, x: Dictionary<'Key,'T> , [<Optional>]_output: Dictionary<'Key,'U>  , [<Optional>]_mthd: ParApply) : Dictionary<'Key,'U> =
       let dct = Dictionary ()
       for KeyValue(k, vf) in f do
           match x.TryGetValue k with
           | true, vx -> dct.Add (k, vf vx)
           | _        -> ()
       dct

    static member        ``</>`` (f: IDictionary<'Key,_>, x: IDictionary<'Key,'T> , [<Optional>]_output: IDictionary<'Key,'U>  , [<Optional>]_mthd: ParApply) : IDictionary<'Key,'U> =
       let dct = Dictionary ()
       for KeyValue(k, vf) in f do
           match x.TryGetValue k with
           | true, vx -> dct.Add (k, vf vx)
           | _        -> ()
       dct :> IDictionary<'Key,'U>

    static member        ``</>`` (f: IReadOnlyDictionary<'Key,_>, x: IReadOnlyDictionary<'Key,'T> , [<Optional>]_output: IReadOnlyDictionary<'Key,'U>  , [<Optional>]_mthd: ParApply) : IReadOnlyDictionary<'Key,'U> =
       let dct = Dictionary ()
       for KeyValue(k, vf) in f do
           match x.TryGetValue k with
           | true, vx -> dct.Add (k, vf vx)
           | _        -> ()
       dct :> IReadOnlyDictionary<'Key,'U>

    #if !FABLE_COMPILER
    static member        ``</>`` (f: Expr<'T->'U>, x: Expr<'T>, [<Optional>]_output: Expr<'U>, [<Optional>]_mthd: ParApply) = Expr.Cast<'U> (Expr.Application (f, x))
    #endif
    static member        ``</>`` (f: ('T->'U) ResizeArray, x: 'T ResizeArray, [<Optional>]_output: 'U ResizeArray, [<Optional>]_mthd: ParApply) = ResizeArray.apply f x : 'U ResizeArray

    static member inline Invoke (f: '``ParApplicative<'T -> 'U>``) (x: '``ParApplicative<'T>``) : '``ParApplicative<'U>`` =
        let inline call (mthd : ^M, input1: ^I1, input2: ^I2, output: ^R) =
            ((^M or ^I1 or ^I2 or ^R) : (static member ``</>`` : _*_*_*_ -> _) input1, input2, output, mthd)
        call(Unchecked.defaultof<ParApply>, f, x, Unchecked.defaultof<'``ParApplicative<'U>``>)


#endif

    static member inline InvokeOnInstance (f: '``ParApplicative<'T->'U>``) (x: '``ParApplicative<'T>``) : '``ParApplicative<'U>`` =
        ((^``ParApplicative<'T->'U>`` or ^``ParApplicative<'T>`` or ^``ParApplicative<'U>``) : (static member (<*>) : _*_ -> _) (f, x))

#if (!FABLE_COMPILER || FABLE_COMPILER_3) && !FABLE_COMPILER_4

type ParLift2 =
    inherit Default1

    static member        ParLift2 (f, (x: Lazy<_>            , y: Lazy<_>            ), _mthd: ParLift2) = Lazy.map2 f x y
    static member        ParLift2 (f, (x: seq<_>             , y: seq<_>             ), _mthd: ParLift2) = Seq.map2 f x y
    static member        ParLift2 (f, (x: NonEmptySeq<_>     , y: NonEmptySeq<_>     ), _mthd: ParLift2) = NonEmptySeq.map2 f x y
    static member        ParLift2 (f, (x: IEnumerator<_>     , y: IEnumerator<_>     ), _mthd: ParLift2) = Enumerator.map2 f x y
    static member        ParLift2 (f, (x                     , y                     ), _mthd: ParLift2) = List.map2Shortest f x y
    static member        ParLift2 (f, (x                     , y                     ), _mthd: ParLift2) = Array.lift2 f x y
    static member        ParLift2 (f, (x: 'R -> 'T           , y: 'R -> 'U           ), _mthd: ParLift2) = fun a -> f (x a) (y a)
    static member inline ParLift2 (f, ((a: 'Monoid, x: 'T)   , (b: 'Monoid, y: 'U)   ), _mthd: ParLift2) = Plus.Invoke a b, f x y
    static member inline ParLift2 (f, (struct (a: 'Monoid, x: 'T), struct (b: 'Monoid, y: 'U)), _mthd: ParLift2) = struct (Plus.Invoke a b, f x y)
    #if !FABLE_COMPILER
    static member        ParLift2 (f, (x: Task<'T>           , y: Task<'U>           ), _mthd: ParLift2) = Task.map2  f x y
    #endif
    #if NETSTANDARD2_1 && !FABLE_COMPILER
    static member        ParLift2 (f, (x: ValueTask<'T>      , y: ValueTask<'U>      ), _mthd: ParLift2) = ValueTask.map2  f x y
    #endif
    static member        ParLift2 (f, (x                     , y                     ), _mthd: ParLift2) = Async.parMap2 f x y
    static member        ParLift2 (f, (x                     , y                     ), _mthd: ParLift2) = Option.map2 f x y
    
    #if !FABLE_COMPILER
    static member        ParLift2 (f, (x                     , y                     ), _mthd: ParLift2) = ValueOption.map2 f x y
    #endif
    static member inline ParLift2 (f, (x: Result<'T,'Error>  , y: Result<'U,'Error>  ), _mthd: ParLift2) = Result.apply2With Plus.Invoke f x y
    static member inline ParLift2 (f, (x: Choice<'T,'Error>  , y: Choice<'U,'Error>  ), _mthd: ParLift2) = Choice.map2 f x y
    static member        ParLift2 (f, (x: Map<'Key,'T>       , y : Map<'Key,'U>      ), _mthd: ParLift2) = Map.mapValues2 f x y
    static member        ParLift2 (f, (x: Dictionary<'Key,'T>, y: Dictionary<'Key,'U>), _mthd: ParLift2) = Dictionary.map2 f x y
    #if !FABLE_COMPILER
    static member        ParLift2 (f, (x: Expr<'T>           , y: Expr<'U>           ), _mthd: ParLift2) = <@ f %x %y @>
    #endif
    static member        ParLift2 (f, (x: ResizeArray<'T>    , y: ResizeArray<'U>    ), _mthd: ParLift2) = ResizeArray.lift2 f x y

    static member inline Invoke (f: 'T -> 'U -> 'V) (x: '``ParApplicative<'T>``) (y: '``ParApplicative<'U>``) : '``ParApplicative<'V>`` =
        let inline call (mthd : ^M, input1: ^I1, input2: ^I2, _output: ^R) =
            ((^M or ^I1 or ^I2 or ^R) : (static member ParLift2 : _*(_*_)*_ -> _) f, (input1, input2), mthd)
        call (Unchecked.defaultof<ParLift2>, x, y, Unchecked.defaultof<'``ParApplicative<'V>``>)

    static member inline InvokeOnInstance (f: 'T -> 'U -> 'V) (x: '``ParApplicative<'T>``) (y: '``ParApplicative<'U>``) =
        ((^``ParApplicative<'T>`` or ^``ParApplicative<'U>``) : (static member ParLift2 : _*_*_ -> _) f, x, y)

type ParLift2 with
    static member inline ParLift2 (f, (x, y), _mthd: Default2) = (((ParReturn.InvokeOnInstance f, x) ||> ParApply.InvokeOnInstance), y) ||> ParApply.InvokeOnInstance

    static member inline ParLift2 (_, (_:'t when 't: null and 't: struct, _: ^u when ^u : null and ^u: struct), _mthd: Default1) = id
    static member inline ParLift2 (f: 'T -> 'U -> 'V, (x: '``ParApplicative<'T>``, y: '``ParApplicative<'U>``)      , _mthd: Default1) = ((^``ParApplicative<'T>`` or ^``ParApplicative<'U>`` ) : (static member ParLift2 : _*_*_ -> _) f, x, y)

type ParLift3 =
    inherit Default1

    static member        ParLift3 (f, (x: Lazy<_>            , y: Lazy<_>            , z: Lazy<_>             ), _mthd: ParLift3) = Lazy.map3 f x y z
    static member        ParLift3 (f, (x: seq<_>             , y: seq<_>             , z: seq<_>              ), _mthd: ParLift3) = Seq.map3 f x y z
    static member        ParLift3 (f, (x: NonEmptySeq<_>     , y: NonEmptySeq<_>     , z: NonEmptySeq<_>      ), _mthd: ParLift3) = NonEmptySeq.map3 f x y z
    static member        ParLift3 (f, (x: IEnumerator<_>     , y: IEnumerator<_>     , z: IEnumerator<_>      ), _mthd: ParLift3) = Enumerator.map3 f x y z
    static member        ParLift3 (f, (x                     , y                     , z                      ), _mthd: ParLift3) = List.map3Shortest f x y z
    static member        ParLift3 (f, (x                     , y                     , z                      ), _mthd: ParLift3) = Array.lift3 f x y z
    static member        ParLift3 (f, (x: 'R -> 'T           , y: 'R -> 'U           , z: 'R -> 'V            ), _mthd: ParLift3) = fun a -> f (x a) (y a) (z a)
    static member inline ParLift3 (f, ((a: 'Monoid, x: 'T)   , (b: 'Monoid, y: 'U)   , (c: 'Monoid, z: 'U)    ), _mthd: ParLift3) = Plus.Invoke (Plus.Invoke a b) c, f x y z
    static member inline ParLift3 (f, (struct (a: 'Monoid, x: 'T), struct (b: 'Monoid, y: 'U), struct (c: 'Monoid, z: 'U)), _mthd: ParLift3) = struct (Plus.Invoke (Plus.Invoke a b) c, f x y z)
    #if !FABLE_COMPILER
    static member        ParLift3 (f, (x: Task<'T>           , y: Task<'U>           , z: Task<'V>            ), _mthd: ParLift3) = Task.map3  f x y z
    #endif
    #if NETSTANDARD2_1 && !FABLE_COMPILER
    static member        ParLift3 (f, (x: ValueTask<'T>      , y: ValueTask<'U>      , z: ValueTask<'V>       ), _mthd: ParLift3) = ValueTask.map3  f x y z
    #endif
    static member        ParLift3 (f, (x                     , y                     , z                      ), _mthd: ParLift3) = Async.parMap3  f x y z
    static member        ParLift3 (f, (x                     , y                     , z                      ), _mthd: ParLift3) = Option.map3 f x y z
    
    #if !FABLE_COMPILER
    static member        ParLift3 (f, (x                     , y                     , z                      ), _mthd: ParLift3) = ValueOption.map3 f x y z
    #endif
    static member inline ParLift3 (f, (x: Result<'T,'Error>  , y: Result<'U,'Error>  , z: Result<'V, 'Error>  ), _mthd: ParLift3) = Result.apply3With Plus.Invoke f x y z
    static member inline ParLift3 (f, (x: Choice<'T,'Error>  , y: Choice<'U,'Error>  , z: Choice<'V, 'Error>  ), _mthd: ParLift3) = Choice.apply3With Plus.Invoke f x y z
    static member        ParLift3 (f, (x: Map<'Key,'T>       , y: Map<'Key,'U>       , z: Map<'Key, 'V>       ), _mthd: ParLift3) = Map.mapValues3 f x y z
    static member        ParLift3 (f, (x: Dictionary<'Key,'T>, y: Dictionary<'Key,'U>, z: Dictionary<'Key, 'V>), _mthd: ParLift3) = Dictionary.map3 f x y z
    #if !FABLE_COMPILER
    static member        ParLift3 (f, (x: Expr<'T>           , y: Expr<'U>           , z: Expr<'V>            ), _mthd: ParLift3) = <@ f %x %y %z @>
    #endif
    static member        ParLift3 (f, (x: ResizeArray<'T>    , y: ResizeArray<'U>    , z: ResizeArray<'V>     ), _mthd: ParLift3) = ResizeArray.lift3 f x y z

    static member inline Invoke (f: 'T -> 'U -> 'V -> 'W) (x: '``ParApplicative<'T>``) (y: '``ParApplicative<'U>``) (z: '``ParApplicative<'V>``): '``ParApplicative<'W>`` =
        let inline call (mthd : ^M, input1: ^I1, input2: ^I2, input3: ^I3, _output: ^R) =
            ((^M or ^I1 or ^I2 or ^I3 or ^R) : (static member ParLift3 : _*(_*_*_)*_ -> _) f, (input1, input2, input3), mthd)
        call (Unchecked.defaultof<ParLift3>, x, y, z, Unchecked.defaultof<'``ParApplicative<'W>``>)

    static member inline InvokeOnInstance (f: 'T -> 'U -> 'V -> 'W) (x: '``ParApplicative<'T>``) (y: '``ParApplicative<'U>``) (z: '``ParApplicative<'V>``)=
        ((^``ParApplicative<'T>`` or ^``ParApplicative<'U>`` or ^``ParApplicative<'V>``) : (static member ParLift3 : _*_*_*_ -> _) f, x, y, z)

type ParLift3 with
    static member inline ParLift3 (f, (x, y, z), _mthd: Default3) = ((((ParReturn.InvokeOnInstance f, x) ||> ParApply.InvokeOnInstance), y) ||> ParApply.InvokeOnInstance, z) ||> ParApply.InvokeOnInstance
    static member inline ParLift3 (_, (_:'t when 't: null and 't: struct, _: ^u when ^u : null and ^u: struct, _: ^v when ^v : null and ^v: struct), _mthd: Default1) = id
    static member inline ParLift3 (f: 'T -> 'U -> 'V -> 'W, (x: '``ParApplicative<'T>``, y: '``ParApplicative<'U>``, z: '``ParApplicative<'V>``)            , _mthd: Default1) = ((^``ParApplicative<'T>`` or ^``ParApplicative<'U>`` or ^``ParApplicative<'V>`` ) : (static member ParLift3 : _*_*_*_ -> _) f, x, y, z)

type IsParLeftZero =
    inherit Default1

    static member IsParLeftZero (_: ref<seq<_>>      , _mthd: IsParLeftZero) = false
    static member IsParLeftZero (_: ref<NonEmptySeq<_>>, _mthd: IsParLeftZero) = false
    static member IsParLeftZero (_: ref<list<_>>     , _mthd: IsParLeftZero) = false
    static member IsParLeftZero (t: ref<array<_>>    , _mthd: IsParLeftZero) = Array.isEmpty t.Value
    static member IsParLeftZero (t: ref<option<_>>   , _mthd: IsParLeftZero) = Option.isNone t.Value
    #if !FABLE_COMPILER
    static member IsParLeftZero (t: ref<voption<_>>  , _mthd: IsParLeftZero) = ValueOption.isNone t.Value
    #endif
    static member IsParLeftZero (_: ref<Result<_,_>> , _mthd: IsParLeftZero) = false
    static member IsParLeftZero (_: ref<Choice<_,_>> , _mthd: IsParLeftZero) = false

    static member inline Invoke (x: '``ParApplicative<'T>``) : bool =
        let inline call (mthd : ^M, input: ^I) =
            ((^M or ^I) : (static member IsParLeftZero : _*_ -> _) ref input, mthd)
        call(Unchecked.defaultof<IsParLeftZero>, x)

    static member inline InvokeOnInstance (x: '``ParApplicative<'T>``) : bool =
        ((^``ParApplicative<'T>``) : (static member IsParLeftZero : _ -> _) x)

type IsParLeftZero with

    static member inline IsParLeftZero (_: ref<'T>   when 'T : struct    , _mthd: Default4) = false
    static member inline IsParLeftZero (_: ref<'T>   when 'T : not struct, _mthd: Default3) = false

    // empty <*> f = empty  ==> empty is left zero for <*>
    static member inline IsParLeftZero (t: ref<'``Alternative<'T>``>        , _mthd: Default2) = (t.Value = Empty.InvokeOnInstance ())

    static member inline IsParLeftZero (t: ref<'``ParApplicative<'T>``>        , _mthd: Default1) = (^``ParApplicative<'T>`` : (static member IsParLeftZero : _ -> _) t.Value)
    static member inline IsParLeftZero (_: ref< ^t> when ^t: null and ^t: struct, _: Default1) = ()

#endif