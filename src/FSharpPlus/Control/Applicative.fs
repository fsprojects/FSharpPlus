namespace FSharpPlus.Control

open System
open System.Runtime.InteropServices
open System.Collections.Generic
open System.Threading.Tasks
open Microsoft.FSharp.Quotations

open FSharpPlus.Internals
open FSharpPlus
open FSharpPlus.Data


type Apply =
    inherit Default1
 
#if !FABLE_COMPILER || (FABLE_COMPILER_3 || FABLE_COMPILER_4)

    static member inline ``<*>`` (f: '``Monad<'T->'U>``      , x: '``Monad<'T>``      , [<Optional>]_output: '``Monad<'U>``      , [<Optional>]_mthd:Default2) : '``Monad<'U>``       = Bind.InvokeOnInstance f (fun (x1: 'T->'U) -> Bind.InvokeOnInstance x (fun x2 -> Return.InvokeOnInstance (x1 x2)))
    static member inline ``<*>`` (f: '``Applicative<'T->'U>``, x: '``Applicative<'T>``, [<Optional>]_output: '``Applicative<'U>``, [<Optional>]_mthd:Default1) : '``Applicative<'U>`` = ((^``Applicative<'T->'U>`` or ^``Applicative<'T>`` or ^``Applicative<'U>``) : (static member (<*>) : _*_ -> _) f, x)

    static member        ``<*>`` (f: Lazy<'T->'U>     , x: Lazy<'T>             , [<Optional>]_output: Lazy<'U>             , [<Optional>]_mthd: Apply) = Lazy.apply f x                               : Lazy<'U>
    static member        ``<*>`` (f: seq<_>           , x: seq<'T>              , [<Optional>]_output: seq<'U>              , [<Optional>]_mthd: Apply) = Seq.apply  f x                               : seq<'U>
    static member        ``<*>`` (f: NonEmptySeq<_>   , x: NonEmptySeq<'T>      , [<Optional>]_output: NonEmptySeq<'U>      , [<Optional>]_mthd: Apply) = NonEmptySeq.apply  f x                       : NonEmptySeq<'U>
    static member        ``<*>`` (f: IEnumerator<_>   , x: IEnumerator<'T>      , [<Optional>]_output: IEnumerator<'U>      , [<Optional>]_mthd: Apply) = Enumerator.map2 id f x : IEnumerator<'U>
    static member        ``<*>`` (f: list<_>          , x: list<'T>             , [<Optional>]_output: list<'U>             , [<Optional>]_mthd: Apply) = List.apply f x                               : list<'U>
    static member        ``<*>`` (f: _ []             , x: 'T []                , [<Optional>]_output: 'U []                , [<Optional>]_mthd: Apply) = Array.apply f x                              : 'U []
    static member        ``<*>`` (f: 'r -> _          , g: _ -> 'T              , [<Optional>]_output:  'r -> 'U            , [<Optional>]_mthd: Apply) = fun x -> let f' = f x in f' (g x)            : 'U
    static member inline ``<*>`` ((a: 'Monoid, f)     , (b: 'Monoid, x: 'T)     , [<Optional>]_output: 'Monoid * 'U         , [<Optional>]_mthd: Apply) = (Plus.Invoke a b, f x)                       : 'Monoid *'U
    static member inline ``<*>`` (struct (a: 'Monoid, f), struct (b: 'Monoid, x: 'T), [<Optional>]_output: struct ('Monoid * 'U), [<Optional>]_mthd: Apply) = struct (Plus.Invoke a b, f x)            : struct ('Monoid * 'U)
    #if !FABLE_COMPILER
    static member        ``<*>`` (f: Task<_>          , x: Task<'T>             , [<Optional>]_output: Task<'U>             , [<Optional>]_mthd: Apply) = Task.apply   f x : Task<'U>
    #endif
    #if NETSTANDARD2_1 && !FABLE_COMPILER
    static member        ``<*>`` (f: ValueTask<_>     , x: ValueTask<'T>        , [<Optional>]_output: ValueTask<'U>        , [<Optional>]_mthd: Apply) = ValueTask.apply   f x : ValueTask<'U>
    #endif
    static member        ``<*>`` (f: Async<_>         , x: Async<'T>            , [<Optional>]_output: Async<'U>            , [<Optional>]_mthd: Apply) = Async.apply  f x : Async<'U>
    static member        ``<*>`` (f: option<_>        , x: option<'T>           , [<Optional>]_output: option<'U>           , [<Optional>]_mthd: Apply) = Option.apply f x : option<'U>
    static member        ``<*>`` (f: voption<_>       , x: voption<'T>          , [<Optional>]_output: voption<'U>          , [<Optional>]_mthd: Apply) = ValueOption.apply f x : voption<'U>
    static member        ``<*>`` (f: Result<_,'E>     , x: Result<'T,'E>        , [<Optional>]_output: Result<'b,'E>        , [<Optional>]_mthd: Apply) = Result.apply f x : Result<'U,'E>
    static member        ``<*>`` (f: Choice<_,'E>     , x: Choice<'T,'E>        , [<Optional>]_output: Choice<'b,'E>        , [<Optional>]_mthd: Apply) = Choice.apply f x : Choice<'U,'E>
    static member inline ``<*>`` (KeyValue(a: 'Key, f), KeyValue(b: 'Key, x: 'T), [<Optional>]_output: KeyValuePair<'Key,'U>, [<Optional>]_mthd: Apply) : KeyValuePair<'Key,'U> = KeyValuePair (Plus.Invoke a b, f x)

    static member        ``<*>`` (f: Map<'Key,_>      , x: Map<'Key,'T>         , [<Optional>]_output: Map<'Key,'U>         , [<Optional>]_mthd: Apply) : Map<'Key,'U> = Map (seq {
       for KeyValue(k, vf) in f do
           match Map.tryFind k x with
           | Some vx -> yield k, vf vx
           | _       -> () })

    static member        ``<*>`` (f: Dictionary<'Key,_>, x: Dictionary<'Key,'T> , [<Optional>]_output: Dictionary<'Key,'U>  , [<Optional>]_mthd: Apply) : Dictionary<'Key,'U> =
       let dct = Dictionary ()
       for KeyValue(k, vf) in f do
           match x.TryGetValue k with
           | true, vx -> dct.Add (k, vf vx)
           | _        -> ()
       dct

    static member        ``<*>`` (f: IDictionary<'Key,_>, x: IDictionary<'Key,'T> , [<Optional>]_output: IDictionary<'Key,'U>  , [<Optional>]_mthd: Apply) : IDictionary<'Key,'U> =
       let dct = Dictionary ()
       for KeyValue(k, vf) in f do
           match x.TryGetValue k with
           | true, vx -> dct.Add (k, vf vx)
           | _        -> ()
       dct :> IDictionary<'Key,'U>

    static member        ``<*>`` (f: IReadOnlyDictionary<'Key,_>, x: IReadOnlyDictionary<'Key,'T> , [<Optional>]_output: IReadOnlyDictionary<'Key,'U>  , [<Optional>]_mthd: Apply) : IReadOnlyDictionary<'Key,'U> =
       let dct = Dictionary ()
       for KeyValue(k, vf) in f do
           match x.TryGetValue k with
           | true, vx -> dct.Add (k, vf vx)
           | _        -> ()
       dct :> IReadOnlyDictionary<'Key,'U>

    #if !FABLE_COMPILER
    static member        ``<*>`` (f: Expr<'T->'U>, x: Expr<'T>, [<Optional>]_output: Expr<'U>, [<Optional>]_mthd: Apply) = Expr.Cast<'U> (Expr.Application (f, x))
    #endif
    static member        ``<*>`` (f: ('T->'U) ResizeArray, x: 'T ResizeArray, [<Optional>]_output: 'U ResizeArray, [<Optional>]_mthd: Apply) = ResizeArray.apply f x : 'U ResizeArray

    static member inline Invoke (f: '``Applicative<'T -> 'U>``) (x: '``Applicative<'T>``) : '``Applicative<'U>`` =
        let inline call (mthd : ^M, input1: ^I1, input2: ^I2, output: ^R) =
            ((^M or ^I1 or ^I2 or ^R) : (static member ``<*>`` : _*_*_*_ -> _) input1, input2, output, mthd)
        call(Unchecked.defaultof<Apply>, f, x, Unchecked.defaultof<'``Applicative<'U>``>)


#endif

    static member inline InvokeOnInstance (f: '``Applicative<'T->'U>``) (x: '``Applicative<'T>``) : '``Applicative<'U>`` =
        ((^``Applicative<'T->'U>`` or ^``Applicative<'T>`` or ^``Applicative<'U>``) : (static member (<*>) : _*_ -> _) (f, x))

#if !FABLE_COMPILER || (FABLE_COMPILER_3 || FABLE_COMPILER_4)

type Lift2 =
    inherit Default1

    static member        Lift2 (f, (x: Lazy<_>            , y: Lazy<_>            ), _mthd: Lift2) = Lazy.map2 f x y
    static member        Lift2 (f, (x: seq<_>             , y: seq<_>             ), _mthd: Lift2) = Seq.lift2 f x y
    static member        Lift2 (f, (x: NonEmptySeq<_>     , y: NonEmptySeq<_>     ), _mthd: Lift2) = NonEmptySeq.lift2 f x y
    static member        Lift2 (f, (x: IEnumerator<_>     , y: IEnumerator<_>     ), _mthd: Lift2) = Enumerator.map2 f x y
    static member        Lift2 (f, (x                     , y                     ), _mthd: Lift2) = List.lift2 f x y
    static member        Lift2 (f, (x                     , y                     ), _mthd: Lift2) = Array.lift2 f x y
    static member        Lift2 (f, (x: 'R -> 'T           , y: 'R -> 'U           ), _mthd: Lift2) = fun a -> f (x a) (y a)
    static member inline Lift2 (f, ((a: 'Monoid, x: 'T)   , (b: 'Monoid, y: 'U)   ), _mthd: Lift2) = Plus.Invoke a b, f x y
    static member inline Lift2 (f, (struct (a: 'Monoid, x: 'T), struct (b: 'Monoid, y: 'U)), _mthd: Lift2) = struct (Plus.Invoke a b, f x y)
    #if !FABLE_COMPILER
    static member        Lift2 (f, (x: Task<'T>           , y: Task<'U>           ), _mthd: Lift2) = Task.map2  f x y
    #endif
    #if NETSTANDARD2_1 && !FABLE_COMPILER
    static member        Lift2 (f, (x: ValueTask<'T>      , y: ValueTask<'U>      ), _mthd: Lift2) = ValueTask.map2  f x y
    #endif
    static member        Lift2 (f, (x                     , y                     ), _mthd: Lift2) = Async.map2  f x y
    static member        Lift2 (f, (x                     , y                     ), _mthd: Lift2) = Option.map2 f x y
    
    #if !FABLE_COMPILER
    static member        Lift2 (f, (x                     , y                     ), _mthd: Lift2) = ValueOption.map2 f x y
    #endif
    static member        Lift2 (f, (x: Result<'T,'Error>  , y: Result<'U,'Error>  ), _mthd: Lift2) = Result.map2 f x y
    static member        Lift2 (f, (x: Choice<'T,'Error>  , y: Choice<'U,'Error>  ), _mthd: Lift2) = Choice.map2 f x y
    static member        Lift2 (f, (x: Map<'Key,'T>       , y : Map<'Key,'U>      ), _mthd: Lift2) = Map.mapValues2 f x y
    static member        Lift2 (f, (x: Dictionary<'Key,'T>, y: Dictionary<'Key,'U>), _mthd: Lift2) = Dictionary.map2 f x y
    #if !FABLE_COMPILER
    static member        Lift2 (f, (x: Expr<'T>           , y: Expr<'U>           ), _mthd: Lift2) = <@ f %x %y @>
    #endif
    static member        Lift2 (f, (x: ResizeArray<'T>    , y: ResizeArray<'U>    ), _mthd: Lift2) = ResizeArray.lift2 f x y

    static member inline Invoke (f: 'T -> 'U -> 'V) (x: '``Applicative<'T>``) (y: '``Applicative<'U>``) : '``Applicative<'V>`` =
        let inline call (mthd : ^M, input1: ^I1, input2: ^I2, _output: ^R) =
            ((^M or ^I1 or ^I2 or ^R) : (static member Lift2 : _*(_*_)*_ -> _) f, (input1, input2), mthd)
        call (Unchecked.defaultof<Lift2>, x, y, Unchecked.defaultof<'``Applicative<'V>``>)

    static member inline InvokeOnInstance (f: 'T -> 'U -> 'V) (x: '``Applicative<'T>``) (y: '``Applicative<'U>``) =
        ((^``Applicative<'T>`` or ^``Applicative<'U>``) : (static member Lift2 : _*_*_ -> _) f, x, y)

type Lift2 with
    static member inline Lift2 (f, (x, y), _mthd: Default2) = (((Return.InvokeOnInstance f, x) ||> Apply.InvokeOnInstance), y) ||> Apply.InvokeOnInstance

    static member inline Lift2 (_, (_:'t when 't: null and 't: struct, _: ^u when ^u : null and ^u: struct), _mthd: Default1) = id
    static member inline Lift2 (f: 'T -> 'U -> 'V, (x: '``Applicative<'T>``, y: '``Applicative<'U>``)      , _mthd: Default1) = ((^``Applicative<'T>`` or ^``Applicative<'U>`` ) : (static member Lift2 : _*_*_ -> _) f, x, y)

type Lift3 =
    inherit Default1

    static member        Lift3 (f, (x: Lazy<_>            , y: Lazy<_>            , z: Lazy<_>             ), _mthd: Lift3) = Lazy.map3 f x y z
    static member        Lift3 (f, (x: seq<_>             , y: seq<_>             , z: seq<_>              ), _mthd: Lift3) = Seq.lift3 f x y z
    static member        Lift3 (f, (x: NonEmptySeq<_>     , y: NonEmptySeq<_>     , z: NonEmptySeq<_>      ), _mthd: Lift3) = NonEmptySeq.lift3 f x y z
    static member        Lift3 (f, (x: IEnumerator<_>     , y: IEnumerator<_>     , z: IEnumerator<_>      ), _mthd: Lift3) = Enumerator.map3 f x y z
    static member        Lift3 (f, (x                     , y                     , z                      ), _mthd: Lift3) = List.lift3 f x y z
    static member        Lift3 (f, (x                     , y                     , z                      ), _mthd: Lift3) = Array.lift3 f x y z
    static member        Lift3 (f, (x: 'R -> 'T           , y: 'R -> 'U           , z: 'R -> 'V            ), _mthd: Lift3) = fun a -> f (x a) (y a) (z a)
    static member inline Lift3 (f, ((a: 'Monoid, x: 'T)   , (b: 'Monoid, y: 'U)   , (c: 'Monoid, z: 'U)    ), _mthd: Lift3) = Plus.Invoke (Plus.Invoke a b) c, f x y z
    static member inline Lift3 (f, (struct (a: 'Monoid, x: 'T), struct (b: 'Monoid, y: 'U), struct (c: 'Monoid, z: 'U)), _mthd: Lift3) = struct (Plus.Invoke (Plus.Invoke a b) c, f x y z)
    #if !FABLE_COMPILER
    static member        Lift3 (f, (x: Task<'T>           , y: Task<'U>           , z: Task<'V>            ), _mthd: Lift3) = Task.map3  f x y z
    #endif
    #if NETSTANDARD2_1 && !FABLE_COMPILER
    static member        Lift3 (f, (x: ValueTask<'T>      , y: ValueTask<'U>      , z: ValueTask<'V>       ), _mthd: Lift3) = ValueTask.map3  f x y z
    #endif
    static member        Lift3 (f, (x                     , y                     , z                      ), _mthd: Lift3) = Async.map3  f x y z
    static member        Lift3 (f, (x                     , y                     , z                      ), _mthd: Lift3) = Option.map3 f x y z
    
    #if !FABLE_COMPILER
    static member        Lift3 (f, (x                     , y                     , z                      ), _mthd: Lift3) = ValueOption.map3 f x y z
    #endif
    static member        Lift3 (f, (x: Result<'T,'Error>  , y: Result<'U,'Error>  , z: Result<'V, 'Error>  ), _mthd: Lift3) = Result.map3 f x y z
    static member        Lift3 (f, (x: Choice<'T,'Error>  , y: Choice<'U,'Error>  , z: Choice<'V, 'Error>  ), _mthd: Lift3) = Choice.map3 f x y z
    static member        Lift3 (f, (x: Map<'Key,'T>       , y: Map<'Key,'U>       , z: Map<'Key, 'V>       ), _mthd: Lift3) = Map.mapValues3 f x y z
    static member        Lift3 (f, (x: Dictionary<'Key,'T>, y: Dictionary<'Key,'U>, z: Dictionary<'Key, 'V>), _mthd: Lift3) = Dictionary.map3 f x y z
    #if !FABLE_COMPILER
    static member        Lift3 (f, (x: Expr<'T>           , y: Expr<'U>           , z: Expr<'V>            ), _mthd: Lift3) = <@ f %x %y %z @>
    #endif
    static member        Lift3 (f, (x: ResizeArray<'T>    , y: ResizeArray<'U>    , z: ResizeArray<'V>     ), _mthd: Lift3) = ResizeArray.lift3 f x y z

    static member inline Invoke (f: 'T -> 'U -> 'V -> 'W) (x: '``Applicative<'T>``) (y: '``Applicative<'U>``) (z: '``Applicative<'V>``): '``Applicative<'W>`` =
        let inline call (mthd : ^M, input1: ^I1, input2: ^I2, input3: ^I3, _output: ^R) =
            ((^M or ^I1 or ^I2 or ^I3 or ^R) : (static member Lift3 : _*(_*_*_)*_ -> _) f, (input1, input2, input3), mthd)
        call (Unchecked.defaultof<Lift3>, x, y, z, Unchecked.defaultof<'``Applicative<'W>``>)

    static member inline InvokeOnInstance (f: 'T -> 'U -> 'V -> 'W) (x: '``Applicative<'T>``) (y: '``Applicative<'U>``) (z: '``Applicative<'V>``)=
        ((^``Applicative<'T>`` or ^``Applicative<'U>`` or ^``Applicative<'V>``) : (static member Lift3 : _*_*_*_ -> _) f, x, y, z)

type Lift3 with
    static member inline Lift3 (f, (x, y, z), _mthd: Default3) = ((((Return.InvokeOnInstance f, x) ||> Apply.InvokeOnInstance), y) ||> Apply.InvokeOnInstance, z) ||> Apply.InvokeOnInstance
    static member inline Lift3 (_, (_:'t when 't: null and 't: struct, _: ^u when ^u : null and ^u: struct, _: ^v when ^v : null and ^v: struct), _mthd: Default1) = id
    static member inline Lift3 (f: 'T -> 'U -> 'V -> 'W, (x: '``Applicative<'T>``, y: '``Applicative<'U>``, z: '``Applicative<'V>``)            , _mthd: Default1) = ((^``Applicative<'T>`` or ^``Applicative<'U>`` or ^``Applicative<'V>`` ) : (static member Lift3 : _*_*_*_ -> _) f, x, y, z)

type IsLeftZero =
    inherit Default1

    static member IsLeftZero (t: ref<seq<_>>      , _mthd: IsLeftZero) = Seq.isEmpty t.Value
    static member IsLeftZero (_: ref<NonEmptySeq<_>>, _mthd: IsLeftZero) = false
    static member IsLeftZero (t: ref<list<_>>     , _mthd: IsLeftZero) = List.isEmpty t.Value
    static member IsLeftZero (t: ref<array<_>>    , _mthd: IsLeftZero) = Array.isEmpty t.Value
    static member IsLeftZero (t: ref<option<_>>   , _mthd: IsLeftZero) = Option.isNone t.Value
    #if !FABLE_COMPILER
    static member IsLeftZero (t: ref<voption<_>>  , _mthd: IsLeftZero) = ValueOption.isNone t.Value
    #endif
    static member IsLeftZero (t: ref<Result<_,_>> , _mthd: IsLeftZero) = match t.Value with Error _      -> true | _ -> false
    static member IsLeftZero (t: ref<Choice<_,_>> , _mthd: IsLeftZero) = match t.Value with Choice2Of2 _ -> true | _ -> false

    static member inline Invoke (x: '``Applicative<'T>``) : bool =
        let inline call (mthd : ^M, input: ^I) =
            ((^M or ^I) : (static member IsLeftZero : _*_ -> _) ref input, mthd)
        call(Unchecked.defaultof<IsLeftZero>, x)

    static member inline InvokeOnInstance (x: '``Applicative<'T>``) : bool =
        ((^``Applicative<'T>``) : (static member IsLeftZero : _ -> _) x)

type IsLeftZero with

    static member inline IsLeftZero (_: ref<'T>   when 'T : struct    , _mthd: Default4) = false
    static member inline IsLeftZero (_: ref<'T>   when 'T : not struct, _mthd: Default3) = false

    // empty <*> f = empty  ==> empty is left zero for <*>
    static member inline IsLeftZero (t: ref<'``Alternative<'T>``>        , _mthd: Default2) = (t.Value = Empty.InvokeOnInstance ())

    static member inline IsLeftZero (t: ref<'``Applicative<'T>``>        , _mthd: Default1) = (^``Applicative<'T>`` : (static member IsLeftZero : _ -> _) t.Value)
    static member inline IsLeftZero (_: ref< ^t> when ^t: null and ^t: struct, _: Default1) = ()

#endif