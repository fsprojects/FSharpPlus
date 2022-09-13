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
 
#if !FABLE_COMPILER || FABLE_COMPILER_3

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
    #if !FABLE_COMPILER
    static member        ``<*>`` (f: Task<_>          , x: Task<'T>             , [<Optional>]_output: Task<'U>             , [<Optional>]_mthd: Apply) = Task.apply   f x : Task<'U>
    #endif
    static member        ``<*>`` (f: Async<_>         , x: Async<'T>            , [<Optional>]_output: Async<'U>            , [<Optional>]_mthd: Apply) = Async.apply  f x : Async<'U>
    static member        ``<*>`` (f: option<_>        , x: option<'T>           , [<Optional>]_output: option<'U>           , [<Optional>]_mthd: Apply) = Option.apply f x : option<'U>
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

#if !FABLE_COMPILER || FABLE_COMPILER_3

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
    #if !FABLE_COMPILER
    static member        Lift2 (f, (x: Task<'T>           , y: Task<'U>           ), _mthd: Lift2) = Task.map2  f x y
    #endif
    static member        Lift2 (f, (x                     , y                     ), _mthd: Lift2) = Async.map2  f x y
    static member        Lift2 (f, (x                     , y                     ), _mthd: Lift2) = Option.map2 f x y
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
    #if !FABLE_COMPILER
    static member        Lift3 (f, (x: Task<'T>           , y: Task<'U>           , z: Task<'V>            ), _mthd: Lift3) = Task.map3  f x y z
    #endif
    static member        Lift3 (f, (x                     , y                     , z                      ), _mthd: Lift3) = Async.map3  f x y z
    static member        Lift3 (f, (x                     , y                     , z                      ), _mthd: Lift3) = Option.map3 f x y z
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

type IsLeftZeroHelper =
    static member Seq (xs: seq<'t>) = Seq.isEmpty xs
    static member NonEmptySeq (_: NonEmptySeq<'t>) = false
    static member List (xs: list<'t>) = List.isEmpty xs
    static member Array (xs: array<'t>) = Array.isEmpty xs
    static member Option (x: option<'t>) = Option.isNone x
    static member Result (x: Result<'t, 'u>) = match x with Error _ -> true | _ -> false
    static member Choice (x: Choice<'t, 'u>) = match x with Choice2Of2 _ -> true | _ -> false

#if !FABLE_COMPILER
type IsLeftZeroHelper<'a>() =
    static let isLeftZero =
        let ty = typeof<'a>
        let check typedef = ty.IsGenericType && (ty.GetGenericTypeDefinition() = typedef)
        let helperTy = typeof<IsLeftZeroHelper>
        let helper helperName tprms : 'a -> bool =
            let meth = helperTy.GetMethod(helperName).MakeGenericMethod(tprms)
            fun x -> meth.Invoke(null, [|box x|]) |> unbox
        if check typedefof<seq<_>> then helper "Seq" (ty.GetGenericArguments())
        else if check typedefof<NonEmptySeq<_>> then helper "NonEmptySeq" (ty.GetGenericArguments())
        else if check typedefof<list<_>> then helper "List" (ty.GetGenericArguments())
        else if ty.IsArray then helper "Array" [| ty.GetElementType() |]
        else if check typedefof<option<_>> then helper "Option" (ty.GetGenericArguments())
        else if check typedefof<Result<_, _>> then helper "Result" (ty.GetGenericArguments())
        else if check typedefof<Choice<_, _>> then helper "Choice" (ty.GetGenericArguments())
        else
            let isLeftZero = ty.GetMethod("IsLeftZero")
            if not (isNull isLeftZero) then
                let isLeftZero =
                    let targs = ty.GetGenericArguments()
                    if Array.isEmpty targs || not isLeftZero.ContainsGenericParameters then isLeftZero
                    else isLeftZero.MakeGenericMethod(targs)
                (fun x -> isLeftZero.Invoke(null, [| box x |]) |> unbox)
            else
                let compareWith (obj: obj) = fun (x: 'a) -> obj.Equals(x)
                let emptyProp = ty.GetProperty("Empty")
                if not (isNull emptyProp) then emptyProp.GetValue(null) |> compareWith
                else
                    let emptyMeth = ty.GetMethod("get_Empty")
                    if not (isNull emptyMeth) then emptyMeth.Invoke(null, null) |> compareWith
                    else fun _ -> false
    static member Invoke(x: 'a) = isLeftZero x
#endif

type IsLeftZero =
    inherit Default1

    static member IsLeftZero (t: ref<seq<_>>      , _mthd: IsLeftZero) = IsLeftZeroHelper.Seq t.Value
    static member IsLeftZero (t: ref<NonEmptySeq<_>>, _mthd: IsLeftZero) = IsLeftZeroHelper.NonEmptySeq t.Value
    static member IsLeftZero (t: ref<list<_>>     , _mthd: IsLeftZero) = IsLeftZeroHelper.List t.Value
    static member IsLeftZero (t: ref<array<_>>    , _mthd: IsLeftZero) = IsLeftZeroHelper.Array t.Value
    static member IsLeftZero (t: ref<option<_>>   , _mthd: IsLeftZero) = IsLeftZeroHelper.Option t.Value
    static member IsLeftZero (t: ref<Result<_,_>> , _mthd: IsLeftZero) = IsLeftZeroHelper.Result t.Value
    static member IsLeftZero (t: ref<Choice<_,_>> , _mthd: IsLeftZero) = IsLeftZeroHelper.Choice t.Value

    #if !FABLE_COMPILER
    static member        Invoke (x: '``Applicative<'T>``) : bool =
        IsLeftZeroHelper<'``Applicative<'T>``>.Invoke(x)
    #else
    static member inline Invoke (x: '``Applicative<'T>``) : bool =
        let inline call (mthd : ^M, input: ^I) =
            ((^M or ^I) : (static member IsLeftZero : _*_ -> _) ref input, mthd)
        call(Unchecked.defaultof<IsLeftZero>, x)
    #endif

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