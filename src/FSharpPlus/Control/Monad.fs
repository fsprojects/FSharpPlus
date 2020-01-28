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

[<Sealed>]
type Set2<'T when 'T: comparison >() = class end


// Monad class ------------------------------------------------------------

type Bind =
    static member        (>>=) (source: Lazy<'T>   , f: 'T -> Lazy<'U>    ) = lazy (f source.Value).Value                                   : Lazy<'U>
    static member        (>>=) (source: seq<'T>    , f: 'T -> seq<'U>     ) = Seq.bind f source                                             : seq<'U> 
#if !FABLE_COMPILER
    static member        (>>=) (source: Task<'T>   , f: 'T -> Task<'U>    ) = source.ContinueWith(fun (x: Task<_>) -> f x.Result).Unwrap () : Task<'U>
#endif
    static member        (>>=) (source             , f: 'T -> _           ) = Option.bind   f source                                        : option<'U>
    static member        (>>=) (source             , f: 'T -> _           ) = List.collect  f source                                        : list<'U>  
    static member        (>>=) (source             , f: 'T -> _           ) = Array.collect f source                                        : 'U []     
    static member        (>>=) (source             , k: 'T -> _           ) = (fun r -> k (source r) r)                                     : 'R->'U    
    static member inline (>>=) ((w: 'Monoid, a: 'T), k: 'T -> 'Monoid * 'U) = let m, b = k a in (Plus.Invoke w m, b)                        : 'Monoid*'U
    static member        (>>=) (source             , f: 'T -> _           ) = async.Bind (source, f)                                        : Async<'U>
    static member        (>>=) (source             , k: 'T -> _           ) = Result.bind k source                                          : Result<'U,'E>
    static member        (>>=) (source             , k: 'T -> _           ) = Choice.bind k source                                          : Choice<'U,'E>

    static member (>>=) (source: Map<'Key,'T>, f: 'T -> Map<'Key,'U>) = Map (seq {
                   for KeyValue(k, v) in source do
                       match Map.tryFind k (f v) with
                       | Some v -> yield k, v
                       | _      -> () })

    static member (>>=) (source: Dictionary<'Key,'T>, f: 'T -> Dictionary<'Key,'U>) = 
                   let dct = Dictionary ()
                   for KeyValue(k, v) in source do
                       match (f v).TryGetValue (k) with
                       | true, v -> dct.Add (k, v)
                       | _       -> ()
                   dct

    static member (>>=) (source: ResizeArray<'T>, f: 'T -> ResizeArray<'U>) = ResizeArray (Seq.bind (f >> seq<_>) source) : ResizeArray<'U>

    static member inline Invoke (source: '``Monad<'T>``) (binder: 'T -> '``Monad<'U>``) : '``Monad<'U>`` =
        let inline call (_mthd: 'M, input: 'I, _output: 'R, f) = ((^M or ^I or ^R) : (static member (>>=) : _*_ -> _) input, f)
        call (Unchecked.defaultof<Bind>, source, Unchecked.defaultof<'``Monad<'U>``>, binder)

    static member inline InvokeOnInstance (source: '``Monad<'T>``) (binder: 'T -> '``Monad<'U>``) : '``Monad<'U>`` =
        ((^``Monad<'T>`` or ^``Monad<'U>``) : (static member (>>=) : _*_ -> _) source, binder)


type Join =
    inherit Default1
    static member inline Join (x: '``Monad<'Monad<'T>>``, [<Optional>]_output: '``Monad<'T>``  , [<Optional>]_mthd: Default2) = Bind.InvokeOnInstance x id : '``Monad<'T>``
    static member inline Join (x: '``Monad<'Monad<'T>>``, [<Optional>]_output: '``Monad<'T>``  , [<Optional>]_mthd: Default1) = ((^``Monad<'Monad<'T>>`` or  ^``Monad<'T>``) : (static member Join : _ -> _) x) : '``Monad<'T>``
    static member        Join (x: Lazy<Lazy<_>>         , [<Optional>]_output: Lazy<'T>        , [<Optional>]_mthd: Join    ) = lazy x.Value.Value         : Lazy<'T>
    static member        Join (x: seq<seq<_>>           , [<Optional>]_output: seq<'T>         , [<Optional>]_mthd: Join    ) = Seq.concat x               : seq<'T>
    static member        Join (x: Id<_>                 , [<Optional>]_output: Id<'T>          , [<Optional>]_mthd: Join    ) = x.getValue                 : Id<'T>
#if !FABLE_COMPILER                                                                                                                             
    static member        Join (x: Task<Task<_>>         , [<Optional>]_output: Task<'T>        , [<Optional>]_mthd: Join    ) = Task.join x                : Task<'T>
#endif
    static member        Join (x                        , [<Optional>]_output: option<'T>      , [<Optional>]_mthd: Join    ) = Option.flatten x           : option<'T>
    static member        Join (x: list<list<_>>         , [<Optional>]_output: list<'T>        , [<Optional>]_mthd: Join    ) = List.concat x              : list<'T>
    static member        Join (x: _ [][]                , [<Optional>]_output: 'T []           , [<Optional>]_mthd: Join    ) = Array.concat x             : 'T []
    static member        Join (g                        , [<Optional>]_output: 'R->'T          , [<Optional>]_mthd: Join    ) = (fun r -> (g r) r)         : 'R->'T
    static member inline Join (m1, (m2, x)              , [<Optional>]_output: 'Monoid * 'T    , [<Optional>]_mthd: Join    ) = Plus.Invoke m1 m2, x       : 'Monoid*'T
    static member        Join (x                        , [<Optional>]_output: Async<'T>       , [<Optional>]_mthd: Join    ) = async.Bind (x, id)         : Async<'T>
    static member        Join (x                        , [<Optional>]_output: Result<'T,'E>   , [<Optional>]_mthd: Join    ) = Result.flatten x           : Result<'T,'E>
    static member        Join (x                        , [<Optional>]_output: Choice<'T,'E>   , [<Optional>]_mthd: Join    ) = Choice.flatten x           : Choice<'T,'E>

    static member        Join (x: Map<_,_>              , [<Optional>]_output: Map<'Key,'Value>, [<Optional>]_mthd: Join    )                              : Map<'Key,'Value> =
                    Map (seq {
                        for KeyValue(k, v) in x do
                            match Map.tryFind k v with
                            | Some v -> yield k, v
                            | _      -> () })

    static member        Join (x: Dictionary<_,Dictionary<_,_>>, [<Optional>]_output: Dictionary<'Key,'Value>, [<Optional>]_mthd: Join)                   : Dictionary<'Key,'Value> =
                    let dct = Dictionary ()
                    for KeyValue(k, v) in x do
                        match v.TryGetValue (k)  with
                        | true, v -> dct.Add (k, v)
                        | _       -> ()
                    dct

    static member        Join (x: ResizeArray<ResizeArray<'T>> , [<Optional>]_output: ResizeArray<'T>        , [<Optional>]_mthd: Join) = ResizeArray (Seq.bind seq<_> x) : ResizeArray<'T> 

    static member inline Invoke (source: '``Monad<Monad<'T>>``) : '``Monad<'T>`` =
        let inline call (mthd: 'M, input: 'I, output: 'R) = ((^M or ^I or ^R) : (static member Join : _*_*_ -> _) input, output, mthd)
        call (Unchecked.defaultof<Join>, source, Unchecked.defaultof<'``Monad<'T>``>)


type Return =
    inherit Default1

    #if !FABLE_COMPILER
    static member inline Invoke (x: 'T) : '``Applicative<'T>`` =
        let inline call (mthd: ^M, output: ^R) = ((^M or ^R) : (static member Return : _*_ -> _) output, mthd)
        call (Unchecked.defaultof<Return>, Unchecked.defaultof<'``Applicative<'T>``>) x
    #endif
    
    static member inline InvokeOnInstance (x: 'T) = (^``Applicative<'T>`` : (static member Return : ^T -> ^``Applicative<'T>``) x)

    static member        Return (_: seq<'a>        , _: Default2) = fun  x      -> Seq.singleton x : seq<'a>
    #if !FABLE_COMPILER
    static member        Return (_: IEnumerator<'a>, _: Default2) = fun  x      -> Enumerator.upto None (fun _ -> x) : IEnumerator<'a>
    #endif
    static member inline Return (_: 'R             , _: Default1) = fun (x: 'T) -> Return.InvokeOnInstance x         : 'R

    static member        Return (_: Lazy<'a>       , _: Return  ) = fun x -> Lazy<_>.CreateFromValue x : Lazy<'a>
#if !FABLE_COMPILER       
    static member        Return (_: 'a Task        , _: Return  ) = fun x -> 
        let s = TaskCompletionSource ()
        s.SetResult x
        s.Task : 'a Task
#endif
    static member        Return (_: option<'a>     , _: Return  ) = fun x -> Some x                               : option<'a>
    static member        Return (_: list<'a>       , _: Return  ) = fun x -> [ x ]                                : list<'a>
    static member        Return (_: 'a []          , _: Return  ) = fun x -> [|x|]                                : 'a []
    static member        Return (_: 'r -> 'a       , _: Return  ) = const': 'a -> 'r -> _
    static member inline Return (_:  'm * 'a       , _: Return  ) = fun (x: 'a) -> (Zero.Invoke (): 'm), x
    static member        Return (_: 'a Async       , _: Return  ) = fun (x: 'a) -> async.Return x
    static member        Return (_: Result<'a,'e>  , _: Return  ) = fun x -> Ok x                                 : Result<'a,'e>
    static member        Return (_: Choice<'a,'e>  , _: Return  ) = fun x -> Choice1Of2 x                         : Choice<'a,'e>
    #if !FABLE_COMPILER
    static member        Return (_: Expr<'a>       , _: Return  ) = fun x -> Expr.Cast<'a> (Expr.Value (x: 'a))
    #endif
    static member        Return (_: ResizeArray<'a>, _: Return  ) = fun x -> ResizeArray<'a> (Seq.singleton x)

    //Restricted
    static member        Return (_: string         , _: Return  ) = fun (x: char) -> string x : string
    static member        Return (_: StringBuilder  , _: Return  ) = fun (x: char) -> new StringBuilder (string x) : StringBuilder
    static member        Return (_: 'a Set         , _: Return  ) = fun (x: 'a  ) -> Set.singleton x
    static member        Return (_: 'a Set2        , _: Return  ) = fun (_: 'a  ) -> Set2() : 'a Set2

type Apply =
    inherit Default1
    
    static member inline ``<*>`` (f: '``Monad<'T->'U>``      , x: '``Monad<'T>``      , [<Optional>]_output: '``Monad<'U>``      , [<Optional>]_mthd:Default2) : '``Monad<'U>``       = Bind.InvokeOnInstance f (fun (x1: 'T->'U) -> Bind.InvokeOnInstance x (fun x2 -> Return.InvokeOnInstance (x1 x2)))
    static member inline ``<*>`` (f: '``Applicative<'T->'U>``, x: '``Applicative<'T>``, [<Optional>]_output: '``Applicative<'U>``, [<Optional>]_mthd:Default1) : '``Applicative<'U>`` = ((^``Applicative<'T->'U>`` or ^``Applicative<'T>`` or ^``Applicative<'U>``) : (static member (<*>) : _*_ -> _) f, x)

    static member        ``<*>`` (f: Lazy<'T->'U>     , x: Lazy<'T>             , [<Optional>]_output: Lazy<'U>             , [<Optional>]_mthd: Apply) = Lazy<_>.Create (fun () -> f.Value x.Value)   : Lazy<'U>
    static member        ``<*>`` (f: seq<_>           , x: seq<'T>              , [<Optional>]_output: seq<'U>              , [<Optional>]_mthd: Apply) = Seq.apply  f x                               : seq<'U>
    #if !FABLE_COMPILER
    static member        ``<*>`` (f: IEnumerator<_>   , x: IEnumerator<'T>      , [<Optional>]_output: IEnumerator<'U>      , [<Optional>]_mthd: Apply) = Enumerator.map2 id f x : IEnumerator<'U>
    #endif
    static member        ``<*>`` (f: list<_>          , x: list<'T>             , [<Optional>]_output: list<'U>             , [<Optional>]_mthd: Apply) = List.apply f x                               : list<'U>
    static member        ``<*>`` (f: _ []             , x: 'T []                , [<Optional>]_output: 'U []                , [<Optional>]_mthd: Apply) = Array.collect (fun x1 -> Array.collect (fun x2 -> [|x1 x2|]) x) f : 'U []
    static member        ``<*>`` (f: 'r -> _          , g: _ -> 'T              , [<Optional>]_output:  'r -> 'U            , [<Optional>]_mthd: Apply) = fun x -> f x (g x)                           : 'U
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
    static member        ``<*>`` (f: ('T->'U) ResizeArray, x: 'T ResizeArray, [<Optional>]_output: 'U ResizeArray, [<Optional>]_mthd: Apply) =
       ResizeArray (Seq.collect (fun x1 -> Seq.collect (fun x2 -> Seq.singleton (x1 x2)) x) f) : 'U ResizeArray

    static member inline Invoke (f: '``Applicative<'T -> 'U>``) (x: '``Applicative<'T>``) : '``Applicative<'U>`` =
        let inline call (mthd : ^M, input1: ^I1, input2: ^I2, output: ^R) =
            ((^M or ^I1 or ^I2 or ^R) : (static member ``<*>`` : _*_*_*_ -> _) input1, input2, output, mthd)
        call(Unchecked.defaultof<Apply>, f, x, Unchecked.defaultof<'``Applicative<'U>``>)

    static member inline InvokeOnInstance (f: '``Applicative<'T->'U>``) (x: '``Applicative<'T>``) : '``Applicative<'U>`` =
        ((^``Applicative<'T->'U>`` or ^``Applicative<'T>`` or ^``Applicative<'U>``) : (static member (<*>) : _*_ -> _) (f, x))


type Delay =
    inherit Default1
    
    static member inline Delay (_mthd: Default3, x: unit-> ^``Monad<'T>`` when ^``Monad<'T>`` :     struct, _: Default2) = x ()
    static member inline Delay (_mthd: Default3, x: unit-> ^``Monad<'T>`` when ^``Monad<'T>`` : not struct, _: Default1) = x ()
    static member inline Delay (_mthd: Default1, x: unit-> ^I                                             , _: Delay   ) = (^I : (static member Delay : _->_) x) : ^I
    static member inline Delay (_mthd: Default1, _: unit-> ^t when  ^t : null and ^t  : struct            , _          ) = ()

    static member        Delay (_mthd: Default2, x: unit-> _                                              , _          ) = Seq.delay x      : seq<'T> 
    static member        Delay (_mthd: Delay   , x: unit-> _                                              , _          ) = async.Delay x    : Async<'T>
    static member        Delay (_mthd: Delay   , x: unit-> Lazy<_>                                        , _          ) = lazy (x().Value) : Lazy<'T>

    static member inline Invoke source : 'R =
        let inline call (mthd: ^M, input: unit -> ^I) = ((^M or ^I) : (static member Delay : _*_*_ -> _) mthd, input, Unchecked.defaultof<Delay>)
        call (Unchecked.defaultof<Delay>, source)


type TryWith =
    inherit Default1

    static member        TryWith (computation: '``Monad<'T>``, catchHandler: exn -> '``Monad<'T>``, _: Default3) = try computation with e -> catchHandler e
    #if !FABLE_COMPILER
    static member inline TryWith (computation: '``Monad<'T>``, catchHandler: exn -> '``Monad<'T>``, _: Default1) = (^``Monad<'T>`` : (static member TryWith : _*_->_) computation, catchHandler) : '``Monad<'T>``
    static member inline TryWith (_: ^t when ^t: null and ^t: struct, _    : exn -> 't            , _: Default1) = ()
    #endif

    static member        TryWith (computation: seq<_>        , catchHandler: exn -> seq<_>        , _: Default2) = seq (try (Seq.toArray computation) with e -> Seq.toArray (catchHandler e))
    static member        TryWith (computation: Async<_>      , catchHandler: exn -> Async<_>      , _: TryWith ) = async.TryWith (computation, catchHandler)
    static member        TryWith (computation: Lazy<_>       , catchHandler: exn -> Lazy<_>       , _: TryWith ) = lazy (try computation.Force () with e -> (catchHandler e).Force ()) : Lazy<_>

    static member inline Invoke (source: '``Monad<'T>``) (f: exn -> '``Monad<'T>``) : '``Monad<'T>`` =
        let inline call (mthd: 'M, input: 'I, _output: 'R, h: exn -> 'I) = ((^M or ^I) : (static member TryWith : _*_*_ -> _) input, h, mthd)
        call (Unchecked.defaultof<TryWith>, source, Unchecked.defaultof<'``Monad<'T>``>, f)


type TryFinally =
    inherit Default1

    static member        TryFinally ((computation: seq<_>  , compensation: unit -> unit), _: Default2  , _) = seq (try (Seq.toArray computation) finally compensation ())
    static member        TryFinally ((computation: Id<_>   , compensation: unit -> unit), _: TryFinally, _) = try computation finally compensation()
    static member        TryFinally ((computation: Async<_>, compensation: unit -> unit), _: TryFinally, _) = async.TryFinally (computation, compensation) : Async<_>
    static member        TryFinally ((computation: Lazy<_> , compensation: unit -> unit), _: TryFinally, _) = lazy (try computation.Force () finally compensation ()) : Lazy<_>

    static member inline Invoke (source: '``Monad<'T>``) (f: unit -> unit) : '``Monad<'T>`` =
        let inline call (mthd: 'M, input: 'I, _output: 'I, h: unit -> unit) = ((^M or ^I) : (static member TryFinally : (_*_)*_*_ -> _) (input, h), mthd, Unchecked.defaultof<TryFinally>)
        call (Unchecked.defaultof<TryFinally>, source, Unchecked.defaultof<'``Monad<'T>``>, f)

    static member inline InvokeOnInstance (source: '``Monad<'T>``) (f: unit -> unit) : '``Monad<'T>`` = (^``Monad<'T>`` : (static member TryFinally : _*_->_) source, f) : '``Monad<'T>``

type TryFinally with
    static member        TryFinally ((computation: '``Monad<'T>`` when '``Monad<'T>`` :     struct, compensation: unit -> unit), _: Default3, _: Default2  ) = try computation finally compensation ()
    static member        TryFinally ((computation: '``Monad<'T>`` when '``Monad<'T>`` : not struct, compensation: unit -> unit), _: Default3, _: Default1  ) = try computation finally compensation ()
    static member inline TryFinally ((computation: '``Monad<'T>``                                 , compensation: unit -> unit), _: Default1, _: TryFinally) = TryFinally.InvokeOnInstance computation compensation: '``Monad<'T>``
    static member inline TryFinally (( _         : ^t when ^t:null and ^t:struct                  , _           : unit -> unit), _: Default1, _            ) = ()


type Using =
    inherit Default1
    
    static member        Using (resource: 'T when 'T :> IDisposable, body: 'T -> seq<'U>  , _: Using   ) = seq (try Seq.toArray (body resource) finally if not (isNull (box resource)) then resource.Dispose ()) : seq<'U>
    static member        Using (resource: 'T when 'T :> IDisposable, body: 'T -> Async<'U>, _: Using   ) = async.Using (resource, body)
    static member        Using (resource: 'T when 'T :> IDisposable, body: 'T -> Lazy<'U> , _: Using   ) = lazy (try (body resource).Force () finally if not (isNull (box resource)) then resource.Dispose ()) : Lazy<'U>

    static member inline Invoke (source : 'T when 'T :> IDisposable) (f : 'T -> '``Monad<'U>``) : '``Monad<'U>`` =
        let inline call (mthd: 'M, input: 'T, _output: 'R, h: 'T -> 'I) = ((^M or ^I) : (static member Using : _*_*_ -> _) input, h, mthd)
        call (Unchecked.defaultof<Using>, source, Unchecked.defaultof<'``Monad<'U>``>, f)

    static member inline InvokeOnInstance (resource: 'T when 'T :> IDisposable) (body: 'T -> '``Monad<'U>``) : '``Monad<'U>`` =
        (^``Monad<'U>`` : (static member Using : _*_->_) resource, body) : '``Monad<'U>``

type Using with
    static member inline Using (resource: 'T when 'T :> IDisposable, body: 'T -> '``Monad<'U>`` when '``Monad<'U>``:     struct , _: Default3) = using resource body
    static member inline Using (resource: 'T when 'T :> IDisposable, body: 'T -> '``Monad<'U>`` when '``Monad<'U>``: not struct , _: Default2) = using resource body
    static member inline Using (resource: 'T when 'T :> IDisposable, body: 'T -> '``Monad<'U>``                                 , _: Default1) = TryFinally.InvokeOnInstance (body resource) (fun () -> if not (isNull (box resource)) then resource.Dispose ()) : '``Monad<'U>``
    #if !FABLE_COMPILER
    static member inline Using (resource: 'T when 'T :> IDisposable, body: 'T -> '``Monad<'U>``                                 , _: Using   ) = Using.InvokeOnInstance resource body : '``Monad<'U>``
    static member inline Using (_                                  , _   : 'a -> ^t when ^t : null and ^t: struct               , _: Using   ) = ()
    #endif

