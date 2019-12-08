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

type Apply =
    inherit Default1

    static member        ``<*>`` (f: Lazy<'T->'U>     , x: Lazy<'T>             , _output: ref<Lazy<'U>>             , [<Optional>]_mthd: Apply) = Lazy<_>.Create (fun () -> f.Value x.Value)   : Lazy<'U>
    static member        ``<*>`` (f: seq<_>           , x: seq<'T>              , _output: ref<seq<'U>>              , [<Optional>]_mthd: Apply) = Seq.apply  f x                               : seq<'U>
    #if !FABLE_COMPILER
    static member        ``<*>`` (f: IEnumerator<_>   , x: IEnumerator<'T>      , _output: ref<IEnumerator<'U>>      , [<Optional>]_mthd: Apply) = Enumerator.map2 id f x : IEnumerator<'U>
    #endif
    static member        ``<*>`` (f: list<_>          , x: list<'T>             , _output: ref<list<'U>>             , [<Optional>]_mthd: Apply) = List.apply f x                               : list<'U>
    static member        ``<*>`` (f: _ []             , x: 'T []                , _output: ref<'U []>                , [<Optional>]_mthd: Apply) = Array.collect (fun x1 -> Array.collect (fun x2 -> [|x1 x2|]) x) f : 'U []
    static member        ``<*>`` (f: 'r -> _          , g: _ -> 'T              , _output: ref< 'r -> 'U>            , [<Optional>]_mthd: Apply) = fun x -> f x (g x)                           : 'U
    static member inline ``<*>`` ((a: 'Monoid, f)     , (b: 'Monoid, x: 'T)     , _output: ref<'Monoid * 'U>         , [<Optional>]_mthd: Apply) = (Plus.Invoke a b, f x)                       : 'Monoid *'U
    #if !FABLE_COMPILER
    static member        ``<*>`` (f: Task<_>          , x: Task<'T>             , _output: ref<Task<'U>>             , [<Optional>]_mthd: Apply) = Task.apply   f x : Task<'U>
    #endif
    static member        ``<*>`` (f: Async<_>         , x: Async<'T>            , _output: ref<Async<'U>>            , [<Optional>]_mthd: Apply) = Async.apply  f x : Async<'U>
    static member        ``<*>`` (f: option<_>        , x: option<'T>           , _output: ref<option<'U>>           , [<Optional>]_mthd: Apply) = Option.apply f x : option<'U>
    static member        ``<*>`` (f: Result<_,'E>     , x: Result<'T,'E>        , _output: ref<Result<'b,'E>>        , [<Optional>]_mthd: Apply) = Result.apply f x : Result<'U,'E>
    static member        ``<*>`` (f: Choice<_,'E>     , x: Choice<'T,'E>        , _output: ref<Choice<'b,'E>>        , [<Optional>]_mthd: Apply) = Choice.apply f x : Choice<'U,'E>
    static member inline ``<*>`` (KeyValue(a: 'Key, f), KeyValue(b: 'Key, x: 'T), _output: ref<KeyValuePair<'Key,'U>>, [<Optional>]_mthd: Apply) : KeyValuePair<'Key,'U> = KeyValuePair (Plus.Invoke a b, f x)

    static member        ``<*>`` (f: Map<'Key,_>      , x: Map<'Key,'T>         , _output: ref<Map<'Key,'U>>         , [<Optional>]_mthd: Apply) : Map<'Key,'U> = Map (seq {
       for KeyValue(k, vf) in f do
           match Map.tryFind k x with
           | Some vx -> yield k, vf vx
           | _       -> () })

    static member        ``<*>`` (f: Dictionary<'Key,_>, x: Dictionary<'Key,'T> , _output: ref<Dictionary<'Key,'U>>  , [<Optional>]_mthd: Apply) : Dictionary<'Key,'U> =
       let dct = Dictionary ()
       for KeyValue(k, vf) in f do
           match x.TryGetValue k with
           | true, vx -> dct.Add (k, vf vx)
           | _        -> ()
       dct
    
    #if !FABLE_COMPILER
    static member        ``<*>`` (f: Expr<'T->'U>, x: Expr<'T>, _output: ref<Expr<'U>>, [<Optional>]_mthd: Apply) = Expr.Cast<'U> (Expr.Application (f, x))
    #endif
    static member        ``<*>`` (f: ('T->'U) ResizeArray, x: 'T ResizeArray, _output: ref<'U ResizeArray>, [<Optional>]_mthd: Apply) =
       ResizeArray (Seq.collect (fun x1 -> Seq.collect (fun x2 -> Seq.singleton (x1 x2)) x) f) : 'U ResizeArray

    static member inline Invoke (f: '``Applicative<'T -> 'U>``) (x: '``Applicative<'T>``) : '``Applicative<'U>`` =
        let inline call (mthd : ^M, input1: ^I1, input2: ^I2, output: ^R) = ((^M or ^I1 or ^I2 or ^R) : (static member ``<*>`` : _*_*_*_ -> _) input1, input2, ref output, mthd)
        call(Unchecked.defaultof<Apply>, f, x, Unchecked.defaultof<'``Applicative<'U>``>)

    static member inline InvokeOnInstance (f: '``Applicative<'T->'U>``) (x: '``Applicative<'T>``) : '``Applicative<'U>`` =
        ((^``Applicative<'T->'U>`` or ^``Applicative<'T>`` or ^``Applicative<'U>``) : (static member (<*>) : _*_ -> _) (f, x))

type Apply with
    static member inline ``<*>`` (f: '``Monad<'T->'U>``      , x: '``Monad<'T>``      , _output: ref<'``Monad<'U>``>      , _mthd:Default2) : '``Monad<'U>`` = Bind.InvokeOnInstance f (fun (x1: 'T->'U) -> Bind.InvokeOnInstance x (fun x2 -> Return.InvokeOnInstance (x1 x2)))
    static member inline ``<*>`` (f: '``Applicative<'T->'U>``, x: '``Applicative<'T>``, _output: ref<'``Applicative<'U>``>, _mthd:Default1) : '``Applicative<'U>`` = Apply.InvokeOnInstance f x
    static member inline ``<*>`` (_: ^t when ^t : null and ^t: struct, _: ^u when ^u : null and ^u: struct, _output: ref< ^r> when ^r : null and ^r: struct, _mthd: Default1) = id

// Functor class ----------------------------------------------------------

type Iterate =
    static member Iterate (x: Lazy<'T>   , action) = action x.Value : unit
    static member Iterate (x: seq<'T>    , action) = Seq.iter action x
    static member Iterate (x: option<'T> , action) = match x with Some x -> action x | _ -> ()
    static member Iterate (x: list<'T>   , action) = List.iter action x
    static member Iterate ((_: 'W, a: 'T), action) = action a :unit
    static member Iterate (x: 'T []      , action) = Array.iter   action x
    #if !FABLE_COMPILER
    static member Iterate (x: 'T [,]     , action) = Array2D.iter action x
    static member Iterate (x: 'T [,,]    , action) = Array3D.iter action x
    static member Iterate (x: 'T [,,,]   , action) =
                    for i = 0 to Array4D.length1 x - 1 do
                        for j = 0 to Array4D.length2 x - 1 do
                            for k = 0 to Array4D.length3 x - 1 do
                                for l = 0 to Array4D.length4 x - 1 do
                                    action x.[i,j,k,l]
    #endif
    static member Iterate (x: Async<'T>            , action) = action (Async.RunSynchronously x) : unit
    static member Iterate (x: Result<'T, 'E>       , action) = match x with Ok x         -> action x | _ -> ()
    static member Iterate (x: Choice<'T, 'E>       , action) = match x with Choice1Of2 x -> action x | _ -> ()
    static member Iterate (KeyValue(_: 'Key, x: 'T), action) = action x : unit
    static member Iterate (x: Map<'Key,'T>         , action) = Map.iter (const' action) x 
    static member Iterate (x: Dictionary<'Key, 'T> , action) = Seq.iter action x.Values
    static member Iterate (x: _ ResizeArray        , action) = Seq.iter action x

    // Restricted
    static member Iterate (x:string         , action) = String.iter action x
    static member Iterate (x:StringBuilder  , action) = String.iter action (string x)
    static member Iterate (x:Set<'T>        , action) = Set.iter action x

    static member inline Invoke (action: 'T->unit) (source: '``Functor<'T>``) : unit =
        let inline call (_: ^M, source: ^I) =  ((^M or ^I) : (static member Iterate : _*_ -> _) source, action)
        call (Unchecked.defaultof<Iterate>, source)

type Map =
    inherit Default1

    static member Map ((x: Lazy<_>             , f: 'T->'U), _mthd: Map) = Lazy<_>.Create (fun () -> f x.Value) : Lazy<'U>
    #if !FABLE_COMPILER
    static member Map ((x: Task<'T>            , f: 'T->'U), _mthd: Map) = Task.map f x : Task<'U>
    #endif
    static member Map ((x: option<_>           , f: 'T->'U), _mthd: Map) = Option.map  f x
    static member Map ((x: list<_>             , f: 'T->'U), _mthd: Map) = List.map    f x : list<'U>
    static member Map ((g: 'R->'T              , f: 'T->'U), _mthd: Map) = (>>) g f
    static member Map ((g: Func<'R, 'T>        , f: 'T->'U), _mthd: Map) = Func<'R, 'U> (g.Invoke >> f)
    static member Map (((m: 'Monoid, a)        , f: 'T->'U), _mthd: Map) = (m, f a)
    static member Map ((x: _ []                , f: 'T->'U), _mthd: Map) = Array.map   f x
    #if !FABLE_COMPILER
    static member Map ((x: _ [,]               , f: 'T->'U), _mthd: Map) = Array2D.map f x
    static member Map ((x: _ [,,]              , f: 'T->'U), _mthd: Map) = Array3D.map f x
    static member Map ((x: _ [,,,]             , f: 'T->'U), _mthd: Map) = Array4D.init (x.GetLength 0) (x.GetLength 1) (x.GetLength 2) (x.GetLength 3) (fun a b c d -> f x.[a,b,c,d])
    #endif
    static member Map ((x: Async<_>            , f: 'T->'U), _mthd: Map) = async.Bind (x, async.Return << f)
    static member Map ((x: Result<_,'E>        , f: 'T->'U), _mthd: Map) = Result.map f x
    static member Map ((x: Choice<_,'E>        , f: 'T->'U), _mthd: Map) = Choice.map f x
    static member Map ((KeyValue(k, x)         , f: 'T->'U), _mthd: Map) = KeyValuePair (k, f x)
    static member Map ((x: Map<'Key,'T>        , f: 'T->'U), _mthd: Map) = Map.map (const' f) x : Map<'Key,'U>
    static member Map ((x: Dictionary<_,_>     , f: 'T->'U), _mthd: Map) = let d = Dictionary () in Seq.iter (fun (KeyValue(k, v)) -> d.Add (k, f v)) x; d : Dictionary<'Key,'U>
    #if !FABLE_COMPILER
    static member Map ((x: Expr<'T>            , f: 'T->'U), _mthd: Map) = Expr.Cast<'U> (Expr.Application (Expr.Value (f), x))
    #endif
    static member Map ((x: ResizeArray<'T>     , f: 'T->'U), _mthd: Map) = ResizeArray (Seq.map f x) : ResizeArray<'U>

    // Restricted
    static member Map ((x: string              , f        ), _mthd: Map) = String.map f x
    static member Map ((x: StringBuilder       , f        ), _mthd: Map) = new StringBuilder (String.map f (string x))
    static member Map ((x: Set<_>              , f        ), _mthd: Map) = Set.map f x


    static member inline Invoke (mapping: 'T->'U) (source: '``Functor<'T>``) : '``Functor<'U>`` = 
        let inline call (mthd: ^M, source: ^I, _output: ^R) = ((^M or ^I or ^R) : (static member Map : (_*_)*_ -> _) (source, mapping), mthd)
        call (Unchecked.defaultof<Map>, source, Unchecked.defaultof<'``Functor<'U>``>)

    static member inline InvokeOnInstance (mapping: 'T->'U) (source: '``Functor<'T>``) : '``Functor<'U>`` = 
        (^``Functor<'T>`` : (static member Map : _ * _ -> _) source, mapping)

type Map with
    static member inline Map ((x: '``Monad<'T>``       when '``Monad<'T>`` : (static member (>>=)  : '``Monad<'T>`` * ('T -> '``Monad<'U>``) -> '``Monad<'U>``)
                                                       and  '``Monad<'U>`` : (static member Return : 'U -> '``Monad<'U>``)
                                                          , f: 'T->'U), [<Optional>]_mthd: Default4) = Bind.InvokeOnInstance x (f >> Return.InvokeOnInstance) : '``Monad<'U>``

    static member inline Map ((x: '``Applicative<'T>`` when '``Applicative<'T>``     : (static member (<*>)  : '``Applicative<'T->'U>`` * '``Applicative<'T>`` -> '``Applicative<'U>``)
                                                       and  '``Applicative<'T->'U>`` : (static member Return : ('T -> 'U) -> '``Applicative<'T->'U>``)
                                                          , f: 'T->'U), [<Optional>]_mthd: Default3) = Apply.InvokeOnInstance (Return.InvokeOnInstance f: '``Applicative<'T->'U>``) x : '``Applicative<'U>``

    static member        Map ((x: seq<_>                  , f: 'T->'U), _mthd: Default2) = Seq.map f x              : seq<'U>
    #if !FABLE_COMPILER
    static member        Map ((x: IEnumerator<_>          , f: 'T->'U), _mthd: Default2) = Enumerator.map f x       : IEnumerator<'U>
    #endif
    static member        Map ((x: IDictionary<_,_>        , f: 'T->'U), _mthd: Default2) = let d = Dictionary () in Seq.iter (fun (KeyValue(k, v)) -> d.Add (k, f v)) x; d :> IDictionary<'Key,'U>
    static member        Map ((x: IReadOnlyDictionary<_,_>, f: 'T->'U), _mthd: Default2) = IReadOnlyDictionary.map f x : IReadOnlyDictionary<'Key,_>
    static member        Map ((x: IObservable<'T>         , f: 'T->'U), _mthd: Default2) = Observable.map f x       : IObservable<'U>
    #if !FABLE_COMPILER
    static member inline Map ((x: '``Functor<'T>``        , f: 'T->'U), _mthd: Default1) = Map.InvokeOnInstance f x : '``Functor<'U>``
    static member inline Map ((_: ^t when ^t: null and ^t: struct, _ ), _mthd: Default1) = ()
    #endif    


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



type Unzip =
    inherit Default1
    static member inline Unzip (source: '``Functor<'T * 'U>``             , [<Optional>]_output: '``Functor<'T>`` * '``Functor<'U>``                    , [<Optional>]_mthd: Default2) = Map.Invoke fst source, Map.Invoke snd source : '``Functor<'T>`` * '``Functor<'U>``
    static member inline Unzip (source: '``Functor<'T * 'U>``             , [<Optional>]_output: '``Functor<'T>`` * '``Functor<'U>``                    , [<Optional>]_mthd: Default1) = (^``Functor<'T * 'U>``: (static member Unzip : _->_) source) : '``Functor<'T>`` * '``Functor<'U>``
    static member inline Unzip ( _    : ^t when ^t: null and ^t: struct   , _                                                                           , _                          ) = ()
    static member        Unzip (source: list<'T * 'U>                     , [<Optional>]_output: list<'T> * list<'U>                                    , [<Optional>]_mthd: Unzip   ) = List.unzip   source
    static member        Unzip (source: ('T * 'U) []                      , [<Optional>]_output: 'T [] * 'U []                                          , [<Optional>]_mthd: Unzip   ) = Array.unzip  source
    static member        Unzip (source: option<'T * 'U>                   , [<Optional>]_output: option<'T> * option<'U>                                , [<Optional>]_mthd: Unzip   ) = Option.unzip source
    static member        Unzip (source: Map<'Key, 'T * 'U>                , [<Optional>]_output: Map<_, 'T> * Map<_, 'U>                                , [<Optional>]_mthd: Unzip   ) = Map.unzip    source
    static member        Unzip (source: IDictionary<'Key, 'T * 'U>        , [<Optional>]_output: IDictionary<_, 'T> * IDictionary<_, 'U>                , [<Optional>]_mthd: Unzip   ) = Dict.unzip   source
    static member        Unzip (source: IReadOnlyDictionary<'Key, 'T * 'U>, [<Optional>]_output: IReadOnlyDictionary<_, 'T> * IReadOnlyDictionary<_, 'U>, [<Optional>]_mthd: Unzip   ) = IReadOnlyDictionary.unzip source

    static member inline Invoke (source: '``Functor<'T1 * 'T2>``) =
        let inline call_3 (a: ^a, b: ^b, d: ^d) = ((^a or ^b or ^d) : (static member Unzip : _*_*_ -> _) b, d, a)
        let inline call (a: 'a, b: 'b) = call_3 (a, b, Unchecked.defaultof<'r>) : 'r
        call (Unchecked.defaultof<Unzip>, source) : '``Functor<'T1>`` * '``Functor<'T2>``


type Zip =
    inherit Default1
    #if !FABLE_COMPILER
    static member Zip ((x: IEnumerator<'T>            , y: IEnumerator<'U>           , _output: IEnumerator<'T*'U>           ), _mthd: Zip) = Enumerator.zip x y
    #endif
    static member Zip ((x: seq<'T>                    , y: seq<'U>                   , _output: seq<'T*'U>                   ), _mthd: Zip) = Seq.zip        x y
    static member Zip ((x: IDictionary<'K, 'T>        , y: IDictionary<'K,'U>        , _output: IDictionary<'K,'T*'U>        ), _mthd: Zip) = Dict.zip       x y
    static member Zip ((x: IReadOnlyDictionary<'K, 'T>, y: IReadOnlyDictionary<'K,'U>, _output: IReadOnlyDictionary<'K,'T*'U>), _mthd: Zip) = IReadOnlyDictionary.zip x y
    static member Zip ((x: Dictionary<'K, 'T>         , y: Dictionary<'K,'U>         , _output: Dictionary<'K,'T*'U>         ), _mthd: Zip) = Dict.zip       x y :?> Dictionary<'K,'T*'U>
    static member Zip ((x: Map<'K, 'T>                , y: Map<'K,'U>                , _output: Map<'K,'T*'U>                ), _mthd: Zip) = Map.zip        x y
    static member Zip ((x: list<'T>                   , y: list<'U>                  , _output: list<'T*'U>                  ), _mthd: Zip) = List.zip       x y
    static member Zip ((x: 'T []                      , y: 'U []                     , _output: ('T*'U) []                   ), _mthd: Zip) = Array.zip      x y
    static member Zip ((x: option<'T>                 , y: option<'U>                , _output: option<'T*'U>                ), _mthd: Zip) = Option.zip     x y
    static member Zip ((x: Async<'T>                  , y: Async<'U>                 , _output: Async<'T*'U>                 ), _mthd: Zip) = Async.zip      x y
    #if !FABLE_COMPILER
    static member Zip ((x: Task<'T>                   , y: Task<'U>                  , _output: Task<'T*'U>                  ), _mthd: Zip) = Task.zip       x y
    #endif

    static member inline Invoke (source1: '``ZipFunctor<'T1>``) (source2: '``ZipFunctor<'T2>``) =
        let inline call_4 (a: ^a, b: ^b, c: ^c, d: ^d) = ((^a or ^b or ^c or ^d) : (static member Zip : (_*_*_)*_ -> _) (b, c, d), a)
        let inline call (a: 'a, b: 'b, c: 'c) = call_4 (a, b, c, Unchecked.defaultof<'r>) : 'r
        call (Unchecked.defaultof<Zip>, source1, source2) : '``ZipFunctor<'T1 * 'T2>``

    static member inline InvokeOnInstance (source1: '``ZipFunctor<'T1>``) (source2: '``ZipFunctor<'T2>``) : '``ZipFunctor<'T1 * 'T2>`` =
        ((^``ZipFunctor<'T1>`` or ^``ZipFunctor<'T2>`` or  ^``ZipFunctor<'T1 * 'T2>``) : (static member Zip : _*_ -> _) source1, source2)

#if !FABLE_COMPILER
type Zip with    
    static member inline Zip ((_: ^t when ^t : null and ^t: struct, _: ^u when ^u : null and ^u: struct, _output: ^r when ^r : null and ^r: struct), _mthd: Default1) = id
    static member inline Zip ((x: '``ZipFunctor<'T1>``            , y: '``ZipFunctor<'T2>``            , _output: '``ZipFunctor<'T1 * 'T2>``      ), _mthd: Default1) = Zip.InvokeOnInstance x y : '``ZipFunctor<'T1 * 'T2>``
#endif

// Comonad class ----------------------------------------------------------

type Extract =
    static member        Extract (x: Async<'T>    ) = Async.RunSynchronously x
    static member        Extract (x: Lazy<'T>     ) = x.Value
    static member        Extract ((_: 'W, a: 'T)  ) = a
    static member inline Extract (f: 'Monoid -> 'T) = f (Zero.Invoke ())
    static member        Extract (f: 'T Id        ) = f

#if !FABLE_COMPILER
    static member        Extract (f: Task<'T>     ) = f.Result
#endif

    static member inline Invoke (x: '``Comonad<'T>``) : 'T =
        let inline call_2 (_mthd: ^M, x: ^I) = ((^M or ^I) : (static member Extract : _ -> _) x)
        call_2 (Unchecked.defaultof<Extract>, x)

type Extend =
    static member        (=>>) (g: Async<'T>    , f: Async<'T> -> 'U) = async.Return (f g)              : Async<'U>
    static member        (=>>) (g: Lazy<'T>     , f: Lazy<'T> -> 'U ) = Lazy<_>.Create  (fun () -> f g) : Lazy<'U>
    static member        (=>>) ((w: 'W, a: 'T)  , f: _ -> 'U        ) = (w, f (w, a))        
    static member inline (=>>) (g: 'Monoid -> 'T, f: _ -> 'U        ) = fun a -> f (fun b -> g (Plus.Invoke a b))
    static member        (=>>) (g: Id<'T>       , f: Id<'T> -> 'U   ) = f g

#if !FABLE_COMPILER
    static member        (=>>) (g: Task<'T>     , f: Task<'T> -> 'U) = g.ContinueWith (f)
#endif

    // Restricted Comonads
    static member        (=>>) (s: list<'T>     , g) = List.map  g (List.tails s) : list<'U>
    static member        (=>>) (s: 'T []        , g) = Array.map g (s |> Array.toList |> List.tails |> List.toArray |> Array.map List.toArray) : 'U []
    static member        (=>>) (s: seq<'T>      , g) = Seq.map   g (s |> Seq.toList   |> List.tails |> List.toSeq   |> Seq.map   List.toSeq)   : 'U seq

    static member inline Invoke (g: '``Comonad<'T>``->'U) (s: '``Comonad<'T>``) : '``Comonad<'U>`` =
        let inline call (_mthd: 'M, source: 'I, _output: 'R) = ((^M or ^I or ^R) : (static member (=>>) : _*_ -> _) source, g)
        call (Unchecked.defaultof<Extend>, s, Unchecked.defaultof<'``Comonad<'U>``>)

type Duplicate =
    inherit Default1
    static member inline Duplicate (x: '``Comonad<'T>``, [<Optional>]_mthd: Default1 ) = Extend.Invoke id x          : '``Comonad<'Comonad<'T>>``
    static member        Duplicate (s: Async<'T>       , [<Optional>]_mthd: Duplicate) = async.Return s              : Async<Async<'T>>
    static member        Duplicate (s: Lazy<'T>        , [<Optional>]_mthd: Duplicate) = Lazy<_>.CreateFromValue s   : Lazy<Lazy<'T>>
    static member        Duplicate (s: Id<'T>          , [<Optional>]_mthd: Duplicate) = Id s                        : Id<Id<'T>>
    static member        Duplicate ((w: 'W, a: 'T)     , [<Optional>]_mthd: Duplicate) = w, (w, a)
    static member inline Duplicate (f: 'Monoid -> 'T   , [<Optional>]_mthd: Duplicate) = fun a b -> f (Plus.Invoke a b)

    // Restricted Comonads
    static member Duplicate (s:  list<'T>       , [<Optional>]_mthd: Duplicate) = List.tails s
    static member Duplicate (s: array<'T>       , [<Optional>]_mthd: Duplicate) = s |> Array.toList |> List.tails |> List.toArray |> Array.map List.toArray  

    static member inline Invoke (x: '``Comonad<'T>``) : '``Comonad<'Comonad<'T>>`` =
        let inline call (mthd: ^M, source: ^I, _output: ^R) = ((^M or ^I or ^R) : (static member Duplicate : _*_ -> _) source, mthd)
        call (Unchecked.defaultof<Duplicate>, x, Unchecked.defaultof<'``Comonad<'Comonad<'T>>``>)


// Bifunctor class --------------------------------------------------------

type Bimap =
    inherit Default1
       
    static member Bimap ((x, y)        , f: 'T->'U, g: 'V->'W, [<Optional>]_mthd: Bimap) = (f x, g y)
    static member Bimap (x: Result<_,_>, f: 'T->'U, g: 'V->'W, [<Optional>]_mthd: Bimap) = Result.either (Ok << f)         (Error << g) x
    static member Bimap (KeyValue(k, x), f: 'T->'U, g: 'V->'W, [<Optional>]_mthd: Bimap) = KeyValuePair (f k, g x)
    static member Bimap (x: Choice<_,_>, f: 'T->'U, g: 'V->'W, [<Optional>]_mthd: Bimap) = Choice.either (Choice1Of2 << f) (Choice2Of2 << g) x

    static member inline Invoke (f: 'T->'U) (g: 'V->'W) (source: '``Bifunctor<'T,'V>``) : '``Bifunctor<'U,'W>`` =
        let inline call (mthd: ^M, source: ^I, _output: ^R) = ((^M or ^I or ^R) : (static member Bimap : _*_*_*_ -> _) source, f, g, mthd)
        call (Unchecked.defaultof<Bimap>, source, Unchecked.defaultof<'``Bifunctor<'U,'W>``>)

    static member inline InvokeOnInstance (f: 'T->'U) (g: 'V->'W) (source: '``Bifunctor<'T,'V>``) : '``Bifunctor<'U,'W>`` =
        (^``Bifunctor<'T,'V>``: (static member Bimap : _*_*_ -> _) source, f, g)


type MapFirst =
    inherit Default1

    static member First ((x, y)        , f: 'T->'U, [<Optional>]_mthd: MapFirst) = (f x, y)
    static member First (x: Result<_,_>, f: 'T->'U, [<Optional>]_mthd: MapFirst) = Result.either (Ok         << f) Error      x
    static member First (x: Choice<_,_>, f: 'T->'U, [<Optional>]_mthd: MapFirst) = Choice.either (Choice1Of2 << f) Choice2Of2 x
    static member First (KeyValue(k, x), f: 'T->'U, [<Optional>]_mthd: MapFirst) = KeyValuePair(f k, x)

    static member inline Invoke (f: 'T->'U) (source: '``Bifunctor<'T,'V>``) : '``Bifunctor<'U,'V>`` =
        let inline call (mthd: ^M, source: ^I, _output: ^R) = ((^M or ^I or ^R) : (static member First : _*_*_ -> _) source, f, mthd)
        call (Unchecked.defaultof<MapFirst>, source, Unchecked.defaultof<'``Bifunctor<'U,'V>``>)

    static member inline InvokeOnInstance (f: 'T->'V) (source: '``Bifunctor<'T,'V>``) : '``Bifunctor<'U,'V>`` =
        (^``Bifunctor<'T,'V>`` : (static member First : _*_ -> _) source, f)

type MapFirst with
    static member inline First (x: '``Bifunctor<'T,'V>``, f: 'T->'U, [<Optional>]_mthd: Default2) = Bimap.InvokeOnInstance f id x  : '``Bifunctor<'U,'V>``
    #if !FABLE_COMPILER
    static member inline First (x: '``Bifunctor<'T,'V>``, f: 'T->'U, [<Optional>]_mthd: Default1) = MapFirst.InvokeOnInstance f x  : '``Bifunctor<'U,'V>``
    static member inline First (_: ^t when ^t: null and ^t: struct, _ : 'T->'U,  _mthd: Default1) = ()
    #endif


type Map with
    static member inline Map ((x: '``Bifunctor<'T,'V>``, f: 'V->'W), [<Optional>]_mthd: Default6) = Bimap.InvokeOnInstance id f x


type Bimap with
    static member inline Bimap (x: '``Bifunctor<'T,'V>``, f: 'T->'U, g: 'V->'W, [<Optional>]_mthd: Default2) = x |> MapFirst.InvokeOnInstance f |> Map.InvokeOnInstance g : '``Bifunctor<'U,'W>``
    #if !FABLE_COMPILER
    static member inline Bimap (x: '``Bifunctor<'T,'V>``, f: 'T->'U, g: 'V->'W, [<Optional>]_mthd: Default1) = Bimap.InvokeOnInstance f g x                               : '``Bifunctor<'U,'W>``
    static member inline Bimap (_: ^t when ^t: null and ^t: struct, _: 'T->'U, _: 'V->'W,   _mthd: Default1) = ()
    #endif


// Profunctor class -------------------------------------------------------

type Dimap =
    inherit Default1

    static member Dimap (f             , g: 'A->'B, h: 'C->'D, [<Optional>]_mthd: Dimap) = g >> f >> h : 'A->'D
    static member Dimap (f: Func<'B,'C>, g: 'A->'B, h: 'C->'D, [<Optional>]_mthd: Dimap) = Func<'A,'D> (g >> f.Invoke >> h)
    
    static member inline Invoke (ab: 'A->'B) (cd: 'C->'D) (source: '``Profunctor<'B,'C>``) : '``Profunctor<'A,'D>`` =
        let inline call (mthd: ^M, source: ^I, _output: ^R) = ((^M or ^I or ^R) : (static member Dimap : _*_*_*_ -> _) source, ab, cd, mthd)
        call (Unchecked.defaultof<Dimap>, source, Unchecked.defaultof<'``Profunctor<'A,'D>``>)

    static member inline InvokeOnInstance (ab: 'A->'B) (cd: 'C->'D) (source: '``Profunctor<'B,'C>``) : '``Profunctor<'A,'D>`` =
        (^``Profunctor<'B,'C>`` : (static member Dimap : _*_*_ -> _) source, ab, cd)


// Contravariant class ----------------------------------------------------

type Contramap =
    inherit Default1

    static member inline Invoke (f: 'U -> 'T) (source: '``Contravariant<'T>``) : '``Contravariant<'U>`` = 
        let inline call (mthd: ^M, source: ^I, _output: ^R) = ((^M or ^I or ^R) : (static member Contramap : _*_*_ -> _) source, f, mthd)
        call (Unchecked.defaultof<Contramap>, source, Unchecked.defaultof<'``Contravariant<'U>``>)

    static member inline InvokeOnInstance (ab: 'A->'B) (source: '``Profunctor<'B,'C>``) : '``Profunctor<'A,'C>`` =
        (^``Profunctor<'B,'C>`` : (static member Contramap : _*_ -> _) source, ab)

    static member Contramap (k: 'T -> 'C            , f: 'U -> 'T, [<Optional>]_mthd: Contramap) = f >> k : 'U->'C
    static member Contramap (k: Func<'T, 'C>        , f: 'U -> 'T, [<Optional>]_mthd: Contramap) = Func<'U, 'C> (f >> k.Invoke)
    #if !FABLE_COMPILER
    static member Contramap (p: Predicate<_>        , f: 'U -> 'T, [<Optional>]_mthd: Contramap) = Predicate (fun x -> p.Invoke (f x))
    #endif
    static member Contramap (c: IComparer<_>        , f: 'U -> 'T, [<Optional>]_mthd: Contramap) = { new IComparer<'U> with member __.Compare (x, y) = c.Compare (f x, f y) }
    static member Contramap (c: IEqualityComparer<_>, f: 'U -> 'T, [<Optional>]_mthd: Contramap) = { 
                    new IEqualityComparer<'U> with
                        member __.Equals (x, y) = c.Equals (f x, f y)
                        member __.GetHashCode x = c.GetHashCode (f x) }
    
type Contramap with
    static member inline Contramap (x: '``Profunctor<'B,'C>``, f: 'A->'B, [<Optional>]_mthd: Default2) = Dimap.InvokeOnInstance f id x : '``Profunctor<'A,'C>``
    #if !FABLE_COMPILER
    static member inline Contramap (x: '``Contravariant<'T>``, f: 'U->'T, [<Optional>]_mthd: Default1) = Contramap.InvokeOnInstance f x: '``Contravariant<'U>``
    static member inline Contramap (_: ^t when ^t: null and ^t: struct  , _: 'A->'B,  _mthd: Default1) = ()
    #endif


type Map with
    static member inline Map ((x: '``Profunctor<'B,'C>``, cd: 'C->'D), [<Optional>]_mthd: Default5) = Dimap.InvokeOnInstance id cd x : '``Profunctor<'B,'D>``


type Dimap with
    static member inline Dimap (x: '``Profunctor<'B,'C>``, ab: 'A->'B, cd: 'C->'D, [<Optional>]_mthd: Default2) = x |> Map.InvokeOnInstance cd |> Contramap.InvokeOnInstance ab : '``Profunctor<'A,'D>``
    #if !FABLE_COMPILER
    static member inline Dimap (x: '``Profunctor<'B,'C>``, ab: 'A->'B, cd: 'C->'D, [<Optional>]_mthd: Default1) = Dimap.InvokeOnInstance ab cd x                                : '``Profunctor<'A,'D>``
    static member inline Dimap (_: ^t when ^t: null and ^t: struct,     _: 'T->'U, _: 'V->'W,  _mthd: Default1) = ()
    #endif

// Invariant functor

type Invmap = static member inline Invoke (f: 'T -> 'U) (g: 'U -> 'T) (source: '``InvariantFunctor<'T>``) = (^``InvariantFunctor<'T>`` : (static member Invmap : _*_*_ -> _) source, f, g) : '``InvariantFunctor<'U>``


// Invokable class --------------------------------------------------------

type Invoke =
    inherit Default1

    #if !FABLE_COMPILER
    static member inline Invoke (_: ^t when ^t : null and ^t : struct, _, _output: ^O, _mthd: Default1) = id
    static member inline Invoke (_: 'T, x, _output: ^O, _mthd: Default1) = (^T : (static member Invoke : _ -> _) x)
    #endif
    static member        Invoke (g:  'T -> 'U  , x: 'T, _output: 'U, _mthd: Invoke) = g x        : 'U
    static member        Invoke (g: Func<'T,'U>, x: 'T, _output: 'U, _mthd: Invoke) = g.Invoke x : 'U

    // No return type check
    static member inline InvokeNRTC (f: '``Category<'T,'U>``, x : 'T) =
        let inline call (_: ^I, x: 'TT) = ((^I or ^TT) : (static member Invoke : _-> _) x)
        call (f, x)  

    static member inline Invoke (f: '``Category<'T,'U>``, x: 'T) : 'U =
        let inline call (mthd: ^M, f: ^I, output: ^R, x: 'TT) = ((^M or ^TT) : (static member Invoke : _*_*_*_ -> _) f, x, output, mthd)
        call (Unchecked.defaultof<Invoke>, f, Unchecked.defaultof<'U>, x)

type ComposedStaticInvokable< ^F, ^G>  =
    static member inline Invoke x =
        let i  =  Invoke.Invoke (Unchecked.defaultof<'G>, x)
        Invoke.Invoke (Unchecked.defaultof<'F>, i)


// Category class ---------------------------------------------------------

#nowarn "0077"

type Id =
    inherit Default1
    static member Id ([<Optional>]_output:  'T -> 'T  , [<Optional>]_mthd: Id) = id             : 'T -> 'T
    static member Id ([<Optional>]_output: Func<'T,'T>, [<Optional>]_mthd: Id) = Func<'T,'T> id : Func<'T,'T>

    static member inline Invoke () : '``Category<'T,'T>`` =
        let inline call (mthd: ^M, output: ^R) = ((^M or ^R) : (static member Id : _*_ -> _) output, mthd)
        call (Unchecked.defaultof<Id>, Unchecked.defaultof<'``Category<'T,'T>``>)

    static member inline InvokeOnInstance () : '``Category<'T,'T>`` = (^``Category<'T,'T>`` : (static member Id : _) ())

#if !FABLE_COMPILER
type Id with
    static member inline Id (_output:  '``Category<'T,'T>``        , _mthd: Default1) = Id.InvokeOnInstance () : '``Category<'T,'T>``
    static member inline Id (_output: ^t when ^t:null and ^t:struct, _mthd: Default1) = id                       
#endif


type Comp =
    inherit Default1
    static member ``<<<`` (f:  'U -> 'V  , g:  'T -> 'U  , [<Optional>]_output (*: 'T -> 'V   *), [<Optional>]_mthd: Comp) = g >> f : 'T -> 'V
    static member ``<<<`` (f: Func<'U,'V>, g: Func<'T,'U>, [<Optional>]_output (*: Func<'T,'V>*), [<Optional>]_mthd: Comp) = Func<'T,'V> (g.Invoke >> f.Invoke)

    static member inline Invoke (f : '``Category<'U,'V>``) (g : '``Category<'T,'U>``) : '``Category<'T,'V>`` =
        let inline call (mthd: ^M, f: ^I, output: ^R) = ((^M or ^I or ^R) : (static member ``<<<`` : _*_*_*_ -> _) f, g, output, mthd)
        call (Unchecked.defaultof<Comp>, f, Unchecked.defaultof<Comp>)  //Unchecked.defaultof<'``Category<'T,'V>``>)

    static member inline InvokeOnInstance  (f: '``Category<'U,'V>``) (g: '``Category<'T,'U>``) : '``Category<'T,'V>`` = ( ^``Category<'T,'V>`` : (static member (<<<) : _*_ -> _) f, g)
    static member inline InvokeOnInstance' (f: '``Category<'U,'V>``) (g: '``Category<'T,'U>``) : '``Category<'T,'V>`` = ((^``Category<'U,'V>`` or ^``Category<'T,'U>``) : (static member (<<<) : _*_ -> _) f, g) : '``Category<'T,'V>``

#if !FABLE_COMPILER
type Comp with
    static member inline ``<<<`` (f: '``Category<'U,'V>``, g: '``Category<'T,'U>``, _output (* : '``Category<'T,'V>``   *) , _mthd : Default1) = Comp.InvokeOnInstance' f g : '``Category<'T,'V>``
    static member inline ``<<<`` (f: 'F, g: 'G, _, _mthd: Default1) =         
        let inline ivk (f: 'T) (x: 'U)  = (^T : (static member Invoke : _*_ -> _) f, x)
        let inline h f g x = 
            let i = ivk f x
            ivk g i
        let _ = h f g
        Unchecked.defaultof<ComposedStaticInvokable<'F, 'G>>
#endif


// Arrow class ------------------------------------------------------------

type Arr =
    inherit Default1
    static member Arr (f: 'T -> 'U, [<Optional>]_output:  'T-> 'U   , [<Optional>]_mthd: Arr) = f
    static member Arr (f: 'T -> 'U, [<Optional>]_output: Func<'T,'U>, [<Optional>]_mthd: Arr) = Func<'T,'U> f

    static member inline Invoke (f: 'T -> 'U) : '``Arrow<'T,'U>`` = 
        let inline call (mthd: ^M, output: ^R) = ((^M or ^R) : (static member Arr: _*_*_ -> _) f, output, mthd)
        call (Unchecked.defaultof<Arr>, Unchecked.defaultof<'``Arrow<'T,'U>``>)

    static member inline InvokeOnInstance (f: 'T -> 'U) : '``Arrow<'T,'U>`` = (^``Arrow<'T,'U>`` : (static member Arr: _ -> _) f)

#if !FABLE_COMPILER
type Arr with
    static member inline Arr (f: 'T -> 'U, _output: '``Arrow<'T,'U>``                , _mthd: Default1) = Arr.InvokeOnInstance f : '``Arrow<'T,'U>``
    static member inline Arr (_: 'T -> 'U, _output: ^t when ^t : null and ^t : struct, _mthd: Default1) = id
#endif


type ArrFirst =
    inherit Default1
    static member First (f: 'T -> 'U   , [<Optional>]_output:   'T*'V -> 'U*'V  , [<Optional>]_mthd: ArrFirst) =            fun (x, y) -> (f x       , y)  : 'U*'V
    static member First (f: Func<'T,'U>, [<Optional>]_output: Func<'T*'V,'U*'V> , [<Optional>]_mthd: ArrFirst) = Func<_,_> (fun (x, y) -> (f.Invoke x, y)) : Func<'T*'V,'U*'V>

    static member inline Invoke (f: '``Arrow<'T,'U>``) : '``Arrow<('T * 'V),('U * 'V)>`` =
        let inline call (mthd: ^M, source: ^I, output: ^R) = ((^M or ^I or ^R) : (static member First : _*_*_ -> _) source, output, mthd)
        call (Unchecked.defaultof<ArrFirst>, f, Unchecked.defaultof<'``Arrow<('T * 'V),('U * 'V)>``>)

    static member inline InvokeOnInstance (f: '``Arrow<'T,'U>``) : '``Arrow<('T * 'V),('U * 'V)>`` = ((^``Arrow<'T,'U>`` or ^``Arrow<('T * 'V),('U * 'V)>``) : (static member First : _ -> _) f)

#if !FABLE_COMPILER
type ArrFirst with
    static member inline First (f: '``Arrow<'T,'U>``, _output: '``Arrow<('T * 'V),('U * 'V)>``, _mthd: Default1) = ArrFirst.InvokeOnInstance f  : '``Arrow<('T * 'V),('U * 'V)>``
    static member inline First (_: ^t when ^t : null and ^t : struct  , _output               , _mthd: Default1) = id
#endif


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

    #if !FABLE_COMPILER
    static member inline Second (f: '``Arrow<'T,'U>``, _output: '``Arrow<('V * 'T),('V * 'U)>``, _mthd: Default1) = ArrSecond.InvokeOnInstance f : '``Arrow<('V * 'T),('V * 'U)>``
    static member inline Second (_: ^t when ^t : null and ^t : struct  , _output               , _mthd: Default1) = id
    #endif

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
    #if !FABLE_COMPILER
    static member inline ``***`` (f: '``Arrow<'T1,'U1>``, g: '``Arrow<'T2,'U2>``, _output: '``Arrow<('T1 * 'T2),('U1 * 'U2)>``, _mthd: Default1) = ArrCombine.InvokeOnInstance f g                                                    : '``Arrow<('T1 * 'T2),('U1 * 'U2)>``
    static member inline ``***`` (_: '``Arrow<'T1,'U1>``, _: '``Arrow<'T2,'U2>``, _output: ^t when ^t : null and ^t : struct  , _mthd: Default1) = id
    #endif


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
    #if !FABLE_COMPILER
    static member inline ``&&&`` (f: '``Arrow<'T,'U1>``, g: '``Arrow<'T,'U2>``, _output: '``Arrow<'T,('U1 * 'U2)>``,    _mthd: Default1) = Fanout.InvokeOnInstance f g                                                                                                                         : '``Arrow<'T,('U1 * 'U2)>``
    static member inline ``&&&`` (_: '``Arrow<'T,'U1>``, _: '``Arrow<'T,'U2>``, _output: ^t when ^t:null and ^t:struct, _mthd: Default1) = id
    #endif

// ArrowChoice class ------------------------------------------------------

type Fanin =
    inherit Default1
    static member ``|||`` (f:  'T -> 'V  , g: 'U -> 'V   , [<Optional>]_output: Choice<'U,'T> -> 'V   , [<Optional>]_mthd: Fanin) = Choice.either g f                                        : Choice<'U,'T> -> 'V
    static member ``|||`` (f: Func<'T,'V>, g: Func<'U,'V>, [<Optional>]_output: Func<Choice<'U,'T>,'V>, [<Optional>]_mthd: Fanin) = Func<Choice<'U,'T>,'V> (Choice.either g.Invoke f.Invoke) : Func<Choice<'U,'T>,'V>

    static member inline Invoke (f: '``ArrowChoice<'T,'V>``) (g: '``ArrowChoice<'U,'V>``) : '``ArrowChoice<Choice<'U,'T>,'V>`` =
        let inline call (mthd: ^M, output: ^R) = ((^M or ^R) : (static member ``|||`` : _*_*_*_ -> _) f, g, output, mthd)
        call (Unchecked.defaultof<Fanin>, Unchecked.defaultof<'``ArrowChoice<Choice<'U,'T>,'V>``>)

    static member inline InvokeOnInstance (f: '``ArrowChoice<'T,'V>``) (g: '``ArrowChoice<'U,'V>``) : '``ArrowChoice<Choice<'U,'T>,'V>`` = (^``ArrowChoice<Choice<'U,'T>,'V>`` : (static member (|||) : _*_ -> _) f, g)

#if !FABLE_COMPILER
type Fanin with
    static member inline ``|||`` (f: '``ArrowChoice<'T,'V>``, g: '``ArrowChoice<'U,'V>``, _output: '``ArrowChoice<Choice<'U,'T>,'V>``, _mthd: Default1) = Fanin.InvokeOnInstance f g : '``ArrowChoice<Choice<'U,'T>,'V>``
    static member inline ``|||`` (_: '``ArrowChoice<'T,'V>``, _: '``ArrowChoice<'U,'V>``, _output: ^t when ^t : null and ^t : struct , _mthd: Default1) = id
#endif


type AcMerge =
    inherit Default1
    static member ``+++`` (f: 'T1 -> 'U1   , g: 'T2 -> 'U2   , [<Optional>]_output:  Choice<'T2,'T1> ->  Choice<'U2,'U1> , [<Optional>]_mthd: AcMerge) = Fanin.Invoke (Choice2Of2 << f) (Choice1Of2 << g)                                       : Choice<'T2,'T1> ->  Choice<'U2,'U1>
    static member ``+++`` (f: Func<'T1,'U1>, g: Func<'T2,'U2>, [<Optional>]_output: Func<Choice<'T2,'T1>,Choice<'U2,'U1>>, [<Optional>]_mthd: AcMerge) = Fanin.Invoke (Func<_,_> (Choice2Of2 << f.Invoke)) (Func<_,_> (Choice1Of2 << g.Invoke)) : Func<Choice<'T2,'T1>,Choice<'U2,'U1>>

    static member inline Invoke (f: '``ArrowChoice<'T1,'U1>``) (g: '``ArrowChoice<'T2,'U2>``) : '``ArrowChoice<Choice<'T2,'T1>,Choice<'U2,'U1>>`` =
        let inline call (mthd: ^M, output: ^R) = ((^M or ^R) : (static member ``+++`` : _*_*_*_ -> _) f, g, output, mthd)
        call (Unchecked.defaultof<AcMerge>, Unchecked.defaultof<'``ArrowChoice<Choice<'T2,'T1>,Choice<'U2,'U1>>``>)

    static member inline InvokeOnInstance (f: '``ArrowChoice<'T1,'U1>``) (g: '``ArrowChoice<'T2,'U2>``) : '``ArrowChoice<Choice<'T2,'T1>,Choice<'U2,'U1>>`` = (^``ArrowChoice<Choice<'T2,'T1>,Choice<'U2,'U1>>`` : (static member (+++) : _*_ -> _) f, g)

#if !FABLE_COMPILER
type AcMerge with
    static member inline ``+++`` (f: '``ArrowChoice<'T1,'U1>``, g: '``ArrowChoice<'T2,'U2>``, _output: '``ArrowChoice<Choice<'T2,'T1>,Choice<'U2,'U1>>``, _mthd: Default1) = AcMerge.InvokeOnInstance f g : '``ArrowChoice<Choice<'T2,'T1>,Choice<'U2,'U1>>``
    static member inline ``+++`` (_: '``ArrowChoice<'T1,'U1>``, _: '``ArrowChoice<'T2,'U2>``, _output: ^t when ^t : null and ^t : struct                , _mthd: Default1) = id
#endif


type AcLeft =
    inherit Default1
    static member inline Left (f:  'T -> 'U   , [<Optional>]_output:   Choice<'V,'T> -> Choice<'V,'U> , [<Optional>]_mthd: AcLeft) = AcMerge.Invoke f id : Choice<'V,'T> -> Choice<'V,'U>
    static member inline Left (f: Func<'T,'U> , [<Optional>]_output: Func<Choice<'V,'T>,Choice<'V,'U>>, [<Optional>]_mthd: AcLeft) = AcMerge.Invoke f (Func<'V,_> id)

    static member inline Invoke (f: '``ArrowChoice<'T,'U>``) : '``ArrowChoice<Choice<'V,'T>,Choice<'V,'U>>`` =
        let inline call (mthd: ^M, source: ^I, output: ^R) = ((^M or ^I or ^R) : (static member Left : _*_*_ -> _) source, output, mthd)
        call (Unchecked.defaultof<AcLeft>, f, Unchecked.defaultof<'``ArrowChoice<Choice<'V,'T>,Choice<'V,'U>>``>)

    static member inline InvokeOnInstance (f: '``ArrowChoice<'T,'U>``) : '``ArrowChoice<Choice<'V,'T>,Choice<'V,'U>>`` = ((^``ArrowChoice<'T,'U>`` or ^``ArrowChoice<Choice<'V,'T>,Choice<'V,'U>>``) : (static member Left : _ -> _) f)

#if !FABLE_COMPILER
type AcLeft with
    static member inline Left (f: '``ArrowChoice<'T,'U>``, _output: '``ArrowChoice<Choice<'V,'T>,Choice<'V,'U>>``, _mthd: Default1) = AcLeft.InvokeOnInstance f: '``ArrowChoice<Choice<'V,'T>,Choice<'V,'U>>``
    static member inline Left (_: '``ArrowChoice<'T,'U>``, _output: ^t when ^t : null and ^t : struct            , _mthd: Default1) = id
#endif

type AcRight =
    inherit Default1
    static member inline Right (f:  'T -> 'U   , [<Optional>]_output:   Choice<'T,'V> -> Choice<'U,'V> , [<Optional>]_mthd: AcRight) = AcMerge.Invoke id f : Choice<'T,'V> -> Choice<'U,'V>
    static member inline Right (f: Func<'T,'U> , [<Optional>]_output: Func<Choice<'T,'V>,Choice<'U,'V>>, [<Optional>]_mthd: AcRight) = AcMerge.Invoke (Func<_,'V> id) f

    static member inline Invoke (f: '``ArrowChoice<'T,'U>``) : '``ArrowChoice<Choice<'T,'V>,Choice<'U,'V>>``   =
        let inline call (mthd: ^M, source: ^I, output: ^R) = ((^M or ^I or ^R) : (static member Right : _*_*_ -> _) source, output, mthd)
        call (Unchecked.defaultof<AcRight>, f, Unchecked.defaultof<'``ArrowChoice<Choice<'T,'V>,Choice<'U,'V>>``>)

    static member inline InvokeOnInstance (f: '``ArrowChoice<'T,'U>``) : '``ArrowChoice<Choice<'V,'T>,Choice<'U,'V>>`` = ((^``ArrowChoice<'T,'U>`` or ^``ArrowChoice<Choice<'V,'T>,Choice<'U,'V>>``) : (static member Right : _ -> _) f)

#if !FABLE_COMPILER
type AcRight with
    static member inline Right (f: '``ArrowChoice<'T,'U>``, _output: '``ArrowChoice<Choice<'V,'T>,Choice<'U,'V>>``, _mthd: Default1) = AcRight.InvokeOnInstance f : '``ArrowChoice<Choice<'V,'T>,Choice<'U,'V>>``
    static member inline Right (_: '``ArrowChoice<'T,'U>``, _output: ^t when ^t : null and ^t : struct            , _mthd: Default1) = id
#endif


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


namespace FSharpPlus.Internals
module internal MonadOps =
    open FSharpPlus.Control

    let inline (>>=) x f = Bind.Invoke x f
    #if !FABLE_COMPILER
    let inline result  x = Return.Invoke x
    #endif
    let inline (<*>) f x = Apply.Invoke f x
    let inline (<|>) x y = Append.Invoke x y
    let inline (>=>) (f: 'a->'``Monad<'b>``) (g: 'b->'``Monad<'c>``) (x: 'a) : '``Monad<'c>`` = f x >>= g
