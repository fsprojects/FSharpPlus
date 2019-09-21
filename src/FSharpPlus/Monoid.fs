namespace FSharpPlus.Control

open System
open System.Text
open System.Collections.Generic
open System.Runtime.CompilerServices
open System.Runtime.InteropServices
open Microsoft.FSharp.Quotations
#if NET35
#else
open System.Threading.Tasks
#endif
open FSharpPlus
open FSharpPlus.Internals
open FSharpPlus.Internals.Prelude


[<Extension; Sealed>]
type Plus =     
    inherit Default1
    static member inline ``+`` (x: 'Plus             , y: 'Plus             ,             _mthd: Default2) = (^Plus :  (static member (<|>) : _*_ -> _) x, y) : ^Plus
    static member inline ``+`` (x: 'Plus             , y: 'Plus             , [<Optional>]_mthd: Default1) = x + y : ^Plus
    static member inline ``+`` (_: ^t when ^t: null and ^t: struct, _: ^t   , [<Optional>]_mthd: Default1) = id
    static member        ``+`` (x: list<_>           , y                    , [<Optional>]_mthd: Plus    ) = x @ y
    static member        ``+`` (x: array<_>          , y                    , [<Optional>]_mthd: Plus    ) = Array.append x y
    static member        ``+`` (()                   , ()                   , [<Optional>]_mthd: Plus    ) = ()
    static member        ``+`` (x: Set<_>            , y                    , [<Optional>]_mthd: Plus    ) = Set.union x y
    static member        ``+`` (x: StringBuilder     , y: StringBuilder     , [<Optional>]_mthd: Plus    ) = StringBuilder().Append(x).Append(y)    
    static member        ``+`` (x: AggregateException, y: AggregateException, [<Optional>]_mthd: Plus    ) = new AggregateException (seq {yield! x.InnerExceptions; yield! y.InnerExceptions})
    static member        ``+`` (_: Id0               , _: Id0               , [<Optional>]_mthd: Plus    ) = Id0 ""    
    static member        ``+`` (x: exn               , y: exn               , [<Optional>]_mthd: Plus    ) =
        let f (e: exn) = match e with :? AggregateException as a -> a.InnerExceptions :> seq<_> | _ -> Seq.singleton e
        new AggregateException (seq {yield! f x; yield! f y}) :> exn

    static member inline Invoke (x: 'Plus) (y: 'Plus) : 'Plus =
        let inline call (mthd : ^M, input1 : ^I, input2 : ^I) = ((^M or ^I) : (static member ``+`` : _*_*_ -> _) input1, input2, mthd)
        call (Unchecked.defaultof<Plus>, x, y)

type Plus with
    static member inline ``+`` (x: option<_>, y, [<Optional>]_mthd: Plus) =
                    match x, y with
                    | (Some a , Some b) -> Some (Plus.Invoke a b)
                    | (Some a , None  ) -> Some a
                    | (None   , Some b) -> Some b
                    | _                 -> None

type Plus with
    static member inline ``+`` (x: Result<_,_>, y, [<Optional>]_mthd: Plus) =
                    match x, y with
                    | Ok a   , Ok b    -> Ok (Plus.Invoke a b)
                    | Ok a   , Error _ -> Ok a
                    | Error _, Ok b    -> Ok b
                    | Error a, Error b -> Error (Plus.Invoke a b)

type Plus with
    static member inline ``+`` (x: Choice<_,_>, y, [<Optional>]_mthd: Plus) =
                    match x, y with
                    | Choice1Of2 a, Choice1Of2 b -> Choice1Of2 (Plus.Invoke a b)
                    | Choice1Of2 a, Choice2Of2 _ -> Choice1Of2 a
                    | Choice2Of2 _, Choice1Of2 b -> Choice1Of2 b
                    | Choice2Of2 a, Choice2Of2 b -> Choice2Of2 (Plus.Invoke a b)

type Plus with 
    #if !FABLE_COMPILER
    static member inline ``+`` (x, y, _mthd: Plus) : 't =
        let xr, yr = (^t : (member Rest : 'tr) x), (^t : (member Rest : 'tr) y)
        let x7, y7 = (^t : (member Item7: 't7) x), (^t : (member Item7: 't7) y)
        let x6, y6 = (^t : (member Item6: 't6) x), (^t : (member Item6: 't6) y)
        let x5, y5 = (^t : (member Item5: 't5) x), (^t : (member Item5: 't5) y)
        let x4, y4 = (^t : (member Item4: 't4) x), (^t : (member Item4: 't4) y)
        let x3, y3 = (^t : (member Item3: 't3) x), (^t : (member Item3: 't3) y)
        let x2, y2 = (^t : (member Item2: 't2) x), (^t : (member Item2: 't2) y)
        let x1, y1 = (^t : (member Item1: 't1) x), (^t : (member Item1: 't1) y)
        Tuple<_,_,_,_,_,_,_,_> (Plus.Invoke x1 y1, Plus.Invoke x2 y2, Plus.Invoke x3 y3, Plus.Invoke x4 y4, Plus.Invoke x5 y5, Plus.Invoke x6 y6, Plus.Invoke x7 y7, Plus.Invoke xr yr) |> retype : 't
    #endif

    static member inline ``+`` ( x: Tuple<'a>         ,  y: Tuple<'a>         , [<Optional>]_mthd: Plus) = Tuple<'a> (Plus.Invoke x.Item1 y.Item1) : Tuple<'a>
    static member inline ``+`` ((x1,x2               ), (y1,y2               ), [<Optional>]_mthd: Plus) = (Plus.Invoke x1 y1, Plus.Invoke x2 y2                                                                                               ) :'a*'b
    static member inline ``+`` ((x1,x2,x3            ), (y1,y2,y3            ), [<Optional>]_mthd: Plus) = (Plus.Invoke x1 y1, Plus.Invoke x2 y2, Plus.Invoke x3 y3                                                                            ) :'a*'b*'c
    static member inline ``+`` ((x1,x2,x3,x4         ), (y1,y2,y3,y4         ), [<Optional>]_mthd: Plus) = (Plus.Invoke x1 y1, Plus.Invoke x2 y2, Plus.Invoke x3 y3, Plus.Invoke x4 y4                                                         ) :'a*'b*'c*'d
    static member inline ``+`` ((x1,x2,x3,x4,x5      ), (y1,y2,y3,y4,y5      ), [<Optional>]_mthd: Plus) = (Plus.Invoke x1 y1, Plus.Invoke x2 y2, Plus.Invoke x3 y3, Plus.Invoke x4 y4, Plus.Invoke x5 y5                                      ) :'a*'b*'c*'d*'e
    static member inline ``+`` ((x1,x2,x3,x4,x5,x6   ), (y1,y2,y3,y4,y5,y6   ), [<Optional>]_mthd: Plus) = (Plus.Invoke x1 y1, Plus.Invoke x2 y2, Plus.Invoke x3 y3, Plus.Invoke x4 y4, Plus.Invoke x5 y5, Plus.Invoke x6 y6                   ) :'a*'b*'c*'d*'e*'f
    static member inline ``+`` ((x1,x2,x3,x4,x5,x6,x7), (y1,y2,y3,y4,y5,y6,y7), [<Optional>]_mthd: Plus) = (Plus.Invoke x1 y1, Plus.Invoke x2 y2, Plus.Invoke x3 y3, Plus.Invoke x4 y4, Plus.Invoke x5 y5, Plus.Invoke x6 y6, Plus.Invoke x7 y7) :'a*'b*'c*'d*'e*'f*'g


type Plus with    
    
#if NET35
#else
    static member inline ``+`` (x: 'a Task, y: 'a Task, [<Optional>]_mthd: Plus) =
                    x.ContinueWith(fun (t: Task<_>) -> 
                        (fun a -> 
                            y.ContinueWith(fun (u: Task<_>) -> 
                                Plus.Invoke a u.Result)) t.Result).Unwrap ()
#endif

    static member inline ``+`` (x: Map<'a,'b>             , y                         , [<Optional>]_mthd: Plus) = Map.unionWith Plus.Invoke x y

    static member inline ``+`` (x: Dictionary<'Key,'Value>, y: Dictionary<'Key,'Value>, [<Optional>]_mthd: Plus) =
                    let d = Dictionary<'Key,'Value> ()
                    let plus = OptimizedClosures.FSharpFunc<_,_,_>.Adapt Plus.Invoke
                    for KeyValue(k, v ) in x do d.[k] <- v
                    for KeyValue(k, v') in y do d.[k] <- match d.TryGetValue k with true, v -> plus.Invoke (v, v') | _ -> v'
                    d

    static member inline ``+`` (f: 'T->'Monoid, g: 'T->'Monoid, [<Optional>]_mthd: Plus) = (fun x -> Plus.Invoke (f x) (g x)) : 'T->'Monoid

    static member inline ``+`` (x: 'S Async   , y: 'S Async   , [<Optional>]_mthd: Plus) = Async.map2 Plus.Invoke x y

    static member inline ``+`` (x: 'a Expr    , y: 'a Expr    , [<Optional>]_mthd: Plus) : 'a Expr =
                    let inline f (x: 'a)  : 'a -> 'a = Plus.Invoke x
                    Expr.Cast<'a> (Expr.Application (Expr.Application (Expr.Value (f), x), y))
   

    static member inline ``+`` (x: 'a Lazy                   , y: 'a Lazy                   , [<Optional>]_mthd: Plus    ) = lazy Plus.Invoke x.Value y.Value
    static member        ``+`` (x: _ ResizeArray             , y: _ ResizeArray             , [<Optional>]_mthd: Plus    ) = ResizeArray (Seq.append x y)
    static member        ``+`` (x: _ IObservable             , y                            , [<Optional>]_mthd: Default3) = Observable.merge x y
    static member        ``+`` (x: _ seq                     , y                            , [<Optional>]_mthd: Default3) = Seq.append x y
    
    #if !FABLE_COMPILER
    static member        ``+`` (x: _ IEnumerator             , y                            , [<Optional>]_mthd: Default3) = Enumerator.concat <| (seq {yield x; yield y}).GetEnumerator ()
    #endif

    static member inline ``+`` (x: IDictionary<'K,'V>        , y: IDictionary<'K,'V>        , [<Optional>]_mthd: Default3) = Dict.unionWith Plus.Invoke x y
    static member inline ``+`` (x: IReadOnlyDictionary<'K,'V>, y: IReadOnlyDictionary<'K,'V>, [<Optional>]_mthd: Default3) = IReadOnlyDictionary.unionWith Plus.Invoke x y


[<Extension; Sealed>]
type Sum =
    inherit Default1
    static member inline Sum (x: seq<Dictionary<'a,'b>>, [<Optional>]_output: Dictionary<'a,'b>, [<Optional>]_impl: Sum) =
                    let dct = Dictionary<'a,'b> ()
                    let plus = OptimizedClosures.FSharpFunc<_,_,_>.Adapt Plus.Invoke
                    for d in x do
                        for KeyValue(k, u) in d do
                            dct.[k] <- match dct.TryGetValue k with true, v -> plus.Invoke (v, u) | _ -> u
                    dct

    static member inline Sum (x: seq<IDictionary<'a,'b>>, [<Optional>]_output: IDictionary<'a,'b>, [<Optional>]_impl: Sum) =
                    let dct = Dictionary<'a,'b> ()
                    let plus = OptimizedClosures.FSharpFunc<_,_,_>.Adapt Plus.Invoke
                    for d in x do
                        for KeyValue(k, u) in d do
                            dct.[k] <- match dct.TryGetValue k with true, v -> plus.Invoke (v, u) | _ -> u
                    dct :> IDictionary<'a,'b>

    static member inline Sum (x: seq<ResizeArray<'a>>, [<Optional>]_output: 'a ResizeArray, [<Optional>]_impl: Sum) = ResizeArray (Seq.concat x)
    static member        Sum (x: seq<list<'a>>       , [<Optional>]_output: list<'a>      , [<Optional>]_impl: Sum) = List.concat   x
    static member        Sum (x: seq<array<'a>>      , [<Optional>]_output: array<'a>     , [<Optional>]_impl: Sum) = Array.concat  x
    static member        Sum (x: seq<string>         , [<Optional>]_output: string        , [<Optional>]_impl: Sum) = String.Concat x
    static member        Sum (x: seq<StringBuilder>  , [<Optional>]_output: StringBuilder , [<Optional>]_impl: Sum) = (StringBuilder (), x) ||> Seq.fold (fun x -> x.Append)

    static member inline Invoke (x: seq<'T>) : 'T =
        let inline call_3 (a: ^a, b: ^b, c: ^c) = ((^a or ^b or ^c) : (static member Sum : _*_*_ -> _) b, c, a)
        let inline call (a: 'a, b: 'b) = call_3 (a, b, Unchecked.defaultof<'r>) :'r
        call (Unchecked.defaultof<Sum>, x)

    static member inline InvokeOnInstance (x: seq<'Monoid>) : 'Monoid =
        (^Monoid : (static member Sum : seq<'Monoid> -> 'Monoid) x)

type Sum with
    static member inline Sum (x: seq<'a * 'b>, [<Optional>]_output: 'a * 'b, [<Optional>]_impl: Sum) =
                    Sum.Invoke (Seq.map fst x), 
                    Sum.Invoke (Seq.map snd x)
    
type Sum with
    static member inline Sum (x: seq<'a * 'b * 'c>, [<Optional>]_output: 'a * 'b * 'c, [<Optional>]_impl: Sum) =
                    Sum.Invoke (Seq.map (fun (x,_,_) -> x) x), 
                    Sum.Invoke (Seq.map (fun (_,x,_) -> x) x), 
                    Sum.Invoke (Seq.map (fun (_,_,x) -> x) x)
    
type Sum with
    static member inline Sum (x: seq<'a * 'b * 'c * 'd>, [<Optional>]_output: 'a * 'b * 'c * 'd, [<Optional>]_impl: Sum) =
                    Sum.Invoke (Seq.map (fun (x,_,_,_) -> x) x), 
                    Sum.Invoke (Seq.map (fun (_,x,_,_) -> x) x), 
                    Sum.Invoke (Seq.map (fun (_,_,x,_) -> x) x),
                    Sum.Invoke (Seq.map (fun (_,_,_,x) -> x) x)

type Sum with
    static member inline Sum (x: seq< 'a>, [<Optional>]_output: 'a           , _: Default2) = Seq.fold Plus.Invoke (Zero.Invoke ()) x : 'a
    
type Sum with
    static member inline Sum (x: seq< ^R>, [<Optional>]_output: ^R           , _: Default1) = Sum.InvokeOnInstance x
    static member inline Sum (_: seq< ^R>, _: ^t when ^t: null and ^t: struct, _: Default1) = fun () -> id