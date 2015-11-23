namespace FsControl

open System
open System.Text
open System.Collections.Generic
open System.Runtime.CompilerServices
open System.Runtime.InteropServices
open Microsoft.FSharp.Quotations
#if NOTNET35
open System.Threading.Tasks
#endif
open FsControl.Core.Internals
open FsControl.Core.Internals.Prelude


type Empty =
    inherit Default1
        
    static member        Empty (_:list<'a>     , _:Empty) = []   :  list<'a>
    static member        Empty (_:option<'a>   , _:Empty) = None :option<'a>
    static member        Empty (_:array<'a>    , _:Empty) = [||] : array<'a>
    static member        Empty (_:string       , _:Empty) = ""
    static member        Empty (_:StringBuilder, _:Empty) = new StringBuilder()
    static member        Empty (_:unit         , _:Empty) = ()
    static member        Empty (_:Set<'a>      , _:Empty) = Set.empty : Set<'a>
    static member        Empty (_:Map<'a,'b>   , _:Empty) = Map.empty : Map<'a,'b>
    static member        Empty (_:'a->'a       , _:Empty) = id :'a->'a 

    static member inline Invoke() = 
        let inline call_2 (a:^a, b:^b) = ((^a or ^b) : (static member Empty: _*_ -> _) b, a)
        let inline call (a:'a) = call_2 (a, Unchecked.defaultof<'r>) :'r
        call Unchecked.defaultof<Empty>

type Empty with static member inline Empty (_ : 'a*'b         , _:Empty) = (Empty.Invoke(), Empty.Invoke()                                                ): 'a*'b
type Empty with static member inline Empty (_ : 'a*'b*'c      , _:Empty) = (Empty.Invoke(), Empty.Invoke(), Empty.Invoke()                                ): 'a*'b*'c
type Empty with static member inline Empty (_ : 'a*'b*'c*'d   , _:Empty) = (Empty.Invoke(), Empty.Invoke(), Empty.Invoke(), Empty.Invoke()                ): 'a*'b*'c*'d
type Empty with static member inline Empty (_ : 'a*'b*'c*'d*'e, _:Empty) = (Empty.Invoke(), Empty.Invoke(), Empty.Invoke(), Empty.Invoke(), Empty.Invoke()): 'a*'b*'c*'d*'e

type Empty with
    static member inline Empty (_:'R, _:Default1) = ((^R) : (static member Empty: unit -> ^R) ()):'R

#if NOTNET35        

    static member inline Empty (_:Task<'a>,        _:Empty) =
        let (v:'a) = Empty.Invoke()
        let s = TaskCompletionSource()
        s.SetResult v
        s.Task
#endif

    static member inline Empty (_:Async<'a>        , _:Empty) = let (v:'a) = Empty.Invoke() in async.Return v
    static member inline Empty (_:Expr<'a>         , _:Empty) = let (v:'a) = Empty.Invoke() in Expr.Cast<'a>(Expr.Value(v))
    static member inline Empty (_:Lazy<'a>         , _:Empty) = let (v:'a) = Empty.Invoke() in lazy v
    static member        Empty (_:Dictionary<'a,'b>, _:Empty) = Dictionary<'a,'b>()
    static member        Empty (_:ResizeArray<'a>  , _:Empty) = ResizeArray() : ResizeArray<'a>
    static member        Empty (_:seq<'a>          , _:Empty) = Seq.empty   :  seq<'a>


[<Extension; Sealed>]
type Append =       
    [<Extension>]static member        Append (x:list<_>      , y ) = x @ y       
    [<Extension>]static member        Append (x:array<_>     , y ) = Array.append x y
    [<Extension>]static member        Append (()             , ()) =  ()
    [<Extension>]static member        Append (x:Set<_>       , y ) = Set.union x y
    [<Extension>]static member        Append (x:string       , y ) = x + y
    [<Extension>]static member        Append (x:StringBuilder, y:StringBuilder) = StringBuilder().Append(x).Append(y)
    [<Extension>]static member        Append (x:'a->'a       , y ) = x << y

    static member inline Invoke (x:'T) (y:'T) :'T =
        let inline call_3 (m:^M, a:^t, b:^t) = ((^M or ^t) : (static member Append: _*_ -> _) a, b)
        call_3 (Unchecked.defaultof<Append>, x, y)

type Append with
    [<Extension>]static member inline Append (x:option<_>,y ) =
                    match (x,y) with
                    | (Some a , Some b) -> Some (Append.Invoke a b)
                    | (Some a , None  ) -> Some a
                    | (None   , Some b) -> Some b
                    | _                 -> None


type Append with 
    [<Extension>]static member inline Append ((x1,x2         ), (y1,y2         )) = (Append.Invoke x1 y1, Append.Invoke x2 y2                                                               ) :'a*'b
type Append with 
    [<Extension>]static member inline Append ((x1,x2,x3      ), (y1,y2,y3      )) = (Append.Invoke x1 y1, Append.Invoke x2 y2, Append.Invoke x3 y3                                          ) :'a*'b*'c
type Append with 
    [<Extension>]static member inline Append ((x1,x2,x3,x4   ), (y1,y2,y3,y4   )) = (Append.Invoke x1 y1, Append.Invoke x2 y2, Append.Invoke x3 y3, Append.Invoke x4 y4                     ) :'a*'b*'c*'d
type Append with 
    [<Extension>]static member inline Append ((x1,x2,x3,x4,x5), (y1,y2,y3,y4,y5)) = (Append.Invoke x1 y1, Append.Invoke x2 y2, Append.Invoke x3 y3, Append.Invoke x4 y4, Append.Invoke x5 y5) :'a*'b*'c*'d*'e
    
type Append with    
    
#if NOTNET35
    [<Extension>]static member inline Append (x:'a Task, y:'a Task) =
                    x.ContinueWith(fun (t: Task<_>) -> 
                        (fun a -> 
                            y.ContinueWith(fun (u: Task<_>) -> 
                                Append.Invoke a u.Result)) t.Result).Unwrap()
#endif

    [<Extension>]static member inline Append (x:Map<'a,'b>, y) =
                    Map.fold (fun m k v' -> Map.add k (match Map.tryFind k m with Some v -> Append.Invoke v v' | None -> v') m) x y

    [<Extension>]static member inline Append (x:Dictionary<'Key,'Value>, y:Dictionary<'Key,'Value>) =
                    let d = Dictionary<'Key,'Value>()
                    for KeyValue(k, v ) in x do d.[k] <- v
                    for KeyValue(k, v') in y do d.[k] <- match d.TryGetValue k with true, v -> Append.Invoke v v' | _ -> v'
                    d

    [<Extension>]static member inline Append (x:'S Async, y:'S Async) = async {
                    let! a = x
                    let! b = y
                    return Append.Invoke a b}

    [<Extension>]static member inline Append (x:'a Expr, y:'a Expr) :'a Expr =
                    let inline f (x:'a)  :'a -> 'a = Append.Invoke x
                    Expr.Cast<'a>(Expr.Application(Expr.Application(Expr.Value(f), x), y))
   

    [<Extension>]static member inline Append (x:'a Lazy      , y:'a Lazy)       = lazy Append.Invoke (x.Value) (y.Value)
    [<Extension>]static member        Append (x:_ ResizeArray, y:_ ResizeArray) = ResizeArray (Seq.append x y)
    [<Extension>]static member        Append (x:_ IObservable, y              ) = Observable.merge x y
    [<Extension>]static member        Append (x:_ seq        , y              ) = Seq.append x y


[<Extension; Sealed>]
type Concat =
    inherit Default1
    [<Extension>]static member inline Concat (x:seq<Dictionary<'a,'b>>, [<Optional>]output:Dictionary<'a,'b>, [<Optional>]impl:Concat) =
                    let dct = Dictionary<'a,'b>()
                    for d in x do
                        for KeyValue(k, u) in d do
                            dct.[k] <- match dct.TryGetValue k with true, v -> Append.Invoke v u | _ -> u
                    dct

    [<Extension>]static member inline Concat (x:seq<ResizeArray<'a>>, [<Optional>]output:'a ResizeArray, [<Optional>]impl:Concat) = ResizeArray (Seq.concat x)
    [<Extension>]static member        Concat (x:seq<list<'a>>       , [<Optional>]output:list<'a>      , [<Optional>]impl:Concat) = List.concat   x
    [<Extension>]static member        Concat (x:seq<array<'a>>      , [<Optional>]output:array<'a>     , [<Optional>]impl:Concat) = Array.concat  x
    [<Extension>]static member        Concat (x:seq<string>         , [<Optional>]output:string        , [<Optional>]impl:Concat) = String.Concat x
    [<Extension>]static member        Concat (x:seq<StringBuilder>  , [<Optional>]output:StringBuilder , [<Optional>]impl:Concat) = (StringBuilder(), x) ||> Seq.fold (fun x -> x.Append)

    static member inline Invoke (x:seq<'T>) : 'T =
        let inline call_3 (a:^a, b:^b, c:^c) = ((^a or ^b or ^c) : (static member Concat: _*_*_ -> _) b, c, a)
        let inline call (a:'a, b:'b) = call_3 (a, b, Unchecked.defaultof<'r>) :'r
        call (Unchecked.defaultof<Concat>, x)

type Concat with
    [<Extension>]static member inline Concat (x:seq<'a * 'b>, [<Optional>]output:'a * 'b, [<Optional>]impl:Concat) =
                    Concat.Invoke (Seq.map fst x), 
                    Concat.Invoke (Seq.map snd x)
    
type Concat with
    [<Extension>]static member inline Concat (x:seq<'a * 'b * 'c>, [<Optional>]output:'a * 'b * 'c, [<Optional>]impl:Concat) =
                    Concat.Invoke (Seq.map (fun (x,_,_) -> x) x), 
                    Concat.Invoke (Seq.map (fun (_,x,_) -> x) x), 
                    Concat.Invoke (Seq.map (fun (_,_,x) -> x) x)
    
type Concat with
    [<Extension>]static member inline Concat (x:seq<'a * 'b * 'c * 'd>, [<Optional>]output:'a * 'b * 'c * 'd, [<Optional>]impl:Concat) =
                    Concat.Invoke (Seq.map (fun (x,_,_,_) -> x) x), 
                    Concat.Invoke (Seq.map (fun (_,x,_,_) -> x) x), 
                    Concat.Invoke (Seq.map (fun (_,_,x,_) -> x) x),
                    Concat.Invoke (Seq.map (fun (_,_,_,x) -> x) x)

type Concat with
    [<Extension>]static member inline Concat (x:seq< 'a>, [<Optional>]output:'a, _:Default2) = Seq.fold Append.Invoke (Empty.Invoke()) x:'a
    
type Concat with
    [<Extension>]static member inline Concat (x:seq< ^R>, [<Optional>]output:^R, _:Default1) = ((^R) : (static member Concat: 'R seq -> ^R) x)
                 static member inline Concat (x:seq< ^R>, _:^t when ^t: null and ^t: struct, _:Default1) = fun () -> id