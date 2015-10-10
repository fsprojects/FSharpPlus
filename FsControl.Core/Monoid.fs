namespace FsControl.Core.TypeMethods

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


type Mempty =
    inherit Default1
        
    static member        Mempty (_:list<'a>     , _:Mempty) = []   :  list<'a>
    static member        Mempty (_:option<'a>   , _:Mempty) = None :option<'a>
    static member        Mempty (_:array<'a>    , _:Mempty) = [||] : array<'a>
    static member        Mempty (_:string       , _:Mempty) = ""
    static member        Mempty (_:StringBuilder, _:Mempty) = new StringBuilder()
    static member        Mempty (_:unit         , _:Mempty) = ()
    static member        Mempty (_:Set<'a>      , _:Mempty) = Set.empty : Set<'a>
    static member        Mempty (_:Map<'a,'b>   , _:Mempty) = Map.empty : Map<'a,'b>
    static member        Mempty (_:'a->'a       , _:Mempty) = id :'a->'a 

    static member inline Invoke() = 
        let inline call_2 (a:^a, b:^b) = ((^a or ^b) : (static member Mempty: _*_ -> _) b, a)
        let inline call (a:'a) = call_2 (a, Unchecked.defaultof<'r>) :'r
        call Unchecked.defaultof<Mempty>

type Mempty with static member inline Mempty (_ : 'a*'b         , _:Mempty) = (Mempty.Invoke(), Mempty.Invoke()                                                   ): 'a*'b
type Mempty with static member inline Mempty (_ : 'a*'b*'c      , _:Mempty) = (Mempty.Invoke(), Mempty.Invoke(), Mempty.Invoke()                                  ): 'a*'b*'c
type Mempty with static member inline Mempty (_ : 'a*'b*'c*'d   , _:Mempty) = (Mempty.Invoke(), Mempty.Invoke(), Mempty.Invoke(), Mempty.Invoke()                 ): 'a*'b*'c*'d
type Mempty with static member inline Mempty (_ : 'a*'b*'c*'d*'e, _:Mempty) = (Mempty.Invoke(), Mempty.Invoke(), Mempty.Invoke(), Mempty.Invoke(), Mempty.Invoke()): 'a*'b*'c*'d*'e

type Mempty with
    static member inline Mempty (_:'R, _:Default1) = ((^R) : (static member Mempty: unit -> ^R) ()):'R

#if NOTNET35        

    static member inline Mempty (_:Task<'a>,        _:Mempty) =
        let (v:'a) = Mempty.Invoke()
        let s = TaskCompletionSource()
        s.SetResult v
        s.Task
#endif

    static member inline Mempty (_:Async<'a>        , _:Mempty) = let (v:'a) = Mempty.Invoke() in async.Return v
    static member inline Mempty (_:Expr<'a>         , _:Mempty) = let (v:'a) = Mempty.Invoke() in Expr.Cast<'a>(Expr.Value(v))
    static member inline Mempty (_:Lazy<'a>         , _:Mempty) = let (v:'a) = Mempty.Invoke() in lazy v
    static member        Mempty (_:Dictionary<'a,'b>, _:Mempty) = Dictionary<'a,'b>()
    static member        Mempty (_:ResizeArray<'a>  , _:Mempty) = ResizeArray() : ResizeArray<'a>
    static member        Mempty (_:seq<'a>          , _:Mempty) = Seq.empty   :  seq<'a>


[<Extension; Sealed>]
type Mappend =       
    [<Extension>]static member        Mappend (x:list<_>      , y ) = x @ y       
    [<Extension>]static member        Mappend (x:array<_>     , y ) = Array.append x y
    [<Extension>]static member        Mappend (()             , ()) =  ()
    [<Extension>]static member        Mappend (x:Set<_>       , y ) = Set.union x y
    [<Extension>]static member        Mappend (x:string       , y ) = x + y
    [<Extension>]static member        Mappend (x:StringBuilder, y:StringBuilder) = StringBuilder().Append(x).Append(y)
    [<Extension>]static member        Mappend (x:'a->'a       , y ) = x << y

    static member inline Invoke (x:'T) (y:'T) :'T =
        let inline call_3 (m:^M, a:^t, b:^t) = ((^M or ^t) : (static member Mappend: _*_ -> _) a, b)
        call_3 (Unchecked.defaultof<Mappend>, x, y)

type Mappend with
    [<Extension>]static member inline Mappend (x:option<_>,y ) =
                    match (x,y) with
                    | (Some a , Some b) -> Some (Mappend.Invoke a b)
                    | (Some a , None  ) -> Some a
                    | (None   , Some b) -> Some b
                    | _                 -> None


type Mappend with 
    [<Extension>]static member inline Mappend ((x1,x2         ), (y1,y2         )) = (Mappend.Invoke x1 y1, Mappend.Invoke x2 y2                                                                  ) :'a*'b
type Mappend with 
    [<Extension>]static member inline Mappend ((x1,x2,x3      ), (y1,y2,y3      )) = (Mappend.Invoke x1 y1, Mappend.Invoke x2 y2, Mappend.Invoke x3 y3                                            ) :'a*'b*'c
type Mappend with 
    [<Extension>]static member inline Mappend ((x1,x2,x3,x4   ), (y1,y2,y3,y4   )) = (Mappend.Invoke x1 y1, Mappend.Invoke x2 y2, Mappend.Invoke x3 y3, Mappend.Invoke x4 y4                      ) :'a*'b*'c*'d
type Mappend with 
    [<Extension>]static member inline Mappend ((x1,x2,x3,x4,x5), (y1,y2,y3,y4,y5)) = (Mappend.Invoke x1 y1, Mappend.Invoke x2 y2, Mappend.Invoke x3 y3, Mappend.Invoke x4 y4, Mappend.Invoke x5 y5) :'a*'b*'c*'d*'e
    
type Mappend with    
    
#if NOTNET35
    [<Extension>]static member inline Mappend (x:'a Task, y:'a Task) =
                    x.ContinueWith(fun (t: Task<_>) -> 
                        (fun a -> 
                            y.ContinueWith(fun (u: Task<_>) -> 
                                Mappend.Invoke a u.Result)) t.Result).Unwrap()
#endif

    [<Extension>]static member inline Mappend (x:Map<'a,'b>, y) =
                    Map.fold (fun m k v' -> Map.add k (match Map.tryFind k m with Some v -> Mappend.Invoke v v' | None -> v') m) x y

    [<Extension>]static member inline Mappend (x:Dictionary<'Key,'Value>, y:Dictionary<'Key,'Value>) =
                    let d = Dictionary<'Key,'Value>()
                    for KeyValue(k, v ) in x do d.[k] <- v
                    for KeyValue(k, v') in y do d.[k] <- match d.TryGetValue k with true, v -> Mappend.Invoke v v' | _ -> v'
                    d

    [<Extension>]static member inline Mappend (x:'S Async, y:'S Async) = async {
                    let! a = x
                    let! b = y
                    return Mappend.Invoke a b}

    [<Extension>]static member inline Mappend (x:'a Expr, y:'a Expr) :'a Expr =
                    let inline f (x:'a)  :'a -> 'a = Mappend.Invoke x
                    Expr.Cast<'a>(Expr.Application(Expr.Application(Expr.Value(f), x), y))
   

    [<Extension>]static member inline Mappend (x:'a Lazy      , y:'a Lazy)       = lazy Mappend.Invoke (x.Value) (y.Value)
    [<Extension>]static member        Mappend (x:_ ResizeArray, y:_ ResizeArray) = ResizeArray (Seq.append x y)
    [<Extension>]static member        Mappend (x:_ IObservable, y              ) = Observable.merge x y
    [<Extension>]static member        Mappend (x:_ seq        , y              ) = Seq.append x y


[<Extension; Sealed>]
type Mconcat =
    inherit Default1
    [<Extension>]static member inline Mconcat (x:seq<Dictionary<'a,'b>>, [<Optional>]output:Dictionary<'a,'b>, [<Optional>]impl:Mconcat) =
                    let dct = Dictionary<'a,'b>()
                    for d in x do
                        for KeyValue(k, u) in d do
                            dct.[k] <- match dct.TryGetValue k with true, v -> Mappend.Invoke v u | _ -> u
                    dct

    [<Extension>]static member inline Mconcat (x:seq<ResizeArray<'a>>, [<Optional>]output:'a ResizeArray, [<Optional>]impl:Mconcat) = ResizeArray (Seq.concat x)
    [<Extension>]static member        Mconcat (x:seq<list<'a>>       , [<Optional>]output:list<'a>      , [<Optional>]impl:Mconcat) = List.concat   x
    [<Extension>]static member        Mconcat (x:seq<array<'a>>      , [<Optional>]output:array<'a>     , [<Optional>]impl:Mconcat) = Array.concat  x
    [<Extension>]static member        Mconcat (x:seq<string>         , [<Optional>]output:string        , [<Optional>]impl:Mconcat) = String.Concat x
    [<Extension>]static member        Mconcat (x:seq<StringBuilder>  , [<Optional>]output:StringBuilder , [<Optional>]impl:Mconcat) = (StringBuilder(), x) ||> Seq.fold (fun x -> x.Append)

    static member inline Invoke (x:seq<'T>) : 'T =
        let inline call_3 (a:^a, b:^b, c:^c) = ((^a or ^b or ^c) : (static member Mconcat: _*_*_ -> _) b, c, a)
        let inline call (a:'a, b:'b) = call_3 (a, b, Unchecked.defaultof<'r>) :'r
        call (Unchecked.defaultof<Mconcat>, x)

type Mconcat with
    [<Extension>]static member inline Mconcat (x:seq<'a * 'b>, [<Optional>]output:'a * 'b, [<Optional>]impl:Mconcat) =
                    Mconcat.Invoke (Seq.map fst x), 
                    Mconcat.Invoke (Seq.map snd x)
    
type Mconcat with
    [<Extension>]static member inline Mconcat (x:seq<'a * 'b * 'c>, [<Optional>]output:'a * 'b * 'c, [<Optional>]impl:Mconcat) =
                    Mconcat.Invoke (Seq.map (fun (x,_,_) -> x) x), 
                    Mconcat.Invoke (Seq.map (fun (_,x,_) -> x) x), 
                    Mconcat.Invoke (Seq.map (fun (_,_,x) -> x) x)
    
type Mconcat with
    [<Extension>]static member inline Mconcat (x:seq<'a * 'b * 'c * 'd>, [<Optional>]output:'a * 'b * 'c * 'd, [<Optional>]impl:Mconcat) =
                    Mconcat.Invoke (Seq.map (fun (x,_,_,_) -> x) x), 
                    Mconcat.Invoke (Seq.map (fun (_,x,_,_) -> x) x), 
                    Mconcat.Invoke (Seq.map (fun (_,_,x,_) -> x) x),
                    Mconcat.Invoke (Seq.map (fun (_,_,_,x) -> x) x)

type Mconcat with
    [<Extension>]static member inline Mconcat (x:seq< 'a>, [<Optional>]output:'a, _:Default2) = Seq.fold Mappend.Invoke (Mempty.Invoke()) x:'a
    
type Mconcat with
    [<Extension>]static member inline Mconcat (x:seq< ^R>, [<Optional>]output:^R, _:Default1) = ((^R) : (static member Mconcat: 'R seq -> ^R) x)
                 static member inline Mconcat (x:seq< ^R>, _:^t when ^t: null and ^t: struct, _:Default1) = fun () -> id