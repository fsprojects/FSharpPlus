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


type MEmpty =
    inherit Default1
        
    static member        MEmpty (_:list<'a>     , _:MEmpty) = []   :  list<'a>
    static member        MEmpty (_:option<'a>   , _:MEmpty) = None :option<'a>
    static member        MEmpty (_:array<'a>    , _:MEmpty) = [||] : array<'a>
    static member        MEmpty (_:string       , _:MEmpty) = ""
    static member        MEmpty (_:StringBuilder, _:MEmpty) = new StringBuilder()
    static member        MEmpty (_:unit         , _:MEmpty) = ()
    static member        MEmpty (_:Set<'a>      , _:MEmpty) = Set.empty : Set<'a>
    static member        MEmpty (_:Map<'a,'b>   , _:MEmpty) = Map.empty : Map<'a,'b>
    static member        MEmpty (_:'a->'a       , _:MEmpty) = id :'a->'a 

    static member inline Invoke() = 
        let inline call_2 (a:^a, b:^b) = ((^a or ^b) : (static member MEmpty: _*_ -> _) b, a)
        let inline call (a:'a) = call_2 (a, Unchecked.defaultof<'r>) :'r
        call Unchecked.defaultof<MEmpty>

type MEmpty with static member inline MEmpty (_ : 'a*'b         , _:MEmpty) = (MEmpty.Invoke(), MEmpty.Invoke()                                                   ): 'a*'b
type MEmpty with static member inline MEmpty (_ : 'a*'b*'c      , _:MEmpty) = (MEmpty.Invoke(), MEmpty.Invoke(), MEmpty.Invoke()                                  ): 'a*'b*'c
type MEmpty with static member inline MEmpty (_ : 'a*'b*'c*'d   , _:MEmpty) = (MEmpty.Invoke(), MEmpty.Invoke(), MEmpty.Invoke(), MEmpty.Invoke()                 ): 'a*'b*'c*'d
type MEmpty with static member inline MEmpty (_ : 'a*'b*'c*'d*'e, _:MEmpty) = (MEmpty.Invoke(), MEmpty.Invoke(), MEmpty.Invoke(), MEmpty.Invoke(), MEmpty.Invoke()): 'a*'b*'c*'d*'e

type MEmpty with
    static member inline MEmpty (_:'R, _:Default1) = ((^R) : (static member MEmpty: unit -> ^R) ()):'R

#if NOTNET35        

    static member inline MEmpty (_:Task<'a>,        _:MEmpty) =
        let (v:'a) = MEmpty.Invoke()
        let s = TaskCompletionSource()
        s.SetResult v
        s.Task
#endif

    static member inline MEmpty (_:Async<'a>        , _:MEmpty) = let (v:'a) = MEmpty.Invoke() in async.Return v
    static member inline MEmpty (_:Expr<'a>         , _:MEmpty) = let (v:'a) = MEmpty.Invoke() in Expr.Cast<'a>(Expr.Value(v))
    static member inline MEmpty (_:Lazy<'a>         , _:MEmpty) = let (v:'a) = MEmpty.Invoke() in lazy v
    static member        MEmpty (_:Dictionary<'a,'b>, _:MEmpty) = Dictionary<'a,'b>()
    static member        MEmpty (_:ResizeArray<'a>  , _:MEmpty) = ResizeArray() : ResizeArray<'a>
    static member        MEmpty (_:seq<'a>          , _:MEmpty) = Seq.empty   :  seq<'a>


[<Extension; Sealed>]
type MAppend =       
    [<Extension>]static member        MAppend (x:list<_>      , y ) = x @ y       
    [<Extension>]static member        MAppend (x:array<_>     , y ) = Array.append x y
    [<Extension>]static member        MAppend (()             , ()) =  ()
    [<Extension>]static member        MAppend (x:Set<_>       , y ) = Set.union x y
    [<Extension>]static member        MAppend (x:string       , y ) = x + y
    [<Extension>]static member        MAppend (x:StringBuilder, y:StringBuilder) = StringBuilder().Append(x).Append(y)
    [<Extension>]static member        MAppend (x:'a->'a       , y ) = x << y

    static member inline Invoke (x:'T) (y:'T) :'T =
        let inline call_3 (m:^M, a:^t, b:^t) = ((^M or ^t) : (static member MAppend: _*_ -> _) a, b)
        call_3 (Unchecked.defaultof<MAppend>, x, y)

type MAppend with
    [<Extension>]static member inline MAppend (x:option<_>,y ) =
                    match (x,y) with
                    | (Some a , Some b) -> Some (MAppend.Invoke a b)
                    | (Some a , None  ) -> Some a
                    | (None   , Some b) -> Some b
                    | _                 -> None


type MAppend with 
    [<Extension>]static member inline MAppend ((x1,x2         ), (y1,y2         )) = (MAppend.Invoke x1 y1, MAppend.Invoke x2 y2                                                                  ) :'a*'b
type MAppend with 
    [<Extension>]static member inline MAppend ((x1,x2,x3      ), (y1,y2,y3      )) = (MAppend.Invoke x1 y1, MAppend.Invoke x2 y2, MAppend.Invoke x3 y3                                            ) :'a*'b*'c
type MAppend with 
    [<Extension>]static member inline MAppend ((x1,x2,x3,x4   ), (y1,y2,y3,y4   )) = (MAppend.Invoke x1 y1, MAppend.Invoke x2 y2, MAppend.Invoke x3 y3, MAppend.Invoke x4 y4                      ) :'a*'b*'c*'d
type MAppend with 
    [<Extension>]static member inline MAppend ((x1,x2,x3,x4,x5), (y1,y2,y3,y4,y5)) = (MAppend.Invoke x1 y1, MAppend.Invoke x2 y2, MAppend.Invoke x3 y3, MAppend.Invoke x4 y4, MAppend.Invoke x5 y5) :'a*'b*'c*'d*'e
    
type MAppend with    
    
#if NOTNET35
    [<Extension>]static member inline MAppend (x:'a Task, y:'a Task) =
                    x.ContinueWith(fun (t: Task<_>) -> 
                        (fun a -> 
                            y.ContinueWith(fun (u: Task<_>) -> 
                                MAppend.Invoke a u.Result)) t.Result).Unwrap()
#endif

    [<Extension>]static member inline MAppend (x:Map<'a,'b>, y) =
                    Map.fold (fun m k v' -> Map.add k (match Map.tryFind k m with Some v -> MAppend.Invoke v v' | None -> v') m) x y

    [<Extension>]static member inline MAppend (x:Dictionary<'Key,'Value>, y:Dictionary<'Key,'Value>) =
                    let d = Dictionary<'Key,'Value>()
                    for KeyValue(k, v ) in x do d.[k] <- v
                    for KeyValue(k, v') in y do d.[k] <- match d.TryGetValue k with true, v -> MAppend.Invoke v v' | _ -> v'
                    d

    [<Extension>]static member inline MAppend (x:'S Async, y:'S Async) = async {
                    let! a = x
                    let! b = y
                    return MAppend.Invoke a b}

    [<Extension>]static member inline MAppend (x:'a Expr, y:'a Expr) :'a Expr =
                    let inline f (x:'a)  :'a -> 'a = MAppend.Invoke x
                    Expr.Cast<'a>(Expr.Application(Expr.Application(Expr.Value(f), x), y))
   

    [<Extension>]static member inline MAppend (x:'a Lazy      , y:'a Lazy)       = lazy MAppend.Invoke (x.Value) (y.Value)
    [<Extension>]static member        MAppend (x:_ ResizeArray, y:_ ResizeArray) = ResizeArray (Seq.append x y)
    [<Extension>]static member        MAppend (x:_ IObservable, y              ) = Observable.merge x y
    [<Extension>]static member        MAppend (x:_ seq        , y              ) = Seq.append x y


[<Extension; Sealed>]
type MConcat =
    inherit Default1
    [<Extension>]static member inline MConcat (x:seq<Dictionary<'a,'b>>, [<Optional>]output:Dictionary<'a,'b>, [<Optional>]impl:MConcat) =
                    let dct = Dictionary<'a,'b>()
                    for d in x do
                        for KeyValue(k, u) in d do
                            dct.[k] <- match dct.TryGetValue k with true, v -> MAppend.Invoke v u | _ -> u
                    dct

    [<Extension>]static member inline MConcat (x:seq<ResizeArray<'a>>, [<Optional>]output:'a ResizeArray, [<Optional>]impl:MConcat) = ResizeArray (Seq.concat x)
    [<Extension>]static member        MConcat (x:seq<list<'a>>       , [<Optional>]output:list<'a>      , [<Optional>]impl:MConcat) = List.concat   x
    [<Extension>]static member        MConcat (x:seq<array<'a>>      , [<Optional>]output:array<'a>     , [<Optional>]impl:MConcat) = Array.concat  x
    [<Extension>]static member        MConcat (x:seq<string>         , [<Optional>]output:string        , [<Optional>]impl:MConcat) = String.Concat x
    [<Extension>]static member        MConcat (x:seq<StringBuilder>  , [<Optional>]output:StringBuilder , [<Optional>]impl:MConcat) = (StringBuilder(), x) ||> Seq.fold (fun x -> x.Append)

    static member inline Invoke (x:seq<'T>) : 'T =
        let inline call_3 (a:^a, b:^b, c:^c) = ((^a or ^b or ^c) : (static member MConcat: _*_*_ -> _) b, c, a)
        let inline call (a:'a, b:'b) = call_3 (a, b, Unchecked.defaultof<'r>) :'r
        call (Unchecked.defaultof<MConcat>, x)

type MConcat with
    [<Extension>]static member inline MConcat (x:seq<'a * 'b>, [<Optional>]output:'a * 'b, [<Optional>]impl:MConcat) =
                    MConcat.Invoke (Seq.map fst x), 
                    MConcat.Invoke (Seq.map snd x)
    
type MConcat with
    [<Extension>]static member inline MConcat (x:seq<'a * 'b * 'c>, [<Optional>]output:'a * 'b * 'c, [<Optional>]impl:MConcat) =
                    MConcat.Invoke (Seq.map (fun (x,_,_) -> x) x), 
                    MConcat.Invoke (Seq.map (fun (_,x,_) -> x) x), 
                    MConcat.Invoke (Seq.map (fun (_,_,x) -> x) x)
    
type MConcat with
    [<Extension>]static member inline MConcat (x:seq<'a * 'b * 'c * 'd>, [<Optional>]output:'a * 'b * 'c * 'd, [<Optional>]impl:MConcat) =
                    MConcat.Invoke (Seq.map (fun (x,_,_,_) -> x) x), 
                    MConcat.Invoke (Seq.map (fun (_,x,_,_) -> x) x), 
                    MConcat.Invoke (Seq.map (fun (_,_,x,_) -> x) x),
                    MConcat.Invoke (Seq.map (fun (_,_,_,x) -> x) x)

type MConcat with
    [<Extension>]static member inline MConcat (x:seq< 'a>, [<Optional>]output:'a, _:Default2) = Seq.fold MAppend.Invoke (MEmpty.Invoke()) x:'a
    
type MConcat with
    [<Extension>]static member inline MConcat (x:seq< ^R>, [<Optional>]output:^R, _:Default1) = ((^R) : (static member MConcat: 'R seq -> ^R) x)
                 static member inline MConcat (x:seq< ^R>, _:^t when ^t: null and ^t: struct, _:Default1) = fun () -> id