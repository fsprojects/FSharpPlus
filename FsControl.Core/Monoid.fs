namespace FsControl.Core.TypeMethods

open System
open System.Text
open System.Collections.Generic
open FsControl.Core.Prelude
open Microsoft.FSharp.Quotations
#if NOTNET35
open System.Threading.Tasks
#endif


type Mempty() =
    inherit Default1()
    static member val Instance = Mempty()
        
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
        call Mempty.Instance

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


type Mappend() =

    static member val Instance = Mappend()
           
    static member        Mappend (x:list<_>      , y ) = x @ y       
    static member        Mappend (x:array<_>     , y ) = Array.append x y
    static member        Mappend (()             , ()) =  ()
    static member        Mappend (x:Set<_>       , y ) = Set.union x y
    static member        Mappend (x:string       , y ) = x + y
    static member        Mappend (x:StringBuilder, y:StringBuilder) =
        let sb = new StringBuilder()
        sb.Append(x.ToString()) |> ignore
        sb.Append(y.ToString()) |> ignore
        y
    static member        Mappend (x:'a->'a       , y ) = x << y

    static member inline Invoke (x:'T) (y:'T) :'T =
        let inline call_3 (m:^M, a:^t, b:^t) = ((^M or ^t) : (static member Mappend: _*_ -> _) a, b)
        call_3 (Mappend.Instance, x, y)

type Mappend with
    static member inline Mappend (x:option<_>,y ) =
        match (x,y) with
        | (Some a , Some b) -> Some (Mappend.Invoke a b)
        | (Some a , None  ) -> Some a
        | (None   , Some b) -> Some b
        | _                 -> None


type Mappend with 
    static member inline Mappend ((x1,x2         ), (y1,y2         )) = (Mappend.Invoke x1 y1, Mappend.Invoke x2 y2                                                                  ) :'a*'b
type Mappend with 
    static member inline Mappend ((x1,x2,x3      ), (y1,y2,y3      )) = (Mappend.Invoke x1 y1, Mappend.Invoke x2 y2, Mappend.Invoke x3 y3                                            ) :'a*'b*'c
type Mappend with 
    static member inline Mappend ((x1,x2,x3,x4   ), (y1,y2,y3,y4   )) = (Mappend.Invoke x1 y1, Mappend.Invoke x2 y2, Mappend.Invoke x3 y3, Mappend.Invoke x4 y4                      ) :'a*'b*'c*'d
type Mappend with 
    static member inline Mappend ((x1,x2,x3,x4,x5), (y1,y2,y3,y4,y5)) = (Mappend.Invoke x1 y1, Mappend.Invoke x2 y2, Mappend.Invoke x3 y3, Mappend.Invoke x4 y4, Mappend.Invoke x5 y5) :'a*'b*'c*'d*'e
    
type Mappend with    
    
#if NOTNET35
    static member inline Mappend (x:'a Task, y:'a Task) =
        x.ContinueWith(fun (t: Task<_>) -> 
            (fun a -> 
                y.ContinueWith(fun (u: Task<_>) -> 
                    Mappend.Invoke a u.Result)) t.Result).Unwrap()
#endif

    static member inline Mappend (x:Map<'a,'b>, y) =
        Map.fold (fun m k v' -> Map.add k (match Map.tryFind k m with Some v -> Mappend.Invoke v v' | None -> v') m) x y

    static member inline Mappend (x:Dictionary<'Key,'Value>, y:Dictionary<'Key,'Value>) =
        let d = Dictionary<'Key,'Value>()
        for KeyValue(k, v ) in x do d.[k] <- v
        for KeyValue(k, v') in y do d.[k] <- match d.TryGetValue k with true, v -> Mappend.Invoke v v' | _ -> v'
        d

    static member inline Mappend (x:'S Async, y:'S Async) = async {
        let! a = x
        let! b = y
        return Mappend.Invoke a b}

    static member inline Mappend (x:'a Expr, y:'a Expr) :'a Expr =
        let inline f (x:'a)  :'a -> 'a = Mappend.Invoke x
        Expr.Cast<'a>(Expr.Application(Expr.Application(Expr.Value(f), x), y))
   

    static member inline Mappend (x:'a Lazy      , y:'a Lazy)       = lazy Mappend.Invoke (x.Value) (y.Value)
    static member        Mappend (x:_ ResizeArray, y:_ ResizeArray) = ResizeArray (Seq.append x y)
    static member        Mappend (x:_ IObservable, y              ) = Observable.merge x y
    static member        Mappend (x:_ seq        , y              ) = Seq.append x y


type Mconcat() =
    inherit Default1()
    static member val Instance = Mconcat()

    static member inline Mconcat (x:list<Dictionary<'a,'b>>, _:Dictionary<'a,'b>, _:Mconcat) =
        let dct = Dictionary<'a,'b>()
        for d in x do
            for KeyValue(k, u) in d do
                dct.[k] <- match dct.TryGetValue k with true, v -> Mappend.Invoke v u | _ -> u
        dct

    static member inline Mconcat (x:list<ResizeArray<'a>>, _:'a ResizeArray, _:Mconcat) = ResizeArray(Seq.concat x)
    static member        Mconcat (x:list<list<'a>>       , _:list<'a>      , _:Mconcat) = List.concat x
    static member        Mconcat (x:list<array<'a>>      , _:array<'a>     , _:Mconcat) = Array.concat x
    static member        Mconcat (x:list<string>         , _:string        , _:Mconcat) = String.Concat x
    static member        Mconcat (x:list<StringBuilder>  , _:StringBuilder , _:Mconcat) =
        let sb = new StringBuilder()
        List.iter (fun s -> sb.Append(s.ToString()) |> ignore) x
        sb

    static member inline Invoke (x:list<'T>) : 'T =
        let inline call_3 (a:^a, b:^b, c:^c) = ((^a or ^b or ^c) : (static member Mconcat: _*_*_ -> _) b, c, a)
        let inline call (a:'a, b:'b) = call_3 (a, b, Unchecked.defaultof<'r>) :'r
        call (Mconcat.Instance, x)

type Mconcat with
    static member inline Mconcat (x:list<'a * 'b>, _:'a * 'b, _:Mconcat) =
        Mconcat.Invoke (List.map fst x), 
        Mconcat.Invoke (List.map snd x)
    
type Mconcat with
    static member inline Mconcat (x:list<'a * 'b * 'c>, _:'a * 'b * 'c, _:Mconcat) =
        Mconcat.Invoke (List.map (fun (x,_,_) -> x) x), 
        Mconcat.Invoke (List.map (fun (_,x,_) -> x) x), 
        Mconcat.Invoke (List.map (fun (_,_,x) -> x) x)
    
type Mconcat with
    static member inline Mconcat (x:list<'a * 'b * 'c * 'd>, _:'a * 'b * 'c * 'd, _:Mconcat) =
        Mconcat.Invoke (List.map (fun (x,_,_,_) -> x) x), 
        Mconcat.Invoke (List.map (fun (_,x,_,_) -> x) x), 
        Mconcat.Invoke (List.map (fun (_,_,x,_) -> x) x),
        Mconcat.Invoke (List.map (fun (_,_,_,x) -> x) x)

type Mconcat with
    static member inline Mconcat (x:list< 'a>, _:'a, _:Default2) =
        List.foldBack Mappend.Invoke x (Mempty.Invoke()) :'a
    
type Mconcat with
    static member inline Mconcat (x:list< ^R>, r:^R, _:Default1) = ((^R) : (static member Mconcat: 'R list -> ^R) x)
    static member inline Mconcat (x:list< ^R>, _:^t when ^t: null and ^t: struct, _:Default1) = fun () -> id




namespace FsControl.Core.Types
open FsControl.Core.Prelude
open FsControl.Core.TypeMethods

type Dual<'a> = Dual of 'a with
    static member inline Mempty  (_:Dual<'m>, _:Mempty) = Dual (Mempty.Invoke()) :Dual<'m>
    static member inline Mappend (  Dual x  ,   Dual y) = Dual (Mappend.Invoke y x)
module Dual = let inline  internal getDual (Dual x) = x

type Endo<'a> = Endo of ('a -> 'a) with
    static member        Mempty  (_:Endo<'m>, _:Mempty) = Endo id  :Endo<'m>
    static member        Mappend (  Endo f  ,   Endo g) = Endo (f << g)
module Endo = let inline  internal appEndo (Endo f) = f


type All = All of bool with
    static member Mempty  (_:All, _:Mempty) = All true
    static member Mappend (  All x, All y ) = All (x && y)

type Any = Any of bool with
    static member Mempty  (_:Any, _:Mempty) = Any false
    static member Mappend (  Any x, Any y ) = Any (x || y)