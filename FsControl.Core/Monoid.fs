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
        
    static member        Mempty (_:Mempty, _:list<'a>     ) = []   :  list<'a>
    static member        Mempty (_:Mempty, _:option<'a>   ) = None :option<'a>
    static member        Mempty (_:Mempty, _:array<'a>    ) = [||] : array<'a>
    static member        Mempty (_:Mempty, _:string       ) = ""
    static member        Mempty (_:Mempty, _:StringBuilder) = new StringBuilder()
    static member        Mempty (_:Mempty, _:unit         ) = ()
    static member        Mempty (_:Mempty, _:Set<'a>      ) = Set.empty : Set<'a>
    static member        Mempty (_:Mempty, _:Map<'a,'b>   ) = Map.empty : Map<'a,'b>
    static member        Mempty (_:Mempty, _:'a->'a       ) = id :'a->'a 

    static member inline internal Invoke() = 
        let inline call_2 (a:^a, b:^b) = ((^a or ^b) : (static member Mempty: _*_ -> _) a, b)
        let inline call (a:'a) = call_2 (a, Unchecked.defaultof<'r>) :'r
        call Mempty.Instance

type Mempty with static member inline Mempty (_:Mempty, _ : 'a*'b         ) = (Mempty.Invoke(), Mempty.Invoke()                                                   ): 'a*'b
type Mempty with static member inline Mempty (_:Mempty, _ : 'a*'b*'c      ) = (Mempty.Invoke(), Mempty.Invoke(), Mempty.Invoke()                                  ): 'a*'b*'c
type Mempty with static member inline Mempty (_:Mempty, _ : 'a*'b*'c*'d   ) = (Mempty.Invoke(), Mempty.Invoke(), Mempty.Invoke(), Mempty.Invoke()                 ): 'a*'b*'c*'d
type Mempty with static member inline Mempty (_:Mempty, _ : 'a*'b*'c*'d*'e) = (Mempty.Invoke(), Mempty.Invoke(), Mempty.Invoke(), Mempty.Invoke(), Mempty.Invoke()): 'a*'b*'c*'d*'e

type Mempty with
    static member inline Mempty (_:Default1, _:'R) = ((^R) : (static member Mempty: unit -> ^R) ()):'R

#if NOTNET35        

    static member inline Mempty (_:Mempty, _:Task<'a>       ) =
        let (v:'a) = Mempty.Invoke()
        let s = TaskCompletionSource()
        s.SetResult v
        s.Task
#endif

    static member inline Mempty (_:Mempty, _:Async<'a>        ) = let (v:'a) = Mempty.Invoke() in async.Return v
    static member inline Mempty (_:Mempty, _:Expr<'a>         ) = let (v:'a) = Mempty.Invoke() in Expr.Cast<'a>(Expr.Value(v))
    static member inline Mempty (_:Mempty, _:Lazy<'a>         ) = let (v:'a) = Mempty.Invoke() in lazy v
    static member        Mempty (_:Mempty, _:Dictionary<'a,'b>) = Dictionary<'a,'b>()
    static member        Mempty (_:Mempty, _:ResizeArray<'a>  ) = ResizeArray() : ResizeArray<'a>
    static member        Mempty (_:Mempty, _:seq<'a>          ) = Seq.empty   :  seq<'a>



type Mappend() =
    inherit Default1()
    static member val Instance = Mappend()
           
    static member        Mappend (_:Mappend, x:list<_>      , _) = fun y -> x @ y       
    static member        Mappend (_:Mappend, x:array<_>     , _) = fun y -> Array.append x y
    static member        Mappend (_:Mappend, ()             , _) = fun () -> ()
    static member        Mappend (_:Mappend, x:Set<_>       , _) = fun y -> Set.union x y
    static member        Mappend (_:Mappend, x:string       , _) = fun y -> x + y
    static member        Mappend (_:Mappend, x:StringBuilder, _) = fun (y:StringBuilder) -> 
        let sb = new StringBuilder()
        sb.Append(x.ToString()) |> ignore
        sb.Append(y.ToString()) |> ignore
        y
    static member        Mappend (_:Mappend, x:'a->'a       , _) = fun y -> x << y

    static member inline internal Invoke (x:'T) (y:'T) :'T =
        let inline call_3 (a:^a, b:^b, c:^c) = ((^a or ^b or ^c) : (static member Mappend: _*_*_ -> _) a, b, c)
        let inline call (a:'a, b:'b) = fun (x:'x) -> call_3 (a, b, Unchecked.defaultof<'r>) x :'r
        call (Mappend.Instance, x) y

type Mappend with
    static member inline Mappend (_:Mappend, x:option<_> , _) = fun y ->
        match (x,y) with
        | (Some a , Some b) -> Some (Mappend.Invoke a b)
        | (Some a , None  ) -> Some a
        | (None   , Some b) -> Some b
        | _                 -> None


type Mappend with static member inline Mappend (_:Mappend, (x1,x2         ), _) = fun (y1,y2         ) -> (Mappend.Invoke x1 y1, Mappend.Invoke x2 y2                                                                  ) :'a*'b
type Mappend with static member inline Mappend (_:Mappend, (x1,x2,x3      ), _) = fun (y1,y2,y3      ) -> (Mappend.Invoke x1 y1, Mappend.Invoke x2 y2, Mappend.Invoke x3 y3                                            ) :'a*'b*'c
type Mappend with static member inline Mappend (_:Mappend, (x1,x2,x3,x4   ), _) = fun (y1,y2,y3,y4   ) -> (Mappend.Invoke x1 y1, Mappend.Invoke x2 y2, Mappend.Invoke x3 y3, Mappend.Invoke x4 y4                      ) :'a*'b*'c*'d
type Mappend with static member inline Mappend (_:Mappend, (x1,x2,x3,x4,x5), _) = fun (y1,y2,y3,y4,y5) -> (Mappend.Invoke x1 y1, Mappend.Invoke x2 y2, Mappend.Invoke x3 y3, Mappend.Invoke x4 y4, Mappend.Invoke x5 y5) :'a*'b*'c*'d*'e

type Mappend with

#if NOTNET35
    static member inline Mappend (_:Mappend, x:'a Task, _) = fun (y:'a Task) ->
        x.ContinueWith(fun (t: Task<_>) -> 
            (fun a -> 
                y.ContinueWith(fun (u: Task<_>) -> 
                    Mappend.Invoke a u.Result)) t.Result).Unwrap()
#endif

    static member inline Mappend (_:Mappend, x:Map<'a,'b>, _) = fun y ->
        Map.fold (fun m k v' -> Map.add k (match Map.tryFind k m with Some v -> Mappend.Invoke v v' | None -> v') m) x y

    static member inline Mappend (_:Mappend, x:Dictionary<'a,'b>, _) = fun (y:Dictionary<'a,'b>) ->
        let d = Dictionary<'a,'b>()
        for KeyValue(k, v ) in x do d.[k] <- v
        for KeyValue(k, v') in y do d.[k] <- match d.TryGetValue(k) with true, v -> Mappend.Invoke v v' | _ -> v'
        d

    static member inline Mappend (_:Mappend, x:'a Async     , _) = fun (y:'a Async) -> async {
        let! a = x
        let! b = y
        return Mappend.Invoke a b}

    static member inline Mappend (_:Mappend, x:'a Expr      , _) = fun (y:'a Expr)  -> 
        let (f:'a->'a->'a) = Mappend.Invoke
        Expr.Cast<'a>(Expr.Application(Expr.Application(Expr.Value(f), x), y))

    static member inline Mappend (_:Mappend, x:'a Lazy      , _) = fun (y:'a Lazy)       -> lazy Mappend.Invoke (x.Value) (y.Value)
    static member        Mappend (_:Mappend, x:_ ResizeArray, _) = fun (y:_ ResizeArray) -> ResizeArray (Seq.append x y)
    static member        Mappend (_:Mappend, x:_ IObservable, _) = fun  y                -> Observable.merge x y
    static member        Mappend (_:Mappend, x:_ seq        , _) = fun  y                -> Seq.append x y

type Mappend with
    static member inline Mappend (_:Default1, x, r:^T) = fun y -> ((^T) : (static member Mappend: 'T->'T->'T) (x, y))

type Mconcat() =
    inherit Default1()
    static member val Instance = Mconcat()

    static member inline Mconcat (_:Mconcat ,x:list<Dictionary<'a,'b>>,  _:Dictionary<'a,'b>) =
        let dct = Dictionary<'a,'b>()
        for d in x do
            for KeyValue(k, u) in d do
                dct.[k] <- match dct.TryGetValue k with true, v -> Mappend.Invoke v u | _ -> u
        dct

    static member inline Mconcat (_:Mconcat ,x:list<ResizeArray<'a>>, _:'a ResizeArray) = ResizeArray(Seq.concat x)
    static member        Mconcat (_:Mconcat ,x:list<list<'a>>       , _:list<'a>      ) = List.concat x
    static member        Mconcat (_:Mconcat ,x:list<array<'a>>      , _:array<'a>     ) = Array.concat x
    static member        Mconcat (_:Mconcat ,x:list<string>         , _:string        ) = String.Concat x
    static member        Mconcat (_:Mconcat ,x:list<StringBuilder>  , _:StringBuilder ) =
        let sb = new StringBuilder()
        List.iter (fun s -> sb.Append(s.ToString()) |> ignore) x
        sb

    static member inline internal Invoke (x:list<'T>) : 'T =
        let inline call_3 (a:^a, b:^b, c:^c) = ((^a or ^b or ^c) : (static member Mconcat: _*_*_ -> _) a, b, c)
        let inline call (a:'a, b:'b) = call_3 (a, b, Unchecked.defaultof<'r>) :'r
        call (Mconcat.Instance, x)

type Mconcat with
    static member inline Mconcat (_:Mconcat , x:list<'a * 'b>, _:'a * 'b   ) =
        Mconcat.Invoke (List.map fst x), 
        Mconcat.Invoke (List.map snd x)
    
type Mconcat with
    static member inline Mconcat (_:Mconcat ,x:list<'a * 'b * 'c>, _:'a * 'b * 'c) =
        Mconcat.Invoke (List.map (fun (x,_,_) -> x) x), 
        Mconcat.Invoke (List.map (fun (_,x,_) -> x) x), 
        Mconcat.Invoke (List.map (fun (_,_,x) -> x) x)
    
type Mconcat with
    static member inline Mconcat (_:Mconcat ,x:list<'a * 'b * 'c * 'd>, _:'a * 'b * 'c * 'd) =
        Mconcat.Invoke (List.map (fun (x,_,_,_) -> x) x), 
        Mconcat.Invoke (List.map (fun (_,x,_,_) -> x) x), 
        Mconcat.Invoke (List.map (fun (_,_,x,_) -> x) x),
        Mconcat.Invoke (List.map (fun (_,_,_,x) -> x) x)

type Mconcat with
    static member inline Mconcat (_:Default2, x:list< 'a>, _:'a) =
        List.foldBack Mappend.Invoke x (Mempty.Invoke()) :'a
    
type Mconcat with
    static member inline Mconcat (_:Default1, x:list< ^R>, r:^R) = ((^R) : (static member Mconcat: 'R list -> ^R) x)
    static member inline Mconcat (_:Default1, x:list< ^R>, _:^t when ^t: null and ^t: struct) = fun () -> id


namespace FsControl.Core.Types
open FsControl.Core.Prelude
open FsControl.Core.TypeMethods

type Dual<'a> = Dual of 'a with
    static member inline Mempty  (_:Mempty , _:Dual<'m>   ) = Dual (Mempty.Invoke()) :Dual<'m>
    static member inline Mappend (_:Mappend,   Dual x  , _) = fun (Dual y) -> Dual (Mappend.Invoke y x)
module Dual = let inline  internal getDual (Dual x) = x

type Endo<'a> = Endo of ('a -> 'a) with
    static member        Mempty  (_:Mempty , _:Endo<'m>   ) = Endo id  :Endo<'m>
    static member        Mappend (_:Mappend,   Endo f  , _) = fun (Endo g) -> Endo (f << g)
module Endo = let inline  internal appEndo (Endo f) = f


type All = All of bool with
    static member Mempty  (_:Mempty, _:All     ) = All true
    static member Mappend (_:Mappend,  All x, _) = fun (All y) -> All (x && y)

type Any = Any of bool with
    static member Mempty  (_:Mempty, _:Any     ) = Any false
    static member Mappend (_:Mappend,  Any x, _) = fun (Any y) -> Any (x || y)