namespace FsControl.Core.TypeMethods

open System
open System.Text
open System.Collections.Generic
open FsControl.Core.Prelude
open Microsoft.FSharp.Quotations
#if NOTNET35
open System.Threading.Tasks
#endif

module Monoid =

    type Mempty() =
        inherit Typ1()
        
        static member        instance (_:Mempty, _:list<'a>  ) = fun () -> []   :  list<'a>
        static member        instance (_:Mempty, _:option<'a>) = fun () -> None :option<'a>
        static member        instance (_:Mempty, _:array<'a> ) = fun () -> [||] : array<'a>
        static member        instance (_:Mempty, _:string    ) = fun () -> ""
        static member        instance (_:Mempty, _:StringBuilder) = fun () -> new StringBuilder()
        static member        instance (_:Mempty, _:unit      ) = fun () -> ()
        static member        instance (_:Mempty, _:Set<'a>   ) = fun () -> Set.empty : Set<'a>
        static member        instance (_:Mempty, _:Map<'a,'b>) = fun () -> Map.empty : Map<'a,'b>
        static member        instance (_:Mempty, _:'a->'a    ) = fun () -> id :'a->'a 

    let Mempty = Mempty()
    let inline internal mempty() = Inline.instance Mempty ()

    type Mempty with static member inline instance (_:Mempty, _ : 'a*'b         ) = fun () ->
                        (mempty(),mempty()                           ): 'a*'b
    type Mempty with static member inline instance (_:Mempty, _ : 'a*'b*'c      ) = fun () ->
                        (mempty(),mempty(),mempty()                  ): 'a*'b*'c
    type Mempty with static member inline instance (_:Mempty, _ : 'a*'b*'c*'d   ) = fun () ->
                        (mempty(),mempty(),mempty(),mempty()         ): 'a*'b*'c*'d
    type Mempty with static member inline instance (_:Mempty, _ : 'a*'b*'c*'d*'e) = fun () ->
                        (mempty(),mempty(),mempty(),mempty(),mempty()): 'a*'b*'c*'d*'e

    type Mempty with
        static member inline instance (_:Typ1, _:'R) = fun () -> ((^R) : (static member Mempty: unit -> ^R) ()):'R

#if NOTNET35        

        static member inline instance (_:Mempty, _:Task<'a>       ) = fun () -> 
            let (v:'a) = mempty()
            let s = TaskCompletionSource()
            s.SetResult v
            s.Task
#endif

        static member inline instance (_:Mempty, _:Async<'a>        ) = fun () -> let (v:'a) = mempty() in async.Return v
        static member inline instance (_:Mempty, _:Expr<'a>         ) = fun () -> let (v:'a) = mempty() in Expr.Cast<'a>(Expr.Value(v))
        static member inline instance (_:Mempty, _:Lazy<'a>         ) = fun () -> let (v:'a) = mempty() in lazy v
        static member        instance (_:Mempty, _:Dictionary<'a,'b>) = fun () -> Dictionary<'a,'b>()
        static member        instance (_:Mempty, _:ResizeArray<'a>  ) = fun () -> ResizeArray() : ResizeArray<'a>
        static member        instance (_:Mempty, _:seq<'a>          ) = fun () -> Seq.empty   :  seq<'a>



    type Mappend() =
        inherit Typ1()
           
        static member        instance (_:Mappend, x:list<_>      , _) = fun y -> x @ y       
        static member        instance (_:Mappend, x:array<_>     , _) = fun y -> Array.append x y
        static member        instance (_:Mappend, ()             , _) = fun () -> ()
        static member        instance (_:Mappend, x:Set<_>       , _) = fun y -> Set.union x y
        static member        instance (_:Mappend, x:string       , _) = fun y -> x + y
        static member        instance (_:Mappend, x:StringBuilder, _) = fun (y:StringBuilder) -> 
            let sb = new StringBuilder()
            sb.Append(x.ToString()) |> ignore
            sb.Append(y.ToString()) |> ignore
            y
        static member        instance (_:Mappend, x:'a->'a       , _) = fun y -> x << y

    let Mappend = Mappend()
    let inline internal mappend (x:'a) (y:'a) :'a = Inline.instance (Mappend, x) y

    type Mappend with
        static member inline instance (_:Mappend, x:option<_> , _) = fun y ->
            match (x,y) with
            | (Some a , Some b) -> Some (mappend a b)
            | (Some a , None  ) -> Some a
            | (None   , Some b) -> Some b
            | _                 -> None


    type Mappend with static member inline instance (_:Mappend, (x1,x2         ), _) = fun (y1,y2         ) ->
                        (mappend x1 y1,mappend x2 y2                                          ) :'a*'b
    type Mappend with static member inline instance (_:Mappend, (x1,x2,x3      ), _) = fun (y1,y2,y3      ) ->
                        (mappend x1 y1,mappend x2 y2,mappend x3 y3                            ) :'a*'b*'c
    type Mappend with static member inline instance (_:Mappend, (x1,x2,x3,x4   ), _) = fun (y1,y2,y3,y4   ) ->
                        (mappend x1 y1,mappend x2 y2,mappend x3 y3,mappend x4 y4              ) :'a*'b*'c*'d
    type Mappend with static member inline instance (_:Mappend, (x1,x2,x3,x4,x5), _) = fun (y1,y2,y3,y4,y5) ->
                        (mappend x1 y1,mappend x2 y2,mappend x3 y3,mappend x4 y4,mappend x5 y5) :'a*'b*'c*'d*'e

    type Mappend with

#if NOTNET35
        static member inline instance (_:Mappend, x:'a Task, _) = fun (y:'a Task) ->
            x.ContinueWith(fun (t: Task<_>) -> 
                (fun a -> 
                    y.ContinueWith(fun (u: Task<_>) -> 
                        mappend a u.Result)) t.Result).Unwrap()
#endif

        static member inline instance (_:Mappend, x:Map<'a,'b>, _) = fun y ->
            Map.fold (fun m k v' -> Map.add k (match Map.tryFind k m with Some v -> mappend v v' | None -> v') m) x y

        static member inline instance (_:Mappend, x:Dictionary<'a,'b>, _) = fun (y:Dictionary<'a,'b>) ->
            let d = Dictionary<'a,'b>()
            for KeyValue(k, v ) in x do d.[k] <- v
            for KeyValue(k, v') in y do d.[k] <- match d.TryGetValue(k) with true, v -> mappend v v' | _ -> v'
            d

        static member inline instance (_:Mappend, x:'a Async     , _) = fun (y:'a Async) -> async {
            let! a = x
            let! b = y
            return mappend a b}

        static member inline instance (_:Mappend, x:'a Expr      , _) = fun (y:'a Expr)  -> 
            let (f:'a->'a->'a) = mappend
            Expr.Cast<'a>(Expr.Application(Expr.Application(Expr.Value(f), x), y))

        static member inline instance (_:Mappend, x:'a Lazy      , _) = fun (y:'a Lazy)       -> lazy mappend (x.Value) (y.Value)
        static member        instance (_:Mappend, x:_ ResizeArray, _) = fun (y:_ ResizeArray) -> ResizeArray (Seq.append x y)
        static member        instance (_:Mappend, x:_ IObservable, _) = fun  y                -> Observable.merge x y
        static member        instance (_:Mappend, x:_ seq        , _) = fun  y                -> Seq.append x y

    type Mappend with
        static member inline instance (_:Typ1, x, r:^T) = fun y -> ((^T) : (static member Mappend: 'T->'T->'T) (x, y))

    type Mconcat() =
        inherit Typ1()

        static member inline instance (_:Mconcat ,x:list<Dictionary<'a,'b>>,  _:Dictionary<'a,'b>) = fun () ->
            let r = Dictionary<'a,'b>()
            for d in x do
                for KeyValue(k, u) in d do
                    r.[k] <- match r.TryGetValue(k) with true, v -> mappend v u | _ -> u
            r

        static member inline instance (_:Mconcat ,x:list<ResizeArray<'a>>, _:'a ResizeArray) = fun () -> ResizeArray(Seq.concat x)
        static member        instance (_:Mconcat ,x:list<list<'a>>       , _:list<'a>      ) = fun () -> List.concat x
        static member        instance (_:Mconcat ,x:list<array<'a>>      , _:array<'a>     ) = fun () -> Array.concat x
        static member        instance (_:Mconcat ,x:list<string>         , _:string        ) = fun () -> String.Concat x
        static member        instance (_:Mconcat ,x:list<StringBuilder>  , _:StringBuilder ) = fun () ->
            let sb = new StringBuilder()
            List.iter (fun s -> sb.Append(s.ToString()) |> ignore) x
            sb

    let Mconcat = Mconcat()
    let inline internal mconcat (x:list<'a>) : 'a = Inline.instance (Mconcat, x) ()

    type Mconcat with
        static member inline instance (_:Mconcat , x:list<'a * 'b>, _:'a * 'b   ) = fun () ->
            mconcat (List.map fst x), 
            mconcat (List.map snd x)
    
    type Mconcat with
        static member inline instance (_:Mconcat ,x:list<'a * 'b * 'c>, _:'a * 'b * 'c) = fun () ->
            mconcat (List.map (fun (x,_,_) -> x) x), 
            mconcat (List.map (fun (_,x,_) -> x) x), 
            mconcat (List.map (fun (_,_,x) -> x) x)
    
    type Mconcat with
        static member inline instance (_:Mconcat ,x:list<'a * 'b * 'c * 'd>, _:'a * 'b * 'c * 'd) = fun () ->
            mconcat (List.map (fun (x,_,_,_) -> x) x), 
            mconcat (List.map (fun (_,x,_,_) -> x) x), 
            mconcat (List.map (fun (_,_,x,_) -> x) x),
            mconcat (List.map (fun (_,_,_,x) -> x) x)

    type Mconcat with
        static member inline instance (_:Typ2, x:list< 'a>, _:'a) = fun () -> 
            List.foldBack mappend x (mempty()) :'a
    
    type Mconcat with
        static member inline instance (_:Typ1, x:list< ^R>, r:^R) = fun () -> ((^R) : (static member Mconcat: 'R list -> ^R) x)
        static member inline instance (_:Typ1, x:list< ^R>, _:^t when ^t: null and ^t: struct) = fun () -> id

namespace FsControl.Core.Types
open FsControl.Core.Prelude
open FsControl.Core.TypeMethods
open FsControl.Core.TypeMethods.Monoid


type Dual<'a> = Dual of 'a with
    static member inline instance (_:Monoid.Mempty , _:Dual<'m>   ) = fun () -> Dual (mempty()) :Dual<'m>
    static member inline instance (_:Monoid.Mappend,   Dual x  , _) = fun (Dual y) -> Dual (mappend y x)
module Dual = let inline  internal getDual (Dual x) = x

type Endo<'a> = Endo of ('a -> 'a) with
    static member        instance (_:Monoid.Mempty , _:Endo<'m>   ) = fun () -> Endo id  :Endo<'m>
    static member        instance (_:Monoid.Mappend,   Endo f  , _) = fun (Endo g) -> Endo (f << g)
module Endo = let inline  internal appEndo (Endo f) = f


type All = All of bool with
    static member instance (_:Monoid.Mempty, _:All     ) = fun () -> All true
    static member instance (_:Monoid.Mappend,  All x, _) = fun (All y) -> All (x && y)

type Any = Any of bool with
    static member instance (_:Monoid.Mempty, _:Any     ) = fun () -> Any false
    static member instance (_:Monoid.Mappend,  Any x, _) = fun (Any y) -> Any (x || y)