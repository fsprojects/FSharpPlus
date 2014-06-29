namespace FsControl.Core.TypeMethods

open System
open System.Text
open FsControl.Core.Prelude
open Microsoft.FSharp.Quotations
#if NOTNET35
open System.Threading.Tasks
#endif

module Monoid =
    type Mempty = Mempty with           
        static member        instance (Mempty, _:list<'a>  ) = fun () -> []   :  list<'a>
        static member        instance (Mempty, _:option<'a>) = fun () -> None :option<'a>
        static member        instance (Mempty, _:array<'a> ) = fun () -> [||] : array<'a>
        static member        instance (Mempty, _:string    ) = fun () -> ""
        static member        instance (Mempty, _:StringBuilder) = fun () -> new StringBuilder()
        static member        instance (Mempty, _:unit      ) = fun () -> ()
        static member        instance (Mempty, _:Set<'a>   ) = fun () -> Set.empty : Set<'a>
        static member        instance (Mempty, _:Map<'a,'b>) = fun () -> Map.empty : Map<'a,'b>

    let inline internal mempty() = Inline.instance Mempty ()

    type Mempty with static member inline instance (Mempty, _ : 'a*'b         ) = fun () ->
                        (mempty(),mempty()                           ): 'a*'b
    type Mempty with static member inline instance (Mempty, _ : 'a*'b*'c      ) = fun () ->
                        (mempty(),mempty(),mempty()                  ): 'a*'b*'c
    type Mempty with static member inline instance (Mempty, _ : 'a*'b*'c*'d   ) = fun () ->
                        (mempty(),mempty(),mempty(),mempty()         ): 'a*'b*'c*'d
    type Mempty with static member inline instance (Mempty, _ : 'a*'b*'c*'d*'e) = fun () ->
                        (mempty(),mempty(),mempty(),mempty(),mempty()): 'a*'b*'c*'d*'e

    type Mempty with

#if NOTNET35        

        static member inline instance (Mempty, _:Task<'a>       ) = fun () -> 
            let (v:'a) = mempty()
            let s = TaskCompletionSource()
            s.SetResult v
            s.Task
#endif
        static member inline instance (Mempty, _:Async<'a>      ) = fun () -> let (v:'a) = mempty() in async.Return v
        static member inline instance (Mempty, _:Expr<'a>       ) = fun () -> let (v:'a) = mempty() in Expr.Cast<'a>(Expr.Value(v))
        static member inline instance (Mempty, _:Lazy<'a>       ) = fun () -> let (v:'a) = mempty() in lazy v
        static member        instance (Mempty, _:ResizeArray<'a>) = fun () -> ResizeArray() : ResizeArray<'a>
        static member        instance (Mempty, _:seq<'a>        ) = fun () -> Seq.empty   :  seq<'a>



    type Mappend = Mappend with       
        static member        instance (Mappend, x:list<_>      , _) = fun y -> x @ y       
        static member        instance (Mappend, x:array<_>     , _) = fun y -> Array.append x y
        static member        instance (Mappend, ()             , _) = fun () -> ()
        static member        instance (Mappend, x:Set<_>       , _) = fun y -> Set.union x y
        static member        instance (Mappend, x:string       , _) = fun y -> x + y
        static member        instance (Mappend, x:StringBuilder, _) = fun (y:StringBuilder) -> 
            let sb = new StringBuilder()
            sb.Append(x.ToString()) |> ignore
            sb.Append(y.ToString()) |> ignore
            y

    let inline internal mappend (x:'a) (y:'a) :'a = Inline.instance (Mappend, x) y

    type Mappend with
        static member inline instance (Mappend, x:option<_> , _) = fun y ->
            match (x,y) with
            | (Some a , Some b) -> Some (mappend a b)
            | (Some a , None  ) -> Some a
            | (None   , Some b) -> Some b
            | _                 -> None


    type Mappend with static member inline instance (Mappend, (x1,x2         ), _) = fun (y1,y2         ) ->
                        (mappend x1 y1,mappend x2 y2                                          ) :'a*'b
    type Mappend with static member inline instance (Mappend, (x1,x2,x3      ), _) = fun (y1,y2,y3      ) ->
                        (mappend x1 y1,mappend x2 y2,mappend x3 y3                            ) :'a*'b*'c
    type Mappend with static member inline instance (Mappend, (x1,x2,x3,x4   ), _) = fun (y1,y2,y3,y4   ) ->
                        (mappend x1 y1,mappend x2 y2,mappend x3 y3,mappend x4 y4              ) :'a*'b*'c*'d
    type Mappend with static member inline instance (Mappend, (x1,x2,x3,x4,x5), _) = fun (y1,y2,y3,y4,y5) ->
                        (mappend x1 y1,mappend x2 y2,mappend x3 y3,mappend x4 y4,mappend x5 y5) :'a*'b*'c*'d*'e

    type Mappend with

#if NOTNET35
        static member inline instance (Mappend, x:'a Task     , _) = fun (y:'a Task) ->
            x.ContinueWith(fun (t: Task<_>) -> 
                (fun a -> 
                    y.ContinueWith(fun (u: Task<_>) -> 
                        mappend a u.Result)) t.Result).Unwrap()
#endif

        static member inline instance (Mappend, x:Map<'a,'b>   , _) = fun y ->
            Map.fold (fun m k v' -> Map.add k (match Map.tryFind k m with Some v -> mappend v v' | None -> v') m) x y

        static member inline instance (Mappend, x:'a Async     , _) = fun (y:'a Async) -> async {
            let! a = x
            let! b = y
            return mappend a b}

        static member inline instance (Mappend, x:'a Expr      , _) = fun (y:'a Expr)  -> 
            let (f:'a->'a->'a) = mappend
            Expr.Cast<'a>(Expr.Application(Expr.Application(Expr.Value(f), x), y))
        static member inline instance (Mappend, x:'a Lazy      , _) = fun (y:'a Lazy)       -> lazy mappend (x.Value) (y.Value)
        static member        instance (Mappend, x:_ ResizeArray, _) = fun (y:_ ResizeArray) -> ResizeArray (Seq.append x y)
        static member        instance (Mappend, x:_ IObservable, _) = fun  y                -> Observable.merge x y
        static member        instance (Mappend, x:_ seq        , _) = fun  y                -> Seq.append x y


namespace FsControl.Core.Types
open FsControl.Core.Prelude
open FsControl.Core.TypeMethods
open FsControl.Core.TypeMethods.Monoid


type Dual<'a> = Dual of 'a with
    static member inline instance (Monoid.Mempty , _:Dual<'m>   ) = fun () -> Dual (mempty()) :Dual<'m>
    static member inline instance (Monoid.Mappend,   Dual x  , _) = fun (Dual y) -> Dual (mappend y x)
module Dual = let inline  internal getDual (Dual x) = x

type Endo<'a> = Endo of ('a -> 'a) with
    static member        instance (Monoid.Mempty , _:Endo<'m>   ) = fun () -> Endo id  :Endo<'m>
    static member        instance (Monoid.Mappend,   Endo f  , _) = fun (Endo g) -> Endo (f << g)
module Endo = let inline  internal appEndo (Endo f) = f


type All = All of bool with
    static member instance (Monoid.Mempty, _:All     ) = fun () -> All true
    static member instance (Monoid.Mappend,  All x, _) = fun (All y) -> All (x && y)

type Any = Any of bool with
    static member instance (Monoid.Mempty, _:Any     ) = fun () -> Any false
    static member instance (Monoid.Mappend,  Any x, _) = fun (Any y) -> Any (x || y)