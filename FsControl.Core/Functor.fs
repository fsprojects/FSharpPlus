namespace FsControl.Core.TypeMethods

open System
open System.Text
#if NOTNET35
open System.Threading.Tasks
#endif
open Microsoft.FSharp.Quotations
open FsControl.Core
open FsControl.Core.Prelude
open FsControl.Core.Types
open Monoid

type internal keyValue<'a,'b> = System.Collections.Generic.KeyValuePair<'a,'b>

// Monad class ------------------------------------------------------------
module Monad =
    type Bind = Bind with
        static member        instance (Bind, x:Lazy<'a>     , _:Lazy<'b>     ) = fun (f:_->Lazy<'b>  ) -> f x.Value
        static member        instance (Bind, x:seq<_>       , _:seq<'b>      ) = fun (f:_->seq<'b>   ) -> Seq.collect   f x
        static member        instance (Bind, x:Id<'a>       , _:'b Id        ) = fun (f:_->Id<'b>    ) -> Id(f x)

#if NOTNET35
        static member        instance (Bind, x:Task<'a>     , _:'b Task      ) = fun (f:_->Task<'b>  ) -> x.ContinueWith(fun (x: Task<_>) -> f x.Result).Unwrap()
#endif

        static member        instance (Bind, x:option<_>    , _:option<'b>   ) = fun (f:_->option<'b>) -> Option.bind   f x
        static member        instance (Bind, x:List<_>      , _:List<'b>     ) = fun (f:_->List<'b>  ) -> List.collect  f x
        static member        instance (Bind, x:_ []         , _:'b []        ) = fun (f:_->'b []     ) -> Array.collect f x
        static member        instance (Bind, f:'r->'a       , _:'r->'b       ) = fun (k:_->_->'b) r    -> k (f r) r
        static member inline instance (Bind, (w, a):'m * 'a , _:'m * 'b      ) = fun (k:_->'m * 'b   ) -> let m, b = k a in (mappend w m, b)
        static member        instance (Bind, x:Async<'a>    , _:'b Async     ) = fun (f:_->Async<'b> ) -> async.Bind(x,f)
        static member        instance (Bind, x:Choice<'a,'e>, _:Choice<'b,'e>) = fun (k:'a->Choice<'b,'e>) -> Error.bind k x
        static member        instance (Bind, x:Map<'k,'a>   , _:Map<'k,'b>   ) = fun (f:'a->Map<'k,'b>) -> Map (seq {
            for KeyValue(k, v) in x do
                match Map.tryFind k (f v) with
                | Some v -> yield k, v
                | _      -> () })

        //Restricted Monad
        static member instance (Bind, x:Nullable<_> , _:'b Nullable) = fun f -> if x.HasValue then f x.Value else Nullable() : Nullable<'b>

    let inline internal (>>=) x (f:_->'R) : 'R = Inline.instance (Bind, x) f


    type JoinDefault() =
        static member inline instance (_:JoinDefault, x:#obj, _:#obj) = fun () -> x >>= id :#obj

    type Join() =
        inherit JoinDefault()

        static member        instance (_:Join, x:Lazy<Lazy<'a>>    , _:Lazy<'a>  ) = fun () -> x.Value
        static member        instance (_:Join, x:option<option<'a>>, _:option<'a>) = fun () -> Option.bind   id x
        static member        instance (_:Join, x:List<_>           , _:List<'b>  ) = fun () -> List.collect  id x
        static member        instance (_:Join, x:'b [] []          , _:'b []     ) = fun () -> Array.collect id x
        static member        instance (_:Join, x:Id<Id<'a>>        , _:Id<'a>    ) = fun () -> x.getValue

    let Join = Join()

    let inline internal join (x:'Monad'Monad'a) : 'Monad'a = Inline.instance (Join, x) ()



open Monad

module Applicative =
    type Pure = Pure with
        static member        instance (Pure, _:Lazy<'a>      ) = fun x -> Lazy.CreateFromValue x : Lazy<'a>
        static member        instance (Pure, _:seq<'a>       ) = fun x -> Seq.singleton x :seq<'a>
        static member        instance (Pure, _:Id<'a>        ) = fun x -> Id x :Id<'a>

#if NOTNET35        
        static member        instance (Pure, _:'a Task       ) = fun x -> 
            let s = TaskCompletionSource()
            s.SetResult x
            s.Task
#endif        
        static member        instance (Pure, _:option<'a>    ) = fun x -> Some x      :option<'a>
        static member        instance (Pure, _:List<'a>      ) = fun x -> [ x ]       :List<'a>
        static member        instance (Pure, _:'a []         ) = fun x -> [|x|]       :'a []
        static member        instance (Pure, _:'r -> 'a      ) = const':'a  -> 'r -> _
        static member inline instance (Pure, _: 'm * 'a      ) = fun (x:'a) -> (mempty(), x)
        static member        instance (Pure, _:'a Async      ) = fun (x:'a) -> async.Return x
        static member        instance (Pure, _:Choice<'a,'e> ) = fun x -> Choice1Of2 x :Choice<'a,'e>
        static member        instance (Pure, _:Expr<'a>      ) = fun x -> Expr.Cast<'a>(Expr.Value(x))
        static member        instance (Pure, _:'a ResizeArray) = fun x -> ResizeArray<'a>(Seq.singleton x)

        //Restricted
        static member instance (Pure, _:'a Nullable  ) = fun (x:'a  ) -> Nullable x:'a Nullable
        static member instance (Pure, _:string       ) = fun (x:char) -> string x : string
        static member instance (Pure, _:StringBuilder) = fun (x:char) -> new StringBuilder(string x):StringBuilder
        static member instance (Pure, _:'a Set       ) = fun (x:'a  ) -> Set.singleton x
        

    let inline internal pure' x   = Inline.instance Pure x


    type DefaultImpl =        
        static member inline ApplyFromMonad f x = f >>= fun x1 -> x >>= fun x2 -> pure'(x1 x2)

    type ApplyDefault() =
        static member inline instance (_:ApplyDefault, f:#obj , x, _:#obj) = fun () -> (f >>= fun x1 -> x >>= fun x2 -> pure'(x1 x2)) 

    type Apply() =
        inherit ApplyDefault()
        static member        instance (_:Apply, f:Lazy<'a->'b>, x:Lazy<'a>     , _:Lazy<'b>     ) = fun () -> Lazy.Create (fun () -> f.Value x.Value) : Lazy<'b>
        static member        instance (_:Apply, f:seq<_>      , x:seq<'a>      , _:seq<'b>      ) = fun () -> DefaultImpl.ApplyFromMonad f x :seq<'b>
        static member        instance (_:Apply, f:List<_>     , x:List<'a>     , _:List<'b>     ) = fun () -> DefaultImpl.ApplyFromMonad f x :List<'b>
        static member        instance (_:Apply, f:_ []        , x:'a []        , _:'b []        ) = fun () -> DefaultImpl.ApplyFromMonad f x :'b []
        static member        instance (_:Apply, f:'r -> _     , g: _ -> 'a     , _: 'r -> 'b    ) = fun () -> fun x -> f x (g x) :'b
        static member inline instance (_:Apply, (a:'m, f)     , (b:'m, x:'a)   , _:'m * 'b      ) = fun () -> (mappend a b, f x) :'m *'b
        static member        instance (_:Apply, f:Async<_>    , x:Async<'a>    , _:Async<'b>    ) = fun () -> DefaultImpl.ApplyFromMonad f x :Async<'b>

        static member        instance (_:Apply, f:option<_>   , x:option<'a>   , _:option<'b>   ) = fun () -> 
            match (f,x) with 
            | Some f, Some x -> Some (f x) 
            | _              -> None :option<'b>
        
        static member        instance (_:Apply, f:Choice<_,'e>, x:Choice<'a,'e>, _:Choice<'b,'e>) = fun () ->
            match (f,x) with
            | (Choice1Of2 a, Choice1Of2 b) -> Choice1Of2 (a b)
            | (Choice2Of2 a, _)            -> Choice2Of2 a
            | (_, Choice2Of2 b)            -> Choice2Of2 b :Choice<'b,'e>

        static member        instance (_:Apply, KeyValue(k:'k,f), KeyValue(k:'k,x:'a), _:keyValue<'k,'b>) :unit->keyValue<'k,'b> = fun () -> keyValue(k, f x)
        static member        instance (_:Apply, f:Map<'k,_>     , x:Map<'k,'a>       , _:Map<'k,'b>     ) :unit->Map<'k,'b>      = fun () -> Map (seq {
            for KeyValue(k, vf) in f do
                match Map.tryFind k x with
                | Some vx -> yield k, vf vx
                | _       -> () })

        static member        instance (_:Apply, f:Expr<'a->'b>, x:Expr<'a>, _:Expr<'b>) = fun () -> Expr.Cast<'b>(Expr.Application(f,x))

        static member        instance (_:Apply, f:('a->'b) ResizeArray, x:'a ResizeArray, _:'b ResizeArray) = fun () ->
            ResizeArray(Seq.collect (fun x1 -> Seq.collect (fun x2 -> Seq.singleton (x1 x2)) x) f) :'b ResizeArray

    let Apply = Apply()
   
    let inline internal (<*>) x y = Inline.instance (Apply, x, y) ()

open Applicative


module Alternative =
    type Empty = Empty with
        static member instance (Empty, _:option<'a>) = fun () -> None
        static member instance (Empty, _:List<'a>  ) = fun () -> [  ]
        static member instance (Empty, _:'a []     ) = fun () -> [||]

    type Append = Append with  
        static member instance (Append, x:option<_>, _) = fun y -> match x with None -> y | xs -> xs
        static member instance (Append, x:List<_>  , _) = fun y -> x @ y
        static member instance (Append, x:_ []     , _) = fun y -> Array.append x y


// Functor class ----------------------------------------------------------

module Functor =

    type DefaultImpl =        
        static member inline MapFromApplicative f x = pure' f <*> x
        static member inline MapFromMonad f x = x >>= (pure' << f)

    type MapDefault() =
        static member inline instance (_:MapDefault, x:'f when 'f :> obj, _:'r when 'r :> obj) = fun (f:'a->'b) -> pure' f <*> x :'r

    type Map() =
        inherit MapDefault()
        static member instance (_:Map, x:Lazy<_>      , _:Lazy<'b>) = fun f -> Lazy.Create (fun () -> f x.Value) : Lazy<'b>
        static member instance (_:Map, x:seq<_>       , _:seq<'b>) = fun f -> Seq.map f x :seq<'b>
        static member instance (_:Map, x:option<_>    , _) = fun f -> Option.map  f x
        static member instance (_:Map, x:List<_>      , _:List<'b>) = fun f -> List.map f x :List<'b>
        static member instance (_:Map, g:_->_         , _) = (>>) g
        static member instance (_:Map, (m,a)          , _) = fun f -> (m, f a)
        static member instance (_:Map, x:_ []         , _) = fun f -> Array.map   f x
        static member instance (_:Map, x:_ [,]        , _) = fun f -> Array2D.map f x
        static member instance (_:Map, x:_ [,,]       , _) = fun f -> Array3D.map f x
        static member instance (_:Map, x:_ [,,,]      , _) = fun f ->
            Array4D.init (x.GetLength 0) (x.GetLength 1) (x.GetLength 2) (x.GetLength 3) (fun a b c d -> f x.[a,b,c,d])
        static member instance (_:Map, x:Async<_>     , _) = fun f -> DefaultImpl.MapFromMonad f x
        static member instance (_:Map, x:Choice<_,_>  , _) = fun f -> Error.map f x
        static member instance (_:Map, KeyValue(k, x) , _) = fun (f:'b->'c) -> keyValue(k, f x)
        static member instance (_:Map, x:Map<'a,'b>   , _) = fun (f:'b->'c) -> Map.map (const' f) x : Map<'a,'c>
        static member instance (_:Map, x:Expr<'a>     , _) = fun (f:'a->'b) -> Expr.Cast<'b>(Expr.Application(Expr.Value(f),x))
        static member instance (_:Map, x:_ ResizeArray, _) = fun f -> ResizeArray(Seq.map f x) : ResizeArray<'b>
        static member instance (_:Map, x:_ IObservable, _) = fun f -> Observable.map f x

        // Restricted
        static member instance (_:Map, x:Nullable<_>  , _) = fun f -> if x.HasValue then Nullable(f x.Value) else Nullable()
        static member instance (_:Map, x:string       , _) = fun f -> String.map f x
        static member instance (_:Map, x:StringBuilder, _) = fun f -> new StringBuilder(String.map f (x.ToString()))
        static member instance (_:Map, x:Set<_>       , _) = fun f -> Set.map f x
        

    let Map = Map()

    let inline internal fmap   f x = Inline.instance (Map, x) f

   
    let inline internal sequence ms =
        let k m m' = m >>= fun (x:'a) -> m' >>= fun xs -> (pure' :list<'a> -> 'M) (List.Cons(x,xs))
        List.foldBack k ms ((pure' :list<'a> -> 'M) [])

    let inline internal mapM f as' = sequence (List.map f as')

    let inline internal liftM  f m1    = m1 >>= (pure' << f)
    let inline internal liftM2 f m1 m2 = m1 >>= fun x1 -> m2 >>= fun x2 -> pure' (f x1 x2)
    let inline internal ap     x y     = liftM2 id x y

    let inline internal (>=>)  f g x   = f x >>= g

    // Do notation ------------------------------------------------------------

    type DoNotationBuilder() =
        member inline b.Return(x)    = pure' x
        member inline b.Bind(p,rest) = p >>= rest
        member        b.Let (p,rest) = rest p
        member    b.ReturnFrom(expr) = expr
    let inline  internal do'() = new DoNotationBuilder()

    let inline internal return' x = Inline.instance Pure x


open Functor

module Comonad =

    type Extract = Extract with
        static member        instance (Extract, x:'t Lazy,_:'t) = fun () -> x.Value
        static member        instance (Extract, (w:'w,a:'a) ,_) = fun () -> a
        static member inline instance (Extract, f:'m->'t ,_:'t) = fun () -> f (mempty())
        static member        instance (Extract, f:'t Id  ,_:'t) = fun () -> f

#if NOTNET35
        static member        instance (Extract, f:'t Task,_:'t) = fun () -> f.Result
#endif

        // Restricted        
        static member        instance (Extract, x:'t list         , _:'t) = fun () -> List.head x
        static member        instance (Extract, x:'t []           , _:'t) = fun () -> x.[0]
        static member        instance (Extract, x:string          , _   ) = fun () -> x.[0]
        static member        instance (Extract, x:StringBuilder   , _   ) = fun () -> x.ToString().[0]
        static member        instance (Extract, x:'t seq          , _:'t) = fun () -> Seq.head x

    let inline internal extract x = Inline.instance (Extract, x) ()


    type Extend = Extend with
        static member        instance (Extend, (g:'a Lazy), _:'b Lazy) = fun (f:Lazy<'a>->'b) -> Lazy.Create (fun () -> f g) : Lazy<'b>
        static member        instance (Extend, (w:'w, a:'a), _:'w *'b) = fun (f:_->'b) -> (w, f (w,a))        
        static member inline instance (Extend, (g:'m -> 'a), _:'m->'b) = fun (f:_->'b) a -> f (fun b -> g (mappend a b))
        static member        instance (Extend, (g:'a Id   ), _:'b Id ) = fun (f:Id<'a>->'b) -> f g

#if NOTNET35
        static member        instance (Extend, (g:'a Task), _:'b Task) = fun (f:Task<'a>->'b) -> g.ContinueWith(f)
#endif

        // Restricted
        static member        instance (Extend, s:List<'a>, _:List<'b>) = fun g -> 
            let rec tails = function [] -> [] | x::xs as s -> s::(tails xs)
            List.map g (tails s) :List<'b>

        static member        instance (Extend, s:'a [], _:'b []) = fun g -> 
            let rec tails = function [] -> [] | x::xs as s -> s::(tails xs)
            Array.map g (s |> Array.toList |> tails |> List.toArray |> Array.map List.toArray) :'b []

        static member        instance (Extend, s:'a seq, _:'b seq) = fun g -> 
            let rec tails = function [] -> [] | x::xs as s -> s::(tails xs)
            Seq.map g (s |> Seq.toList |> tails |> List.toSeq |> Seq.map List.toSeq) :'b seq

    let inline internal extend g s = Inline.instance (Extend, s) g
    let inline internal (=>>)  s g = extend g s

    type DuplicateDefault() =
        static member inline instance (_:DuplicateDefault, x:#obj, _:#obj) = fun () -> extend id x :#obj

    type Duplicate() =
        inherit DuplicateDefault()
        static member        instance (_:Duplicate, s:Lazy<'a>, _:Lazy<Lazy<'a>>) = fun () -> Lazy.CreateFromValue s : Lazy<Lazy<'a>>
        static member        instance (_:Duplicate, (w:'w, a:'a), _:'w * ('w*'a)) = fun ()     -> (w, (w, a))
        static member inline instance (_:Duplicate,  f:'m -> 'a , _:'m->'m->'a  ) = fun () a b -> f (mappend a b)

        // Restricted
        static member        instance (_:Duplicate, s:List<'a>, _:List<List<'a>>) = fun () -> 
            let rec tails = function [] -> [] | x::xs as s -> s::(tails xs)
            tails s

        static member        instance (_:Duplicate, s: array<'a>, _: array<array<'a>>) = fun () -> 
            let rec tails = function [] -> [] | x::xs as s -> s::(tails xs)
            s |> Array.toList |> tails |> List.toArray |> Array.map List.toArray

    let Duplicate = Duplicate()
    
    let inline internal duplicate x = Inline.instance (Duplicate, x) ()


// MonadPlus class ------------------------------------------------------------

module MonadPlus =
    type Mzero = Mzero with
        static member        instance (Mzero, _:option<'a>) = fun () -> None        :option<'a>
        static member        instance (Mzero, _:List<'a>  ) = fun () -> [  ]        :List<'a>  
        static member        instance (Mzero, _:'a []     ) = fun () -> [||]        :'a []     
        static member        instance (Mzero, _:seq<'a>   ) = fun () -> Seq.empty   :seq<'a>
        static member inline instance (Mzero, _:Id<'a>    ) = fun () -> Id (mempty()) :Id<'a>

    type Mplus = Mplus with
        static member        instance (Mplus, x:_ option, _) = fun y -> match x with None -> y | xs -> xs
        static member        instance (Mplus, x:_ list  , _) = fun y -> x @ y
        static member        instance (Mplus, x:_ []    , _) = fun y -> Array.append x y
        static member        instance (Mplus, x:_ seq   , _) = fun y -> Seq.append   x y
        static member inline instance (Mplus, x:_ Id    , _) = fun y -> Id (mappend (Id.run x) (Id.run y))
        

    let inline internal mzero () = Inline.instance Mzero ()
    let inline internal mplus (x:'a) (y:'a) : 'a = Inline.instance (Mplus, x) y
