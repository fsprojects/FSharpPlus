namespace FsControl.Core.TypeMethods

open System
open System.Text
open System.Collections.Generic
#if NOTNET35
open System.Threading.Tasks
#endif
open Microsoft.FSharp.Quotations
open FsControl.Core
open FsControl.Core.Prelude
open FsControl.Core.Types
open Monoid

// Monad class ------------------------------------------------------------
module Monad =

    type Bind() =
        inherit Typ1()

        static member inline instance (_:Typ1, x:'M, r:'R) = fun (f:'t->'R) -> 
            ((^M or ^R) : (static member Bind: ^M -> (('t->'R) -> 'R)) x) f

        static member        instance (_:Bind, x:Lazy<'a>     , _:Lazy<'b>     ) = fun (f:_->Lazy<'b>  ) -> lazy (f x.Value).Value
        static member        instance (_:Bind, x:seq<_>       , _:seq<'b>      ) = fun (f:_->seq<'b>   ) -> Seq.bind f x
        static member        instance (_:Bind, x:Id<'a>       , _:'b Id        ) = fun (f:_->Id<'b>    ) -> f x.getValue

#if NOTNET35
        static member        instance (_:Bind, x:Task<'a>     , _:'b Task      ) = fun (f:_->Task<'b>  ) -> x.ContinueWith(fun (x: Task<_>) -> f x.Result).Unwrap()
#endif

        static member        instance (_:Bind, x:option<_>    , _:option<'b>   ) = fun (f:_->option<'b>) -> Option.bind   f x
        static member        instance (_:Bind, x:list<_>      , _:list<'b>     ) = fun (f:_->list<'b>  ) -> List.collect  f x
        static member        instance (_:Bind, x:_ []         , _:'b []        ) = fun (f:_->'b []     ) -> Array.collect f x
        static member        instance (_:Bind, f:'r->'a       , _:'r->'b       ) = fun (k:_->_->'b) r    -> k (f r) r
        static member inline instance (_:Bind, (w, a):'m * 'a , _:'m * 'b      ) = fun (k:_->'m * 'b   ) -> let m, b = k a in (mappend w m, b)
        static member        instance (_:Bind, x:Async<'a>    , _:'b Async     ) = fun (f:_->Async<'b> ) -> async.Bind(x,f)
        static member        instance (_:Bind, x:Choice<'a,'e>, _:Choice<'b,'e>) = fun (k:'a->Choice<'b,'e>) -> Error.bind k x

        static member        instance (_:Bind, x:Map<'k,'a>   , _:Map<'k,'b>   ) = fun (f:'a->Map<'k,'b>) -> Map (seq {
            for KeyValue(k, v) in x do
                match Map.tryFind k (f v) with
                | Some v -> yield k, v
                | _      -> () })

        static member        instance (_:Bind, x:Dictionary<'k,'a>   , _:Dictionary<'k,'b>   ) = fun (f:'a->Dictionary<'k,'b>) -> 
            let d = Dictionary()
            for KeyValue(k, v) in x do
                match (f v).TryGetValue(k)  with
                | true, v -> d.Add(k, v)
                | _       -> ()
            d

        //Restricted Monad
        static member instance (_:Bind, x:Nullable<_> , _:'b Nullable) = fun f -> if x.HasValue then f x.Value else Nullable() : Nullable<'b>

    let Bind = Bind()
    let inline internal (>>=) x (f:_->'R) : 'R = Inline.instance (Bind, x) f


    type Join() =
        inherit Typ1()

        static member inline instance (_:Typ1, x:#obj, _:#obj) = fun () -> x >>= id :#obj
        static member        instance (_:Join, x:Lazy<Lazy<'a>>    , _:Lazy<'a>  ) = fun () -> lazy x.Value.Value
        static member        instance (_:Join, x:option<option<'a>>, _:option<'a>) = fun () -> Option.bind   id x
        static member        instance (_:Join, x:list<_>           , _:list<'b>  ) = fun () -> List.collect  id x
        static member        instance (_:Join, x:'b [] []          , _:'b []     ) = fun () -> Array.collect id x
        static member        instance (_:Join, x:Id<Id<'a>>        , _:Id<'a>    ) = fun () -> x.getValue

#if NOTNET35        
        static member        instance (_:Join, x:Task<Task<'a>>    , _:Task<'a>  ) = fun () -> x.Unwrap()
#endif

    let Join = Join()
    let inline internal join (x:'Monad'Monad'a) : 'Monad'a = Inline.instance (Join, x) ()



open Monad

module Applicative =

    type Pure() =
        inherit Typ1()

        static member inline instance (_:Typ1, r:'R) = fun (x:'T) -> 
            ((^R) : (static member Return: ^T -> ^R) x)

        static member        instance (_:Pure, _:Lazy<'a>      ) = fun x -> Lazy.CreateFromValue x : Lazy<'a>
        static member        instance (_:Pure, _:seq<'a>       ) = fun x -> Seq.singleton x :seq<'a>
        static member        instance (_:Pure, _:Id<'a>        ) = fun x -> Id x :Id<'a>

#if NOTNET35        
        static member        instance (_:Pure, _:'a Task       ) = fun x -> 
            let s = TaskCompletionSource()
            s.SetResult x
            s.Task
#endif        
        static member        instance (_:Pure, _:option<'a>    ) = fun x -> Some x      :option<'a>
        static member        instance (_:Pure, _:list<'a>      ) = fun x -> [ x ]       :list<'a>
        static member        instance (_:Pure, _:'a []         ) = fun x -> [|x|]       :'a []
        static member        instance (_:Pure, _:'r -> 'a      ) = const':'a  -> 'r -> _
        static member inline instance (_:Pure, _: 'm * 'a      ) = fun (x:'a) -> (mempty(), x)
        static member        instance (_:Pure, _:'a Async      ) = fun (x:'a) -> async.Return x
        static member        instance (_:Pure, _:Choice<'a,'e> ) = fun x -> Choice1Of2 x :Choice<'a,'e>
        static member        instance (_:Pure, _:Expr<'a>      ) = fun x -> Expr.Cast<'a>(Expr.Value(x))
        static member        instance (_:Pure, _:'a ResizeArray) = fun x -> ResizeArray<'a>(Seq.singleton x)

        //Restricted
        static member instance (_:Pure, _:'a Nullable  ) = fun (x:'a  ) -> Nullable x:'a Nullable
        static member instance (_:Pure, _:string       ) = fun (x:char) -> string x : string
        static member instance (_:Pure, _:StringBuilder) = fun (x:char) -> new StringBuilder(string x):StringBuilder
        static member instance (_:Pure, _:'a Set       ) = fun (x:'a  ) -> Set.singleton x

    let Pure = Pure()
    let inline internal pure' x = Inline.instance Pure x


    type DefaultImpl =        
        static member inline ApplyFromMonad f x = f >>= fun x1 -> x >>= fun x2 -> pure'(x1 x2)

    type Apply() =
        inherit Typ1()

        static member inline instance (_:Typ2, f:#obj , x, _:#obj) = fun () -> (f >>= fun x1 -> x >>= fun x2 -> pure'(x1 x2))
        static member inline instance (_:Typ1, f:'F , x:'X, _:'R) = fun () -> 
            ((^F or ^X or ^R) : (static member (<*>): ^F -> ^X -> 'R) (f, x))

        static member        instance (_:Apply, f:Lazy<'a->'b>, x:Lazy<'a>     , _:Lazy<'b>     ) = fun () -> Lazy.Create (fun () -> f.Value x.Value) : Lazy<'b>
        static member        instance (_:Apply, f:seq<_>      , x:seq<'a>      , _:seq<'b>      ) = fun () -> Seq.apply  f x :seq<'b>
        static member        instance (_:Apply, f:list<_>     , x:list<'a>     , _:list<'b>     ) = fun () -> List.apply f x :list<'b>
        static member        instance (_:Apply, f:_ []        , x:'a []        , _:'b []        ) = fun () -> DefaultImpl.ApplyFromMonad f x :'b []
        static member        instance (_:Apply, f:'r -> _     , g: _ -> 'a     , _: 'r -> 'b    ) = fun () -> fun x -> f x (g x) :'b
        static member inline instance (_:Apply, (a:'m, f)     , (b:'m, x:'a)   , _:'m * 'b      ) = fun () -> (mappend a b, f x) :'m *'b
        static member        instance (_:Apply, f:Async<_>    , x:Async<'a>    , _:Async<'b>    ) = fun () -> DefaultImpl.ApplyFromMonad f x :Async<'b>
        static member        instance (_:Apply, f:option<_>   , x:option<'a>   , _:option<'b>   ) = fun () -> Option.apply f x
        static member        instance (_:Apply, f:Choice<_,'e>, x:Choice<'a,'e>, _:Choice<'b,'e>) = fun () -> Error.apply f x :Choice<'b,'e>

        static member        instance (_:Apply, KeyValue(k:'k,f)  , KeyValue(k:'k,x:'a), _:KeyValuePair<'k,'b>) :unit->KeyValuePair<'k,'b> = fun () -> KeyValuePair(k, f x)

        static member        instance (_:Apply, f:Map<'k,_>       , x:Map<'k,'a>       , _:Map<'k,'b>         ) :unit->Map<'k,'b>          = fun () -> Map (seq {
            for KeyValue(k, vf) in f do
                match Map.tryFind k x with
                | Some vx -> yield k, vf vx
                | _       -> () })

        static member        instance (_:Apply, f:Dictionary<'k,_>, x:Dictionary<'k,'a>, _:Dictionary<'k,'b>  ) :unit->Dictionary<'k,'b>          = fun () ->
            let d = Dictionary()
            for KeyValue(k, vf) in f do
                match x.TryGetValue k with
                | true, vx -> d.Add(k, vf vx)
                | _        -> ()
            d

        static member        instance (_:Apply, f:Expr<'a->'b>, x:Expr<'a>, _:Expr<'b>) = fun () -> Expr.Cast<'b>(Expr.Application(f,x))

        static member        instance (_:Apply, f:('a->'b) ResizeArray, x:'a ResizeArray, _:'b ResizeArray) = fun () ->
            ResizeArray(Seq.collect (fun x1 -> Seq.collect (fun x2 -> Seq.singleton (x1 x2)) x) f) :'b ResizeArray

    let Apply = Apply()
    let inline internal (<*>) x y = Inline.instance (Apply, x, y) ()

open Applicative


module Alternative =

    type Empty = Empty with
        static member instance (Empty, _:option<'a>) = fun () -> None
        static member instance (Empty, _:list<'a>  ) = fun () -> [  ]
        static member instance (Empty, _:'a []     ) = fun () -> [||]

    type Append = Append with  
        static member instance (Append, x:option<_>, _) = fun y -> match x with None -> y | xs -> xs
        static member instance (Append, x:list<_>  , _) = fun y -> x @ y
        static member instance (Append, x:_ []     , _) = fun y -> Array.append x y


// Functor class ----------------------------------------------------------

module Functor =

    type DefaultImpl =        
        static member inline MapFromApplicative f x = pure' f <*> x
        static member inline MapFromMonad f x = x >>= (pure' << f)

    type Map() =
        inherit Typ1()

        static member inline instance (_:Typ2, x:'f when 'f :> obj, _:'r when 'r :> obj) = fun (f:'a->'b) -> pure' f <*> x :'r
        static member inline instance (_:Typ1, x:'F, _:'R) =
            fun (f:'a->'b) -> ((^F) : (static member (<!>): ('a->'b) -> ^F -> ^R) (f, x))

        static member instance (_:Map, x:Lazy<_>        , _:Lazy<'b>) = fun f -> Lazy.Create (fun () -> f x.Value) : Lazy<'b>
        static member instance (_:Map, x:seq<_>         , _:seq<'b> ) = fun f -> Seq.map f x :seq<'b>
        static member instance (_:Map, x:option<_>      , _) = fun f -> Option.map  f x
        static member instance (_:Map, x:list<_>        , _:list<'b>) = fun f -> List.map f x :list<'b>
        static member instance (_:Map, g:_->_           , _) = (>>) g
        static member instance (_:Map, (m,a)            , _) = fun f -> (m, f a)
        static member instance (_:Map, x:_ []           , _) = fun f -> Array.map   f x
        static member instance (_:Map, x:_ [,]          , _) = fun f -> Array2D.map f x
        static member instance (_:Map, x:_ [,,]         , _) = fun f -> Array3D.map f x
        static member instance (_:Map, x:_ [,,,]        , _) = fun f ->
            Array4D.init (x.GetLength 0) (x.GetLength 1) (x.GetLength 2) (x.GetLength 3) (fun a b c d -> f x.[a,b,c,d])
        static member instance (_:Map, x:Async<_>       , _) = fun f -> DefaultImpl.MapFromMonad f x
        static member instance (_:Map, x:Choice<_,_>    , _) = fun f -> Error.map f x
        static member instance (_:Map, KeyValue(k, x)   , _) = fun (f:'b->'c) -> KeyValuePair(k, f x)
        static member instance (_:Map, x:Map<'a,'b>     , _) = fun (f:'b->'c) -> Map.map (const' f) x : Map<'a,'c>
        static member instance (_:Map, x:Dictionary<_,_>, _) = fun (f:'b->'c) -> let d = Dictionary() in Seq.iter (fun (KeyValue(k, v)) -> d.Add(k, f v)) x; d: Dictionary<'a,'c>
        static member instance (_:Map, x:Expr<'a>       , _) = fun (f:'a->'b) -> Expr.Cast<'b>(Expr.Application(Expr.Value(f),x))
        static member instance (_:Map, x:_ ResizeArray  , _) = fun f -> ResizeArray(Seq.map f x) : ResizeArray<'b>
        static member instance (_:Map, x:_ IObservable  , _) = fun f -> Observable.map f x

        // Restricted
        static member instance (_:Map, x:Nullable<_>    , _) = fun f -> if x.HasValue then Nullable(f x.Value) else Nullable()
        static member instance (_:Map, x:string         , _) = fun f -> String.map f x
        static member instance (_:Map, x:StringBuilder  , _) = fun f -> new StringBuilder(String.map f (x.ToString()))
        static member instance (_:Map, x:Set<_>         , _) = fun f -> Set.map f x
        

    let Map = Map()
    let inline internal fmap f x = Inline.instance (Map, x) f

   
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
        static member        instance (Extract, x:'t Async,_:'t) = fun () -> Async.RunSynchronously x
        static member        instance (Extract, x:'t Lazy, _:'t) = fun () -> x.Value
        static member        instance (Extract, (w:'w,a:'a) , _) = fun () -> a
        static member inline instance (Extract, f:'m->'t , _:'t) = fun () -> f (mempty())
        static member        instance (Extract, f:'t Id  , _:'t) = fun () -> f

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
        static member        instance (Extend, (g:'a Async), _:'b Async) = fun (f:Async<'a>->'b) -> async.Return (f g) : Async<'b>
        static member        instance (Extend, (g:'a Lazy ), _:'b Lazy ) = fun (f:Lazy<'a> ->'b) -> Lazy.Create  (fun () -> f g) : Lazy<'b>
        static member        instance (Extend, (w:'w, a:'a), _:'w *'b)   = fun (f:_->'b) -> (w, f (w,a))        
        static member inline instance (Extend, (g:'m -> 'a), _:'m->'b)   = fun (f:_->'b) a -> f (fun b -> g (mappend a b))
        static member        instance (Extend, (g:'a Id   ), _:'b Id )   = fun (f:Id<'a>->'b) -> f g

#if NOTNET35
        static member        instance (Extend, (g:'a Task), _:'b Task) = fun (f:Task<'a>->'b) -> g.ContinueWith(f)
#endif

        // Restricted
        static member        instance (Extend, s:list<'a>, _:list<'b>) = fun g -> 
            let rec tails = function [] -> [] | x::xs as s -> s::(tails xs)
            List.map g (tails s) :list<'b>

        static member        instance (Extend, s:'a [], _:'b []) = fun g -> 
            let rec tails = function [] -> [] | x::xs as s -> s::(tails xs)
            Array.map g (s |> Array.toList |> tails |> List.toArray |> Array.map List.toArray) :'b []

        static member        instance (Extend, s:'a seq, _:'b seq) = fun g -> 
            let rec tails = function [] -> [] | x::xs as s -> s::(tails xs)
            Seq.map g (s |> Seq.toList |> tails |> List.toSeq |> Seq.map List.toSeq) :'b seq

    let inline internal extend g s = Inline.instance (Extend, s) g
    let inline internal (=>>)  s g = extend g s


    type Duplicate() =
        inherit Typ1()
        static member inline instance (_:Typ1, x:#obj, _:#obj) = fun () -> extend id x :#obj
        static member        instance (_:Duplicate, s:Async<'a>, _:Async<Async<'a>>) = fun () -> async.Return s : Async<Async<'a>>
        static member        instance (_:Duplicate, s:Lazy<'a> , _:Lazy<Lazy<'a>>  ) = fun () -> Lazy.CreateFromValue s : Lazy<Lazy<'a>>
        static member        instance (_:Duplicate, (w:'w, a:'a), _:'w * ('w*'a)) = fun ()     -> (w, (w, a))
        static member inline instance (_:Duplicate,  f:'m -> 'a , _:'m->'m->'a  ) = fun () a b -> f (mappend a b)

        // Restricted
        static member        instance (_:Duplicate, s:list<'a>, _:list<list<'a>>) = fun () -> 
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
        static member        instance (Mzero, _:list<'a>  ) = fun () -> [  ]        :list<'a>  
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


type Kleisli<'a, 'm> = Kleisli of ('a -> 'm)

[<RequireQualifiedAccess>]
module Kleisli = let run (Kleisli f) = f

namespace FsControl.Core.TypeMethods

open FsControl.Core.Prelude
open FsControl.Core.TypeMethods.Monad
open FsControl.Core.TypeMethods.Functor
open FsControl.Core.Types
open FsControl.Core.TypeMethods.MonadPlus

module Category =

    type Id = Id with
        static member        instance (Id, _: 'r -> 'r     ) = fun () -> id              : 'r -> 'r
        static member inline instance (Id, _:Kleisli<'a,'b>) = fun () -> Kleisli return' :Kleisli<'a,'b>

    type Comp = Comp with
        static member        instance (Comp,         f, _) = fun (g: _ -> _) ->          g >>  f
        static member inline instance (Comp, Kleisli f, _) = fun (Kleisli g) -> Kleisli (g >=> f)

    let inline internal id'() = Inline.instance Id ()
    let inline internal (<<<) f g = Inline.instance (Comp, f) g

open Category

module Arrow =

    type Arr = Arr with
        static member        instance (Arr, _: _ -> _     ) = fun (f:_->_) -> f
        static member inline instance (Arr, _:Kleisli<_,_>) = fun  f       -> Kleisli (return' <<< f)

    type First = First with
        static member        instance (First, f        , _: 'a -> 'b   ) = fun () -> fun (x,y) -> (f x, y)
        static member inline instance (First, Kleisli f, _:Kleisli<_,_>) = fun () -> Kleisli (fun (b,d) -> f b >>= fun c -> return' (c,d))

    let inline internal arr   f = Inline.instance  Arr    f
    let inline internal first f = Inline.instance (First, f) ()

    type SecondDefault() =
        static member inline instance (_:SecondDefault, f:#obj, _:#obj) = fun () ->
            let aswap = Inline.instance Arr (fun (x,y) -> (y,x))
            aswap <<< first f <<< aswap

    type Second() =
        inherit SecondDefault()
        static member        instance (_:Second, f        , _: 'a -> 'b   ) = fun () -> fun (x,y) -> (x, f y)
        static member inline instance (_:Second, Kleisli f, _:Kleisli<_,_>) = fun () -> Kleisli (fun (d,b) -> f b >>= fun c -> return' (d,c))
    
    let Second = Second()

open Arrow

module ArrowChoice =

    type AcEither = AcEither with
        static member inline instance (AcEither, _:Choice<_,_>->_) = fun (         f ,          g ) ->          choice f g
        static member inline instance (AcEither, _:Kleisli<_,_>  ) = fun ((Kleisli f), (Kleisli g)) -> Kleisli (choice f g)

    let inline internal (|||) f g = Inline.instance AcEither (f, g)

    type AcMerge = AcMerge with
        static member inline instance (AcMerge, _: _->    Choice<_,_>      ) = fun (f, g)  ->  (Choice2Of2 << f) ||| (Choice1Of2 << g)
        static member inline instance (AcMerge, _:Kleisli<Choice<'v,'t>,'z>) = fun ((Kleisli (f:'t->'u)), (Kleisli (g:'v->'w))) ->
            Kleisli (f >=> (return' <<< Choice2Of2)) ||| Kleisli (g >=> (return' <<< Choice1Of2)) :Kleisli<Choice<'v,'t>,'z>

    let inline internal (+++) f g = Inline.instance AcMerge (f, g)

    type AcLeft = AcLeft with
        static member inline instance (AcLeft, f:_->_   , _) = fun () ->          f  +++      id
        static member inline instance (AcLeft, Kleisli f, _) = fun () -> (Kleisli f) +++ arr (id'())

    type AcRight = AcRight with
        static member inline instance (AcRight, f:_->_   , _) = fun () -> id          +++ f
        static member inline instance (AcRight, Kleisli f, _) = fun () -> arr (id'()) +++ Kleisli f


module ArrowApply =

    type Apply = Apply with
        static member instance (Apply, _: ('a -> 'b) * 'a -> 'b          ) = fun () ->          fun (f,x)          -> f x
        static member instance (Apply, _: Kleisli<Kleisli<'a,'b> * 'a,'b>) = fun () -> Kleisli (fun (Kleisli f, x) -> f x)
        
type Dummy<'a, 'm> = Dummy of ('a -> 'm)

module ArrowZero =

    type ZeroArrow = ZeroArrow with
        static member inline instance (ZeroArrow, _:Dummy<_,_>  ) = fun () -> Dummy   (fun _ -> mzero ())
        static member inline instance (ZeroArrow, _:Kleisli<_,_>) = fun () -> Kleisli (fun _ -> mzero ())
 
module ArrowPlus =
    
    type Plus = Plus with
        static member inline instance (Plus, Dummy   f, _) = fun (Dummy   g) -> Dummy  (fun x -> mplus (f x) (g x))
        static member inline instance (Plus, Kleisli f, _) = fun (Kleisli g) -> Kleisli(fun x -> mplus (f x) (g x))