namespace FsControl.Core.Types

type Kleisli<'a, 'm> = Kleisli of ('a -> 'm)

[<RequireQualifiedAccess>]
module Kleisli = let run (Kleisli f) = f

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


// Monad class ------------------------------------------------------------

type Bind() =
    static member val Instance = Bind()

    static member        Bind (x:Lazy<'a>     , f:_->Lazy<'b>  ) = lazy (f x.Value).Value
    static member        Bind (x:seq<_>       , f:_->seq<'b>   ) = Seq.bind f x
    static member        Bind (x:Id<'a>       , f:_->Id<'b>    ) = f x.getValue

#if NOTNET35
    static member        Bind (x:Task<'a>     , f:_->Task<'b>  ) = x.ContinueWith(fun (x: Task<_>) -> f x.Result).Unwrap()
#endif

    static member        Bind (x:option<_>    , f:_->option<'b>) = Option.bind   f x
    static member        Bind (x:list<_>      , f:_->list<'b>  ) = List.collect  f x
    static member        Bind (x:_ []         , f:_->'b []     ) = Array.collect f x
    static member        Bind (f:'r->'a       , k:_->_->'b)      = fun r -> k (f r) r
    static member inline Bind ((w, a):'m * 'a , k:_->'m * 'b   ) = let m, b = k a in (Mappend.Invoke w m, b)
    static member        Bind (x:Async<'a>    , f:_->Async<'b> ) = async.Bind(x,f)
    static member        Bind (x:Choice<'a,'e>, k:'a->Choice<'b,'e>) = Error.bind k x

    static member        Bind (x:Map<'k,'a>   , f:'a->Map<'k,'b>) = Map (seq {
        for KeyValue(k, v) in x do
            match Map.tryFind k (f v) with
            | Some v -> yield k, v
            | _      -> () })

    static member        Bind (x:Dictionary<'k,'a>, f:'a->Dictionary<'k,'b>) = 
        let d = Dictionary()
        for KeyValue(k, v) in x do
            match (f v).TryGetValue(k)  with
            | true, v -> d.Add(k, v)
            | _       -> ()
        d

    //Restricted Monad
    static member Bind (x:Nullable<_> , f ) = if x.HasValue then f x.Value else Nullable() : Nullable<'b>

    static member inline Invoke x (f:_->'R) : 'R =
        let inline call_3 (a:^a,b:^b,c:^c,f:^f) = ((^a or ^b or ^c) : (static member Bind: _*_ -> _) b, f)
        call_3 (Bind.Instance, x, Unchecked.defaultof<'R>, f) :'R

    static member inline InvokeOnNonPrimitiveTypes x (f:_->'R) : 'R =
        let inline call_3 (b:^b,c:^c,f:^f) = ((^b or ^c) : (static member Bind: _*_ -> _) b, f)
        call_3 (x, Unchecked.defaultof<'R>, f) :'R

type Join() =
    inherit Default1()
    static member val Instance = Join()

    static member inline Join (x                   , _       , _:Default1) = Bind.Invoke x id 
    static member        Join (x:Lazy<Lazy<'a>>    , _:Lazy<'a>  , _:Join) = lazy x.Value.Value
    static member        Join (x:option<option<'a>>, _:option<'a>, _:Join) = Option.bind   id x
    static member        Join (x:list<_>           , _:list<'b>  , _:Join) = List.collect  id x
    static member        Join (x:'b [] []          , _:'b []     , _:Join) = Array.collect id x
    static member        Join (x:Id<Id<'a>>        , _:Id<'a>    , _:Join) = x.getValue

#if NOTNET35        
    static member        Join (x:Task<Task<'a>>    , _:Task<'a>  , _:Join) = x.Unwrap()
#endif

    static member inline Invoke (x:'Monad'Monad'a) : 'Monad'a =
        let inline call_3 (a:^a, b:^b, c:^c) = ((^a or ^b or ^c) : (static member Join: _*_*_ -> _) b, c, a)
        let inline call (a:'a, b:'b) = call_3 (a, b, Unchecked.defaultof<'r>):'r
        call (Join.Instance, x)


type Return() =
    inherit Default1()
    static member val Instance = Return()
    static member inline Return (r:'R, _:Default1) = fun (x:'T) -> ((^R) : (static member Return: ^T -> ^R) x)

    static member        Return (_:Lazy<'a>, _:Return) = fun x -> Lazy.CreateFromValue x : Lazy<'a>
    static member        Return (_:seq<'a> , _:Return) = fun x -> Seq.singleton x :seq<'a>
    static member        Return (_:Id<'a>  , _:Return) = fun x -> Id x :Id<'a>

#if NOTNET35        
    static member        Return (_:'a Task, _:Return) = fun x -> 
        let s = TaskCompletionSource()
        s.SetResult x
        s.Task
#endif        
    static member        Return (_:option<'a>    , _:Return) = fun x -> Some x      :option<'a>
    static member        Return (_:list<'a>      , _:Return) = fun x -> [ x ]       :list<'a>
    static member        Return (_:'a []         , _:Return) = fun x -> [|x|]       :'a []
    static member        Return (_:'r -> 'a      , _:Return) = const':'a  -> 'r -> _
    static member inline Return (_: 'm * 'a      , _:Return) = fun (x:'a) -> (Mempty.Invoke(), x)
    static member        Return (_:'a Async      , _:Return) = fun (x:'a) -> async.Return x
    static member        Return (_:Choice<'a,'e> , _:Return) = fun x -> Choice1Of2 x :Choice<'a,'e>
    static member        Return (_:Expr<'a>      , _:Return) = fun x -> Expr.Cast<'a>(Expr.Value(x))
    static member        Return (_:'a ResizeArray, _:Return) = fun x -> ResizeArray<'a>(Seq.singleton x)

    //Restricted
    static member Return (_:'a Nullable  , _:Return) = fun (x:'a  ) -> Nullable x:'a Nullable
    static member Return (_:string       , _:Return) = fun (x:char) -> string x : string
    static member Return (_:StringBuilder, _:Return) = fun (x:char) -> new StringBuilder(string x):StringBuilder
    static member Return (_:'a Set       , _:Return) = fun (x:'a  ) -> Set.singleton x

    static member inline Invoke x = 
        let inline call_2 (a:^a,b:^b) = ((^a or ^b) : (static member Return: _*_ -> _) b, a)
        call_2 (Return.Instance, Unchecked.defaultof<'r>) x :'r 
 

type Apply() =
    inherit Default1()
    static member val Instance = Apply()
    static member inline FromMonad f x = Bind.Invoke f (fun x1 -> Bind.Invoke x (fun x2 -> Return.Invoke(x1 x2)))


    static member inline Apply (f:'Atu, x:'At, _, _:Default2) :^Au = Bind.InvokeOnNonPrimitiveTypes f (fun x1 -> Bind.InvokeOnNonPrimitiveTypes x (fun x2 -> Return.Invoke(x1 x2)))
    static member inline Apply (f:'F, x:'X, _:'R, _:Default1) = ((^F or ^X or ^R) : (static member (<*>): ^F -> ^X -> 'R) (f, x))

    static member        Apply (f:Lazy<'a->'b>, x:Lazy<'a>     , _:Lazy<'b>     , _:Apply) = Lazy.Create (fun () -> f.Value x.Value) : Lazy<'b>
    static member        Apply (f:seq<_>      , x:seq<'a>      , _:seq<'b>      , _:Apply) = Seq.apply  f x :seq<'b>
    static member        Apply (f:list<_>     , x:list<'a>     , _:list<'b>     , _:Apply) = List.apply f x :list<'b>
    static member        Apply (f:_ []        , x:'a []        , _:'b []        , _:Apply) = Apply.FromMonad f x :'b []
    static member        Apply (f:'r -> _     , g: _ -> 'a     , _: 'r -> 'b    , _:Apply) = fun x -> f x (g x) :'b
    static member inline Apply ((a:'m, f)     , (b:'m, x:'a)   , _:'m * 'b      , _:Apply) = (Mappend.Invoke a b, f x) :'m *'b
    static member        Apply (f:Async<_>    , x:Async<'a>    , _:Async<'b>    , _:Apply) = Apply.FromMonad f x :Async<'b>
    static member        Apply (f:option<_>   , x:option<'a>   , _:option<'b>   , _:Apply) = Option.apply f x
    static member        Apply (f:Choice<_,'e>, x:Choice<'a,'e>, _:Choice<'b,'e>, _:Apply) = Error.apply f x :Choice<'b,'e>

    static member        Apply (KeyValue(k:'k,f)  , KeyValue(k:'k,x:'a), _:KeyValuePair<'k,'b>, _:Apply) :KeyValuePair<'k,'b> = KeyValuePair(k, f x)

    static member        Apply (f:Map<'k,_>       , x:Map<'k,'a>       , _:Map<'k,'b>, _:Apply) :Map<'k,'b>          = Map (seq {
        for KeyValue(k, vf) in f do
            match Map.tryFind k x with
            | Some vx -> yield k, vf vx
            | _       -> () })

    static member        Apply (f:Dictionary<'k,_>, x:Dictionary<'k,'a>, _:Dictionary<'k,'b>, _:Apply) :Dictionary<'k,'b> =
        let d = Dictionary()
        for KeyValue(k, vf) in f do
            match x.TryGetValue k with
            | true, vx -> d.Add(k, vf vx)
            | _        -> ()
        d

    static member        Apply (f:Expr<'a->'b>, x:Expr<'a>, _:Expr<'b>, _:Apply) = Expr.Cast<'b>(Expr.Application(f,x))

    static member        Apply (f:('a->'b) ResizeArray, x:'a ResizeArray, _:'b ResizeArray, _:Apply) =
        ResizeArray(Seq.collect (fun x1 -> Seq.collect (fun x2 -> Seq.singleton (x1 x2)) x) f) :'b ResizeArray

    static member inline Invoke x y : 'Applicative'U =
        let inline call_4 (a:^a,b:^b,c:^c,d:^d) =                                                          
            ((^a or ^b or ^c or ^d) : (static member Apply: _*_*_*_ -> _) b, c, d, a)
        call_4(Apply.Instance,x,y, Unchecked.defaultof<'Applicative'U>)


// Functor class ----------------------------------------------------------

type Map_() =
    static member val Instance = Map_()
    static member Map_ (x:Lazy<_>  , f) = f x.Value :unit
    static member Map_ (x:seq<_>   , f) = Seq.iter f x
    static member Map_ (x:option<_>, f) = match x with Some x -> f x | _ -> ()
    static member Map_ (x:list<_>  , f) = List.iter f x
    static member Map_ ((m,a)      , f) = f a :unit
    static member Map_ (x:_ []     , f) = Array.iter   f x
    static member Map_ (x:_ [,]    , f) = Array2D.iter f x
    static member Map_ (x:_ [,,]   , f) = Array3D.iter f x
    static member Map_ (x:_ [,,,]  , f) =
        for i = 0 to Array4D.length1 x - 1 do
            for j = 0 to Array4D.length2 x - 1 do
                for k = 0 to Array4D.length3 x - 1 do
                    for l = 0 to Array4D.length4 x - 1 do
                        f x.[i,j,k,l]
    static member Map_ (x:Async<_>       , f) = f (Async.RunSynchronously x) : unit
    static member Map_ (x:Choice<_,_>    , f) = match x with Choice1Of2 x -> f x | _ -> ()
    static member Map_ (KeyValue(k, x)   , f) = f x :unit
    static member Map_ (x:Map<'a,'b>     , f) = Map.iter (const' f) x 
    static member Map_ (x:Dictionary<_,_>, f) = Seq.iter f x.Values
    static member Map_ (x:_ ResizeArray  , f) = Seq.iter f x

    // Restricted
    static member Map_ (x:Nullable<_>    , f) = if x.HasValue then f x.Value else ()
    static member Map_ (x:string         , f) = String.iter f x
    static member Map_ (x:StringBuilder  , f) = String.iter f (x.ToString())
    static member Map_ (x:Set<_>         , f) = Set.iter f x        

    static member inline Invoke (action :'T->unit) (source :'Functor'T) =
        let inline call_3 (a:^a, b:^b, c:^c) =  ((^a or ^b) : (static member Map: _*_ -> _) b, c)
        let inline call (a:'a, b:'b, c:'c) = call_3 (a ,b, c)
        call (Map_.Instance, source, action) :unit

type Map() =
    inherit Default1()
    static member val Instance = Map()
         
    static member inline FromApplicative f x = Return.Invoke (Apply.Invoke f x)
    static member inline FromMonad       f x = Bind.Invoke x (Return.Invoke << f)

    static member inline Map (x:'f, (f:'a->'b), _:Default2) = Return.Invoke (Apply.Invoke f x) :'r
    static member inline Map (x:'F, (f:'a->'b), _:Default1) = ((^F) : (static member Map: ^F * ('a->'b) -> ^R) (x, f))

    static member Map (x:Lazy<_>        , f, _:Map) = Lazy.Create (fun () -> f x.Value) : Lazy<'b>
    static member Map (x:seq<_>         , f, _:Map) = Seq.map f x :seq<'b>
    static member Map (x:option<_>      , f, _:Map) = Option.map  f x
    static member Map (x:list<_>        , f, _:Map) = List.map f x :list<'b>
    static member Map (g:_->_           , f, _:Map) = (>>) g f
    static member Map ((m,a)            , f, _:Map) = (m, f a)
    static member Map (x:_ []           , f, _:Map) = Array.map   f x
    static member Map (x:_ [,]          , f, _:Map) = Array2D.map f x
    static member Map (x:_ [,,]         , f, _:Map) = Array3D.map f x
    static member Map (x:_ [,,,]        , f, _:Map) = Array4D.init (x.GetLength 0) (x.GetLength 1) (x.GetLength 2) (x.GetLength 3) (fun a b c d -> f x.[a,b,c,d])
    static member Map (x:Async<_>       , f, _:Map) = Map.FromMonad f x
    static member Map (x:Choice<_,_>    , f, _:Map) = Error.map f x
    static member Map (KeyValue(k, x)   , (f:'b->'c), _:Map) = KeyValuePair(k, f x)
    static member Map (x:Map<'a,'b>     , (f:'b->'c), _:Map) = Map.map (const' f) x : Map<'a,'c>
    static member Map (x:Dictionary<_,_>, (f:'b->'c), _:Map) = let d = Dictionary() in Seq.iter (fun (KeyValue(k, v)) -> d.Add(k, f v)) x; d: Dictionary<'a,'c>
    static member Map (x:Expr<'a>       , (f:'a->'b), _:Map) = Expr.Cast<'b>(Expr.Application(Expr.Value(f),x))
    static member Map (x:_ ResizeArray  , f, _:Map) = ResizeArray(Seq.map f x) : ResizeArray<'b>
    static member Map (x:_ IObservable  , f, _:Map) = Observable.map f x

    // Restricted
    static member Map (x:Nullable<_>    , f, _:Map) = if x.HasValue then Nullable(f x.Value) else Nullable()
    static member Map (x:string         , f, _:Map) = String.map f x
    static member Map (x:StringBuilder  , f, _:Map) = new StringBuilder(String.map f (x.ToString()))
    static member Map (x:Set<_>         , f, _:Map) = Set.map f x
        
    static member inline Invoke (f:_->_) x = 
        let inline call_3 (a:^a, b:^b, c:^c, f) = ((^a or ^b) : (static member Map: _*_*_ -> _) b, f, a)
        let inline call (a:'a, b:'b, f) = call_3 (a, b, Unchecked.defaultof<'r>, f) :'r
        call (Map.Instance, x, f)


type Zero() =
    static member val Instance = Zero()
    static member        Zero (_:option<'a>, _:Zero) = None        :option<'a>
    static member        Zero (_:list<'a>  , _:Zero) = [  ]        :list<'a>  
    static member        Zero (_:'a []     , _:Zero) = [||]        :'a []     
    static member        Zero (_:seq<'a>   , _:Zero) = Seq.empty   :seq<'a>
    static member inline Zero (_:Id<'a>    , _:Zero) = Id (Mempty.Invoke()) :Id<'a>

    static member inline Invoke () :'Functor'T =
        let inline call_2 (a:^a, b:^b) = ((^a or ^b) : (static member Zero: _*_ -> _) b, a)
        let inline call (a:'a) = call_2 (a, Unchecked.defaultof<'r>) :'r
        call Zero.Instance


type Plus() =
    static member val Instance = Plus()
    static member        Plus (x:_ option, y, _:Plus) = match x with None -> y | xs -> xs
    static member        Plus (x:_ list  , y, _:Plus) = x @ y
    static member        Plus (x:_ []    , y, _:Plus) = Array.append x y
    static member        Plus (x:_ seq   , y, _:Plus) = Seq.append   x y
    static member inline Plus (x:_ Id    , y, _:Plus) = Id (Mappend.Invoke (Id.run x) (Id.run y))

    static member inline Invoke (x:'Functor'T) (y:'Functor'T) :'Functor'T =
        let inline call_3 (m:^M, a:^t, b:^t) = ((^M or ^t) : (static member Plus: _*_*_ -> _) a, b, m)
        call_3 (Plus.Instance, x, y)

type Zero with
    static member inline Zero (_:Kleisli<_,_>, _:Zero) = Kleisli (fun _ -> Zero.Invoke ())
    
type Plus with
    static member inline Plus (Kleisli f, Kleisli g, _:Plus) = Kleisli (fun x -> Plus.Invoke (f x) (g x))


module Monad =

    let inline (>>=) x f = Bind.Invoke x f
    let inline result  x = Return.Invoke x
    let inline (<*>) f x = Apply.Invoke f x
    let inline (<|>) x y = Plus.Invoke x y

    let inline internal sequence ms =
        let k m m' = m >>= fun (x:'a) -> m' >>= fun xs -> (result :list<'a> -> 'M) (List.Cons(x,xs))
        List.foldBack k ms ((result :list<'a> -> 'M) [])

    let inline internal mapM f as' = sequence (List.map f as')

    let inline internal liftM  f m1    = m1 >>= (result << f)
    let inline internal liftM2 f m1 m2 = m1 >>= fun x1 -> m2 >>= fun x2 -> result (f x1 x2)
    let inline internal ap     x y     = liftM2 id x y

    let inline internal (>=>)  f g x   = f x >>= g

    // Do notation ------------------------------------------------------------

    type DoNotationBuilder() =
        member inline b.Return(x)    = result x
        member inline b.Bind(p,rest) = p >>= rest
        member        b.Let (p,rest) = rest p
        member    b.ReturnFrom(expr) = expr
    let inline  internal do'() = new DoNotationBuilder()


open Monad

type Extract() =
    static member val Instance = Extract()
    static member        Extract (x:'t Async ) = Async.RunSynchronously x
    static member        Extract (x:'t Lazy  ) = x.Value
    static member        Extract ((w:'w,a:'a)) = a
    static member inline Extract (f:'m->'t   ) = f (Mempty.Invoke())
    static member        Extract (f:'t Id    ) = f

#if NOTNET35
    static member        Extract (f:'t Task  ) = f.Result
#endif

    // Restricted        
    static member        Extract (x:'t list      ) = List.head x
    static member        Extract (x:'t []        ) = x.[0]
    static member        Extract (x:string       ) = x.[0]
    static member        Extract (x:StringBuilder) = x.ToString().[0]
    static member        Extract (x:'t seq       ) = Seq.head x

    static member inline Invoke (x:'Comonad'T): 'T =
        let inline call_2 (a:^a, b:^b) = ((^a or ^b) : (static member Extract: _ -> _) b)
        let inline call (a:'a, b:'b) = call_2 (a, b)
        call (Extract.Instance, x)

type Extend() =
    static member val Instance = Extend()
    static member        Extend ((g:'a Async), _:'b Async) = fun (f:Async<'a>->'b) -> async.Return (f g) : Async<'b>
    static member        Extend ((g:'a Lazy ), _:'b Lazy ) = fun (f:Lazy<'a> ->'b) -> Lazy.Create  (fun () -> f g) : Lazy<'b>
    static member        Extend ((w:'w, a:'a), _:'w *'b)   = fun (f:_->'b) -> (w, f (w,a))        
    static member inline Extend ((g:'m -> 'a), _:'m->'b)   = fun (f:_->'b) a -> f (fun b -> g (Mappend.Invoke a b))
    static member        Extend ((g:'a Id   ), _:'b Id )   = fun (f:Id<'a>->'b) -> f g

#if NOTNET35
    static member        Extend ((g:'a Task), _:'b Task) = fun (f:Task<'a>->'b) -> g.ContinueWith(f)
#endif

    // Restricted
    static member        Extend (s:list<'a>, _:list<'b>) = fun g -> 
        let rec tails = function [] -> [] | x::xs as s -> s::(tails xs)
        List.map g (tails s) :list<'b>

    static member        Extend (s:'a [], _:'b []) = fun g -> 
        let rec tails = function [] -> [] | x::xs as s -> s::(tails xs)
        Array.map g (s |> Array.toList |> tails |> List.toArray |> Array.map List.toArray) :'b []

    static member        Extend (s:'a seq, _:'b seq) = fun g -> 
        let rec tails = function [] -> [] | x::xs as s -> s::(tails xs)
        Seq.map g (s |> Seq.toList |> tails |> List.toSeq |> Seq.map List.toSeq) :'b seq

    static member inline Invoke (g:'Comonad'T->'U) (s:'Comonad'T): 'Comonad'U =
        let inline call_3 (a:^a, b:^b, c:^c) = ((^a or ^b or ^c) : (static member Extend: _*_ -> _) b, c)
        let inline call (a:'a, b:'b) = fun (x:'x) -> call_3 (a, b, Unchecked.defaultof<'r>) x :'r
        call (Extend.Instance, s) g


type Duplicate() =
    inherit Default1()
    static member val Instance = Duplicate()
    static member inline Duplicate (x           , _                 , _:Default1 ) = Extend.Invoke id x
    static member        Duplicate (s:Async<'a> , _:Async<Async<'a>>, _:Duplicate) = async.Return s : Async<Async<'a>>
    static member        Duplicate (s:Lazy<'a>  , _:Lazy<Lazy<'a>>  , _:Duplicate) = Lazy.CreateFromValue s : Lazy<Lazy<'a>>
    static member        Duplicate ((w:'w, a:'a), _:'w * ('w*'a)    , _:Duplicate) = (w, (w, a))
    static member inline Duplicate ( f:'m -> 'a , _:'m->'m->'a      , _:Duplicate) = fun a b -> f (Mappend.Invoke a b)

    // Restricted
    static member        Duplicate (s:list<'a>, _:list<list<'a>>, _:Duplicate) =
        let rec tails = function [] -> [] | x::xs as s -> s::(tails xs)
        tails s

    static member        Duplicate (s: array<'a>, _: array<array<'a>>, _:Duplicate) =
        let rec tails = function [] -> [] | x::xs as s -> s::(tails xs)
        s |> Array.toList |> tails |> List.toArray |> Array.map List.toArray
    
    static member inline Invoke x =
        let inline call_3 (a:^a, b:^b, c:^c) = ((^a or ^b or ^c) : (static member Duplicate: _*_*_ -> _) b, c, a)
        let inline call (a:'a, b:'b) = call_3 (a, b, Unchecked.defaultof<'r>) :'r
        call (Duplicate.Instance, x)




type Id() =
    static member val Instance = Id()
    static member        Id (_: 'r -> 'r     , _:Id) = id              : 'r -> 'r
    static member inline Id (_:Kleisli<'a,'b>, _:Id) = Kleisli result :Kleisli<'a,'b>

    static member inline Invoke() =
        let inline call_2 (a:^a, b:^b) = ((^a or ^b) : (static member Id: _*_ -> _) b, a)
        let inline call (a:'a) = call_2 (a, Unchecked.defaultof<'r>) :'r
        call Id.Instance


type Comp() =
    static member val Instance = Comp()
    static member        Comp (        f, _, _:Comp) = fun (g: _ -> _) ->          g >>  f
    static member inline Comp (Kleisli f, _, _:Comp) = fun (Kleisli g) -> Kleisli (g >=> f)

    static member inline Invoke f g =
        let inline call_3 (a:^a, b:^b, c:^c) = ((^a or ^b or ^c) : (static member Comp: _*_*_ -> _) b, c, a)
        let inline call (a:'a, b:'b) = fun (x:'x) -> call_3 (a, b, Unchecked.defaultof<'r>) x :'r
        call (Comp.Instance, f) g


type Arr() =
    static member val Instance = Arr()
    static member        Arr (_: _ -> _     , _:Arr) = fun (f:_->_) -> f
    static member inline Arr (_:Kleisli<_,_>, _:Arr) = fun  f       -> Kleisli (Comp.Invoke result f)

    static member inline Invoke f = 
        let inline call_2 (a:^a, b:^b) = ((^a or ^b) : (static member Arr: _*_ -> _) b, a)
        let inline call   (a:'a) = fun (x:'x) -> call_2 (a, Unchecked.defaultof<'r>) x :'r
        call Arr.Instance f


type First() =
    static member val Instance = First()
    static member        First (f        , _: 'a -> 'b   , _:First) = fun (x,y) -> (f x, y)
    static member inline First (Kleisli f, _:Kleisli<_,_>, _:First) = Kleisli (fun (b,d) -> f b >>= fun c -> result (c,d))

    static member inline Invoke f =
        let inline call_3 (a:^a, b:^b, c:^c) = ((^a or ^b or ^c) : (static member First: _*_*_ -> _) b, c, a)
        let inline call (a:'a, b:'b) = call_3 (a, b, Unchecked.defaultof<'r>) :'r
        call (First.Instance, f)


type Second() =
    inherit Default1()
    static member val Instance = Second()
    static member inline Second (f      , _               , _:Default1) = let aswap = Arr.Invoke (fun (x,y) -> (y,x)) in Comp.Invoke aswap (Comp.Invoke (First.Invoke f) aswap)
    static member        Second (f        , _: 'a -> 'b   , _:Second  ) = fun (x,y) -> (x, f y)
    static member inline Second (Kleisli f, _:Kleisli<_,_>, _:Second  ) = Kleisli (fun (d,b) -> f b >>= fun c -> result (d,c))

    static member inline Invoke  f   =
        let inline call_3 (a:^a, b:^b, c:^c) = ((^a or ^b or ^c) : (static member Second: _*_*_ -> _) b, c, a)
        let inline call (a:'a, b:'b) = call_3 (a, b, Unchecked.defaultof<'r>) :'r
        call (Second.Instance, f)

type AcEither() =
    static member val Instance = AcEither()
    static member inline AcEither (_:Choice<_,_>->_, _:AcEither) = fun (         f ,          g ) ->          choice f g
    static member inline AcEither (_:Kleisli<_,_>  , _:AcEither) = fun ((Kleisli f), (Kleisli g)) -> Kleisli (choice f g)

    static member inline Invoke f g =
        let inline call_2 (a:^a, b:^b) = ((^a or ^b) : (static member AcEither: _*_ -> _) b, a)
        let inline call (a:'a) = fun (x:'x) -> call_2 (a, Unchecked.defaultof<'r>) x :'r
        call AcEither.Instance (f, g)


type AcMerge() =
    static member val Instance = AcMerge()
    static member inline AcMerge (_: _->    Choice<_,_>      , _:AcMerge) = fun (f, g)  -> AcEither.Invoke (Choice2Of2 << f) (Choice1Of2 << g)
    static member inline AcMerge (_:Kleisli<Choice<'v,'t>,'z>, _:AcMerge) = fun ((Kleisli (f:'t->'u)), (Kleisli (g:'v->'w))) ->
        AcEither.Invoke (Kleisli (f >=> (Comp.Invoke result Choice2Of2))) (Kleisli (g >=> (Comp.Invoke result Choice1Of2))) :Kleisli<Choice<'v,'t>,'z>     

    static member inline Invoke f g =
        let inline call_2 (a:^a, b:^b) = ((^a or ^b) : (static member AcMerge: _*_ -> _) b, a)
        let inline call (a:'a) = fun (x:'x) -> call_2 (a, Unchecked.defaultof<'r>) x :'r
        call AcMerge.Instance (f, g)


type AcLeft() =
    static member val Instance = AcLeft()
    static member inline AcLeft (f:_->_   , _, _:AcLeft) = AcMerge.Invoke f id
    static member inline AcLeft (Kleisli f, _, _:AcLeft) =
        let inline (+++) a b = AcMerge.Invoke a b
        (+++) (Kleisli f) (Arr.Invoke (Id.Invoke()))

    static member inline Invoke    f   =
        let inline call_3 (a:^a, b:^b, c:^c) = ((^a or ^b or ^c) : (static member AcLeft: _*_*_ -> _) b, c, a)
        let inline call (a:'a, b:'b) = call_3 (a, b, Unchecked.defaultof<'r>) :'r
        call (AcLeft.Instance, f)


type AcRight() =
    static member val Instance = AcRight()
    static member inline AcRight (f:_->_   , _, _:AcRight) = AcMerge.Invoke id f
    static member inline AcRight (Kleisli f, _, _:AcRight) =
        let inline (+++) a b = AcMerge.Invoke a b
        (+++) (Arr.Invoke (Id.Invoke())) (Kleisli f)

    static member inline Invoke   f   =
        let inline call_3 (a:^a, b:^b, c:^c) = ((^a or ^b or ^c) : (static member AcRight: _*_*_ -> _) b, c, a)
        let inline call (a:'a, b:'b) = call_3 (a, b, Unchecked.defaultof<'r>) :'r
        call (AcRight.Instance, f)


type ArrApply() =
    static member val Instance = ArrApply()
    static member ArrApply (_: ('a -> 'b) * 'a -> 'b          , _:ArrApply) =          fun (f, x)         -> f x
    static member ArrApply (_: Kleisli<Kleisli<'a,'b> * 'a,'b>, _:ArrApply) = Kleisli (fun (Kleisli f, x) -> f x)

    static member inline Invoke()     =
        let inline call_2 (a:^a, b:^b) = ((^a or ^b) : (static member ArrApply: _*_ -> _) b, a)
        let inline call (a:'a) = call_2 (a, Unchecked.defaultof<'r>) :'r
        call ArrApply.Instance