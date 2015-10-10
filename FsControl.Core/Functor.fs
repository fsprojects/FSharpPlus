namespace FsControl.Core.Types

type Kleisli<'a, 'm> = Kleisli of ('a -> 'm)

[<RequireQualifiedAccess>]
module Kleisli = let run (Kleisli f) = f

namespace FsControl.Core.TypeMethods

open System
open System.Runtime.CompilerServices
open System.Runtime.InteropServices
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

[<Extension;Sealed>]
type Bind =
    [<Extension>]static member        Bind (x:Lazy<'a>     , f:_->Lazy<'b>  ) = lazy (f x.Value).Value
    [<Extension>]static member        Bind (x:seq<_>       , f:_->seq<'b>   ) = Seq.bind f x
    [<Extension>]static member        Bind (x:Id<'a>       , f:_->Id<'b>    ) = f x.getValue

#if NOTNET35
    [<Extension>]static member        Bind (x:Task<'a>     , f:_->Task<'b>  ) = x.ContinueWith(fun (x: Task<_>) -> f x.Result).Unwrap()
#endif

    [<Extension>]static member        Bind (x:option<_>    , f:_->option<'b>) = Option.bind   f x
    [<Extension>]static member        Bind (x:list<_>      , f:_->list<'b>  ) = List.collect  f x
    [<Extension>]static member        Bind (x:_ []         , f:_->'b []     ) = Array.collect f x
    [<Extension>]static member        Bind (f:'r->'a       , k:_->_->'b)      = fun r -> k (f r) r
    [<Extension>]static member inline Bind ((w, a):'m * 'a , k:_->'m * 'b   ) = let m, b = k a in (Mappend.Invoke w m, b)
    [<Extension>]static member        Bind (x:Async<'a>    , f:_->Async<'b> ) = async.Bind(x,f)
    [<Extension>]static member        Bind (x:Choice<'a,'e>, k:'a->Choice<'b,'e>) = Error.bind k x

    [<Extension>]static member        Bind (x:Map<'k,'a>   , f:'a->Map<'k,'b>) = Map (seq {
                    for KeyValue(k, v) in x do
                        match Map.tryFind k (f v) with
                        | Some v -> yield k, v
                        | _      -> () })

    [<Extension>]static member        Bind (x:Dictionary<'k,'a>, f:'a->Dictionary<'k,'b>) = 
                    let d = Dictionary()
                    for KeyValue(k, v) in x do
                        match (f v).TryGetValue(k)  with
                        | true, v -> d.Add(k, v)
                        | _       -> ()
                    d

    static member inline Invoke x (f:_->'R) : 'R =
        let inline call_3 (a:^a,b:^b,c:^c,f:^f) = ((^a or ^b or ^c) : (static member Bind: _*_ -> _) b, f)
        call_3 (Unchecked.defaultof<Bind>, x, Unchecked.defaultof<'R>, f) :'R

    static member inline InvokeOnNonPrimitiveTypes x (f:_->'R) : 'R =
        let inline call_3 (b:^b,c:^c,f:^f) = ((^b or ^c) : (static member Bind: _*_ -> _) b, f)
        call_3 (x, Unchecked.defaultof<'R>, f) :'R


[<Extension;Sealed>]
type Join =
    inherit Default1
    [<Extension>]static member inline Join (x                   , [<Optional>]output, [<Optional>]impl:Default1) = Bind.Invoke x id 
    [<Extension>]static member        Join (x:Lazy<Lazy<'a>>    , [<Optional>]output:Lazy<'a>  , [<Optional>]impl:Join) = lazy x.Value.Value
    [<Extension>]static member        Join (x:option<option<'a>>, [<Optional>]output:option<'a>, [<Optional>]impl:Join) = Option.bind   id x
    [<Extension>]static member        Join (x:list<_>           , [<Optional>]output:list<'b>  , [<Optional>]impl:Join) = List.collect  id x
    [<Extension>]static member        Join (x:'b [] []          , [<Optional>]output:'b []     , [<Optional>]impl:Join) = Array.collect id x
    [<Extension>]static member        Join (x:Id<Id<'a>>        , [<Optional>]output:Id<'a>    , [<Optional>]impl:Join) = x.getValue

#if NOTNET35        
    [<Extension>]static member        Join (x:Task<Task<'a>>    , [<Optional>]output:Task<'a>  , [<Optional>]impl:Join) = x.Unwrap()
#endif

    static member inline Invoke (x:'Monad'Monad'a) : 'Monad'a =
        let inline call_3 (a:^a, b:^b, c:^c) = ((^a or ^b or ^c) : (static member Join: _*_*_ -> _) b, c, a)
        let inline call (a:'a, b:'b) = call_3 (a, b, Unchecked.defaultof<'r>):'r
        call (Unchecked.defaultof<Join>, x)


type Return =
    inherit Default1
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
    static member        Return (_:Identity<'t>  , _:Return) = fun x -> Identity x :Identity<'t>
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
    static member Return (_:string       , _:Return) = fun (x:char) -> string x : string
    static member Return (_:StringBuilder, _:Return) = fun (x:char) -> new StringBuilder(string x):StringBuilder
    static member Return (_:'a Set       , _:Return) = fun (x:'a  ) -> Set.singleton x

    static member inline Invoke x = 
        let inline call_2 (a:^a,b:^b) = ((^a or ^b) : (static member Return: _*_ -> _) b, a)
        call_2 (Unchecked.defaultof<Return>, Unchecked.defaultof<'r>) x :'r 
 

[<Extension;Sealed>]
type Apply =
    inherit Default1
    static member inline FromMonad f x = Bind.Invoke f (fun x1 -> Bind.Invoke x (fun x2 -> Return.Invoke(x1 x2)))


    [<Extension>]static member inline Apply (f:'Atu, x:'At, [<Optional>]output   , [<Optional>]impl:Default2) :^Au = Bind.InvokeOnNonPrimitiveTypes f (fun x1 -> Bind.InvokeOnNonPrimitiveTypes x (fun x2 -> Return.Invoke(x1 x2)))
    [<Extension>]static member inline Apply (f:'F  , x:'X , [<Optional>]output:'R, [<Optional>]impl:Default1) = ((^F or ^X or ^R) : (static member (<*>): ^F -> ^X -> 'R) (f, x))

    [<Extension>]static member        Apply (f:Lazy<'a->'b>, x:Lazy<'a>     , [<Optional>]output:Lazy<'b>     , [<Optional>]impl:Apply) = Lazy.Create (fun () -> f.Value x.Value) : Lazy<'b>
    [<Extension>]static member        Apply (f:seq<_>      , x:seq<'a>      , [<Optional>]output:seq<'b>      , [<Optional>]impl:Apply) = Seq.apply  f x :seq<'b>
    [<Extension>]static member        Apply (f:list<_>     , x:list<'a>     , [<Optional>]output:list<'b>     , [<Optional>]impl:Apply) = List.apply f x :list<'b>
    [<Extension>]static member        Apply (f:_ []        , x:'a []        , [<Optional>]output:'b []        , [<Optional>]impl:Apply) = Apply.FromMonad f x :'b []
    [<Extension>]static member        Apply (f:'r -> _     , g: _ -> 'a     , [<Optional>]output: 'r -> 'b    , [<Optional>]impl:Apply) = fun x -> f x (g x) :'b
    [<Extension>]static member inline Apply ((a:'m, f)     , (b:'m, x:'a)   , [<Optional>]output:'m * 'b      , [<Optional>]impl:Apply) = (Mappend.Invoke a b, f x) :'m *'b
    [<Extension>]static member        Apply (f:Async<_>    , x:Async<'a>    , [<Optional>]output:Async<'b>    , [<Optional>]impl:Apply) = Apply.FromMonad f x :Async<'b>
    [<Extension>]static member        Apply (f:option<_>   , x:option<'a>   , [<Optional>]output:option<'b>   , [<Optional>]impl:Apply) = Option.apply f x
    [<Extension>]static member        Apply (f:Choice<_,'e>, x:Choice<'a,'e>, [<Optional>]output:Choice<'b,'e>, [<Optional>]impl:Apply) = Error.apply f x :Choice<'b,'e>
    [<Extension>]static member        Apply (KeyValue(k:'k,f)  , KeyValue(k:'k,x:'a), [<Optional>]output:KeyValuePair<'k,'b>, [<Optional>]impl:Apply) :KeyValuePair<'k,'b> = KeyValuePair(k, f x)

    [<Extension>]static member        Apply (f:Map<'k,_>       , x:Map<'k,'a>       , [<Optional>]output:Map<'k,'b>, [<Optional>]impl:Apply) :Map<'k,'b>          = Map (seq {
                    for KeyValue(k, vf) in f do
                        match Map.tryFind k x with
                        | Some vx -> yield k, vf vx
                        | _       -> () })

    [<Extension>]static member        Apply (f:Dictionary<'k,_>, x:Dictionary<'k,'a>, [<Optional>]output:Dictionary<'k,'b>, [<Optional>]impl:Apply) :Dictionary<'k,'b> =
                    let d = Dictionary()
                    for KeyValue(k, vf) in f do
                        match x.TryGetValue k with
                        | true, vx -> d.Add(k, vf vx)
                        | _        -> ()
                    d
    [<Extension>]static member        Apply (Identity (f:'t->'u)     , Identity (x: 't)     , [<Optional>]output:Identity<'u> , [<Optional>]impl:Apply) = Identity (f x)      : Identity<'u>
    [<Extension>]static member inline Apply (Const f:Const<'a,'t->'u>, Const x: Const<'a,'t>, [<Optional>]output:Const<'a,'u> , [<Optional>]impl:Apply) = Const (Mappend.Invoke f x) : Const<'a,'u>

    [<Extension>]static member        Apply (f:Expr<'a->'b>, x:Expr<'a>, [<Optional>]output:Expr<'b>, [<Optional>]impl:Apply) = Expr.Cast<'b>(Expr.Application(f,x))

    [<Extension>]static member        Apply (f:('a->'b) ResizeArray, x:'a ResizeArray, [<Optional>]output:'b ResizeArray, [<Optional>]impl:Apply) =
                    ResizeArray(Seq.collect (fun x1 -> Seq.collect (fun x2 -> Seq.singleton (x1 x2)) x) f) :'b ResizeArray

    static member inline Invoke x y : 'Applicative'U =
        let inline call_4 (a:^a,b:^b,c:^c,d:^d) =                                                          
            ((^a or ^b or ^c or ^d) : (static member Apply: _*_*_*_ -> _) b, c, d, a)
        call_4(Unchecked.defaultof<Apply>,x,y, Unchecked.defaultof<'Applicative'U>)


// Functor class ----------------------------------------------------------

[<Extension;Sealed>]
type Map_ =
    [<Extension>]static member Map_ (x:Lazy<_>  , f) = f x.Value :unit
    [<Extension>]static member Map_ (x:seq<_>   , f) = Seq.iter f x
    [<Extension>]static member Map_ (x:option<_>, f) = match x with Some x -> f x | _ -> ()
    [<Extension>]static member Map_ (x:list<_>  , f) = List.iter f x
    [<Extension>]static member Map_ ((m,a)      , f) = f a :unit
    [<Extension>]static member Map_ (x:_ []     , f) = Array.iter   f x
    [<Extension>]static member Map_ (x:_ [,]    , f) = Array2D.iter f x
    [<Extension>]static member Map_ (x:_ [,,]   , f) = Array3D.iter f x
    [<Extension>]static member Map_ (x:_ [,,,]  , f) =
                    for i = 0 to Array4D.length1 x - 1 do
                        for j = 0 to Array4D.length2 x - 1 do
                            for k = 0 to Array4D.length3 x - 1 do
                                for l = 0 to Array4D.length4 x - 1 do
                                    f x.[i,j,k,l]
    [<Extension>]static member Map_ (x:Async<_>       , f) = f (Async.RunSynchronously x) : unit
    [<Extension>]static member Map_ (x:Choice<_,_>    , f) = match x with Choice1Of2 x -> f x | _ -> ()
    [<Extension>]static member Map_ (KeyValue(k, x)   , f) = f x :unit
    [<Extension>]static member Map_ (x:Map<'a,'b>     , f) = Map.iter (const' f) x 
    [<Extension>]static member Map_ (x:Dictionary<_,_>, f) = Seq.iter f x.Values
    [<Extension>]static member Map_ (x:_ ResizeArray  , f) = Seq.iter f x

    // Restricted
    [<Extension>]static member Map_ (x:string         , f) = String.iter f x
    [<Extension>]static member Map_ (x:StringBuilder  , f) = String.iter f (x.ToString())
    [<Extension>]static member Map_ (x:Set<_>         , f) = Set.iter f x        

    static member inline Invoke (action :'T->unit) (source :'Functor'T) =
        let inline call_3 (a:^a, b:^b, c:^c) =  ((^a or ^b) : (static member Map_: _*_ -> _) b, c)
        let inline call (a:'a, b:'b, c:'c) = call_3 (a ,b, c)
        call (Unchecked.defaultof<Map_>, source, action) :unit

[<Extension;Sealed>]
type Map =
    inherit Default1
    static member inline FromApplicative f x = Return.Invoke (Apply.Invoke f x)
    static member inline FromMonad       f x = Bind.Invoke x (Return.Invoke << f)

    [<Extension>]static member inline Map (x:'f, (f:'a->'b), [<Optional>]impl:Default2) = Return.Invoke (Apply.Invoke f x) :'r
    [<Extension>]static member inline Map (x:'F, (f:'a->'b), [<Optional>]impl:Default1) = ((^F) : (static member Map: ^F * ('a->'b) -> ^R) (x, f))

    [<Extension>]static member Map (x:Lazy<_>        , f, [<Optional>]impl:Map) = Lazy.Create (fun () -> f x.Value) : Lazy<'b>
    [<Extension>]static member Map (x:seq<_>         , f, [<Optional>]impl:Map) = Seq.map f x :seq<'b>
    [<Extension>]static member Map (x:option<_>      , f, [<Optional>]impl:Map) = Option.map  f x
    [<Extension>]static member Map (x:list<_>        , f, [<Optional>]impl:Map) = List.map f x :list<'b>
    [<Extension>]static member Map (g:_->_           , f, [<Optional>]impl:Map) = (>>) g f
    [<Extension>]static member Map ((m,a)            , f, [<Optional>]impl:Map) = (m, f a)
    [<Extension>]static member Map (x:_ []           , f, [<Optional>]impl:Map) = Array.map   f x
    [<Extension>]static member Map (x:_ [,]          , f, [<Optional>]impl:Map) = Array2D.map f x
    [<Extension>]static member Map (x:_ [,,]         , f, [<Optional>]impl:Map) = Array3D.map f x
    [<Extension>]static member Map (x:_ [,,,]        , f, [<Optional>]impl:Map) = Array4D.init (x.GetLength 0) (x.GetLength 1) (x.GetLength 2) (x.GetLength 3) (fun a b c d -> f x.[a,b,c,d])
    [<Extension>]static member Map (x:Async<_>       , f, [<Optional>]impl:Map) = Map.FromMonad f x
    [<Extension>]static member Map (x:Choice<_,_>    , f, [<Optional>]impl:Map) = Error.map f x
    [<Extension>]static member Map (Identity x       , f, [<Optional>]impl:Map) = Identity (f x)
    [<Extension>]static member Map (Const x:Const<_,'u>, f:'u->'v, [<Optional>]impl:Map) = Const x : Const<'t,'v>
    [<Extension>]static member Map (KeyValue(k, x)   , (f:'b->'c), [<Optional>]impl:Map) = KeyValuePair(k, f x)
    [<Extension>]static member Map (x:Map<'a,'b>     , (f:'b->'c), [<Optional>]impl:Map) = Map.map (const' f) x : Map<'a,'c>
    [<Extension>]static member Map (x:Dictionary<_,_>, (f:'b->'c), [<Optional>]impl:Map) = let d = Dictionary() in Seq.iter (fun (KeyValue(k, v)) -> d.Add(k, f v)) x; d: Dictionary<'a,'c>
    [<Extension>]static member Map (x:Expr<'a>       , (f:'a->'b), [<Optional>]impl:Map) = Expr.Cast<'b>(Expr.Application(Expr.Value(f),x))
    [<Extension>]static member Map (x:_ ResizeArray  , f, [<Optional>]impl:Map) = ResizeArray(Seq.map f x) : ResizeArray<'b>
    [<Extension>]static member Map (x:_ IObservable  , f, [<Optional>]impl:Map) = Observable.map f x

    // Restricted
    [<Extension>]static member Map (x:string         , f, [<Optional>]impl:Map) = String.map f x
    [<Extension>]static member Map (x:StringBuilder  , f, [<Optional>]impl:Map) = new StringBuilder(String.map f (x.ToString()))
    [<Extension>]static member Map (x:Set<_>         , f, [<Optional>]impl:Map) = Set.map f x
        
    static member inline Invoke (f:_->_) x = 
        let inline call_3 (a:^a, b:^b, c:^c, f) = ((^a or ^b or ^c) : (static member Map: _*_*_ -> _) b, f, a)
        let inline call (a:'a, b:'b, f) = call_3 (a, b, Unchecked.defaultof<'r>, f) :'r
        call (Unchecked.defaultof<Map>, x, f)


type Zero =
    static member        Zero (_:option<'a>, _:Zero) = None        :option<'a>
    static member        Zero (_:list<'a>  , _:Zero) = [  ]        :list<'a>  
    static member        Zero (_:'a []     , _:Zero) = [||]        :'a []     
    static member        Zero (_:seq<'a>   , _:Zero) = Seq.empty   :seq<'a>
    static member inline Zero (_:Id<'a>    , _:Zero) = Id (Mempty.Invoke()) :Id<'a>

    static member inline Invoke () :'Functor'T =
        let inline call_2 (a:^a, b:^b) = ((^a or ^b) : (static member Zero: _*_ -> _) b, a)
        let inline call (a:'a) = call_2 (a, Unchecked.defaultof<'r>) :'r
        call Unchecked.defaultof<Zero>


[<Extension;Sealed>]
type Plus =
    [<Extension>]static member        Plus (x:_ option, y, [<Optional>]impl:Plus) = match x with None -> y | xs -> xs
    [<Extension>]static member        Plus (x:_ list  , y, [<Optional>]impl:Plus) = x @ y
    [<Extension>]static member        Plus (x:_ []    , y, [<Optional>]impl:Plus) = Array.append x y
    [<Extension>]static member        Plus (x:_ seq   , y, [<Optional>]impl:Plus) = Seq.append   x y
    [<Extension>]static member inline Plus (x:_ Id    , y, [<Optional>]impl:Plus) = Id (Mappend.Invoke (Id.run x) (Id.run y))

    static member inline Invoke (x:'Functor'T) (y:'Functor'T) :'Functor'T =
        let inline call_3 (m:^M, a:^t, b:^t) = ((^M or ^t) : (static member Plus: _*_*_ -> _) a, b, m)
        call_3 (Unchecked.defaultof<Plus>, x, y)

type Zero with
    static member inline Zero (_:Kleisli<_,_>, _:Zero) = Kleisli (fun _ -> Zero.Invoke ())
    
type Plus with
    static member inline Plus (Kleisli f, Kleisli g, _:Plus) = Kleisli (fun x -> Plus.Invoke (f x) (g x))


module internal MonadOps =

    let inline (>>=) x f = Bind.Invoke x f
    let inline result  x = Return.Invoke x
    let inline (<*>) f x = Apply.Invoke f x
    let inline (<|>) x y = Plus.Invoke x y
    let inline (>=>) (f:'a->'Monad'b) (g:'b->'Monad'c) (x:'a) :'Monad'c = f x >>= g

open MonadOps

[<Extension;Sealed>]
type Extract =
    [<Extension>]static member        Extract (x:'t Async ) = Async.RunSynchronously x
    [<Extension>]static member        Extract (x:'t Lazy  ) = x.Value
    [<Extension>]static member        Extract ((w:'w,a:'a)) = a
    [<Extension>]static member inline Extract (f:'m->'t   ) = f (Mempty.Invoke())
    [<Extension>]static member        Extract (f:'t Id    ) = f

#if NOTNET35
    [<Extension>]static member        Extract (f:'t Task  ) = f.Result
#endif

    static member inline Invoke (x:'Comonad'T): 'T =
        let inline call_2 (a:^a, b:^b) = ((^a or ^b) : (static member Extract: _ -> _) b)
        let inline call (a:'a, b:'b) = call_2 (a, b)
        call (Unchecked.defaultof<Extract>, x)

[<Extension;Sealed>]
type Extend =
    [<Extension>]static member        Extend ((g:'a Async), f:Async<'a>->'b) = async.Return (f g) : Async<'b>
    [<Extension>]static member        Extend ((g:'a Lazy ), f:Lazy<'a> ->'b) = Lazy.Create  (fun () -> f g) : Lazy<'b>
    [<Extension>]static member        Extend ((w:'w, a:'a), f:_->'b) = (w, f (w,a))        
    [<Extension>]static member inline Extend ((g:'m -> 'a), f:_->'b) = fun a -> f (fun b -> g (Mappend.Invoke a b))
    [<Extension>]static member        Extend ((g:'a Id   ), f:Id<'a>->'b) = f g

#if NOTNET35
    [<Extension>]static member        Extend ((g:'a Task), f:Task<'a>->'b) = g.ContinueWith(f)
#endif

    // Restricted Comonads
    [<Extension>]static member        Extend (s:list<'a>, g) = List.map g (List.tails s) :list<'b>
    [<Extension>]static member        Extend (s:'a [] , g) = Array.map g (s |> Array.toList |> List.tails |> List.toArray |> Array.map List.toArray) :'b []
    [<Extension>]static member        Extend (s:'a seq, g) = Seq.map g (s |> Seq.toList |> List.tails |> List.toSeq |> Seq.map List.toSeq) :'b seq

    static member inline Invoke (g:'Comonad'T->'U) (s:'Comonad'T): 'Comonad'U =
        let inline call_4 (a:^a, b:^b, c:^c, d:^d) = ((^a or ^b or ^c) : (static member Extend: _*_ -> _) b, d)
        let inline call (a:'a, b:'b, x) = call_4 (a, b, Unchecked.defaultof<'r>, x) :'r
        call (Unchecked.defaultof<Extend>, s, g)

[<Extension;Sealed>]
type Duplicate =
    inherit Default1
    [<Extension>]static member inline Duplicate (x           , [<Optional>]impl:Default1 ) = Extend.Invoke id x
    [<Extension>]static member        Duplicate (s:Async<'a> , [<Optional>]impl:Duplicate) = async.Return s : Async<Async<'a>>
    [<Extension>]static member        Duplicate (s:Lazy<'a>  , [<Optional>]impl:Duplicate) = Lazy.CreateFromValue s : Lazy<Lazy<'a>>
    [<Extension>]static member        Duplicate ((w:'w, a:'a), [<Optional>]impl:Duplicate) = (w, (w, a))
    [<Extension>]static member inline Duplicate ( f:'m -> 'a , [<Optional>]impl:Duplicate) = fun a b -> f (Mappend.Invoke a b)

    // Restricted Comonads
    [<Extension>]static member        Duplicate (s: list<'a>, [<Optional>]impl:Duplicate) = List.tails s
    [<Extension>]static member        Duplicate (s:array<'a>, [<Optional>]impl:Duplicate) = s |> Array.toList |> List.tails |> List.toArray |> Array.map List.toArray
    
    static member inline Invoke x =
        let inline call_3 (a:^a, b:^b, c:^c) = ((^a or ^b or ^c) : (static member Duplicate: _*_ -> _) b, a)
        let inline call (a:'a, b:'b) = call_3 (a, b, Unchecked.defaultof<'r>) :'r
        call (Unchecked.defaultof<Duplicate>, x)


[<Extension;Sealed>]
type Contramap =
    [<Extension>]static member Contramap (g:_->_              , f       ) = (<<) g f
    [<Extension>]static member Contramap (p:Predicate<_>      , f       ) = Predicate(fun x -> p.Invoke(f x))
    [<Extension>]static member Contramap (Const x:Const<'t,'u>, _:'v->'u) = Const x:Const<'t,'v>

    static member inline Invoke (f:_->_) x = 
        let inline call_3 (a:^a, b:^b, c:^c, f) = ((^a or ^b or ^c) : (static member Contramap: _*_ -> _) b, f)
        let inline call (a:'a, b:'b, f) = call_3 (a, b, Unchecked.defaultof<'r>, f) :'r
        call (Unchecked.defaultof<Contramap>, x, f)


[<Extension;Sealed>]
type Bimap =
    [<Extension>]static member Bimap ((x, y)              ) = fun f g          -> (f x, g y)
    [<Extension>]static member Bimap (x:Choice<_,_>       ) = fun f g          -> choice (Choice2Of2 << f) (Choice1Of2 << g) x
    [<Extension>]static member Bimap (Const x:Const<'t,'u>) = fun f (_:'v->'w) -> Const (f x): Const<'v,'w>
    [<Extension>]static member Bimap (KeyValue(k, x)      ) = fun f g          -> KeyValuePair(f k, g x)
    
    static member inline Invoke x =
        let inline call_3 (a:^a, b:^b, c:^c) = ((^a or ^b or ^c) : (static member Bimap: _ -> _) b)
        let inline call (a:'a, b:'b) = call_3 (a, b, Unchecked.defaultof<'r>) :'r
        call (Unchecked.defaultof<Bimap>, x)

type First =
    inherit Default1
    static member inline       First (x                   , f       , [<Optional>]impl:Default1) = Bimap.Invoke x f id
    [<Extension>]static member First ((x, y)              , f       , [<Optional>]impl:First   ) = (f x, y)
    [<Extension>]static member First (x:Choice<_,_>       , f       , [<Optional>]impl:First   ) = choice (Choice2Of2 << f) Choice1Of2 x
    [<Extension>]static member First (Const x:Const<'t,'u>, f       , [<Optional>]impl:First   ) = Const (f x): Const<'v,'u>
    [<Extension>]static member First (KeyValue(k, x)      , f:'b->'c, [<Optional>]impl:First   ) = KeyValuePair(f k, x)
    
    static member inline Invoke f x =
        let inline call_3 (a:^a, b:^b, c:^c, f) = ((^a or ^b or ^c) : (static member First: _*_*_ -> _) b, f, a)
        let inline call (a:'a, b:'b, f) = call_3 (a, b, Unchecked.defaultof<'r>, f) :'r
        call (Unchecked.defaultof<First>, x, f)

type Second =
    inherit Default1
    static member inline       Second (x                   , f       , [<Optional>]impl:Default1) = Bimap.Invoke x id f
    [<Extension>]static member Second ((x, y)              , f       , [<Optional>]impl:Second  ) = (x, f y)
    [<Extension>]static member Second (x:Choice<_,_>       , f       , [<Optional>]impl:Second  ) = choice Choice2Of2 (Choice1Of2 << f) x
    [<Extension>]static member Second (Const x:Const<'t,'u>, _:'u->'v, [<Optional>]impl:Second  ) = Const x: Const<'t,'v>
    [<Extension>]static member Second (KeyValue(k, x)      , f:'b->'c, [<Optional>]impl:Second  ) = KeyValuePair(k, f x)
    
    static member inline Invoke f x =
        let inline call_3 (a:^a, b:^b, c:^c, f) = ((^a or ^b or ^c) : (static member Second: _*_*_ -> _) b, f, a)
        let inline call (a:'a, b:'b, f) = call_3 (a, b, Unchecked.defaultof<'r>, f) :'r
        call (Unchecked.defaultof<Second>, x, f)


[<Extension;Sealed>]
type Dimap =
    [<Extension>]static member inline Dimap (Kleisli bmc) = fun ab cd -> let cmd = Map.Invoke cd in Kleisli (cmd << bmc << ab)
    [<Extension>]static member        Dimap (f          ) = fun g h -> g >> f >> h
    
    static member inline Invoke x =
        let inline call_3 (a:^a, b:^b, c:^c) = ((^a or ^b or ^c) : (static member Dimap: _ -> _) b)
        let inline call (a:'a, b:'b) = call_3 (a, b, Unchecked.defaultof<'r>) :'r
        call (Unchecked.defaultof<Dimap>, x)

type Lmap =
    inherit Default1
    static member inline       Lmap (x             , f       , [<Optional>]impl:Default1) = Dimap.Invoke x f id
    [<Extension>]static member Lmap (f             , k       , [<Optional>]impl:Lmap    ) = k >> f
    [<Extension>]static member Lmap (Kleisli f     , k       , [<Optional>]impl:Lmap    ) = Kleisli (k >> f)
    
    static member inline Invoke f x =
        let inline call_3 (a:^a, b:^b, c:^c, f) = ((^a or ^b or ^c) : (static member Lmap: _*_*_ -> _) b, f, a)
        let inline call (a:'a, b:'b, f) = call_3 (a, b, Unchecked.defaultof<'r>, f) :'r
        call (Unchecked.defaultof<Lmap>, x, f)

type Rmap =
    inherit Default1
    static member inline       Rmap (x             , f       , [<Optional>]impl:Default1) = Dimap.Invoke x id f
    [<Extension>]static member Rmap (f             , k       , [<Optional>]impl:Rmap    ) = f >> k
    [<Extension>]static member Rmap (Kleisli f     , k       , [<Optional>]impl:Rmap    ) = Kleisli (f >> k)
    
    static member inline Invoke f x =
        let inline call_3 (a:^a, b:^b, c:^c, f) = ((^a or ^b or ^c) : (static member Rmap: _*_*_ -> _) b, f, a)
        let inline call (a:'a, b:'b, f) = call_3 (a, b, Unchecked.defaultof<'r>, f) :'r
        call (Unchecked.defaultof<Rmap>, x, f)


type Id =
    static member        Id (_: 'r -> 'r     , _:Id) = id              : 'r -> 'r
    static member inline Id (_:Kleisli<'a,'b>, _:Id) = Kleisli result :Kleisli<'a,'b>

    static member inline Invoke() =
        let inline call_2 (a:^a, b:^b) = ((^a or ^b) : (static member Id: _*_ -> _) b, a)
        let inline call (a:'a) = call_2 (a, Unchecked.defaultof<'r>) :'r
        call Unchecked.defaultof<Id>


type Comp =
    static member        Comp (        f, _, _:Comp) = fun (g: _ -> _) ->          g >>  f
    static member inline Comp (Kleisli f, _, _:Comp) = fun (Kleisli g) -> Kleisli (g >=> f)

    static member inline Invoke f g =
        let inline call_3 (a:^a, b:^b, c:^c) = ((^a or ^b or ^c) : (static member Comp: _*_*_ -> _) b, c, a)
        let inline call (a:'a, b:'b) = fun (x:'x) -> call_3 (a, b, Unchecked.defaultof<'r>) x :'r
        call (Unchecked.defaultof<Comp>, f) g


type Arr =
    static member        Arr (_: _ -> _     , _:Arr) = fun (f:_->_) -> f
    static member inline Arr (_:Kleisli<_,_>, _:Arr) = fun  f       -> Kleisli (Comp.Invoke result f)

    static member inline Invoke f = 
        let inline call_2 (a:^a, b:^b) = ((^a or ^b) : (static member Arr: _*_ -> _) b, a)
        let inline call   (a:'a) = fun (x:'x) -> call_2 (a, Unchecked.defaultof<'r>) x :'r
        call Unchecked.defaultof<Arr> f


type ArrFirst =
    static member        ArrFirst (f        , _: 'a -> 'b   , _:ArrFirst) = fun (x,y) -> (f x, y)
    static member inline ArrFirst (Kleisli f, _:Kleisli<_,_>, _:ArrFirst) = Kleisli (fun (b,d) -> f b >>= fun c -> result (c,d))

    static member inline Invoke f =
        let inline call_3 (a:^a, b:^b, c:^c) = ((^a or ^b or ^c) : (static member ArrFirst: _*_*_ -> _) b, c, a)
        let inline call (a:'a, b:'b) = call_3 (a, b, Unchecked.defaultof<'r>) :'r
        call (Unchecked.defaultof<ArrFirst>, f)


type ArrSecond =
    inherit Default1
    static member inline ArrSecond (f      , _               , _:Default1) = let aswap = Arr.Invoke (fun (x,y) -> (y,x)) in Comp.Invoke aswap (Comp.Invoke (ArrFirst.Invoke f) aswap)
    static member        ArrSecond (f        , _: 'a -> 'b   , _:ArrSecond  ) = fun (x,y) -> (x, f y)
    static member inline ArrSecond (Kleisli f, _:Kleisli<_,_>, _:ArrSecond  ) = Kleisli (fun (d,b) -> f b >>= fun c -> result (d,c))

    static member inline Invoke  f   =
        let inline call_3 (a:^a, b:^b, c:^c) = ((^a or ^b or ^c) : (static member ArrSecond: _*_*_ -> _) b, c, a)
        let inline call (a:'a, b:'b) = call_3 (a, b, Unchecked.defaultof<'r>) :'r
        call (Unchecked.defaultof<ArrSecond>, f)

type AcEither =
    static member inline AcEither (_:Choice<_,_>->_, _:AcEither) = fun (         f ,          g ) ->          choice f g
    static member inline AcEither (_:Kleisli<_,_>  , _:AcEither) = fun ((Kleisli f), (Kleisli g)) -> Kleisli (choice f g)

    static member inline Invoke f g =
        let inline call_2 (a:^a, b:^b) = ((^a or ^b) : (static member AcEither: _*_ -> _) b, a)
        let inline call (a:'a) = fun (x:'x) -> call_2 (a, Unchecked.defaultof<'r>) x :'r
        call Unchecked.defaultof<AcEither> (f, g)


type AcMerge =
    static member inline AcMerge (_: _->    Choice<_,_>      , _:AcMerge) = fun (f, g)  -> AcEither.Invoke (Choice2Of2 << f) (Choice1Of2 << g)
    static member inline AcMerge (_:Kleisli<Choice<'v,'t>,'z>, _:AcMerge) = fun ((Kleisli (f:'t->'u)), (Kleisli (g:'v->'w))) ->
        AcEither.Invoke (Kleisli (f >=> (Comp.Invoke result Choice2Of2))) (Kleisli (g >=> (Comp.Invoke result Choice1Of2))) :Kleisli<Choice<'v,'t>,'z>     

    static member inline Invoke f g =
        let inline call_2 (a:^a, b:^b) = ((^a or ^b) : (static member AcMerge: _*_ -> _) b, a)
        let inline call (a:'a) = fun (x:'x) -> call_2 (a, Unchecked.defaultof<'r>) x :'r
        call Unchecked.defaultof<AcMerge> (f, g)


type AcLeft =
    static member inline AcLeft (f:_->_   , _, _:AcLeft) = AcMerge.Invoke f id
    static member inline AcLeft (Kleisli f, _, _:AcLeft) =
        let inline (+++) a b = AcMerge.Invoke a b
        (+++) (Kleisli f) (Arr.Invoke (Id.Invoke()))

    static member inline Invoke    f   =
        let inline call_3 (a:^a, b:^b, c:^c) = ((^a or ^b or ^c) : (static member AcLeft: _*_*_ -> _) b, c, a)
        let inline call (a:'a, b:'b) = call_3 (a, b, Unchecked.defaultof<'r>) :'r
        call (Unchecked.defaultof<AcLeft>, f)


type AcRight =
    static member inline AcRight (f:_->_   , _, _:AcRight) = AcMerge.Invoke id f
    static member inline AcRight (Kleisli f, _, _:AcRight) =
        let inline (+++) a b = AcMerge.Invoke a b
        (+++) (Arr.Invoke (Id.Invoke())) (Kleisli f)

    static member inline Invoke   f   =
        let inline call_3 (a:^a, b:^b, c:^c) = ((^a or ^b or ^c) : (static member AcRight: _*_*_ -> _) b, c, a)
        let inline call (a:'a, b:'b) = call_3 (a, b, Unchecked.defaultof<'r>) :'r
        call (Unchecked.defaultof<AcRight>, f)


type ArrApply =
    static member ArrApply (_: ('a -> 'b) * 'a -> 'b          , _:ArrApply) =          fun (f, x)         -> f x
    static member ArrApply (_: Kleisli<Kleisli<'a,'b> * 'a,'b>, _:ArrApply) = Kleisli (fun (Kleisli f, x) -> f x)

    static member inline Invoke()     =
        let inline call_2 (a:^a, b:^b) = ((^a or ^b) : (static member ArrApply: _*_ -> _) b, a)
        let inline call (a:'a) = call_2 (a, Unchecked.defaultof<'r>) :'r
        call Unchecked.defaultof<ArrApply>