namespace FsControl

open System
open System.Runtime.CompilerServices
open System.Runtime.InteropServices
open System.Text
open System.Collections.Generic
#if NOTNET35
open System.Threading.Tasks
#endif
open Microsoft.FSharp.Quotations

open FsControl.Data
open FsControl.Internals
open FsControl.Internals.Prelude


// Monad class ------------------------------------------------------------

type Bind =
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
    static member inline Bind ((w, a):'m * 'a , k:_->'m * 'b   ) = let m, b = k a in (Append.Invoke w m, b)
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

    static member inline Invoke (source:'``Monad<'T>``) (binder:'T->'``Monad<'U>``) : '``Monad<'U>`` =
        let inline call (mthd : 'M, input : 'I, output : 'R, f) = ((^M or ^I or ^R) : (static member Bind: _*_ -> _) input, f)
        call (Unchecked.defaultof<Bind>, source, Unchecked.defaultof<'``Monad<'U>``>, binder)

    static member inline InvokeOnInstance (source:'``Monad<'T>``) (binder:'T->'``Monad<'U>``) : '``Monad<'U>`` =
        ((^``Monad<'T>`` or ^``Monad<'U>``) : (static member Bind: _*_ -> _) source, binder)


[<Extension;Sealed>]
type Join =
    inherit Default1
    [<Extension>]static member inline Join (x                   , [<Optional>]output:'R        , [<Optional>]impl:Default1) = Bind.Invoke x id :'R
    [<Extension>]static member        Join (x:Lazy<Lazy<'a>>    , [<Optional>]output:Lazy<'a>  , [<Optional>]impl:Join) = lazy x.Value.Value
    [<Extension>]static member        Join (x:option<option<'a>>, [<Optional>]output:option<'a>, [<Optional>]impl:Join) = Option.bind   id x
    [<Extension>]static member        Join (x:list<_>           , [<Optional>]output:list<'b>  , [<Optional>]impl:Join) = List.collect  id x
    [<Extension>]static member        Join (x:'b [] []          , [<Optional>]output:'b []     , [<Optional>]impl:Join) = Array.collect id x
    [<Extension>]static member        Join (x:Id<Id<'a>>        , [<Optional>]output:Id<'a>    , [<Optional>]impl:Join) = x.getValue

#if NOTNET35        
    [<Extension>]static member        Join (x:Task<Task<'a>>    , [<Optional>]output:Task<'a>  , [<Optional>]impl:Join) = x.Unwrap()
#endif

    static member inline Invoke (x:'``Monad<Monad<'T>>``) : '``Monad<'T>`` =
        let inline call (mthd : 'M, input : 'I, output : 'R) = ((^M or ^I or ^R) : (static member Join: _*_*_ -> _) input, output, mthd)
        call (Unchecked.defaultof<Join>, x, Unchecked.defaultof<'``Monad<'T>``>)


type Return =
    inherit Default1

    static member inline Invoke (x:'T) : '``Applicative<'T>`` =
        let inline call (mthd : ^M, output : ^R) = ((^M or ^R) : (static member Return: _*_ -> _) output, mthd)
        call (Unchecked.defaultof<Return>, Unchecked.defaultof<'``Applicative<'T>``>) x
 
    static member inline InvokeOnInstance (x:'T) = (^``Applicative<'T>`` : (static member Return: ^T -> ^``Applicative<'T>``) x)

    static member inline Return (r:'R, _:Default1) = fun (x:'T) -> Return.InvokeOnInstance x :'R

    static member        Return (_:Lazy<'a>, _:Return) = fun x -> Lazy.CreateFromValue x : Lazy<'a>
    static member        Return (_:seq<'a> , _:Return) = fun x -> Seq.singleton x :seq<'a>
    static member        Return (_:Id<'a>  , _:Return) = fun x -> Id x :Id<'a>
#if NOTNET35        
    static member        Return (_:'a Task , _:Return) = fun x -> 
        let s = TaskCompletionSource()
        s.SetResult x
        s.Task :'a Task
#endif        
    static member        Return (_:Identity<'t>  , _:Return) = fun x -> Identity x :Identity<'t>
    static member        Return (_:option<'a>    , _:Return) = fun x -> Some x      :option<'a>
    static member        Return (_:list<'a>      , _:Return) = fun x -> [ x ]       :list<'a>
    static member        Return (_:'a []         , _:Return) = fun x -> [|x|]       :'a []
    static member        Return (_:'r -> 'a      , _:Return) = const':'a  -> 'r -> _
    static member inline Return (_: 'm * 'a      , _:Return) = fun (x:'a) -> (Empty.Invoke():'m), x
    static member        Return (_:'a Async      , _:Return) = fun (x:'a) -> async.Return x
    static member        Return (_:Choice<'a,'e> , _:Return) = fun x -> Choice1Of2 x :Choice<'a,'e>
    static member        Return (_:Expr<'a>      , _:Return) = fun x -> Expr.Cast<'a>(Expr.Value(x:'a))
    static member        Return (_:'a ResizeArray, _:Return) = fun x -> ResizeArray<'a>(Seq.singleton x)

    //Restricted
    static member Return (_:string       , _:Return) = fun (x:char) -> string x : string
    static member Return (_:StringBuilder, _:Return) = fun (x:char) -> new StringBuilder(string x):StringBuilder
    static member Return (_:'a Set       , _:Return) = fun (x:'a  ) -> Set.singleton x

type Apply =
    inherit Default1
    
    static member inline Apply (f:'``Monad<'T->'U>``  , x:'``Monad<'T>``  , [<Optional>]output:'``Monad<'U>``  , [<Optional>]impl:Default2) : '``Monad<'U>``   = Bind.InvokeOnInstance f (fun (x1:'T->'U) -> Bind.InvokeOnInstance x (fun x2 -> Return.Invoke(x1 x2)))
    static member inline Apply (f:'``Applicative<'T->'U>``, x:'``Applicative<'T>``, [<Optional>]output:'``Applicative<'U>``, [<Optional>]impl:Default1) : '``Applicative<'U>`` = ((^``Applicative<'T->'U>`` or ^``Applicative<'T>`` or ^``Applicative<'U>``) : (static member (<*>): _*_ -> _) f, x)

    static member        Apply (f:Lazy<'T->'U>, x:Lazy<'T>     , [<Optional>]output:Lazy<'U>     , [<Optional>]impl:Apply) = Lazy.Create (fun () -> f.Value x.Value) : Lazy<'U>
    static member        Apply (f:seq<_>      , x:seq<'T>      , [<Optional>]output:seq<'U>      , [<Optional>]impl:Apply) = Seq.apply  f x :seq<'U>
    static member        Apply (f:list<_>     , x:list<'T>     , [<Optional>]output:list<'U>     , [<Optional>]impl:Apply) = List.apply f x :list<'U>
    static member        Apply (f:_ []        , x:'T []        , [<Optional>]output:'U []        , [<Optional>]impl:Apply) = Array.collect (fun x1 -> Array.collect (fun x2 -> [|x1 x2|]) x) f :'U []
    static member        Apply (f:'r -> _     , g: _ -> 'T     , [<Optional>]output: 'r -> 'U    , [<Optional>]impl:Apply) = fun x -> f x (g x) :'U
    static member inline Apply ((a:'Monoid, f), (b:'Monoid, x:'T), [<Optional>]output:'Monoid * 'U, [<Optional>]impl:Apply) = (Append.Invoke a b, f x) :'Monoid *'U
    static member        Apply (f:Async<_>    , x:Async<'T>    , [<Optional>]output:Async<'U>    , [<Optional>]impl:Apply) = async.Bind (f, fun x1 -> async.Bind (x, fun x2 -> async {return x1 x2})) :Async<'U>
    static member        Apply (f:option<_>   , x:option<'T>   , [<Optional>]output:option<'U>   , [<Optional>]impl:Apply) = Option.apply f x    :option<'U>
    static member        Apply (f:Choice<_,'E>, x:Choice<'T,'E>, [<Optional>]output:Choice<'b,'E>, [<Optional>]impl:Apply) = Error.apply f x :Choice<'U,'E>
    static member        Apply (KeyValue(k:'Key, f), KeyValue(k:'Key,x:'T), [<Optional>]output:KeyValuePair<'Key,'U>, [<Optional>]impl:Apply) :KeyValuePair<'Key,'U> = KeyValuePair(k, f x)

    static member        Apply (f:Map<'Key,_>       , x:Map<'Key,'T>       , [<Optional>]output:Map<'Key,'U>, [<Optional>]impl:Apply) :Map<'Key,'U>          = Map (seq {
       for KeyValue(k, vf) in f do
           match Map.tryFind k x with
           | Some vx -> yield k, vf vx
           | _       -> () })

    static member        Apply (f:Dictionary<'Key,_>, x:Dictionary<'Key,'T>, [<Optional>]output:Dictionary<'Key,'U>, [<Optional>]impl:Apply) :Dictionary<'Key,'U> =
       let d = Dictionary()
       for KeyValue(k, vf) in f do
           match x.TryGetValue k with
           | true, vx -> d.Add(k, vf vx)
           | _        -> ()
       d
    static member        Apply (Identity (f:'T->'U)     , Identity (x: 'T)     , [<Optional>]output:Identity<'U> , [<Optional>]impl:Apply) = Identity (f x)      : Identity<'U>
    static member inline Apply (Const f:Const<'C,'T->'U>, Const x: Const<'C,'T>, [<Optional>]output:Const<'C,'U> , [<Optional>]impl:Apply) = Const (Append.Invoke f x) : Const<'C,'U>

    static member        Apply (f:Expr<'T->'U>, x:Expr<'T>, [<Optional>]output:Expr<'U>, [<Optional>]impl:Apply) = Expr.Cast<'U>(Expr.Application(f,x))

    static member        Apply (f:('T->'U) ResizeArray, x:'T ResizeArray, [<Optional>]output:'U ResizeArray, [<Optional>]impl:Apply) =
       ResizeArray(Seq.collect (fun x1 -> Seq.collect (fun x2 -> Seq.singleton (x1 x2)) x) f) :'U ResizeArray

    static member inline Invoke (f:'``Applicative<'T -> 'U>``) (x:'``Applicative<'T>``) : '``Applicative<'U>`` =
        let inline call (mthd : ^M, input1 : ^I1, input2 : ^I2, output : ^R) =                                                          
            ((^M or ^I1 or ^I2 or ^R) : (static member Apply: _*_*_*_ -> _) input1, input2, output, mthd)
        call(Unchecked.defaultof<Apply>, f, x, Unchecked.defaultof<'``Applicative<'U>``>)

    static member inline InvokeOnInstance (f:'``Applicative<'T->'U>``) (x:'``Applicative<'T>``) : '``Applicative<'U>`` =
        ((^``Applicative<'T->'U>`` or ^``Applicative<'T>`` or ^``Applicative<'U>``) : (static member (<*>): _*_ -> _) (f, x))

// Functor class ----------------------------------------------------------

type Iterate =
    static member Iterate (x:Lazy<'T>  , action) = action x.Value :unit
    static member Iterate (x:seq<'T>   , action) = Seq.iter action x
    static member Iterate (x:option<'T>, action) = match x with Some x -> action x | _ -> ()
    static member Iterate (x:list<'T>  , action) = List.iter action x
    static member Iterate ((m:'W, a:'T), action) = action a :unit
    static member Iterate (x:'T []     , action) = Array.iter   action x
    static member Iterate (x:'T [,]    , action) = Array2D.iter action x
    static member Iterate (x:'T [,,]   , action) = Array3D.iter action x
    static member Iterate (x:'T [,,,]  , action) =
       for i = 0 to Array4D.length1 x - 1 do
           for j = 0 to Array4D.length2 x - 1 do
               for k = 0 to Array4D.length3 x - 1 do
                   for l = 0 to Array4D.length4 x - 1 do
                       action x.[i,j,k,l]
    static member Iterate (x:Async<'T>           , action) = action (Async.RunSynchronously x) : unit
    static member Iterate (x:Choice<'T,'E>       , action) = match x with Choice1Of2 x -> action x | _ -> ()
    static member Iterate (KeyValue(k:'Key, x:'T), action) = action x :unit
    static member Iterate (x:Map<'Key,'T>        , action) = Map.iter (const' action) x 
    static member Iterate (x:Dictionary<'Key,'T> , action) = Seq.iter action x.Values
    static member Iterate (x:_ ResizeArray       , action) = Seq.iter action x

    // Restricted
    static member Iterate (x:string         , action) = String.iter action x
    static member Iterate (x:StringBuilder  , action) = String.iter action (x.ToString())
    static member Iterate (x:Set<'T>        , action) = Set.iter action x        

    static member inline Invoke (action : 'T->unit) (source : '``Functor<'T>``) : unit =
        let inline call (mthd : ^M, source : ^I) =  ((^M or ^I) : (static member Iterate: _*_ -> _) source, action)
        call (Unchecked.defaultof<Iterate>, source)

type Map =
    inherit Default1

    static member inline Invoke (mapping :'T->'U) (source : '``Functor<'T>``) : '``Functor<'U>`` = 
        let inline call (mthd : ^M, source : ^I, output : ^R) = ((^M or ^I or ^R) : (static member Map: _*_*_ -> _) source, mapping, mthd)
        call (Unchecked.defaultof<Map>, source, Unchecked.defaultof<'``Functor<'U>``>)

    static member inline InvokeOnInstance (mapping :'T->'U) (source : '``Functor<'T>``) : '``Functor<'U>`` = 
        (^``Functor<'T>`` : (static member Map: _ * _ -> _) source, mapping)

    static member inline Map (x : '``Applicative<'T>``, f : 'T->'U, [<Optional>]impl:Default2) = Apply.InvokeOnInstance (Return.InvokeOnInstance f) x : '``Applicative<'U>``
    static member inline Map (x : '``Functor<'T>``    , f : 'T->'U, [<Optional>]impl:Default1) = Map.InvokeOnInstance f x : '``Functor<'U>``

    static member Map (x:Lazy<_>          , f:'T->'U, [<Optional>]impl:Map) = Lazy.Create (fun () -> f x.Value) : Lazy<'U>
    static member Map (x:seq<_>           , f:'T->'U, [<Optional>]impl:Map) = Seq.map f x :seq<'U>
    static member Map (x:option<_>        , f:'T->'U, [<Optional>]impl:Map) = Option.map  f x
    static member Map (x:list<_>          , f:'T->'U, [<Optional>]impl:Map) = List.map f x :list<'U>
    static member Map (g:'R->'T           , f:'T->'U, [<Optional>]impl:Map) = (>>) g f
    static member Map ((m:'W, a)          , f:'T->'U, [<Optional>]impl:Map) = (m, f a)
    static member Map (x:_ []             , f:'T->'U, [<Optional>]impl:Map) = Array.map   f x
    static member Map (x:_ [,]            , f:'T->'U, [<Optional>]impl:Map) = Array2D.map f x
    static member Map (x:_ [,,]           , f:'T->'U, [<Optional>]impl:Map) = Array3D.map f x
    static member Map (x:_ [,,,]          , f:'T->'U, [<Optional>]impl:Map) = Array4D.init (x.GetLength 0) (x.GetLength 1) (x.GetLength 2) (x.GetLength 3) (fun a b c d -> f x.[a,b,c,d])
    static member Map (x:Async<_>         , f:'T->'U, [<Optional>]impl:Map) = async.Bind(x, async.Return << f)
    static member Map (x:Choice<_,'E>     , f:'T->'U, [<Optional>]impl:Map) = Error.map f x
    static member Map (Identity x         , f:'T->'U, [<Optional>]impl:Map) = Identity (f x)
    static member Map (Const x:Const<_,'T>, f:'T->'U, [<Optional>]impl:Map) = Const x : Const<'C,'U>
    static member Map (KeyValue(k, x)     , f:'T->'U, [<Optional>]impl:Map) = KeyValuePair(k, f x)
    static member Map (x:Map<'Key,'T>     , f:'T->'U, [<Optional>]impl:Map) = Map.map (const' f) x : Map<'Key,'U>
    static member Map (x:Dictionary<_,_>  , f:'T->'U, [<Optional>]impl:Map) = let d = Dictionary() in Seq.iter (fun (KeyValue(k, v)) -> d.Add(k, f v)) x; d: Dictionary<'Key,'U>
    static member Map (x:Expr<'T>         , f:'T->'U, [<Optional>]impl:Map) = Expr.Cast<'U>(Expr.Application(Expr.Value(f),x))
    static member Map (x:ResizeArray<'T>  , f:'T->'U, [<Optional>]impl:Map) = ResizeArray(Seq.map f x) : ResizeArray<'U>
    static member Map (x:IObservable<'T>  , f:'T->'U, [<Optional>]impl:Map) = Observable.map f x

    // Restricted
    static member Map (x:string         , f, [<Optional>]impl:Map) = String.map f x
    static member Map (x:StringBuilder  , f, [<Optional>]impl:Map) = new StringBuilder(String.map f (x.ToString()))
    static member Map (x:Set<_>         , f, [<Optional>]impl:Map) = Set.map f x
        


type MZero =
    static member        MZero (output :option<'T>, mthd :MZero) = None        :option<'T>
    static member        MZero (output :list<'T>  , mthd :MZero) = [  ]        :list<'T>  
    static member        MZero (output :'T []     , mthd :MZero) = [||]        :'T []     
    static member        MZero (output :seq<'T>   , mthd :MZero) = Seq.empty   :seq<'T>
    static member inline MZero (output :Id<'T>    , mthd :MZero) = Id (Empty.Invoke()) :Id<'T>

    static member inline Invoke () : '``FunctorZero<'T>`` =
        let inline call (mthd : ^M, output : ^R) = ((^M or ^R) : (static member MZero: _*_ -> _) output, mthd)
        call (Unchecked.defaultof<MZero>, Unchecked.defaultof<'``FunctorZero<'T>``>)


[<Extension;Sealed>]
type MPlus =
    [<Extension>]static member        MPlus (x :'T option, y, [<Optional>]mthd :MPlus) = match x with None -> y | xs -> xs
    [<Extension>]static member        MPlus (x :'T list  , y, [<Optional>]mthd :MPlus) = x @ y
    [<Extension>]static member        MPlus (x :'T []    , y, [<Optional>]mthd :MPlus) = Array.append x y
    [<Extension>]static member        MPlus (x :'T seq   , y, [<Optional>]mthd :MPlus) = Seq.append   x y
    [<Extension>]static member inline MPlus (x :'T Id    , y, [<Optional>]mthd :MPlus) = Id (Append.Invoke (Id.run x) (Id.run y))

    static member inline Invoke (x:'``FunctorPlus<'T>``) (y:'``FunctorPlus<'T>``)  : '``FunctorPlus<'T>`` =
        let inline call (mthd : ^M, input1 : ^I, input2 : ^I) = ((^M or ^I) : (static member MPlus: _*_*_ -> _) input1, input2, mthd)
        call (Unchecked.defaultof<MPlus>, x, y)

type MZero with
    static member inline MZero (output :Kleisli<'T,'``Monad<'U>``>, mthd :MZero) = Kleisli (fun _ -> MZero.Invoke ())
    
type MPlus with
    static member inline MPlus (Kleisli f, Kleisli g, mthd:MPlus) = Kleisli (fun x -> MPlus.Invoke (f x) (g x))


namespace FsControl.Internals
module internal MonadOps =

    let inline (>>=) x f = FsControl.Bind.Invoke x f
    let inline result  x = FsControl.Return.Invoke x
    let inline (<*>) f x = FsControl.Apply.Invoke f x
    let inline (<|>) x y = FsControl.MPlus.Invoke x y
    let inline (>=>) (f:'a->'Monad'b) (g:'b->'Monad'c) (x:'a) :'Monad'c = f x >>= g


namespace FsControl

open System
open System.Runtime.CompilerServices
open System.Runtime.InteropServices
open System.Collections.Generic
#if NOTNET35
open System.Threading.Tasks
#endif
open FsControl.Data
open FsControl.Internals
open FsControl.Internals.Prelude
open FsControl.Internals.MonadOps

[<Extension;Sealed>]
type Extract =
    [<Extension>]static member        Extract (x:'t Async ) = Async.RunSynchronously x
    [<Extension>]static member        Extract (x:'t Lazy  ) = x.Value
    [<Extension>]static member        Extract ((w:'w,a:'a)) = a
    [<Extension>]static member inline Extract (f:'m->'t   ) = f (Empty.Invoke())
    [<Extension>]static member        Extract (f:'t Id    ) = f

#if NOTNET35
    [<Extension>]static member        Extract (f:'t Task  ) = f.Result
#endif

    static member inline Invoke (x:'Comonad'T): 'T =
        let inline call_2 (a:^a, b:^b) = ((^a or ^b) : (static member Extract: _ -> _) b)
        let inline call (a:'a, b:'b) = call_2 (a, b)
        call (Unchecked.defaultof<Extract>, x)

type Extend =
    static member        Extend ((g:'a Async), f:Async<'a>->'b) = async.Return (f g) : Async<'b>
    static member        Extend ((g:'a Lazy ), f:Lazy<'a> ->'b) = Lazy.Create  (fun () -> f g) : Lazy<'b>
    static member        Extend ((w:'w, a:'a), f:_->'b) = (w, f (w,a))        
    static member inline Extend ((g:'m -> 'a), f:_->'b) = fun a -> f (fun b -> g (Append.Invoke a b))
    static member        Extend ((g:'a Id   ), f:Id<'a>->'b) = f g

#if NOTNET35
    static member        Extend ((g:'a Task), f:Task<'a>->'b) = g.ContinueWith(f)
#endif

    // Restricted Comonads
    static member        Extend (s:list<'a>, g) = List.map g (List.tails s) :list<'b>
    static member        Extend (s:'a [] , g) = Array.map g (s |> Array.toList |> List.tails |> List.toArray |> Array.map List.toArray) :'b []
    static member        Extend (s:'a seq, g) = Seq.map g (s |> Seq.toList |> List.tails |> List.toSeq |> Seq.map List.toSeq) :'b seq

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
    [<Extension>]static member inline Duplicate ( f:'m -> 'a , [<Optional>]impl:Duplicate) = fun a b -> f (Append.Invoke a b)

    // Restricted Comonads
    [<Extension>]static member        Duplicate (s: list<'a>, [<Optional>]impl:Duplicate) = List.tails s
    [<Extension>]static member        Duplicate (s:array<'a>, [<Optional>]impl:Duplicate) = s |> Array.toList |> List.tails |> List.toArray |> Array.map List.toArray
    
    static member inline Invoke x =
        let inline call_3 (a:^a, b:^b, c:^c) = ((^a or ^b or ^c) : (static member Duplicate: _*_ -> _) b, a)
        let inline call (a:'a, b:'b) = call_3 (a, b, Unchecked.defaultof<'r>) :'r
        call (Unchecked.defaultof<Duplicate>, x)


type Contramap =
    static member Contramap (g:_->'R             , f:'U->'T) = (<<) g f
    static member Contramap (p:Predicate<_>      , f:'U->'T) = Predicate(fun x -> p.Invoke(f x))
    static member Contramap (Const x:Const<'C,'T>, _:'U->'T) = Const x :Const<'C,'U>

    static member inline Invoke (f:'U->'T) (x:'``Contravariant<'T>``) : '``Contravariant<'U>`` = 
        let inline call (mthd : ^M, source : ^I, c:^c, f) = ((^M or ^I or ^c) : (static member Contramap: _*_ -> _) source, f)
        call (Unchecked.defaultof<Contramap>, x, Unchecked.defaultof<'``Contravariant<'U>``>, f)


type Bimap =
    static member Bimap ((x, y)              ) = fun f g          -> (f x, g y)
    static member Bimap (x:Choice<_,_>       ) = fun f g          -> choice (Choice2Of2 << f) (Choice1Of2 << g) x
    static member Bimap (Const x:Const<'t,'u>) = fun f (_:'v->'w) -> Const (f x): Const<'v,'w>
    static member Bimap (KeyValue(k, x)      ) = fun f g          -> KeyValuePair(f k, g x)
    
    static member inline Invoke (x:'``Bifunctor<'T,'U>``) :'r =
        let inline call (mthd : ^M, source : ^I, c : ^c) = ((^M or ^I or ^c) : (static member Bimap: _ -> _) source)
        call (Unchecked.defaultof<Bimap>, x, Unchecked.defaultof<'r>)

type First =
    inherit Default1
    static member inline       First (x                   , f       , [<Optional>]mthd :Default1) = Bimap.Invoke x f id
    static member First ((x, y)              , f       , [<Optional>]mthd :First   ) = (f x, y)
    static member First (x:Choice<_,_>       , f       , [<Optional>]mthd :First   ) = choice (Choice2Of2 << f) Choice1Of2 x
    static member First (Const x:Const<'t,'u>, f       , [<Optional>]mthd :First   ) = Const (f x): Const<'v,'u>
    static member First (KeyValue(k, x)      , f:'b->'c, [<Optional>]mthd :First   ) = KeyValuePair(f k, x)
    
    static member inline Invoke f x :'r =
        let inline call (mthd : ^M, source : ^I, c:^c, f) = ((^M or ^I or ^c) : (static member First: _*_*_ -> _) source, f, mthd)
        call (Unchecked.defaultof<First>, x, Unchecked.defaultof<'r>, f)

type Second =
    inherit Default1
    static member inline       Second (x                   , f       , [<Optional>]mthd :Default1) = Bimap.Invoke x id f
    static member Second ((x, y)              , f       , [<Optional>]mthd :Second  ) = (x, f y)
    static member Second (x:Choice<_,_>       , f       , [<Optional>]mthd :Second  ) = choice Choice2Of2 (Choice1Of2 << f) x
    static member Second (Const x:Const<'t,'u>, _:'u->'v, [<Optional>]mthd :Second  ) = Const x: Const<'t,'v>
    static member Second (KeyValue(k, x)      , f:'b->'c, [<Optional>]mthd :Second  ) = KeyValuePair(k, f x)
    
    static member inline Invoke f x :'r =
        let inline call (mthd : ^M, source : ^I, c:^c, f) = ((^M or ^I or ^c) : (static member Second: _*_*_ -> _) source, f, mthd)
        call (Unchecked.defaultof<Second>, x, Unchecked.defaultof<'r>, f)


type Dimap =
    static member inline Dimap (Kleisli bmc :Kleisli<'B,'``Monad<'C>``>) = fun (ab:'A->'B) (cd:'C->'D) -> let cmd = Map.Invoke cd in Kleisli (ab >> bmc >> cmd) : Kleisli<'A,'``Monad<'D>``>
    static member        Dimap (f                                      ) = fun (g :'A->'B) (h :'C->'D) -> g >> f >> h
    
    static member inline Invoke (x : '``Profunctor<'B,'C>``) :'r =
        let inline call (mthd : ^M, source : ^I, c:^c) = ((^M or ^I or ^c) : (static member Dimap: _ -> _) source)
        call (Unchecked.defaultof<Dimap>, x, Unchecked.defaultof<'r>)

type LMap =
    inherit Default1
    static member inline       LMap (x :'``Profunctor<'B,'C>``            , f:'A->'B, [<Optional>]mthd :Default1) = Dimap.Invoke x f id : '``Profunctor<'A,'C>``
    static member LMap (f :'B->'C                            , k:'A->'B, [<Optional>]mthd :LMap    ) = k >> f              : 'A->'C
    static member LMap (Kleisli f :Kleisli<'B,'``Monad<'C>``>, k:'A->'B, [<Optional>]mthd :LMap    ) = Kleisli (k >> f)    : Kleisli<'A,'``Monad<'C>``>
    
    static member inline Invoke (f:'A->'B) (x :'``Profunctor<'B,'C>``) : '``Profunctor<'A,'C>`` =
        let inline call (mthd : ^M, source : ^I, output : ^R, f) = ((^M or ^I or ^R) : (static member LMap: _*_*_ -> _) source, f, mthd)
        call (Unchecked.defaultof<LMap>, x, Unchecked.defaultof<'``Profunctor<'A,'C>``>, f)

type RMap =
    inherit Default1
    static member inline              RMap (x :'``Profunctor<'B,'C>``            , f:'C->'D, [<Optional>]mthd :Default1) = Dimap.Invoke x id f          : '``Profunctor<'B,'D>``
    static member        RMap (f :'B->'C                            , k:'C->'D, [<Optional>]mthd :RMap    ) = f >> k                       : 'B->'D
    static member inline RMap (Kleisli f :Kleisli<'B,'``Monad<'C>``>, k:'C->'D, [<Optional>]mthd :RMap    ) = Kleisli (Map.Invoke k << f)  : Kleisli<'B,'``Monad<'D>``>
    
    static member inline Invoke (f:'C->'D) (x :'``Profunctor<'B,'C>``) : '``Profunctor<'B,'D>`` =
        let inline call (mthd : ^M, source : ^I, output : ^R, f) = ((^M or ^I or ^R) : (static member RMap: _*_*_ -> _) source, f, mthd)
        call (Unchecked.defaultof<RMap>, x, Unchecked.defaultof<'``Profunctor<'B,'D>``>, f)


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