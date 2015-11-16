namespace FsControl

open FsControl.Core.Internals
open FsControl.Core.Internals.Prelude
open FsControl.Core.Internals.MonadOps

type OptionT<'Ma> = OptionT of 'Ma

[<RequireQualifiedAccess>]
module OptionT =
    let run   (OptionT m) = m
    let inline map  f (OptionT m) =  OptionT <| Map.Invoke (Option.map f) m
    let inline bind f (OptionT m) = (OptionT <| (m  >>= (fun maybe_value -> match maybe_value with Some value -> run (f value) | _ -> result None))) :OptionT<'mb>
    let inline apply  (OptionT f) (OptionT x) = OptionT (Map.Invoke Option.apply f <*> x) :OptionT<'r>

type OptionT<'Ma> with
    static member inline Map    (x :OptionT<'ma>, (f:'a->'b)  , _:Map   ) = OptionT.map f x :OptionT<'mb>
    static member inline Return ( _:OptionT<'ma>     , _:Return) = OptionT << result << Some :'a -> OptionT<'ma>
    static member inline Apply  (f , x, _:OptionT<'r>, _:Apply ) = OptionT.apply f x :OptionT<'r>
    static member inline Bind   (x :OptionT<'ma>, f: 'a -> OptionT<'mb>) = OptionT.bind f x :OptionT<'mb>

    static member inline MZero (_:OptionT<_>        , _:MZero) = OptionT <| result None
    static member inline MPlus (OptionT x, OptionT y, _:MPlus) = OptionT <| (x  >>= (fun maybe_value -> match maybe_value with Some value -> x | _ -> y))


type ListT<'Ma> = ListT of 'Ma

[<RequireQualifiedAccess>]
module ListT =
    let run (ListT m) = m

    let inline internal sequence ms =
        let k m m' = m >>= fun (x:'a) -> m' >>= fun xs -> (result :list<'a> -> 'M) (x::xs)
        List.foldBack k ms ((result :list<'a> -> 'M) [])
    
    let inline internal mapM f as' = sequence (List.map f as')

    let inline map  f (ListT m) =  ListT <| Map.Invoke (List.map f) m
    let inline bind f (ListT m) = (ListT (m >>= mapM (run << f) >>= ((List.concat:list<_>->_) >> result))) :ListT<'mb>
    let inline apply  (ListT f) (ListT x) = ListT (Map.Invoke List.apply f <*> x) :ListT<'r>

type ListT<'Ma> with
    static member inline Map    (x:ListT<'ma>, (f:'a->'b)    , _:Map   ) = ListT.map f x :ListT<'mb>
    static member inline Return (       _:ListT<'ma>, _:Return) = ListT << result << List.singleton :'a -> ListT<'ma>
    static member inline Apply  (f, x,  _:ListT<'r> , _:Apply ) = ListT.apply f x :ListT<'r>
    static member inline Bind   (x:ListT<'ma>, f:'a -> ListT<'mb>) = ListT.bind f x :ListT<'mb>

    static member inline MZero (_:ListT<_>      , _:MZero) = ListT <| result []
    static member inline MPlus (ListT x, ListT y, _:MPlus) = ListT <| (x >>= (fun a -> y >>= (fun b ->  result (a @ b ))))


type SeqT<'Ma> = SeqT of 'Ma

[<RequireQualifiedAccess>]
module SeqT =
    let run (SeqT m) = m

    let inline internal sequence ms =
        let k m m' = m >>= fun (x:'a) -> m' >>= fun (xs:seq<'a>) -> (result :seq<'a> -> 'M) (seq {yield x; yield! xs})
        Seq.foldBack k ms ((result :seq<'a> -> 'M) Seq.empty)

    let inline internal mapM f as' = sequence (Seq.map f as')

    let inline map f (SeqT m) = SeqT <| Map.Invoke (Seq.map f: (seq<_>->_)) m
    let inline bind  (f:'a -> SeqT<'mb>) (SeqT m:SeqT<'ma>) = SeqT (m >>= (mapM:_->seq<_>->_) (run << f) >>= ((Seq.concat:seq<seq<_>>->_) >> result)) :SeqT<'mb>
    let inline apply (SeqT f) (SeqT x) = SeqT (Map.Invoke (Seq.apply:seq<_->_>->seq<_>->seq<_>) f <*> x) :SeqT<'r>

type SeqT<'Ma> with
    static member inline Map    (x:SeqT<'ma>, (f:'a->'b)   , _:Map   ) =  SeqT.map f x :SeqT<'mb>
    static member inline Return (      _:SeqT<'ma>, _:Return) = SeqT << result << Seq.singleton :'a -> SeqT<'ma>
    static member inline Apply  (f, x, _:SeqT<'r> , _:Apply ) = SeqT.apply f x :SeqT<'r>
    static member inline Bind   (x:SeqT<'ma>, f: 'a -> SeqT<'mb>) = SeqT.bind f x :SeqT<'mb>

    static member inline MZero (_:MZero, _:SeqT<_>     ) = SeqT <| result Seq.empty
    static member inline MPlus (_:MPlus, SeqT x, SeqT y) = SeqT <| (x >>= (fun a -> y >>= (fun b ->  result ((Seq.append:seq<_>->seq<_>->_) a b))))




// MonadTrans

type OptionT<'Ma> with static member inline Lift (x:'ma) = x |> (Map.FromMonad Some)           |> OptionT : OptionT<'m_a>
type ListT<'Ma>   with static member inline Lift (x:'ma) = x |> (Map.FromMonad List.singleton) |> ListT   :  ListT<'m_a> 
type SeqT<'Ma>    with static member inline Lift (x:'ma) = x |> (Map.FromMonad Seq.singleton ) |> SeqT    :  SeqT<'m_a> 

type Lift =
    static member inline Invoke (x:'ma) = (^R : (static member Lift: _ -> ^R) x)


// MonadAsync

type LiftAsync =

    static member inline Invoke (x:Async<'T>) :'MonadAsync'T =
        let inline call_2 (a:^a, b:^b) = ((^a or ^b) : (static member LiftAsync: _ -> _) b)
        let inline call (a:'a) = fun (x:'x) -> call_2 (a, Unchecked.defaultof<'r>) x :'r
        call Unchecked.defaultof<LiftAsync> x

    static member inline LiftAsync (_:OptionT<'U>) = fun (x :Async<'a>) -> Lift.Invoke (LiftAsync.Invoke x)
    static member inline LiftAsync (_:ListT< 'U> ) = fun (x :Async<'a>) -> Lift.Invoke (LiftAsync.Invoke x)
    static member inline LiftAsync (_:SeqT< 'U>  ) = fun (x :Async<'a>) -> Lift.Invoke (LiftAsync.Invoke x)
    static member        LiftAsync (_:Async<'a>  ) = fun (x :Async<'a>) -> x


type ThrowError =

    static member inline Invoke (x:'e) :'ma =
        let inline call_2 (a:^a, b:^b) = ((^a or ^b) : (static member ThrowError: _ -> _) b)
        let inline call (a:'a) = fun (x:'x) -> call_2 (a, Unchecked.defaultof<'r>) x :'r
        call Unchecked.defaultof<ThrowError> x

    static member inline ThrowError (_:OptionT<'U>  ) = Lift.Invoke << ThrowError.Invoke
    static member inline ThrowError (_:ListT<'U>    ) = Lift.Invoke << ThrowError.Invoke
    static member inline ThrowError (_:SeqT<'U>     ) = Lift.Invoke << ThrowError.Invoke
    static member        ThrowError (_:Choice<'v,'e>) = Error.throw


type CatchError =
    static member inline Invoke (v:'ma) (h:'e->'mb) :'mb =
        let inline call_3 (a:^a, b:^b, c:^c) = ((^a or ^b or ^c) : (static member CatchError: _*_ -> _) b, c)
        let inline call (a:'a, b:'b) = fun (x:'x) -> call_3 (a, b, Unchecked.defaultof<'r>) x :'r
        call (Unchecked.defaultof<CatchError>, v) h

    static member inline CatchError (m:OptionT<'U>  , _:OptionT<'U>  ) = fun (h:'e -> OptionT<'U>) -> OptionT ( (fun v h -> CatchError.Invoke v h) (OptionT.run m) (OptionT.run << h) ) :OptionT<'U>
    static member inline CatchError (m:ListT<'U>    , _:ListT<'U>    ) = fun (h:'e -> ListT<'U>)   -> ListT   ( (fun v h -> CatchError.Invoke v h) (ListT.run   m) (ListT.run   << h) ) :ListT<'U>
    static member inline CatchError (m:SeqT<'U>     , _:SeqT<'U>     ) = fun (h:'e -> SeqT<'U>)    -> SeqT    ( (fun v h -> CatchError.Invoke v h) (SeqT.run    m) (SeqT.run    << h) ) :SeqT<'U>
    static member        CatchError (m:Choice<'v,'t>, _:Choice<'v,'e>) = fun (h:'t -> Choice<'v,'e>) -> Error.catch h m
    static member        CatchError (m:'t * 'v      , _:'e * 'v      ) = fun (h:'t -> 'e * 'v) -> h (fst m)



// MonadCont =
type OptionT<'Ma> with static member CallCC (f:((_ -> OptionT<Cont<_,'b>>) -> _)) = OptionT(Cont.callCC <| fun c -> OptionT.run(f (OptionT << c << Some)))     :OptionT<Cont<'r,option<'a>>>
type ListT<'Ma>   with static member CallCC (f:((_ -> ListT<Cont<_,'b>>  ) -> _)) = ListT  (Cont.callCC <| fun c ->   ListT.run(f (ListT << c << List.singleton))):ListT<Cont<'r, list<'a>>>
type SeqT<'Ma>    with static member CallCC (f:((_ -> SeqT<Cont<_,'b>>   ) -> _)) = SeqT   (Cont.callCC <| fun c ->   SeqT.run (f (SeqT  << c << Seq.singleton ))):SeqT<Cont<'r ,  seq<'a>>>

type CallCC =
    static member inline Invoke f = (^R : (static member CallCC: _ -> ^R) f)


// MonadState =
type OptionT<'Ma> with static member inline get_Get() = Lift.Invoke State.get :OptionT<_>
type ListT<'Ma>   with static member inline get_Get() = Lift.Invoke State.get :ListT<_>  
type SeqT<'Ma>    with static member inline get_Get() = Lift.Invoke State.get : SeqT<_>  

type Get =
    static member inline Invoke() :^R = (^R : (static member Get: ^R) ())


type OptionT<'Ma> with static member inline Put x = x |> State.put |> Lift.Invoke :OptionT<_>
type ListT<'Ma>   with static member inline Put x = x |> State.put |> Lift.Invoke :ListT<_>  
type SeqT<'Ma>    with static member inline Put x = x |> State.put |> Lift.Invoke : SeqT<_>  
   
type Put =
    static member inline Invoke (x:'s) :^R = (^R : (static member Put: _ -> ^R) x)


// MonadReader =
type OptionT<'Ma> with static member get_Ask() = Lift.Invoke Reader.ask :OptionT<Reader<'a,option<'a>>>
type ListT<'Ma>   with static member get_Ask() = Lift.Invoke Reader.ask :  ListT<Reader<'a,  list<'a>>>
type SeqT<'Ma>    with static member get_Ask() = Lift.Invoke Reader.ask :   SeqT<Reader<'a,   seq<'a>>>

type Ask =
    static member inline Invoke() :^R = (^R : (static member Ask: ^R) ())


type OptionT<'Ma> with static member inline Local (OptionT m, f) = OptionT <| Reader.local f m
type ListT<'Ma>   with static member inline Local ( ListT  m, f) =  ListT  <| Reader.local f m
type SeqT<'Ma>    with static member inline Local (  SeqT  m, f) =   SeqT  <| Reader.local f m   

type Local =
    static member inline Invoke (f:'rr) (m:^R) :^R = (^R : (static member Local: _*_ -> ^R) m, f)


// MonadWriter =
type OptionT<'Ma> with static member inline Tell x = x |> Writer.tell |> Lift.Invoke :OptionT<_>
    
type Tell =
    static member inline Invoke (x:'w) :^R = (^R : (static member Tell: _ -> ^R) x)


type OptionT<'Ma> with
    static member inline Listen m =
        let liftMaybe (m, w) = Option.map (fun x -> (x, w)) m
        OptionT (Writer.listen (OptionT.run m) >>= (result << liftMaybe))

type Listen =
    static member inline Invoke (m:'ma) :^R = (^R : (static member Listen: _ -> ^R) m)


type OptionT<'Ma> with static member inline Pass m = OptionT (OptionT.run m >>= option (result None) (Map.Invoke Some << Writer.pass << result))

type Pass =
    static member inline Invoke (m:'maww) :^R = (^R : (static member Pass: _ -> ^R) m)