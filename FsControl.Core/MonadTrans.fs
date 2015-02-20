namespace FsControl.Core.Types

open FsControl.Core
open FsControl.Core.Prelude
open FsControl.Core.TypeMethods
open FsControl.Core.TypeMethods.Monad

type OptionT<'Ma> = OptionT of 'Ma

[<RequireQualifiedAccess>]
module OptionT =
    let run   (OptionT m) = m
    let inline map  f (OptionT m) =  OptionT <| Map.Invoke (Option.map f) m
    let inline bind f (OptionT m) = (OptionT <| do'() {
        let! maybe_value = m
        return! match maybe_value with Some value -> run (f value) | _ -> result None}) :OptionT<'mb>
    let inline apply  (OptionT f) (OptionT x) = OptionT (Map.Invoke Option.apply f <*> x) :OptionT<'r>

type OptionT<'Ma> with
    static member inline Map    (_:Map   ,x :OptionT<'ma>, _  ) = fun (f:'a->'b) -> OptionT.map f x :OptionT<'mb>
    static member inline Return (_:Return, _:OptionT<'ma>     ) = OptionT << result << Some :'a -> OptionT<'ma>
    static member inline Apply  (_:Apply , f, x, _:OptionT<'r>) = OptionT.apply f x :OptionT<'r>
    static member inline Bind   (_:Bind  , x :OptionT<'ma>, _:OptionT<'mb>) = fun (f: 'a -> OptionT<'mb>) -> OptionT.bind f x :OptionT<'mb>

    static member inline Zero (_:Zero, _:OptionT<_>        ) = OptionT <| result None
    static member inline Plus (_:Plus, OptionT x, OptionT y) = OptionT <| do'() {
            let! maybe_value = x
            return! match maybe_value with Some value -> x | _ -> y}


type ListT<'Ma> = ListT of 'Ma

[<RequireQualifiedAccess>]
module ListT =
    let run (ListT m) = m
    let inline map  f (ListT m) =  ListT <| Map.Invoke (List.map f) m
    let inline bind f (ListT m) = (ListT (m >>= mapM (run << f) >>= ((List.concat:list<_>->_) >> result))) :ListT<'mb>
    let inline apply  (ListT f) (ListT x) = ListT (Map.Invoke List.apply f <*> x) :ListT<'r>

type ListT<'Ma> with
    static member inline Map    (_:Map      , x:ListT<'ma>, _    ) = fun (f:'a->'b) -> ListT.map f x :ListT<'mb>
    static member inline Return (_:Return ,        _:ListT<'ma>) = ListT << result << List.singleton :'a -> ListT<'ma>
    static member inline Apply  (_:Apply, f, x,  _:ListT<'r> ) = ListT.apply f x :ListT<'r>
    static member inline Bind   (_:Bind, x:ListT<'ma>, _:ListT<'mb>) = fun (f:'a -> ListT<'mb>) -> ListT.bind f x :ListT<'mb>

    static member inline Zero (_:Zero, _:ListT<_>      ) = ListT <| result []
    static member inline Plus (_:Plus, ListT x, ListT y) = ListT <| do'() {
        let! a = x
        let! b = y
        return (a @ b)}


type SeqT<'Ma> = SeqT of 'Ma

[<RequireQualifiedAccess>]
module SeqT =
    let run (SeqT m) = m
    let inline map f (SeqT m) = SeqT <| Map.Invoke (Seq.map f) m
    let inline internal mapM f as' = as' |> Seq.toList |> List.map f |> sequence |> Map.Invoke List.toSeq
    let inline bind  (f:'a -> SeqT<'mb>) (SeqT m:SeqT<'ma>) = SeqT (m >>= mapM (run << f) >>= ((Seq.concat:seq<seq<_>>->_) >> result)) :SeqT<'mb>
    let inline apply (SeqT f) (SeqT x) = SeqT (Map.Invoke Seq.apply f <*> x) :SeqT<'r>

type SeqT<'Ma> with
    static member inline Map    (_:Map   , x:SeqT<'ma>, _   ) = fun (f:'a->'b) -> SeqT.map f x :SeqT<'mb>
    static member inline Return (_:Return,       _:SeqT<'ma>) = SeqT << result << Seq.singleton :'a -> SeqT<'ma>
    static member inline Apply  (_:Apply , f, x, _:SeqT<'r> ) = SeqT.apply f x :SeqT<'r>
    static member inline Bind   (_:Bind  , x:SeqT<'ma>, _:SeqT<'mb>) = fun (f: 'a -> SeqT<'mb>) -> SeqT.bind f x :SeqT<'mb>

    static member inline Zero (_:Zero, _:SeqT<_>     ) = SeqT <| result Seq.empty
    static member inline Plus (_:Plus, SeqT x, SeqT y) = SeqT <| do'() {
        let! a = x
        let! b = y
        return (Seq.append a b)}



namespace FsControl.Core.TypeMethods

open FsControl.Core
open FsControl.Core.Prelude
open FsControl.Core.TypeMethods
open FsControl.Core.Types
open FsControl.Core.TypeMethods.Monad


// MonadTrans

type Lift() =
    static member val Instance = Lift()
    static member inline Lift (_:Lift, _:OptionT<'m_a>) = OptionT << (liftM Some)          :'ma -> OptionT<'m_a>
    static member inline Lift (_:Lift, _: ListT<'m_a> ) = ListT   << (liftM List.singleton):'ma ->  ListT<'m_a> 
    static member inline Lift (_:Lift, _: SeqT<'m_a>  ) = SeqT    << (liftM Seq.singleton ):'ma ->  SeqT<'m_a> 

    static member inline Invoke (x:'ma) = 
        let inline call_2 (a:^a, b:^b) = ((^a or ^b) : (static member Lift: _*_ -> _) a, b)
        let inline call (a:'a) = fun (x:'x) -> call_2 (a, Unchecked.defaultof<'r>) x :'r
        call Lift.Instance x


// MonadAsync

type LiftAsync() =    
    static member val Instance = LiftAsync()

    static member inline Invoke (x:Async<'T>) :'MonadAsync'T =
        let inline call_2 (a:^a, b:^b) = ((^a or ^b) : (static member LiftAsync: _*_ -> _) a, b)
        let inline call (a:'a) = fun (x:'x) -> call_2 (a, Unchecked.defaultof<'r>) x :'r
        call LiftAsync.Instance x

    static member inline LiftAsync (_:LiftAsync, _:OptionT<'U>) = fun (x :Async<'a>) -> Lift.Invoke (LiftAsync.Invoke x)
    static member inline LiftAsync (_:LiftAsync, _:ListT< 'U> ) = fun (x :Async<'a>) -> Lift.Invoke (LiftAsync.Invoke x)
    static member inline LiftAsync (_:LiftAsync, _:SeqT< 'U>  ) = fun (x :Async<'a>) -> Lift.Invoke (LiftAsync.Invoke x)
    static member        LiftAsync (_:LiftAsync, _:Async<'a>  ) = fun (x :Async<'a>) -> x


type ThrowError() =
    static member val Instance = ThrowError()

    static member inline Invoke (x:'e) :'ma =
        let inline call_2 (a:^a, b:^b) = ((^a or ^b) : (static member ThrowError: _*_ -> _) a, b)
        let inline call (a:'a) = fun (x:'x) -> call_2 (a, Unchecked.defaultof<'r>) x :'r
        call ThrowError.Instance x

    static member inline ThrowError (_:ThrowError, _:OptionT<'U>  ) = Lift.Invoke << ThrowError.Invoke
    static member inline ThrowError (_:ThrowError, _:ListT<'U>    ) = Lift.Invoke << ThrowError.Invoke
    static member inline ThrowError (_:ThrowError, _:SeqT<'U>     ) = Lift.Invoke << ThrowError.Invoke
    static member        ThrowError (_:ThrowError, _:Choice<'v,'e>) = Error.throw


type CatchError() =
    static member val Instance = CatchError()
    static member inline Invoke (v:'ma) (h:'e->'mb) :'mb =
        let inline call_3 (a:^a, b:^b, c:^c) = ((^a or ^b or ^c) : (static member CatchError: _*_*_ -> _) a, b, c)
        let inline call (a:'a, b:'b) = fun (x:'x) -> call_3 (a, b, Unchecked.defaultof<'r>) x :'r
        call (CatchError.Instance, v) h

    static member inline CatchError (_:CatchError,  m:OptionT<'U> , _:OptionT<'U>  ) = fun (h:'e -> OptionT<'U>) -> 
        OptionT ( (fun v h -> CatchError.Invoke v h) (OptionT.run m) (OptionT.run << h) ) :OptionT<'U>
    static member inline CatchError (_:CatchError,  m:ListT<'U> , _:ListT<'U>  ) = fun (h:'e -> ListT<'U>) -> 
        ListT ( (fun v h -> CatchError.Invoke v h) (ListT.run m) (ListT.run << h) ) :ListT<'U>
    static member inline CatchError (_:CatchError,  m:SeqT<'U> , _:SeqT<'U>  ) = fun (h:'e -> SeqT<'U>) -> 
        SeqT ( (fun v h -> CatchError.Invoke v h) (SeqT.run m) (SeqT.run << h) ) :SeqT<'U>
    static member        CatchError (_:CatchError, m:Choice<'v,'t>, _:Choice<'v,'e>) = fun (h:'t -> Choice<'v,'e>) -> Error.catch h m
    static member        CatchError (_:CatchError, m:'t * 'v, _:'e * 'v) = fun (h:'t -> 'e * 'v) -> h (fst m)



// MonadCont =
type CallCC() =
    static member val Instance = CallCC()
    static member CallCC (_:CallCC, _:OptionT<Cont<'r,option<'a>>>) = fun (f:((_ -> OptionT<Cont<_,'b>>) -> _)) -> OptionT(Cont.callCC <| fun c -> OptionT.run(f (OptionT << c << Some)))     :OptionT<Cont<'r,option<'a>>>
    static member CallCC (_:CallCC, _:ListT<Cont<'r ,  list<'a>>> ) = fun (f:((_ -> ListT<Cont<_,'b>>  ) -> _)) -> ListT  (Cont.callCC <| fun c ->   ListT.run(f (ListT << c << List.singleton))):ListT<Cont<'r, list<'a>>>
    static member CallCC (_:CallCC, _: SeqT<Cont<'r ,  seq<'a>>>  ) = fun (f:((_ -> SeqT<Cont<_,'b>>   ) -> _)) -> SeqT   (Cont.callCC <| fun c ->   SeqT.run (f (SeqT  << c << Seq.singleton ))):SeqT<Cont<'r ,  seq<'a>>>
    static member CallCC (_:CallCC, _:Cont<'r,'a>) = Cont.callCC : (('a -> Cont<'r,'b>) -> _) -> _

    static member inline Invoke f =
        let inline call_2 (a:^a, b:^b) = ((^a or ^b) : (static member CallCC: _*_ -> _) a, b)
        let inline call (a:'a) = fun (x:'x) -> call_2 (a, Unchecked.defaultof<'r>) x :'r
        call CallCC.Instance f


// MonadState =
type Get() =
    static member val Instance = Get()
    static member inline Get (_:Get, _:OptionT<_>) = Lift.Invoke (State.get())
    static member inline Get (_:Get, _:ListT<_>  ) = Lift.Invoke (State.get())
    static member inline Get (_:Get, _: SeqT<_>  ) = Lift.Invoke (State.get())
    static member        Get (_:Get, _:State<_,_>) =             (State.get())

    static member inline Invoke() :'ms =
        let inline call_2 (a:^a, b:^b) = ((^a or ^b) : (static member Get: _*_ -> _) a, b)
        let inline call (a:'a) = call_2 (a, Unchecked.defaultof<'r>) :'r
        call Get.Instance


type Put() =
    static member val Instance = Put()
    static member inline Put (_:Put, _:OptionT<_>) = Lift.Invoke << State.put
    static member inline Put (_:Put, _:ListT<_>  ) = Lift.Invoke << State.put
    static member inline Put (_:Put, _: SeqT<_>  ) = Lift.Invoke << State.put
    static member        Put (_:Put, _:State<_,_>) =                State.put

    static member inline Invoke (x:'s) :'m =
        let inline call_2 (a:^a, b:^b) = ((^a or ^b) : (static member Put: _*_ -> _) a, b)
        let inline call (a:'a) = fun (x:'x) -> call_2 (a, Unchecked.defaultof<'r>) x :'r
        call Put.Instance x


// MonadReader =
type Ask() =
    static member val Instance = Ask()
    static member Ask (_:Ask, _:OptionT<Reader<'a,option<'a>>>) = Lift.Invoke (Reader.ask()) :OptionT<Reader<'a,option<'a>>>
    static member Ask (_:Ask, _:ListT<Reader< 'a, list<  'a>>>) = Lift.Invoke (Reader.ask()) :  ListT<Reader<'a,  list<'a>>>
    static member Ask (_:Ask, _: SeqT<Reader< 'a,  seq<  'a>>>) = Lift.Invoke (Reader.ask()) :   SeqT<Reader<'a,   seq<'a>>>
    static member Ask (_:Ask, _:Reader<'r,'r>                 ) =             (Reader.ask()) :Reader<'r,'r>

    static member inline Invoke() :'mr =
        let inline call_2 (a:^a, b:^b) = ((^a or ^b) : (static member Ask: _*_ -> _) a, b)
        let inline call (a:'a) = call_2 (a, Unchecked.defaultof<'r>) :'r
        call Ask.Instance


type Local() =
    static member val Instance = Local()
    static member inline Local (_:Local, OptionT m, _:OptionT<_> ) = fun f -> OptionT <| Reader.local f m
    static member inline Local (_:Local,  ListT  m, _: ListT<_>  ) = fun f ->  ListT  <| Reader.local f m
    static member inline Local (_:Local,   SeqT  m, _:  SeqT<_>  ) = fun f ->   SeqT  <| Reader.local f m
    static member        Local (_:Local,         m, _:Reader<_,_>) = fun f ->            Reader.local f m

    static member inline Invoke (f:'rr) (m:'ma) :'ma =
        let inline call_2 (a:^a, b:^b) = ((^a or ^b) : (static member Local: _*_ -> _) a, b)
        let inline call (a:'a) = fun (x:'x) -> call_2 (a, Unchecked.defaultof<'r>) x :'r
        call (Local.Instance, m) f


// MonadWriter =
type Tell() =
    static member val Instance = Tell()
    static member inline Tell (_:Tell, _:OptionT<_> ) = Lift.Invoke << Writer.tell
    static member        Tell (_:Tell, _:Writer<_,_>) =                Writer.tell

    static member inline Invoke (x:'w) :'m =
        let inline call_2 (a:^a, b:^b) = ((^a or ^b) : (static member Tell: _*_ -> _) a, b)
        let inline call (a:'a) = fun (x:'x) -> call_2 (a, Unchecked.defaultof<'r>) x :'r
        call Tell.Instance x


type Listen() =
    static member val Instance = Listen()
    static member inline Listen (_:Listen, m, _:OptionT<_> ) =
        let liftMaybe (m, w) = Option.map (fun x -> (x, w)) m
        OptionT (Writer.listen (OptionT.run m) >>= (result << liftMaybe))
    static member        Listen (_:Listen, m, _:Writer<_,_>) = Writer.listen m

    static member inline Invoke (m:'ma) :'maw =
        let inline call_2 (a:^a, b:^b) = ((^a or ^b) : (static member Listen: _*_ -> _) a, b)
        let inline call (a:'a) = call_2 (a, Unchecked.defaultof<'r>) :'r
        call (Listen.Instance, m)


type Pass() =
    static member val Instance = Pass()
    static member inline Pass (_:Pass, m, _:OptionT<_> ) = OptionT (OptionT.run m >>= option (result None) (liftM Some << Writer.pass << result))
    static member        Pass (_:Pass, m, _:Writer<_,_>) = Writer.pass m

    static member inline Invoke (m:'maww) :'ma =
        let inline call_2 (a:^a, b:^b) = ((^a or ^b) : (static member Pass: _*_ -> _) a, b)
        let inline call (a:'a) = call_2 (a, Unchecked.defaultof<'r>) :'r
        call (Pass.Instance, m)