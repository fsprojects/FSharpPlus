namespace FsControl.Core.Types

open FsControl.Core
open FsControl.Core.Prelude
open FsControl.Core.TypeMethods
open FsControl.Core.TypeMethods.Functor
open FsControl.Core.TypeMethods.Applicative
open FsControl.Core.TypeMethods.Monad

type OptionT<'Ma> = OptionT of 'Ma

[<RequireQualifiedAccess>]
module OptionT =
    let run   (OptionT m) = m
    let inline map  f (OptionT m) =  OptionT <| fmap (Option.map f) m
    let inline bind f (OptionT m) = (OptionT <| do'() {
        let! maybe_value = m
        return! match maybe_value with Some value -> run (f value) | _ -> result None}) :OptionT<'mb>
    let inline apply  (OptionT f) (OptionT x) = OptionT (fmap Option.apply f <*> x) :OptionT<'r>

type OptionT<'Ma> with
    static member inline Map    (_:Functor.Map      ,x :OptionT<'ma>, _  ) = fun (f:'a->'b) -> OptionT.map f x :OptionT<'mb>
    static member inline Return (_:Applicative.Return , _:OptionT<'ma>     ) = OptionT << result << Some :'a -> OptionT<'ma>
    static member inline Apply  (_:Applicative.Apply, f, x, _:OptionT<'r>) = fun () -> OptionT.apply f x :OptionT<'r>
    static member inline Bind   (_:Monad.Bind  , x :OptionT<'ma>, _:OptionT<'mb>) = fun (f: 'a -> OptionT<'mb>) -> OptionT.bind f x :OptionT<'mb>

    static member inline Zero (_:Functor.Zero, _:OptionT<_>) = fun ()          -> OptionT <| result None
    static member inline Plus (_:Functor.Plus, OptionT x, _) = fun (OptionT y) -> OptionT <| do'() {
            let! maybe_value = x
            return! match maybe_value with Some value -> x | _ -> y}


type ListT<'Ma> = ListT of 'Ma

[<RequireQualifiedAccess>]
module ListT =
    let run (ListT m) = m
    let inline map  f (ListT m) =  ListT <| fmap (List.map f) m
    let inline bind f (ListT m) = (ListT (m >>= mapM (run << f) >>= ((List.concat:list<_>->_) >> result))) :ListT<'mb>
    let inline apply  (ListT f) (ListT x) = ListT (fmap List.apply f <*> x) :ListT<'r>

type ListT<'Ma> with
    static member inline Map    (_:Functor.Map      , x:ListT<'ma>, _    ) = fun (f:'a->'b) -> ListT.map f x :ListT<'mb>
    static member inline Return (_:Applicative.Return ,        _:ListT<'ma>) = ListT << result << List.singleton :'a -> ListT<'ma>
    static member inline Apply  (_:Applicative.Apply, f, x,  _:ListT<'r> ) = fun () -> ListT.apply f x :ListT<'r>
    static member inline Bind   (_:Monad.Bind, x:ListT<'ma>, _:ListT<'mb>) = fun (f:'a -> ListT<'mb>) -> ListT.bind f x :ListT<'mb>

    static member inline Zero (_:Functor.Zero, _:ListT<_>) = fun ()        -> ListT <| result []
    static member inline Plus (_:Functor.Plus, ListT x, _) = fun (ListT y) -> ListT <| do'() {
        let! a = x
        let! b = y
        return (a @ b)}


type SeqT<'Ma> = SeqT of 'Ma

[<RequireQualifiedAccess>]
module SeqT =
    let run (SeqT m) = m
    let inline map f (SeqT m) = SeqT <| fmap (Seq.map f) m
    let inline internal mapM f as' = as' |> Seq.toList |> List.map f |> sequence |> fmap List.toSeq
    let inline bind  (f:'a -> SeqT<'mb>) (SeqT m:SeqT<'ma>) = SeqT (m >>= mapM (run << f) >>= ((Seq.concat:seq<seq<_>>->_) >> result)) :SeqT<'mb>
    let inline apply (SeqT f) (SeqT x) = SeqT (fmap Seq.apply f <*> x) :SeqT<'r>

type SeqT<'Ma> with
    static member inline Map    (_:Functor.Map      , x:SeqT<'ma>, _   ) = fun (f:'a->'b) -> SeqT.map f x :SeqT<'mb>
    static member inline Return (_:Applicative.Return ,       _:SeqT<'ma>) = SeqT << result << Seq.singleton :'a -> SeqT<'ma>
    static member inline Apply  (_:Applicative.Apply, f, x, _:SeqT<'r> ) = fun () -> SeqT.apply f x :SeqT<'r>
    static member inline Bind   (_:Monad.Bind, x:SeqT<'ma>, _:SeqT<'mb>) = fun (f: 'a -> SeqT<'mb>) -> SeqT.bind f x :SeqT<'mb>

    static member inline Zero (_:Functor.Zero, _:SeqT<_>) = fun ()       -> SeqT <| result Seq.empty
    static member inline Plus (_:Functor.Plus, SeqT x, _) = fun (SeqT y) -> SeqT <| do'() {
        let! a = x
        let! b = y
        return (Seq.append a b)}



namespace FsControl.Core.TypeMethods

open FsControl.Core
open FsControl.Core.Prelude
open FsControl.Core.TypeMethods
open FsControl.Core.Types
open FsControl.Core.TypeMethods.Functor
open FsControl.Core.TypeMethods.Applicative
open FsControl.Core.TypeMethods.Monad

module MonadTrans = 
    type Lift() =
        static member inline Lift (_:Lift, _:OptionT<'m_a>) = OptionT << (liftM Some)          :'ma -> OptionT<'m_a>
        static member inline Lift (_:Lift, _: ListT<'m_a> ) = ListT   << (liftM List.singleton):'ma ->  ListT<'m_a> 
        static member inline Lift (_:Lift, _: SeqT<'m_a>  ) = SeqT    << (liftM Seq.singleton ):'ma ->  SeqT<'m_a> 

    let Lift = Lift()
    let inline internal lift (x:'ma) = 
        let inline instance_2 (a:^a, b:^b) = ((^a or ^b) : (static member Lift: _*_ -> _) a, b)
        let inline instance (a:'a) = fun (x:'x) -> instance_2 (a, Unchecked.defaultof<'r>) x :'r
        instance Lift x

open MonadTrans

module MonadAsync =
    let inline internal instance_2 (a:^a, b:^b) = ((^a or ^b) : (static member LiftAsync: _*_ -> _) a, b)
    let inline internal instance (a:'a) = fun (x:'x) -> instance_2 (a, Unchecked.defaultof<'r>) x :'r
    type LiftAsync() =
        static member val Instance = LiftAsync()
        static member inline LiftAsync (_:LiftAsync, _:OptionT<'U>) = fun (x :Async<'a>) -> lift (instance LiftAsync.Instance x)
        static member inline LiftAsync (_:LiftAsync, _:ListT< 'U> ) = fun (x :Async<'a>) -> lift (instance LiftAsync.Instance x)
        static member inline LiftAsync (_:LiftAsync, _:SeqT< 'U>  ) = fun (x :Async<'a>) -> lift (instance LiftAsync.Instance x)
        static member        LiftAsync (_:LiftAsync, _:Async<'a>  ) = fun (x :Async<'a>) -> x

    let inline internal liftAsync (x:Async<'T>) :'MonadAsync'T = instance LiftAsync.Instance x

module MonadError =
    let inline internal callThrowError_2 (a:^a, b:^b) = ((^a or ^b) : (static member ThrowError: _*_ -> _) a, b)
    let inline internal callThrowError (a:'a) = fun (x:'x) -> callThrowError_2 (a, Unchecked.defaultof<'r>) x :'r
    type ThrowError() =
        static member val Instance = ThrowError()
        static member inline ThrowError (_:ThrowError, _:OptionT<'U>  ) = lift << callThrowError ThrowError.Instance
        static member inline ThrowError (_:ThrowError, _:ListT<'U>    ) = lift << callThrowError ThrowError.Instance
        static member inline ThrowError (_:ThrowError, _:SeqT<'U>     ) = lift << callThrowError ThrowError.Instance
        static member        ThrowError (_:ThrowError, _:Choice<'v,'e>) = Error.throw

    let inline callCatchError_3 (a:^a, b:^b, c:^c) = ((^a or ^b or ^c) : (static member CatchError: _*_*_ -> _) a, b, c)
    let inline callCatchError (a:'a, b:'b) = fun (x:'x) -> callCatchError_3 (a, b, Unchecked.defaultof<'r>) x :'r
    type CatchError() =
        static member val Instance = CatchError()

        static member inline CatchError (_:CatchError,  m:OptionT<'U> , _:OptionT<'U>  ) = fun (h:'e -> OptionT<'U>) -> 
            OptionT ( (fun v h -> callCatchError (CatchError.Instance, v) h) (OptionT.run m) (OptionT.run << h) ) :OptionT<'U>
        static member inline CatchError (_:CatchError,  m:ListT<'U> , _:ListT<'U>  ) = fun (h:'e -> ListT<'U>) -> 
            ListT ( (fun v h -> callCatchError (CatchError.Instance, v) h) (ListT.run m) (ListT.run << h) ) :ListT<'U>
        static member inline CatchError (_:CatchError,  m:SeqT<'U> , _:SeqT<'U>  ) = fun (h:'e -> SeqT<'U>) -> 
            SeqT ( (fun v h -> callCatchError (CatchError.Instance, v) h) (SeqT.run m) (SeqT.run << h) ) :SeqT<'U>
        static member        CatchError (_:CatchError, m:Choice<'v,'t>, _:Choice<'v,'e>) = fun (h:'t -> Choice<'v,'e>) -> Error.catch h m
        static member        CatchError (_:CatchError, m:'t * 'v, _:'e * 'v) = fun (h:'t -> 'e * 'v) -> h (fst m)

    let inline throwError x   = callThrowError  ThrowError.Instance x
    let inline catchError v h = callCatchError (CatchError.Instance, v) h


module MonadCont =
    type CallCC() =
        static member CallCC (_:CallCC, _:OptionT<Cont<'r,option<'a>>>) = fun (f:((_ -> OptionT<Cont<_,'b>>) -> _)) -> OptionT(Cont.callCC <| fun c -> OptionT.run(f (OptionT << c << Some)))     :OptionT<Cont<'r,option<'a>>>
        static member CallCC (_:CallCC, _:ListT<Cont<'r ,  list<'a>>> ) = fun (f:((_ -> ListT<Cont<_,'b>>  ) -> _)) -> ListT  (Cont.callCC <| fun c ->   ListT.run(f (ListT << c << List.singleton))):ListT<Cont<'r, list<'a>>>
        static member CallCC (_:CallCC, _: SeqT<Cont<'r ,  seq<'a>>>  ) = fun (f:((_ -> SeqT<Cont<_,'b>>   ) -> _)) -> SeqT   (Cont.callCC <| fun c ->   SeqT.run (f (SeqT  << c << Seq.singleton ))):SeqT<Cont<'r ,  seq<'a>>>
        static member CallCC (_:CallCC, _:Cont<'r,'a>) = Cont.callCC : (('a -> Cont<'r,'b>) -> _) -> _

    let CallCC = CallCC()
    let inline internal callCC f =
        let inline instance_2 (a:^a, b:^b) = ((^a or ^b) : (static member CallCC: _*_ -> _) a, b)
        let inline instance (a:'a) = fun (x:'x) -> instance_2 (a, Unchecked.defaultof<'r>) x :'r
        instance CallCC f


module MonadState =
    type Get() =
        static member inline Get (_:Get, _:OptionT<_>) = fun () -> lift (State.get())
        static member inline Get (_:Get, _:ListT<_>  ) = fun () -> lift (State.get())
        static member inline Get (_:Get, _: SeqT<_>  ) = fun () -> lift (State.get())
        static member        Get (_:Get, _:State<_,_>) = fun () ->      (State.get())

    type Put() =
        static member inline Put (_:Put, _:OptionT<_>) = lift << State.put
        static member inline Put (_:Put, _:ListT<_>  ) = lift << State.put
        static member inline Put (_:Put, _: SeqT<_>  ) = lift << State.put
        static member        Put (_:Put, _:State<_,_>) =         State.put

    let Get, Put = Get(), Put()
    let inline internal get() =
        let inline instance_2 (a:^a, b:^b) = ((^a or ^b) : (static member Get: _*_ -> _) a, b)
        let inline instance (a:'a) = fun (x:'x) -> instance_2 (a, Unchecked.defaultof<'r>) x :'r
        instance Get ()

    let inline internal put x =
        let inline instance_2 (a:^a, b:^b) = ((^a or ^b) : (static member Put: _*_ -> _) a, b)
        let inline instance (a:'a) = fun (x:'x) -> instance_2 (a, Unchecked.defaultof<'r>) x :'r
        instance Put x


module MonadReader =
    type Ask() =
        static member Ask (_:Ask, _:OptionT<Reader<'a,option<'a>>>) = fun () -> lift (Reader.ask()) :OptionT<Reader<'a,option<'a>>>
        static member Ask (_:Ask, _:ListT<Reader< 'a, list<  'a>>>) = fun () -> lift (Reader.ask()) :  ListT<Reader<'a,  list<'a>>>
        static member Ask (_:Ask, _: SeqT<Reader< 'a,  seq<  'a>>>) = fun () -> lift (Reader.ask()) :   SeqT<Reader<'a,   seq<'a>>>
        static member Ask (_:Ask, _:Reader<'r,'r>                 ) = fun () ->      (Reader.ask()) :Reader<'r,'r>

    type Local() =
        static member inline Local (_:Local, OptionT m, _:OptionT<_> ) = fun f -> OptionT <| Reader.local f m
        static member inline Local (_:Local,  ListT  m, _: ListT<_>  ) = fun f ->  ListT  <| Reader.local f m
        static member inline Local (_:Local,   SeqT  m, _:  SeqT<_>  ) = fun f ->   SeqT  <| Reader.local f m
        static member        Local (_:Local,         m, _:Reader<_,_>) = fun f ->            Reader.local f m

    let Ask, Local = Ask(), Local()

    let inline internal ask()     =
        let inline instance_2 (a:^a, b:^b) = ((^a or ^b) : (static member Ask: _*_ -> _) a, b)
        let inline instance (a:'a) = fun (x:'x) -> instance_2 (a, Unchecked.defaultof<'r>) x :'r
        instance  Ask ()

    let inline internal local f m =
        let inline instance_2 (a:^a, b:^b) = ((^a or ^b) : (static member Local: _*_ -> _) a, b)
        let inline instance (a:'a) = fun (x:'x) -> instance_2 (a, Unchecked.defaultof<'r>) x :'r
        instance (Local, m) f


module MonadWriter =
    type Tell() =
        static member inline Tell (_:Tell, _:OptionT<_> ) = lift << Writer.tell
        static member        Tell (_:Tell, _:Writer<_,_>) =         Writer.tell

    type Listen() =
        static member inline Listen (_:Listen, m, _:OptionT<_> ) = fun () ->
            let liftMaybe (m, w) = Option.map (fun x -> (x, w)) m
            OptionT (Writer.listen (OptionT.run m) >>= (result << liftMaybe))
        static member        Listen (_:Listen, m, _:Writer<_,_>) = fun () -> Writer.listen m

    type Pass() =
        static member inline Pass (_:Pass, m, _:OptionT<_> ) = fun () -> OptionT (OptionT.run m >>= option (result None) (liftM Some << Writer.pass << result))
        static member        Pass (_:Pass, m, _:Writer<_,_>) = fun () -> Writer.pass m

    let Tell, Listen, Pass = Tell(), Listen(), Pass()

    let inline internal tell   x =
        let inline instance_2 (a:^a, b:^b) = ((^a or ^b) : (static member Tell: _*_ -> _) a, b)
        let inline instance (a:'a) = fun (x:'x) -> instance_2 (a, Unchecked.defaultof<'r>) x :'r
        instance  Tell x

    let inline internal listen m =
        let inline instance_2 (a:^a, b:^b) = ((^a or ^b) : (static member Listen: _*_ -> _) a, b)
        let inline instance (a:'a) = fun (x:'x) -> instance_2 (a, Unchecked.defaultof<'r>) x :'r
        instance  (Listen, m) ()

    let inline internal pass   m =
        let inline instance_2 (a:^a, b:^b) = ((^a or ^b) : (static member Pass: _*_ -> _) a, b)
        let inline instance (a:'a) = fun (x:'x) -> instance_2 (a, Unchecked.defaultof<'r>) x :'r
        instance (Pass  , m) ()