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
    let inline map f (OptionT m) = OptionT (fmap (Option.map f) m)
    let inline bind f (OptionT m) = (OptionT <| do'() {
        let! maybe_value = m
        return! match maybe_value with Some value -> run (f value) | _ -> return' None}) :OptionT<'mb>

type OptionT<'Ma> with
    static member inline instance (_:Functor.Map  , x :OptionT<'ma>, _) = fun (f:'a->'b) -> OptionT.map f x :OptionT<'mb>
    static member inline instance (Applicative.Pure,            _:OptionT<'ma>) = OptionT << return' << Some :'a -> OptionT<'ma>
    static member inline instance (_:Applicative.Apply, OptionT(f), OptionT(x),  _:OptionT<'r>) = fun () ->
        OptionT(fmap (<*>) f <*> x) :OptionT<'r>
    static member inline instance (Monad.Bind  , x :OptionT<'ma>, _:OptionT<'mb>) = 
        fun (f: 'a -> OptionT<'mb>) -> OptionT.bind f x :OptionT<'mb>

    static member inline instance (MonadPlus.Mzero, _:OptionT<_>) = fun ()          -> OptionT (return' None)
    static member inline instance (MonadPlus.Mplus, OptionT x, _) = fun (OptionT y) -> OptionT <| do'() {
            let! maybe_value = x
            return! match maybe_value with Some value -> x | _ -> y}


type ListT<'Ma> = ListT of 'Ma

[<RequireQualifiedAccess>]
module ListT =
    let run   (ListT m) = m
    let inline map f (ListT m) = ListT (fmap (List.map f) m)
    let inline bind f (ListT m) = (ListT (m >>= mapM (run << f) >>= (List.concat >> return'))) :ListT<'mb>

type ListT<'Ma> with
    static member inline instance (_:Functor.Map   , x:ListT<'ma>, _) = fun (f:'a->'b) -> ListT.map f x :ListT<'mb>
    static member inline instance (Applicative.Pure,           _:ListT<'ma>) = ListT << return' << List.singleton :'a -> ListT<'ma>
    static member inline instance (_:Applicative.Apply, ListT(f), ListT(x),  _:ListT<'r>) = fun () ->
        ListT(fmap (<*>) f <*> x) :ListT<'r>
    static member inline instance (Monad.Bind, x:ListT<'ma>, _:ListT<'mb>) = fun (f:'a -> ListT<'mb>) -> ListT.bind f x :ListT<'mb>

    static member inline instance (MonadPlus.Mzero, _:ListT<_>) = fun ()        -> ListT (return' [])
    static member inline instance (MonadPlus.Mplus, ListT x, _) = fun (ListT y) -> ListT <| do'() {
        let! a = x
        let! b = y
        return (a @ b)}


type SeqT<'Ma> = SeqT of 'Ma

[<RequireQualifiedAccess>]
module SeqT =
    let run   (SeqT m) = m
    let inline map f (SeqT m) = SeqT (fmap (Seq.map f) m)

    let inline internal sequence (ms:seq<_>) =
        let ms = Seq.toList ms
        let k m m' = m >>= fun (x:'a) -> m' >>= fun xs -> (pure' :list<'a> -> 'M) (List.Cons(x,xs))
        List.foldBack k ms ((pure' :list<'a> -> 'M) [])

    let inline internal mapM f as' = sequence (Seq.map f as')
    let inline bind (f:'a -> SeqT<'mb>) (SeqT m:SeqT<'ma>) = SeqT (m >>= mapM (run << f) >>= (Seq.concat >> return')) :SeqT<'mb>

type SeqT<'Ma> with
    static member inline instance (_:Functor.Map   , x:SeqT<'ma>, _) = fun (f:'a->'b) -> SeqT.map f x :SeqT<'mb>
    static member inline instance (Applicative.Pure,           _:SeqT<'ma>) = SeqT << return' << Seq.singleton :'a -> SeqT<'ma>
    static member inline instance (_:Applicative.Apply, SeqT(f), SeqT(x),  _:SeqT<'r>) = fun () ->
        SeqT(fmap (<*>) f <*> x) :SeqT<'r>
    static member inline instance (Monad.Bind, x:SeqT<'ma>, _:SeqT<'mb>) = fun (f: 'a -> SeqT<'mb>) -> SeqT.bind f x :SeqT<'mb>

    static member inline instance (MonadPlus.Mzero, _:SeqT<_>) = fun ()       -> SeqT (return' Seq.empty)
    static member inline instance (MonadPlus.Mplus, SeqT x, _) = fun (SeqT y) -> SeqT <| do'() {
        let! a = x
        let! b = y
        return (Seq.append a b)}



namespace FsControl.Core.TypeMethods

open FsControl.Core
open FsControl.Core.Prelude
open FsControl.Core.TypeMethods
open FsControl.Core.Types
open FsControl.Core.TypeMethods.Functor
open FsControl.Core.TypeMethods.Monad

module MonadTrans = 
    type Lift = Lift with
        static member inline instance (Lift, _:OptionT<'m_a>) = OptionT << (liftM Some)          :'ma -> OptionT<'m_a>
        static member inline instance (Lift, _: ListT<'m_a> ) = ListT   << (liftM List.singleton):'ma ->  ListT<'m_a> 
        static member inline instance (Lift, _: SeqT<'m_a>  ) = SeqT    << (liftM Seq.singleton ):'ma ->  SeqT<'m_a> 

    let inline internal lift (x:'ma) = Inline.instance Lift x

open MonadTrans

module MonadAsync =
    type LiftAsync = LiftAsync with  
        static member inline instance (LiftAsync, _:OptionT<'U>) = fun (x :Async<'a>) -> lift (Inline.instance LiftAsync x)
        static member inline instance (LiftAsync, _:ListT< 'U> ) = fun (x :Async<'a>) -> lift (Inline.instance LiftAsync x)
        static member inline instance (LiftAsync, _:SeqT< 'U>  ) = fun (x :Async<'a>) -> lift (Inline.instance LiftAsync x)
        static member        instance (LiftAsync, _:Async<'a>  ) = fun (x :Async<'a>) -> x

    let inline internal liftAsync (x: Async<'a>) = Inline.instance LiftAsync x



module MonadError =
    type ThrowError = ThrowError with
        static member inline instance (ThrowError, _:OptionT<'U>  ) = lift << Inline.instance ThrowError
        static member inline instance (ThrowError, _:ListT<'U>    ) = lift << Inline.instance ThrowError
        static member inline instance (ThrowError, _:SeqT<'U>     ) = lift << Inline.instance ThrowError
        static member        instance (ThrowError, _:Choice<'v,'e>) = Error.throw

    type CatchError = CatchError with
        static member inline instance (CatchError,  m:OptionT<'U> , _:OptionT<'U>  ) = fun (h:'e -> OptionT<'U>) -> 
            OptionT ( (fun v h -> Inline.instance (CatchError, v) h) (OptionT.run m) (OptionT.run << h) ) :OptionT<'U>
        static member inline instance (CatchError,  m:ListT<'U> , _:ListT<'U>  ) = fun (h:'e -> ListT<'U>) -> 
            ListT ( (fun v h -> Inline.instance (CatchError, v) h) (ListT.run m) (ListT.run << h) ) :ListT<'U>
        static member inline instance (CatchError,  m:SeqT<'U> , _:SeqT<'U>  ) = fun (h:'e -> SeqT<'U>) -> 
            SeqT ( (fun v h -> Inline.instance (CatchError, v) h) (SeqT.run m) (SeqT.run << h) ) :SeqT<'U>
        static member        instance (CatchError, m:Choice<'v,'t>, _:Choice<'v,'e>) = fun (h:'t -> Choice<'v,'e>) -> Error.catch h m
        static member        instance (CatchError, m:'t * 'v, _:'e * 'v) = fun (h:'t -> 'e * 'v) -> h (fst m)

    let inline throwError x   = Inline.instance  ThrowError x
    let inline catchError v h = Inline.instance (CatchError, v) h


module MonadCont =
    type CallCC = CallCC with
        static member instance (CallCC, _:OptionT<Cont<'r,option<'a>>>) = fun (f:((_ -> OptionT<Cont<_,'b>>) -> _)) -> OptionT(Cont.callCC <| fun c -> OptionT.run(f (OptionT << c << Some)))     :OptionT<Cont<'r,option<'a>>>
        static member instance (CallCC, _:ListT<Cont<'r ,  list<'a>>> ) = fun (f:((_ -> ListT<Cont<_,'b>>  ) -> _)) -> ListT  (Cont.callCC <| fun c ->   ListT.run(f (ListT << c << List.singleton))):ListT<Cont<'r, list<'a>>>
        static member instance (CallCC, _: SeqT<Cont<'r ,  seq<'a>>>  ) = fun (f:((_ -> SeqT<Cont<_,'b>>   ) -> _)) -> SeqT   (Cont.callCC <| fun c ->   SeqT.run (f (SeqT  << c << Seq.singleton ))):SeqT<Cont<'r ,  seq<'a>>>
        static member instance (CallCC, _:Cont<'r,'a>) = Cont.callCC : (('a -> Cont<'r,'b>) -> _) -> _

    let inline internal callCC f = Inline.instance CallCC f


module MonadState =
    type Get = Get with
        static member inline instance (Get, _:OptionT<_>) = fun () -> lift (State.get())
        static member inline instance (Get, _:ListT<_>  ) = fun () -> lift (State.get())
        static member inline instance (Get, _: SeqT<_>  ) = fun () -> lift (State.get())
        static member        instance (Get, _:State<_,_>) = fun () ->      (State.get())

    type Put = Put with
        static member inline instance (Put, _:OptionT<_>) = lift << State.put
        static member inline instance (Put, _:ListT<_>  ) = lift << State.put
        static member inline instance (Put, _: SeqT<_>  ) = lift << State.put
        static member        instance (Put, _:State<_,_>) =         State.put

    let inline internal get() = Inline.instance Get ()
    let inline internal put x = Inline.instance Put x


module MonadReader =
    type Ask = Ask with
        static member instance (Ask, _:OptionT<Reader<'a,option<'a>>>) = fun () -> lift (Reader.ask()) :OptionT<Reader<'a,option<'a>>>
        static member instance (Ask, _:ListT<Reader< 'a, list<  'a>>>) = fun () -> lift (Reader.ask()) :  ListT<Reader<'a,  list<'a>>>
        static member instance (Ask, _: SeqT<Reader< 'a,  seq<  'a>>>) = fun () -> lift (Reader.ask()) :   SeqT<Reader<'a,   seq<'a>>>
        static member instance (Ask, _:Reader<'r,'r>                 ) = fun () ->      (Reader.ask()) :Reader<'r,'r>

    type Local = Local with
        static member inline instance (Local, OptionT m, _:OptionT<_> ) = fun f -> OptionT <| Reader.local f m
        static member inline instance (Local,  ListT  m, _: ListT<_>  ) = fun f ->  ListT  <| Reader.local f m
        static member inline instance (Local,   SeqT  m, _:  SeqT<_>  ) = fun f ->   SeqT  <| Reader.local f m
        static member        instance (Local,         m, _:Reader<_,_>) = fun f ->            Reader.local f m

    let inline internal ask()     = Inline.instance  Ask ()
    let inline internal local f m = Inline.instance (Local, m) f


module MonadWriter =
    type Tell = Tell with
        static member inline instance (Tell, _:OptionT<_> ) = lift << Writer.tell
        static member        instance (Tell, _:Writer<_,_>) =         Writer.tell

    type Listen = Listen with
        static member inline instance (Listen, m, _:OptionT<_> ) = fun () ->
            let liftMaybe (m,w) = Option.map (fun x -> (x,w)) m
            OptionT (Writer.listen (OptionT.run m) >>= (return' << liftMaybe))
        static member        instance (Listen, m, _:Writer<_,_>) = fun () -> Writer.listen m

    type Pass = Pass with
        static member inline instance (Pass, m, _:OptionT<_> ) = fun () -> OptionT (OptionT.run m >>= option (return' None) (liftM Some << Writer.pass << return'))
        static member        instance (Pass, m, _:Writer<_,_>) = fun () -> Writer.pass m

    let inline internal tell   x = Inline.instance  Tell x
    let inline internal listen m = Inline.instance (Listen, m) ()
    let inline internal pass   m = Inline.instance (Pass  , m) ()