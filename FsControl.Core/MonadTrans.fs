namespace FsControl.Core.Types

open FsControl.Core.Prelude
open FsControl.Core.Abstractions
open FsControl.Core.Abstractions.Functor
open FsControl.Core.Abstractions.Applicative
open FsControl.Core.Abstractions.Monad

type OptionT<'Ma> = OptionT of 'Ma

[<RequireQualifiedAccess>]
module OptionT =
    let run   (OptionT m) = m
    let map f (OptionT m) = OptionT (f m)

type OptionT<'Ma> with
    static member inline instance (Functor.Map, OptionT x :OptionT<'ma>, _) = fun (f:'a->'b) -> OptionT (fmap (Option.map f) x) :OptionT<'mb>
    static member inline instance (Applicative.Pure,            _:OptionT<'ma>) = OptionT << return' << Some :'a -> OptionT<'ma>
    static member inline instance (Applicative.Apply, OptionT(f), OptionT(x),  _:OptionT<'r>) = fun () ->
        OptionT(fmap (<*>) f <*> x) :OptionT<'r>
    static member inline instance (Monad.Bind  , OptionT x :OptionT<'ma>, _:OptionT<'mb>) = 
        fun (f: 'a -> OptionT<'mb>) -> (OptionT <| do'() {
            let! maybe_value = x
            return! match maybe_value with Some value -> OptionT.run (f value) | _ -> return' None}) :OptionT<'mb>

    static member inline instance (MonadPlus.Mzero, _:OptionT<_>) = fun ()          -> OptionT (return' None)
    static member inline instance (MonadPlus.Mplus, OptionT x, _) = fun (OptionT y) -> OptionT <| do'() {
            let! maybe_value = x
            return! match maybe_value with Some value -> x | _ -> y}

type ListT<'Ma> = ListT of 'Ma

[<RequireQualifiedAccess>]
module ListT =
    let run   (ListT m) = m
    let map f (ListT m) = ListT (f m)

type ListT<'Ma> with
    static member inline instance (Functor.Map ,            ListT x:ListT<'ma>, _) = fun (f:'a->'b) -> ListT (fmap (List.map f) x):ListT<'mb>
    static member inline instance (Applicative.Pure,                     _:ListT<'ma>) = ListT << return' << singleton :'a -> ListT<'ma>
    static member inline instance (Applicative.Apply, ListT(f), ListT(x),  _:ListT<'r>) = fun () ->
        ListT(fmap (<*>) f <*> x) :ListT<'r>
    static member inline instance (Monad.Bind  , ListT x:ListT<'ma>, _:ListT<'mb>) =
        fun (k: 'a -> ListT<'mb>) -> 
            (ListT (x >>= mapM(ListT.run << k) >>= (concat >> return'))) :ListT<'mb>

    static member inline instance (MonadPlus.Mzero, _:ListT<_>) = fun ()        -> ListT (return' [])
    static member inline instance (MonadPlus.Mplus, ListT x, _) = fun (ListT y) -> ListT <| do'() {
        let! a = x
        let! b = y
        return (a @ b)}

namespace FsControl.Core.Abstractions

open FsControl.Core.Prelude
open FsControl.Core.Abstractions
open FsControl.Core.Types
open FsControl.Core.Abstractions.Functor
open FsControl.Core.Abstractions.Monad

module MonadTrans = 
    type Lift = Lift with
        static member inline instance (Lift, _:OptionT<'m_a>) = OptionT << (liftM Some)      :'ma -> OptionT<'m_a>
        static member inline instance (Lift, _: ListT<'m_a> ) = ListT   << (liftM singleton) :'ma ->  ListT<'m_a> 

    let inline internal lift (x:'ma) = Inline.instance Lift x

open MonadTrans

module MonadAsync =
    type LiftAsync = LiftAsync with  
        static member inline instance (LiftAsync, _:OptionT<'U>) = fun (x :Async<'a>) -> lift (Inline.instance LiftAsync x)
        static member inline instance (LiftAsync, _:ListT< 'U> ) = fun (x :Async<'a>) -> lift (Inline.instance LiftAsync x)
        static member        instance (LiftAsync, _:Async<'a>  ) = fun (x :Async<'a>) -> x

    let inline internal liftAsync (x: Async<'a>) = Inline.instance LiftAsync x



module MonadError =
    type ThrowError = ThrowError with
        static member inline instance (ThrowError, _:OptionT<'U>  ) = lift << Inline.instance ThrowError
        static member inline instance (ThrowError, _:ListT<'U>    ) = lift << Inline.instance ThrowError
        static member        instance (ThrowError, _:Choice<'v,'e>) = Choice2Of2

    type CatchError = CatchError with
        static member inline instance (CatchError,  m:OptionT<'U> , _:OptionT<'U>  ) = fun (h:'e -> OptionT<'U>) -> 
            OptionT ( (fun v h -> Inline.instance (CatchError, v) h) (OptionT.run m) (OptionT.run << h) ) :OptionT<'U>
        static member inline instance (CatchError,  m:ListT<'U> , _:ListT<'U>  ) = fun (h:'e -> ListT<'U>) -> 
            ListT ( (fun v h -> Inline.instance (CatchError, v) h) (ListT.run m) (ListT.run << h) ) :ListT<'U>
        static member        instance (CatchError, m:Choice<'v,'e>, _:Choice<'v,'e>) = fun (h:'e -> Choice<'v,'e>) ->
            match m with
            | Choice1Of2 v  -> Choice1Of2 v
            | Choice2Of2 ex -> h ex
        static member        instance (CatchError, m:'e * 'v, _:'e * 'v) = fun (h:'e -> 'e * 'v) -> h (fst m) 

    let inline throwError x   = Inline.instance  ThrowError x
    let inline catchError v h = Inline.instance (CatchError, v) h


module MonadCont =
    type CallCC = CallCC with
        static member instance (CallCC, _:OptionT<Cont<'r,option<'a>>>) = fun (f:((_ -> OptionT<Cont<_,'b>>) -> _)) -> OptionT(Cont.callCC <| fun c -> OptionT.run(f (OptionT << c << Some)))     :OptionT<Cont<'r,option<'a>>>
        static member instance (CallCC, _:ListT<Cont<'r,   List<'a>>> ) = fun (f:((_ -> ListT<Cont<_,'b>>  ) -> _)) -> ListT  (Cont.callCC <| fun c ->   ListT.run(f (ListT  << c << singleton))) :ListT<  Cont<'r,  List<'a>>>    
        static member instance (CallCC, _:Cont<'r,'a>) = Cont.callCC : (('a -> Cont<'r,'b>) -> _) -> _

    let inline internal callCC f = Inline.instance CallCC f


module MonadState =
    type Get = Get with
        static member inline instance (Get, _:OptionT<_>) = fun () -> lift (State.get())
        static member inline instance (Get, _:ListT<_>  ) = fun () -> lift (State.get())
        static member        instance (Get, _:State<_,_>) = fun () ->      (State.get())

    type Put = Put with
        static member inline instance (Put, _:OptionT<_>) = lift << State.put
        static member inline instance (Put, _:ListT<_>  ) = lift << State.put
        static member        instance (Put, _:State<_,_>) =         State.put

    let inline internal get() = Inline.instance Get ()
    let inline internal put x = Inline.instance Put x


module MonadReader =
    type Ask = Ask with
        static member instance (Ask, _:OptionT<Reader<'a,option<'a>>>) = fun () -> lift (Reader.ask()) :OptionT<Reader<'a,option<'a>>>
        static member instance (Ask, _:ListT<Reader< 'a,List<   'a>>>) = fun () -> lift (Reader.ask()) :  ListT<Reader<'a,  List<'a>>>
        static member instance (Ask, _:Reader<'r,'r>                 ) = fun () ->      (Reader.ask()) :Reader<'r,'r>

    type Local = Local with
        static member inline instance (Local, OptionT m, _:OptionT<_> ) = fun f -> OptionT <| Reader.local f m
        static member inline instance (Local, ListT   m, _: ListT<_>  ) = fun f -> ListT   <| Reader.local f m
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
        static member inline instance (Pass, m, _:OptionT<_> ) = fun () -> OptionT (OptionT.run m >>= maybe (return' None) (liftM Some << Writer.pass << return'))
        static member        instance (Pass, m, _:Writer<_,_>) = fun () -> Writer.pass m

    let inline internal tell   x = Inline.instance  Tell x
    let inline internal listen m = Inline.instance (Listen, m) ()
    let inline internal pass   m = Inline.instance (Pass  , m) ()
