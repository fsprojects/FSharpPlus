namespace FsControl.Core.Types

open FsControl.Core
open FsControl.Core.Prelude
open FsControl.Core.TypeMethods
open FsControl.Core.TypeMethods.Functor
open FsControl.Core.TypeMethods.Applicative
open FsControl.Core.TypeMethods.Monad
open FsControl.Core.TypeMethods.MonadTrans
open FsControl.Core.TypeMethods.MonadAsync

type ErrorT<'R> = ErrorT of 'R

[<RequireQualifiedAccess>]
module ErrorT =
    let run (ErrorT x) = x
    let inline map  f (ErrorT m) = ErrorT (fmap (Error.map f) m)
    let inline bind f (ErrorT m) = (ErrorT <| do'() {
        let! a = m
        match a with
        | Choice2Of2 l -> return (Choice2Of2 l)
        | Choice1Of2 r -> return! run (f r)}) :ErrorT<'mb>
    let inline apply (ErrorT f) (ErrorT x) = ErrorT(fmap Error.apply f <*> x) :ErrorT<'mb>

type ErrorT<'R> with
    static member inline Map    (_:Functor.Map, x :ErrorT<'ma>, _ ) = fun f -> ErrorT.map f x :ErrorT<'mb>
    static member inline Return (_:Applicative.Return ,_:ErrorT<'ma>) = ErrorT << result << Choice1Of2 :'a -> ErrorT<'ma>
    static member inline Apply  (_:Applicative.Apply, f, x,   _:ErrorT<'mb>) = fun () -> ErrorT.apply f x :ErrorT<'mb>
    static member inline Bind   (_:Monad.Bind, x:ErrorT<'ma>, _:ErrorT<'mb>) = fun (f:'a -> ErrorT<'mb>) -> ErrorT.bind f x :ErrorT<'mb>

    static member inline Lift (_:MonadTrans.Lift, _:ErrorT<'m_a>) = ErrorT << (liftM Choice1Of2)      :'ma -> ErrorT<'m_a>

    static member inline ThrowError (_:MonadError.ThrowError, _:ErrorT<'ma>) = ErrorT << result << Choice2Of2 :'a -> ErrorT<'ma>
    static member inline CatchError (_:MonadError.CatchError, ErrorT x :ErrorT<'ma>, _:ErrorT<'mb>) = 
        fun (f: 'a -> ErrorT<'mb>) -> (ErrorT <| do'() {
            let! a = x
            match a with
            | Choice2Of2 l -> return! ErrorT.run (f l)
            | Choice1Of2 r -> return (Choice1Of2 r)}) :ErrorT<'mb>

    static member inline LiftAsync (_:MonadAsync.LiftAsync, _:ErrorT<'U>) = fun (x :Async<'a>) -> lift (liftAsync x)

    static member CallCC (_:MonadCont.CallCC, _:ErrorT<Cont<'r,Choice<'aa,'bb>>>) = fun (f:((_ -> ErrorT<Cont<_,'b>>) -> _)) -> ErrorT(Cont.callCC <| fun c -> ErrorT.run(f (ErrorT << c << Choice1Of2)))     :ErrorT<Cont<'r,Choice<'aa,'bb>>>

    static member        Ask   (_:MonadReader.Ask, _: ErrorT<Reader<'a,Choice<'a,'b>>>) = fun () -> (ErrorT << (liftM Choice1Of2)) (Reader.ask()) : ErrorT<Reader<'a,Choice<'a,'b>>>
    static member inline Local (_:MonadReader.Local, ErrorT m, _:ErrorT<_> ) = fun f -> ErrorT <| Reader.local f m

    static member inline Tell   (_:MonadWriter.Tell, _:ErrorT<_> ) = lift << Writer.tell
    static member inline Listen (_:MonadWriter.Listen, m, _:ErrorT<_> ) = fun () ->
        let liftError (m,w) = Error.map (fun x -> (x,w)) m
        ErrorT (Writer.listen (ErrorT.run m) >>= (result << liftError))
    static member inline Pass (_:MonadWriter.Pass, m, _:ErrorT<_> ) = fun () -> ErrorT (ErrorT.run m >>= option (result None) (liftM Some << Writer.pass << result))

    static member inline Get (_:MonadState.Get, _:ErrorT<_>) = fun () -> lift (State.get())
    static member inline Put (_:MonadState.Put, _:ErrorT<_>) = lift << State.put