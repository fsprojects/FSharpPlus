module FsControl.Core.Types.ErrorT

open FsControl.Core.Prelude
open FsControl.Core.Abstractions
open FsControl.Core.Abstractions.Monad
open FsControl.Core.Abstractions.MonadPlus
open FsControl.Core.Types.MonadTrans
open FsControl.Core.Types.MonadAsync
open FsControl.Core.Types.MonadError
open FsControl.Core.Types.Reader
open FsControl.Core.Types.State
open FsControl.Core.Types.Writer
open FsControl.Core.Types.Cont

type ErrorT<'R> = ErrorT of ('R)

module ErrorT =
    let runErrorT   (ErrorT x) = x
    let mapErrorT f (ErrorT m) = ErrorT(f m)
    let inline internal mapError f = function Choice1Of2 x -> Choice1Of2(f x) | Choice2Of2 x -> Choice2Of2 x

open ErrorT

type ErrorT<'R,'Ma> with
    static member inline instance (Functor.Map, ErrorT x :ErrorT<'ma>, _) = fun (f) -> ErrorT (Functor.fmap (mapError f) x) :ErrorT<'mb>

type ErrorT<'R,'Ma> with 
    static member inline instance (Monad.Return, _:ErrorT<'ma>) = ErrorT << return' << Choice1Of2 :'a -> ErrorT<'ma>
    static member inline instance (Monad.Bind  , ErrorT x :ErrorT<'ma>, _:ErrorT<'mb>) = 
        fun (f: 'a -> ErrorT<'mb>) -> (ErrorT <| do'() {
            let! a = x
            match a with
            | Choice2Of2 l -> return (Choice2Of2 l)
            | Choice1Of2 r -> return! runErrorT (f r)}) :ErrorT<'mb>

    static member inline instance (MonadTrans.Lift, _:ErrorT<'m_a>) = ErrorT << (liftM Choice1Of2)      :'ma -> ErrorT<'m_a>

    static member inline instance (MonadError.ThrowError, _:ErrorT<'ma>) = ErrorT << return' << Choice2Of2 :'a -> ErrorT<'ma>
    static member inline instance (MonadError.CatchError, ErrorT x :ErrorT<'ma>, _:ErrorT<'mb>) = 
        fun (f: 'a -> ErrorT<'mb>) -> (ErrorT <| do'() {
            let! a = x
            match a with
            | Choice2Of2 l -> return! runErrorT (f l)
            | Choice1Of2 r -> return (Choice1Of2 r)}) :ErrorT<'mb>

    static member inline instance (LiftAsync, _:ErrorT<'U>) = fun (x :Async<'a>) -> lift (Inline.instance LiftAsync x)

    static member instance (MonadCont.CallCC, _:ErrorT<Cont<'r,Choice<'aa,'bb>>>) = fun (f:((_ -> ErrorT<Cont<_,'b>>) -> _)) -> ErrorT(callCC <| fun c -> runErrorT(f (ErrorT << c << Choice1Of2)))     :ErrorT<Cont<'r,Choice<'aa,'bb>>>

    static member        instance (MonadReader.Ask, _: ErrorT<Reader<'a,Choice<'a,'b>>>) = fun () -> (ErrorT << (liftM Choice1Of2)) (ask()) : ErrorT<Reader<'a,Choice<'a,'b>>>
    static member inline instance (MonadReader.Local, ErrorT m, _:ErrorT<_> ) = fun f -> ErrorT <| local f m

    static member inline instance (MonadWriter.Tell, _:ErrorT<_> ) = lift << tell

    static member inline instance (MonadWriter.Listen, m, _:ErrorT<_> ) = fun () ->
        let liftError (m,w) = mapError (fun x -> (x,w)) m
        ErrorT (listen (runErrorT m) >>= (return' << liftError))

    static member inline instance (MonadWriter.Pass, m, _:ErrorT<_> ) = fun () -> ErrorT (runErrorT m >>= maybe (return' None) (liftM Some << pass << return'))

    static member inline instance (MonadState.Get, _:ErrorT<_>) = fun () -> lift (get())
    static member inline instance (MonadState.Put, _:ErrorT<_>) = lift << put