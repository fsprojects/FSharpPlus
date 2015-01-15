namespace FsControl.Core.Types

open FsControl.Core.Prelude
open FsControl.Core.TypeMethods
open FsControl.Core.TypeMethods.Functor
open FsControl.Core.TypeMethods.Applicative
open FsControl.Core.TypeMethods.Monad
open FsControl.Core.TypeMethods.MonadPlus
open FsControl.Core.TypeMethods.Monoid
open FsControl.Core.TypeMethods.MonadTrans
open FsControl.Core.TypeMethods.MonadAsync
open FsControl.Core.TypeMethods.MonadError

type WriterT<'WMa> = WriterT of 'WMa

[<RequireQualifiedAccess>]
module WriterT =
    let run   (WriterT x) = x
    let inline map f (WriterT m) = WriterT(fmap (Writer.map f) m)
    let inline internal execWriter   (WriterT m) = do'(){
        let! (_, w) = m
        return w}

type WriterT<'WMa> with
    static member inline instance (_:Functor.Map, x, _) = fun f -> WriterT.map f x

    static member inline instance (Applicative.Pure   , _:WriterT<'wma>     ) :'a -> WriterT<'wma> = fun a  -> WriterT (return' (a, mempty()))
    static member inline instance (_:Applicative.Apply,   WriterT(f), WriterT(x),  _:WriterT<'r> ) = fun () ->
        WriterT(fmap (<*>) f <*> x) :WriterT<'r>
    static member inline instance (Monad.Bind, WriterT (m:'wma), _:WriterT<'wmb>) :('a -> WriterT<'wmb>) -> WriterT<'wmb> =
        fun k -> WriterT <| do'(){
            let! (a, w ) = m
            let! (b, w') = WriterT.run (k a)
            return (b, mappend w w')}

    static member inline instance (MonadPlus.Mzero, _:WriterT<_>  ) = fun ()          -> WriterT(mzero())
    static member inline instance (MonadPlus.Mplus,   WriterT m, _) = fun (WriterT n) -> WriterT(mplus m n)

    static member inline instance (MonadWriter.Tell, _:WriterT<_> ) = fun w -> WriterT(return' ((), w))
    static member inline instance (MonadWriter.Listen, WriterT m, _:WriterT<_>) = WriterT <| do'(){
        let! (a, w) = m
        return ((a, w), w)}
    static member inline instance (MonadWriter.Pass  , WriterT m, _:WriterT<_>) = WriterT <| do'() {
        let! ((a, f), w) = m
        return (a, f w)}

    static member inline instance (MonadTrans.Lift   , _:WriterT<'wma>) : 'ma -> WriterT<'wma> = fun m -> WriterT <| do'() {
        let! a = m
        return (a, mempty())}
    
    static member inline instance (MonadAsync.LiftAsync    , _:WriterT<_>   ) = fun (x: Async<_>) -> lift (liftAsync x)

    static member inline instance (MonadError.ThrowError, _:WriterT<'U>    ) = lift << throwError
    static member inline instance (MonadError.CatchError, m:WriterT<'U> , _:WriterT<'U>) = fun (h:'e -> WriterT<'U>) -> 
            WriterT <| catchError (WriterT.run m) (WriterT.run << h) :WriterT<'U>

    static member inline instance (MonadCont.CallCC  , _:WriterT<Cont<'r,'a*'b>>) : (('a->WriterT<Cont<'r,'t>>)->_) -> WriterT<Cont<'r,'a*'b>>= 
        fun f -> WriterT (Cont.callCC <| fun c -> WriterT.run (f (fun a -> WriterT <| c (a, mempty()))))
    
    static member inline instance (MonadReader.Ask, _:WriterT<Reader<'a,'a*'b>> ) = fun () -> lift (Reader.ask()):WriterT<Reader<'a,'a*'b>>
    static member        instance (MonadReader.Local, WriterT m, _:WriterT<Reader<'a,'b>>) :('a->'t) -> WriterT<Reader<'a,'b>> = fun f -> WriterT(Reader.local f m)

    static member inline instance (MonadState.Get , _:WriterT<State<'a,'a*'b>>  ) : unit -> WriterT<State<'a,'a*'b>>   = fun () -> lift (State.get())
    static member inline instance (MonadState.Put , _:WriterT<State<'a,unit*'b>>) :'a    -> WriterT<State<'a,unit*'b>> = lift << State.put