module FsControl.Core.Types.WriterT

open FsControl.Core.Prelude
open FsControl.Core.Abstractions
open FsControl.Core.Abstractions.Monad
open FsControl.Core.Abstractions.MonadPlus
open FsControl.Core.Abstractions.Monoid
open FsControl.Core.Types.MonadTrans
open FsControl.Core.Types.MonadAsync
open FsControl.Core.Types.Cont
open FsControl.Core.Types.Reader
open FsControl.Core.Types.State
open FsControl.Core.Types.Writer

type WriterT<'WMa> = WriterT of 'WMa

module WriterT =
    let runWriterT   (WriterT x) = x
    let mapWriterT f (WriterT m) = WriterT(f m)
    let inline internal execWriter   (WriterT m) = do'(){
        let! (_, w) = m
        return w}

type WriterT<'WMa> with
    static member inline instance (Functor.Fmap, WriterT m, _) = fun f -> WriterT <| do'(){
        let! (a, w) = m
        return (f a, w)}

let inline runWriterT   (WriterT x) = x

type WriterT<'WMa> with
    static member inline instance (Monad.Return,                 _:WriterT<'wma>) :'a -> WriterT<'wma> = fun a -> WriterT (return' (a, mempty()))
    static member inline instance (Monad.Bind, WriterT (m:'wma), _:WriterT<'wmb>) :('a -> WriterT<'wmb>) -> WriterT<'wmb> =
        fun k -> WriterT <| do'(){
            let! (a, w ) = m
            let! (b, w') = runWriterT (k a)
            return (b, w </mappend/> w')}

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

    static member inline instance (MonadCont.CallCC  , _:WriterT<Cont<'r,'a*'b>>) : (('a->WriterT<Cont<'r,'t>>)->_) -> WriterT<Cont<'r,'a*'b>>= 
        fun f -> WriterT (callCC <| fun c -> runWriterT (f (fun a -> WriterT <| c (a, mempty()))))
    
    static member inline instance (MonadReader.Ask, _:WriterT<Reader<'a,'a*'b>> ) = fun () -> lift (ask()):WriterT<Reader<'a,'a*'b>>
    static member        instance (MonadReader.Local, WriterT m, _:WriterT<Reader<'a,'b>>) :('a->'t) -> WriterT<Reader<'a,'b>> = fun f -> WriterT(local f m)

    static member inline instance (MonadState.Get , _:WriterT<State<'a,'a*'b>>  ) : unit -> WriterT<State<'a,'a*'b>>   = fun () -> lift (get())
    static member inline instance (MonadState.Put , _:WriterT<State<'a,unit*'b>>) :'a    -> WriterT<State<'a,unit*'b>> = lift << put