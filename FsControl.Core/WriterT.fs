namespace FsControl.Core.Types

open FsControl.Core.Prelude
open FsControl.Core.TypeMethods
open FsControl.Core.TypeMethods.Monad

type WriterT<'WMa> = WriterT of 'WMa

[<RequireQualifiedAccess>]
module WriterT =

    let run (WriterT x) = x

    let inline map  f (WriterT m) =
        let mapWriter f (a, m) = (f a, m)
        WriterT <| Map.Invoke (mapWriter f) m

    let inline apply  (WriterT f) (WriterT x) =
        let applyWriter (a, w) (b, w') = (a b, Mappend.Invoke w w')
        WriterT (result applyWriter <*> f <*> x)
        
    let inline bind f (WriterT m) = WriterT <| do'() {
        let! (a, w ) = m
        let! (b, w') = run (f a)
        return (b, Mappend.Invoke w w')}

    let inline internal execWriter   (WriterT m) = do'() {
        let! (_, w) = m
        return w}

type WriterT<'WMa> with

    static member inline Map      (_:Map   , x, _) = fun f -> WriterT.map f x
    static member inline Return   (_:Return, _:WriterT<'wma>) :'a -> WriterT<'wma> = fun a -> WriterT (result (a, Mempty.Invoke()))
    static member inline Apply    (_:Apply , f, x, _:WriterT<'r>) = WriterT.apply f x :WriterT<'r>
    static member inline Bind     (_:Bind  , x:WriterT<'wma>, _:WriterT<'wmb>) :('a -> WriterT<'wmb>) -> WriterT<'wmb> = fun f -> WriterT.bind f x

    static member inline Zero (_:Zero, _:WriterT<_>  ) =                    WriterT (Zero.Invoke())
    static member inline Plus (_:Plus,   WriterT m, _) = fun (WriterT n) -> WriterT (m <|> n)

    static member inline Tell   (_:Tell, _:WriterT<_> ) = fun w -> WriterT (result ((), w))
    static member inline Listen (_:Listen, WriterT m, _:WriterT<_>) = WriterT <| do'() {
        let! (a, w) = m
        return ((a, w), w)}
    static member inline Pass (_:Pass  , WriterT m, _:WriterT<_>) = WriterT <| do'() {
        let! ((a, f), w) = m
        return (a, f w)}

    static member inline Lift (_:Lift   , _:WriterT<'wma>) : 'ma -> WriterT<'wma> = fun m -> WriterT <| do'() {
        let! a = m
        return (a, Mempty.Invoke())}
    
    static member inline LiftAsync (_:LiftAsync , _:WriterT<_> ) = fun (x: Async<_>) -> Lift.Invoke (LiftAsync.Invoke x)

    static member inline ThrowError (_:ThrowError, _:WriterT<'U>) = Lift.Invoke << ThrowError.Invoke
    static member inline CatchError (_:CatchError, m:WriterT<'U> , _:WriterT<'U>) = fun (h:'e -> WriterT<'U>) -> 
            WriterT <| CatchError.Invoke (WriterT.run m) (WriterT.run << h) :WriterT<'U>

    static member inline CallCC (_:CallCC  , _:WriterT<Cont<'r,'a*'b>>) : (('a->WriterT<Cont<'r,'t>>)->_) -> WriterT<Cont<'r,'a*'b>>= 
        fun f -> WriterT (Cont.callCC <| fun c -> WriterT.run (f (fun a -> WriterT <| c (a, Mempty.Invoke()))))
    
    static member inline Ask   (_:Ask, _:WriterT<Reader<'a,'a*'b>> ) = Lift.Invoke (Reader.ask()):WriterT<Reader<'a,'a*'b>>
    static member        Local (_:Local, WriterT m, _:WriterT<Reader<'a,'b>>) :('a->'t) -> WriterT<Reader<'a,'b>> = fun f -> 
        WriterT (Reader.local f m)

    static member inline Get (_:Get , _:WriterT<State<'a,'a*'b>>  ) :         WriterT<State<'a,'a*'b>>   = Lift.Invoke (State.get())
    static member inline Put (_:Put , _:WriterT<State<'a,unit*'b>>) :'a    -> WriterT<State<'a,unit*'b>> = Lift.Invoke << State.put