namespace FsControl

open FsControl.Core.Internals.Prelude
open FsControl.Core.Internals.MonadOps

type WriterT<'WMa> = WriterT of 'WMa

[<RequireQualifiedAccess>]
module WriterT =

    let run (WriterT x) = x

    let inline map  f (WriterT m) =
        let mapWriter f (a, m) = (f a, m)
        WriterT <| Map.Invoke (mapWriter f) m

    let inline apply  (WriterT f) (WriterT x) =
        let applyWriter (a, w) (b, w') = (a b, MAppend.Invoke w w')
        WriterT (result applyWriter <*> f <*> x)
        
    let inline bind f (WriterT m) = WriterT (m >>= (fun (a, w) -> run (f a) >>= (fun (b, w') -> result (b, MAppend.Invoke w w'))))

type WriterT<'WMa> with

    static member inline Map      (x, f               , _:Map   ) = WriterT.map f x
    static member inline Return   (_:WriterT<'wma>    , _:Return) :'a -> WriterT<'wma> = fun a -> WriterT (result (a, MEmpty.Invoke()))
    static member inline Apply    (f, x, _:WriterT<'r>, _:Apply ) = WriterT.apply f x :WriterT<'r>
    static member inline Bind     (x:WriterT<'wma>, f :'a -> WriterT<'wmb>) : WriterT<'wmb> = WriterT.bind f x

    static member inline MZero (_:WriterT<_>          , _:MZero) = WriterT (MZero.Invoke())
    static member inline MPlus (  WriterT m, WriterT n, _:MPlus) = WriterT (m <|> n)

    static member inline Tell   (w) = WriterT (result ((), w))
    static member inline Listen (WriterT m) = WriterT (m >>= (fun (a, w) -> result ((a, w), w)))
    static member inline Pass   (WriterT m) = WriterT (m >>= (fun ((a, f), w) -> result (a, f w)))

    static member inline Lift (m: 'ma) : WriterT<'wma> = WriterT (m >>= (fun a -> result (a, MEmpty.Invoke())))
    
    static member inline LiftAsync (_:WriterT<_> ) = fun (x: Async<_>) -> Lift.Invoke (LiftAsync.Invoke x)

    static member inline ThrowError (_:WriterT<'U>) = Lift.Invoke << ThrowError.Invoke
    static member inline CatchError (m:WriterT<'U> , _:WriterT<'U>) = fun (h:'e -> WriterT<'U>) -> 
            WriterT <| CatchError.Invoke (WriterT.run m) (WriterT.run << h) :WriterT<'U>

    static member inline CallCC (f : ('a->WriterT<Cont<'r,'t>>)->_)  :WriterT<Cont<'r,'a*'b>> = 
        WriterT (Cont.callCC <| fun c -> WriterT.run (f (fun a -> WriterT <| c (a, MEmpty.Invoke()))))
       
    static member inline get_Ask() = Lift.Invoke Reader.ask :WriterT<Reader<'a,'a*'b>>
    static member        Local (WriterT m, f:'a->'t) : WriterT<Reader<'a,'b>> = WriterT (Reader.local f m)

    static member inline get_Get()  : WriterT<State<'a,'a*'b>>   = Lift.Invoke State.get
    static member inline Put (x:'a) : WriterT<State<'a,unit*'b>> = x |> State.put |> Lift.Invoke