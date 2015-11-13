namespace FsControl
open FsControl.Core.Internals.Prelude
open FsControl.Core.Internals.MonadOps

type ReaderT<'R,'Ma> = ReaderT of ('R -> 'Ma)

[<RequireQualifiedAccess>]
module ReaderT =
    let  run (ReaderT x) = x
    let inline map (f:'a->'b) (ReaderT m) = ReaderT (Map.Invoke f << m)
    let inline apply (ReaderT f) (ReaderT x) = (ReaderT <| fun r -> f r <*> x r) :ReaderT<'r,'ma>
    let inline bind f (ReaderT m) = ReaderT <| fun r -> m r >>= (fun a -> run (f a) r)

type ReaderT<'R,'Ma> with
    static member inline Map    (x, f, _:Map) = ReaderT.map f x
    static member inline Return (_:ReaderT<'r,'ma>      , _:Return) :'a  -> ReaderT<'r,'ma> = fun a -> ReaderT <| fun _ -> result a
    static member inline Apply  (f, x, _:ReaderT<'r,'mb>, _:Apply ) = ReaderT.apply f x :ReaderT<'r,'mb>
    static member inline Bind (x, f :'b -> ReaderT<'r,'m>) : ReaderT<'r,'m> = ReaderT.bind f x

    static member inline MZero (_:ReaderT<_,_>        , _:MZero) = ReaderT <| fun _ -> MZero.Invoke()
    static member inline MPlus (  ReaderT m, ReaderT n, _:MPlus) = ReaderT <| fun r -> m r <|> n r

    static member Lift (m) : ReaderT<'r,'ma> = ReaderT (fun _ -> m)

    static member CallCC (f : ('a -> ReaderT<'t,Cont<'c,'u>>) -> ReaderT<'r,Cont<'c,'a>>) : ReaderT<'r,Cont<'c,'a>> =
        ReaderT (fun r -> Cont.callCC <| fun c -> ReaderT.run (f (fun a -> ReaderT <| fun _ -> c a)) r)
            
    static member inline get_Ask() = ReaderT result :ReaderT<'r,'a>
    static member        Local (ReaderT m, f) = ReaderT(fun r -> m (f r))

    static member inline LiftAsync (_:ReaderT<_,_>) = fun (x: Async<_>) -> Lift.Invoke (LiftAsync.Invoke x)

    static member inline ThrowError (_:ReaderT<_,_>) = Lift.Invoke << ThrowError.Invoke
    static member inline CatchError ( m:ReaderT<'T,'U> , _:ReaderT<'T,'U>) = fun (h:'e -> ReaderT<'T,'U>) -> 
        ReaderT (fun s -> CatchError.Invoke (ReaderT.run m s)   (fun e -> ReaderT.run (h e) s)):ReaderT<'T,'U>

    static member Tell   x           :ReaderT<'t,Writer<'a,unit>>  = x |> Writer.tell |> Lift.Invoke
    static member Listen (ReaderT m) :ReaderT<'t,Writer<'a,'b*'a>> = ReaderT <| fun w -> Writer.listen (m w)  
    static member Pass   (ReaderT m) :ReaderT<'t,Writer<'a,'b>>    = ReaderT <| fun w -> Writer.pass   (m w)

    static member get_Get()  = Lift.Invoke State.get         :ReaderT<'s, State<'a, 'a>>
    static member Put (x:'a) = x |> State.put |> Lift.Invoke :ReaderT<'s, State<'a, unit>>