namespace FsControl.Core.Types

open FsControl.Core.Prelude
open FsControl.Core.TypeMethods
open FsControl.Core.TypeMethods.Monad

type ReaderT<'R,'Ma> = ReaderT of ('R -> 'Ma)

[<RequireQualifiedAccess>]
module ReaderT =
    let  run (ReaderT x) = x
    let inline map (f:'a->'b) (ReaderT m) = ReaderT (Map.Invoke f << m)
    let inline apply (ReaderT f) (ReaderT x) = (ReaderT <| fun r -> f r <*> x r) :ReaderT<'r,'ma>
    let inline bind f (ReaderT m) = ReaderT <| fun r -> do'() {
        let! a = m r
        return! run (f a) r}    

type ReaderT<'R,'Ma> with
    static member inline Map    (_:Map, x, _) = fun f -> ReaderT.map f x
    static member inline Return (_:Return, _:ReaderT<'r,'ma>) :'a  -> ReaderT<'r,'ma> = fun a -> ReaderT <| fun _ -> result a
    static member inline Apply  (_:Apply , f, x, _:ReaderT<'r,'mb>) = ReaderT.apply f x :ReaderT<'r,'mb>
    static member inline Bind   (_:Bind  , x, _:ReaderT<'r,'m>) :('b -> ReaderT<'r,'m>) -> ReaderT<'r,'m> = fun f -> ReaderT.bind f x

    static member inline Zero (_:Zero, _:ReaderT<_,_>        ) = ReaderT <| fun _ -> Zero.Invoke()
    static member inline Plus (_:Plus,   ReaderT m, ReaderT n) = ReaderT <| fun r -> m r <|> n r

    static member inline Lift (_:Lift, _:ReaderT<'r,'ma>) = fun m -> (ReaderT <| fun _ -> m) : ReaderT<'r,'ma>

    static member CallCC (_:CallCC , _:ReaderT<'r,Cont<'c,'a>> ) : (('a -> ReaderT<'t,Cont<'c,'u>>) -> ReaderT<'r,Cont<'c,'a>>) -> ReaderT<'r,Cont<'c,'a>> =
        fun f -> ReaderT (fun r -> Cont.callCC <| fun c -> ReaderT.run (f (fun a -> ReaderT <| fun _ -> c a)) r)

    static member inline Ask   (_:Ask, _:ReaderT<'r,'a>      ) = ReaderT result :ReaderT<'r,'a>
    static member inline Local (_:Local, ReaderT m, _:ReaderT<_,_>) = fun f  -> ReaderT(fun r -> m (f r))

    static member inline LiftAsync (_:LiftAsync, _:ReaderT<_,_>) = fun (x: Async<_>) -> Lift.Invoke (LiftAsync.Invoke x)

    static member inline ThrowError (_:ThrowError, _:ReaderT<_,_>    ) = Lift.Invoke << ThrowError.Invoke
    static member inline CatchError (_:CatchError,  m:ReaderT<'T,'U> , _:ReaderT<'T,'U>) = fun (h:'e -> ReaderT<'T,'U>) -> 
        ReaderT (fun s -> CatchError.Invoke (ReaderT.run m s)   (fun e -> ReaderT.run (h e) s)):ReaderT<'T,'U>

    static member Tell   (_:Tell  , _:ReaderT<'t,'a->Writer<'a,unit>>          ) :ReaderT<'t,'a->Writer<'a,unit>> = Lift.Invoke Writer.tell
    static member Listen (_:Listen,   ReaderT m, _:ReaderT<'t,Writer<'a,'b*'a>>) :ReaderT<'t,Writer<'a,'b*'a>> = ReaderT <| fun w -> Writer.listen (m w)  
    static member Pass   (_:Pass  ,   ReaderT m, _:ReaderT<'t,Writer<'a,'b>>   ) :ReaderT<'t,Writer<'a,'b>>    = ReaderT <| fun w -> Writer.pass   (m w)

    static member Get (_:Get    , _:ReaderT<'s,State<'a,'a>>  ) = Lift.Invoke (State.get()) :ReaderT<'s,State<'a,'a>>
    static member Put (_:Put    , _:ReaderT<'s,State<'a,unit>>) = Lift.Invoke << State.put : 'a -> ReaderT<'s,State<'a,unit>>