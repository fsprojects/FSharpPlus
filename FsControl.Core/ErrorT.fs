namespace FsControl.Core.Types

open FsControl.Core
open FsControl.Core.Prelude
open FsControl.Core.TypeMethods
open FsControl.Core.TypeMethods.Monad

type ErrorT<'R> = ErrorT of 'R

[<RequireQualifiedAccess>]
module ErrorT =
    let run (ErrorT x) = x
    let inline map  f (ErrorT m) = ErrorT (Map.Invoke (Error.map f) m)
    let inline bind f (ErrorT m) = (ErrorT <| do'() {
        let! a = m
        match a with
        | Choice2Of2 l -> return (Choice2Of2 l)
        | Choice1Of2 r -> return! run (f r)}) :ErrorT<'mb>
    let inline apply (ErrorT f) (ErrorT x) = ErrorT(Map.Invoke Error.apply f <*> x) :ErrorT<'mb>

type ErrorT<'R> with
    static member inline Map    (_:Map   , x :ErrorT<'ma>, _ ) = fun f -> ErrorT.map f x :ErrorT<'mb>
    static member inline Return (_:Return,_:ErrorT<'ma>) = ErrorT << result << Choice1Of2 :'a -> ErrorT<'ma>
    static member inline Apply  (_:Apply , f, x,   _:ErrorT<'mb>) = ErrorT.apply f x :ErrorT<'mb>
    static member inline Bind   (_:Bind  , x:ErrorT<'ma>, _:ErrorT<'mb>) = fun (f:'a -> ErrorT<'mb>) -> ErrorT.bind f x :ErrorT<'mb>

    static member inline Lift (_:Lift, _:ErrorT<'m_a>) = ErrorT << (liftM Choice1Of2)      :'ma -> ErrorT<'m_a>

    static member inline ThrowError (_:ThrowError, _:ErrorT<'ma>) = ErrorT << result << Choice2Of2 :'a -> ErrorT<'ma>
    static member inline CatchError (_:CatchError, ErrorT x :ErrorT<'ma>, _:ErrorT<'mb>) = 
        fun (f: 'a -> ErrorT<'mb>) -> (ErrorT <| do'() {
            let! a = x
            match a with
            | Choice2Of2 l -> return! ErrorT.run (f l)
            | Choice1Of2 r -> return (Choice1Of2 r)}) :ErrorT<'mb>

    static member inline LiftAsync (_:LiftAsync, _:ErrorT<'U>) = fun (x :Async<'a>) -> Lift.Invoke (LiftAsync.Invoke x)

    static member CallCC (_:CallCC, _:ErrorT<Cont<'r,Choice<'aa,'bb>>>) = fun (f:((_ -> ErrorT<Cont<_,'b>>) -> _)) -> ErrorT(Cont.callCC <| fun c -> ErrorT.run(f (ErrorT << c << Choice1Of2)))     :ErrorT<Cont<'r,Choice<'aa,'bb>>>

    static member        Ask   (_:Ask, _: ErrorT<Reader<'a,Choice<'a,'b>>>) = (ErrorT << (liftM Choice1Of2)) (Reader.ask()) : ErrorT<Reader<'a,Choice<'a,'b>>>
    static member inline Local (_:Local, ErrorT m, _:ErrorT<_> ) = fun f -> ErrorT <| Reader.local f m

    static member inline Tell   (_:Tell, _:ErrorT<_> ) = Lift.Invoke << Writer.tell
    static member inline Listen (_:Listen, m, _:ErrorT<_> ) = fun () ->
        let liftError (m,w) = Error.map (fun x -> (x,w)) m
        ErrorT (Writer.listen (ErrorT.run m) >>= (result << liftError))
    static member inline Pass (_:Pass, m, _:ErrorT<_> ) = ErrorT (ErrorT.run m >>= option (result None) (liftM Some << Writer.pass << result))

    static member inline Get (_:Get, _:ErrorT<_>) = Lift.Invoke (State.get())
    static member inline Put (_:Put, _:ErrorT<_>) = Lift.Invoke << State.put