namespace FsControl

open FsControl.Core.Internals
open FsControl.Core.Internals.Prelude
open FsControl.Core.Internals.MonadOps

type ErrorT<'R> = ErrorT of 'R

[<RequireQualifiedAccess>]
module ErrorT =
    let run (ErrorT x) = x
    let inline map  f (ErrorT m) = ErrorT (Map.Invoke (Error.map f) m)
    let inline bind f (ErrorT m) = (ErrorT (m >>= (fun a -> match a with Choice2Of2 l -> result (Choice2Of2 l) | Choice1Of2 r -> run (f r))))
    let inline apply  (ErrorT f) (ErrorT x) = ErrorT(Map.Invoke Error.apply f <*> x) :ErrorT<'mb>

type ErrorT<'R> with
    static member inline Map    ( x :ErrorT<'ma>, f    , _:Map   ) = ErrorT.map f x :ErrorT<'mb>
    static member inline Return (_:ErrorT<'ma>         , _:Return) = ErrorT << result << Choice1Of2 :'a -> ErrorT<'ma>
    static member inline Apply  ( f, x,   _:ErrorT<'mb>, _:Apply ) = ErrorT.apply f x :ErrorT<'mb>
    static member inline Bind   (x:ErrorT<'ma>, f:'a -> ErrorT<'mb>) = ErrorT.bind f x :ErrorT<'mb>

    static member inline Lift (x:'ma) = x |> Map.FromMonad Choice1Of2 |> ErrorT : ErrorT<'m_a>

    static member inline ThrowError (_:ErrorT<'ma>) = ErrorT << result << Choice2Of2 :'a -> ErrorT<'ma>
    static member inline CatchError (ErrorT x :ErrorT<'ma>, _:ErrorT<'mb>) = 
        fun (f: 'a -> ErrorT<'mb>) -> (ErrorT (x >>= (fun a -> match a with Choice2Of2 l -> ErrorT.run (f l) | Choice1Of2 r -> result (Choice1Of2 r)))) :ErrorT<'mb>

    static member inline LiftAsync (_:ErrorT<'U>) = fun (x :Async<'a>) -> Lift.Invoke (LiftAsync.Invoke x)

    static member CallCC (f:(_ -> ErrorT<Cont<_,'b>>) -> _) :ErrorT<Cont<'r,Choice<'aa,'bb>>> = ErrorT(Cont.callCC <| fun c -> ErrorT.run(f (ErrorT << c << Choice1Of2)))

    static member        Ask () = (ErrorT << (Map.FromMonad Choice1Of2)) (Reader.ask()) : ErrorT<Reader<'a,Choice<'a,'b>>>
    static member inline Local (ErrorT m, f) = ErrorT (Reader.local f m)

    static member inline Tell   (x) = x |> Writer.tell |> Lift.Invoke
    static member inline Listen (m) =
        let liftError (m, w) = Error.map (fun x -> (x, w)) m
        ErrorT (Writer.listen (ErrorT.run m) >>= (result << liftError))
    static member inline Pass (m) = ErrorT (ErrorT.run m >>= option (result None) (Map.FromMonad Some << Writer.pass << result))

    static member inline Get ()  = Lift.Invoke (State.get())     :ErrorT<_>
    static member inline Put (x) = x |> State.put |> Lift.Invoke :ErrorT<_>