namespace FSharpPlus

open FsControl

type ErrorT<'``monad<'choice<'t,'e>>``> = ErrorT of '``monad<'choice<'t,'e>>``

[<RequireQualifiedAccess>]
module ErrorT =
    let run (ErrorT x) = x : '``Monad<'Choice<'T,'E>>``
    let inline map  (f:'T->'U) (ErrorT m:ErrorT<'``Monad<'Choice<'T,'E>>``>) = ErrorT (Map.Invoke (Error.map f) m) :ErrorT<'``Monad<'Choice<('T -> 'U),'E>>``>
    let inline bind (f:'T->ErrorT<'``Monad<'Choice<'U,'E>>``>) (ErrorT m:ErrorT<'``Monad<'Choice<'T,'E>>``>) = (ErrorT (m >>= (fun a -> match a with Choice2Of2 l -> result (Choice2Of2 l) | Choice1Of2 r -> run (f r))))
    let inline apply  (ErrorT f:ErrorT<'``Monad<'Choice<('T -> 'U),'E>>``>) (ErrorT x:ErrorT<'``Monad<'Choice<'T,'E>>``>) = ErrorT(Map.Invoke Error.apply f <*> x) : ErrorT<'``Monad<'Choice<'U,'E>>``>

type ErrorT with
    static member inline Map    (x:ErrorT<'``Monad<'Choice<'T,'E>>``>, f:'T->'U, impl:Map) = ErrorT.map f x :ErrorT<'``Monad<'Choice<'U,'E>>``>
    static member inline Return (output:ErrorT<'``Monad<'Choice<'T,'E>>``>, impl:Return) = ErrorT << result << Choice1Of2 :'T -> ErrorT<'``Monad<'Choice<'T,'E>>``>
    static member inline Apply  (f:ErrorT<'``Monad<'Choice<('T -> 'U),'E>>``>, x:ErrorT<'``Monad<'Choice<'T,'E>>``>, output:ErrorT<'``Monad<'Choice<('T -> 'U),'E>>``>, impl:Apply) = ErrorT.apply f x : ErrorT<'``Monad<'Choice<'U,'E>>``>
    static member inline Bind   (x:ErrorT<'``Monad<'Choice<'T,'E>>``>, f:'T->ErrorT<'``Monad<'Choice<'U,'E>>``>) = ErrorT.bind f x

    static member inline Lift (x:'``Monad<'T>``) = x |> Map.FromMonad Choice1Of2 |> ErrorT : ErrorT<'``Monad<Choice<'T,'E>>``>

    static member inline ThrowError (x:'E) =  x |> Choice2Of2 |> result |> ErrorT : ErrorT<'``MonadError<'E,'T>``>
    static member inline CatchError (ErrorT x :ErrorT<'``MonadError<'E1,'T>``>, f: 'E1 -> _) = (ErrorT (x >>= (fun a -> match a with Choice2Of2 l -> ErrorT.run (f l) | Choice1Of2 r -> result (Choice1Of2 r)))) : ErrorT<'``MonadError<'E2,'T>``>

    static member inline LiftAsync (x :Async<'T>) = Lift.Invoke (LiftAsync.Invoke x)

    static member CallCC (f:('T -> ErrorT<Cont<'R,Choice<'U,'E>>>) -> _) :ErrorT<Cont<'R, Choice<'T,'E>>> = ErrorT(Cont.callCC <| fun c -> ErrorT.run(f (ErrorT << c << Choice1Of2)))

    static member get_Ask() = (ErrorT << (Map.FromMonad Choice1Of2)) Reader.ask : ErrorT<Reader<'R,Choice<'R,'E>>>
    static member Local (ErrorT m : ErrorT<Reader<'R2,Choice<'R2,'E>>>, f:'R1->'R2) = ErrorT (Reader.local f m)

    static member inline Tell (w:'Monoid) = w |> Writer.tell |> Lift.Invoke : ErrorT<Writer<'Monoid,Choice<unit,'E>>>
    static member inline Listen m : ErrorT<Writer<'Monoid,Choice<'T*'Monoid,'E>>> =
        let liftError (m, w) = Error.map (fun x -> (x, w)) m
        ErrorT (Writer.listen (ErrorT.run m) >>= (result << liftError))

    static member inline Pass m = ErrorT (ErrorT.run m >>= choice (result << Choice2Of2) (Map.Invoke Choice1Of2 << Writer.pass << result)) : ErrorT<Writer<'Monoid,Choice<'T,'E>>>

    static member get_Get() = Lift.Invoke State.get          : ErrorT<State<'S,Choice<_,'E>>>
    static member Put (x:'S) = x |> State.put |> Lift.Invoke : ErrorT<State<'S,Choice<_,'E>>>