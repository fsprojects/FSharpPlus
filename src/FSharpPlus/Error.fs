namespace FSharpPlus.Data

open System
open FsControl
open FSharpPlus

/// Additional operations on Result
[<RequireQualifiedAccess>]
module Result =
    let inline traverse f = function Ok x -> Map.Invoke Ok (f x) | Error x -> result (Error x)

/// Result<'TSuccess,'TFailure> specialized in 'TFailure = Exception 
[<Runtime.CompilerServices.Extension>]
module ResultOrException =
    [<Runtime.CompilerServices.Extension>]
    let IsResult  :Result<_,exn>   -> _ = function Ok _ -> true | _ -> false

    [<Runtime.CompilerServices.Extension>]
    let IsException :Result<_,exn> -> _ = function Error _ -> true | _ -> false

    [<Runtime.CompilerServices.Extension>]
    let Result :Result<_,exn>      -> _ = function Ok v -> v | Error e -> raise e

    [<Runtime.CompilerServices.Extension>]
    let Exception :Result<_,exn>   -> _ = function Error e -> e | _ -> new Exception()


/// Monad Transformer for Result<'T, 'E>
type ErrorT<'``monad<'result<'t,'e>>``> = ErrorT of '``monad<'result<'t,'e>>``

/// Basic operations on ErrorT
[<RequireQualifiedAccess>]
module ErrorT =
    let run (ErrorT x) = x : '``Monad<'Result<'T,'E>>``
    let inline bind (f:'T->ErrorT<'``Monad<'Result<'U,'E>>``>) (ErrorT m:ErrorT<'``Monad<'Result<'T,'E>>``>) = (ErrorT (m >>= (fun a -> match a with Error l -> result (Error l) | Ok r -> run (f r))))
    let inline apply  (ErrorT f:ErrorT<'``Monad<'Result<('T -> 'U),'E>>``>) (ErrorT x:ErrorT<'``Monad<'Result<'T,'E>>``>) = ErrorT(map Result.apply f <*> x) : ErrorT<'``Monad<'Result<'U,'E>>``>
    let inline map  (f:'T->'U) (ErrorT m:ErrorT<'``Monad<'Result<'T,'E>>``>) = ErrorT (map (Result.map f) m) :ErrorT<'``Monad<'Result<('T -> 'U),'E>>``>

type ErrorT<'``monad<'result<'t,'e>>``> with
    static member inline Return (x : 'T) = ErrorT (result (Ok x))                                                                           : ErrorT<'``Monad<'Result<'T,'E>>``>
    static member inline Map    (x : ErrorT<'``Monad<'Result<'T,'E>>``>, f : 'T->'U) = ErrorT.map f x                                       : ErrorT<'``Monad<'Result<'U,'E>>``>
    static member inline (<*>)  (f : ErrorT<'``Monad<'Result<('T -> 'U),'E>>``>, x : ErrorT<'``Monad<'Result<'T,'E>>``>) = ErrorT.apply f x : ErrorT<'``Monad<'Result<'U,'E>>``>
    static member inline Bind   (x : ErrorT<'``Monad<'Result<'T,'E>>``>, f : 'T->ErrorT<'``Monad<'Result<'U,'E>>``>)     = ErrorT.bind f x

    static member inline Lift (x:'``Monad<'T>``) = x |> liftM Ok |> ErrorT : ErrorT<'``Monad<Result<'T,'E>>``>

    static member inline Throw (x:'E) =  x |> Error |> result |> ErrorT : ErrorT<'``Monad<Result<'T,'E>>``>
    static member inline Catch (ErrorT x :ErrorT<'``MonadError<'E1,'T>``>, f: 'E1 -> _) = (ErrorT (x >>= (fun a -> match a with Error l -> ErrorT.run (f l) | Ok r -> result (Ok r)))) : ErrorT<'``Monad<Result<'T,'E2>>``>

    static member inline LiftAsync (x :Async<'T>) = lift (liftAsync x) : '``ErrorT<'MonadAsync<'T>>``

    static member inline CallCC (f:('T -> ErrorT<'``MonadCont<'R,Result<'U,'E>>``>) -> _) :ErrorT<'``MonadCont<'R, Result<'T,'E>>``> = ErrorT(callCC <| fun c -> ErrorT.run(f (ErrorT << c << Ok)))

    static member inline get_Ask() = (ErrorT << (map Ok)) ask : ErrorT<'``MonadReader<'R,Result<'R,'E>>``>
    static member inline Local (ErrorT m : ErrorT<'``MonadReader<'R2,Result<'R2,'E>>``>, f:'R1->'R2) = ErrorT (local f m)

    static member inline Tell (w:'Monoid) = w |> tell |> lift :   '``ErrorT<Writer<'Monoid,Result<unit,'E>>>``
    static member inline Listen m : ErrorT<'``MonadWriter<'Monoid,Result<'T*'Monoid,'E>>``> =
        let liftError (m, w) = Result.map (fun x -> (x, w)) m
        ErrorT (listen (ErrorT.run m) >>= (result << liftError))

    static member inline Pass m = ErrorT (ErrorT.run m >>= either (map Ok << pass << result) (result << Error)) : ErrorT<'``MonadWriter<'Monoid,Result<'T,'E>>``>

    static member inline get_Get()  = lift get         : '``ErrorT<'MonadState<'S,Result<_,'E>>>``
    static member inline Put (x:'S) = x |> put |> lift : '``ErrorT<'MonadState<'S,Result<_,'E>>>``