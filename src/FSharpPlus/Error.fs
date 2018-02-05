namespace FSharpPlus.Data

open System
open FSharpPlus.Control
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
    let Exception :Result<_,exn>   -> _ = function Error e -> e | _ -> exn ()


/// Monad Transformer for Result<'T, 'E>
[<Struct>]
type ResultT<'``monad<'result<'t,'e>>``> = ResultT of '``monad<'result<'t,'e>>``

/// Basic operations on ResultT
[<RequireQualifiedAccess>]
module ResultT =
    let run (ResultT x) = x : '``Monad<'Result<'T,'E>>``
    let inline bind (f:'T->ResultT<'``Monad<'Result<'U,'E>>``>) (ResultT m:ResultT<'``Monad<'Result<'T,'E>>``>) = (ResultT (m >>= (fun a -> match a with Error l -> result (Error l) | Ok r -> run (f r))))
    let inline apply  (ResultT f:ResultT<'``Monad<'Result<('T -> 'U),'E>>``>) (ResultT x:ResultT<'``Monad<'Result<'T,'E>>``>) = ResultT(map Result.apply f <*> x) : ResultT<'``Monad<'Result<'U,'E>>``>
    let inline map  (f:'T->'U) (ResultT m:ResultT<'``Monad<'Result<'T,'E>>``>) = ResultT (map (Result.map f) m) :ResultT<'``Monad<'Result<('T -> 'U),'E>>``>

type ResultT<'``monad<'result<'t,'e>>``> with
    static member inline Return (x : 'T) = ResultT (result (Ok x))                                                                            : ResultT<'``Monad<'Result<'T,'E>>``>
    static member inline Map    (x : ResultT<'``Monad<'Result<'T,'E>>``>, f : 'T->'U) = ResultT.map f x                                       : ResultT<'``Monad<'Result<'U,'E>>``>
    static member inline (<*>)  (f : ResultT<'``Monad<'Result<('T -> 'U),'E>>``>, x : ResultT<'``Monad<'Result<'T,'E>>``>) = ResultT.apply f x: ResultT<'``Monad<'Result<'U,'E>>``>
    static member inline Bind   (x : ResultT<'``Monad<'Result<'T,'E>>``>, f : 'T->ResultT<'``Monad<'Result<'U,'E>>``>)     = ResultT.bind f x

    static member inline Lift (x:'``Monad<'T>``) = x |> liftM Ok |> ResultT : ResultT<'``Monad<Result<'T,'E>>``>

    static member inline Throw (x:'E) =  x |> Error |> result |> ResultT : ResultT<'``Monad<Result<'T,'E>>``>
    static member inline Catch (ResultT x :ResultT<'``MonadError<'E1,'T>``>, f: 'E1 -> _) = (ResultT (x >>= (fun a -> match a with Error l -> ResultT.run (f l) | Ok r -> result (Ok r)))) : ResultT<'``Monad<Result<'T,'E2>>``>

    static member inline LiftAsync (x :Async<'T>) = lift (liftAsync x) : '``ResultT<'MonadAsync<'T>>``

    static member inline CallCC (f:('T -> ResultT<'``MonadCont<'R,Result<'U,'E>>``>) -> _) :ResultT<'``MonadCont<'R, Result<'T,'E>>``> = ResultT(callCC <| fun c -> ResultT.run(f (ResultT << c << Ok)))

    static member inline get_Ask () = (ResultT << (map Ok)) ask : ResultT<'``MonadReader<'R,Result<'R,'E>>``>
    static member inline Local (ResultT m : ResultT<'``MonadReader<'R2,Result<'R2,'E>>``>, f:'R1->'R2) = ResultT (local f m)

    static member inline Tell (w:'Monoid) = w |> tell |> lift : '``ResultT<Writer<'Monoid,Result<unit,'E>>>``
    static member inline Listen m : ResultT<'``MonadWriter<'Monoid,Result<'T*'Monoid,'E>>``> =
        let liftError (m, w) = Result.map (fun x -> (x, w)) m
        ResultT (listen (ResultT.run m) >>= (result << liftError))

    static member inline Pass m = ResultT (ResultT.run m >>= either (map Ok << pass << result) (result << Error)) : ResultT<'``MonadWriter<'Monoid,Result<'T,'E>>``>

    static member inline get_Get()  = lift get         : '``ResultT<'MonadState<'S,Result<_,'E>>>``
    static member inline Put (x:'S) = x |> put |> lift : '``ResultT<'MonadState<'S,Result<_,'E>>>``