namespace FSharpPlus.Data

open System
open FSharpPlus.Control
open FSharpPlus
open System.ComponentModel


#if !FABLE_COMPILER
/// Additional operations on Result
[<RequireQualifiedAccess>]
module Result =
    let inline traverse f = function Ok x -> Map.Invoke Ok (f x) | Error x -> result (Error x)
#endif
    
/// Result<'TSuccess,'TFailure> specialized in 'TFailure = Exception 
[<Runtime.CompilerServices.Extension>]
module ResultOrException =
    [<Runtime.CompilerServices.Extension>]
    let IsResult : Result<_,exn>    -> _ = function Ok _ -> true | _ -> false

    [<Runtime.CompilerServices.Extension>]
    let IsException : Result<_,exn> -> _ = function Error _ -> true | _ -> false

    [<Runtime.CompilerServices.Extension>]
    let Result : Result<_,exn>      -> _ = function Ok v -> v | Error e -> raise e

    [<Runtime.CompilerServices.Extension>]
    let Exception : Result<_,exn>   -> _ = function Error e -> e | _ -> exn ()


/// Monad Transformer for Result<'T, 'E>
[<Struct>]
type ResultT<'``monad<'result<'t,'e>>``> = ResultT of '``monad<'result<'t,'e>>``

/// Basic operations on ResultT
[<RequireQualifiedAccess>]
module ResultT =
    let run (ResultT x) = x : '``Monad<'Result<'T,'E>>``
    #if !FABLE_COMPILER
    let inline hoist (x: Result<'T,'TError>) = ResultT (result x) : ResultT<'``Monad<Result<'T,'TError>>``>
    let inline bind (f: 'T->ResultT<'``Monad<'Result<'U,'E>>``>) (ResultT m: ResultT<'``Monad<'Result<'T,'E>>``>) = (ResultT (m >>= (fun a -> match a with Error l -> result (Error l) | Ok r -> run (f r))))
    
    let inline apply (ResultT f:ResultT<'``Monad<'Result<('T -> 'U),'E>>``>) (ResultT x: ResultT<'``Monad<'Result<'T,'E>>``>) = ResultT (map Result.apply f <*> x) : ResultT<'``Monad<'Result<'U,'E>>``>
    let inline map (f: 'T->'U) (ResultT m: ResultT<'``Monad<'Result<'T,'E>>``>) = ResultT (map (Result.map f) m) : ResultT<'``Monad<'Result<('T -> 'U),'E>>``>
    #endif

type ResultT<'``monad<'result<'t,'e>>``> with
    #if !FABLE_COMPILER
    static member inline Return (x: 'T) = ResultT (result (Ok x))                                                                           : ResultT<'``Monad<'Result<'T,'E>>``>

    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member inline Map   (x: ResultT<'``Monad<'Result<'T,'E>>``>, f: 'T->'U) = ResultT.map f x                                        : ResultT<'``Monad<'Result<'U,'E>>``>

    static member inline (<*>) (f: ResultT<'``Monad<'Result<('T -> 'U),'E>>``>, x: ResultT<'``Monad<'Result<'T,'E>>``>) = ResultT.apply f x : ResultT<'``Monad<'Result<'U,'E>>``>
    static member inline (>>=) (x: ResultT<'``Monad<'Result<'T,'E>>``>, f: 'T->ResultT<'``Monad<'Result<'U,'E>>``>)     = ResultT.bind  f x
    #endif

    static member inline TryWith (source: ResultT<'``Monad<'Result<'T,'E>>``>, f: exn -> ResultT<'``Monad<'Result<'T,'E>>``>) = ResultT (TryWith.Invoke (ResultT.run source) (ResultT.run << f))
    static member inline TryFinally (computation: ResultT<'``Monad<'Result<'T,'E>>``>, f) = ResultT (TryFinally.Invoke     (ResultT.run computation) f)
    static member inline Using (resource, f: _ -> ResultT<'``Monad<'Result<'T,'E>>``>)    = ResultT (Using.Invoke resource (ResultT.run << f))
    static member inline Delay (body : unit   ->  ResultT<'``Monad<'Result<'T,'E>>``>)    = ResultT (Delay.Invoke (fun _ -> ResultT.run (body ())))

    #if !FABLE_COMPILER
    static member inline Lift (x: '``Monad<'T>``) = x |> liftM Ok |> ResultT : ResultT<'``Monad<Result<'T,'E>>``>

    static member inline Throw (x: 'E) = x |> Error |> result |> ResultT : ResultT<'``Monad<Result<'T,'E>>``>
    static member inline Catch (ResultT x: ResultT<'``MonadError<'E1,'T>``>, f: 'E1 -> _) = (ResultT (x >>= (fun a -> match a with Error l -> ResultT.run (f l) | Ok r -> result (Ok r)))) : ResultT<'``Monad<Result<'T,'E2>>``>
    #endif

    static member inline LiftAsync (x: Async<'T>) = lift (liftAsync x) : '``ResultT<'MonadAsync<'T>>``

    static member inline CallCC (f: ('T -> ResultT<'``MonadCont<'R,Result<'U,'E>>``>) -> _) : ResultT<'``MonadCont<'R, Result<'T,'E>>``> = ResultT (callCC <| fun c -> ResultT.run (f (ResultT << c << Ok)))

    #if !FABLE_COMPILER
    static member inline get_Ask () = (ResultT << (map Ok)) ask : ResultT<'``MonadReader<'R,Result<'R,'E>>``>
    #endif

    static member inline Local (ResultT m : ResultT<'``MonadReader<'R2,Result<'R2,'E>>``>, f: 'R1->'R2) = ResultT (local f m)

    static member inline Tell (w: 'Monoid) = w |> tell |> lift : '``ResultT<Writer<'Monoid,Result<unit,'E>>>``
    #if !FABLE_COMPILER
    static member inline Listen m : ResultT<'``MonadWriter<'Monoid,Result<'T*'Monoid,'E>>``> =
        let liftError (m, w) = Result.map (fun x -> (x, w)) m
        ResultT (listen (ResultT.run m) >>= (result << liftError))

    static member inline Pass m = ResultT (ResultT.run m >>= either (map Ok << pass << result) (result << Error)) : ResultT<'``MonadWriter<'Monoid,Result<'T,'E>>``>
    #endif

    static member inline get_Get ()  = lift get         : '``ResultT<'MonadState<'S,Result<_,'E>>>``
    static member inline Put (x: 'S) = x |> put |> lift : '``ResultT<'MonadState<'S,Result<_,'E>>>``


[<Struct>]
type ChoiceT<'``monad<'choice<'t,'e>>``> = ChoiceT of '``monad<'choice<'t,'e>>``


[<RequireQualifiedAccess>]
module ChoiceT =
    let run (ChoiceT x) = x : '``Monad<'Choice<'T,'E>>``
    #if !FABLE_COMPILER
    let inline hoist (x: Choice<'T,'TError>) = ChoiceT (result x) : ChoiceT<'``Monad<Choice<'T,'TError>>``>
    let inline bind (f: 'T->ChoiceT<'``Monad<'ChoiceT<'U,'E>>``>) (ChoiceT m: ChoiceT<'``Monad<'Choice<'T,'E>>``>) = (ChoiceT (m >>= (fun a -> match a with Choice2Of2 l -> result (Choice2Of2 l) | Choice1Of2 r -> run (f r))))
    
    let inline apply (ChoiceT f: ChoiceT<'``Monad<'Choice<('T -> 'U),'E>>``>) (ChoiceT x: ChoiceT<'``Monad<'Choice<'T,'E>>``>) = ChoiceT (map Choice.apply f <*> x) : ChoiceT<'``Monad<'Choice<'U,'E>>``>
    let inline map  (f: 'T->'U) (ChoiceT m: ChoiceT<'``Monad<'Choice<'T,'E>>``>) = ChoiceT (map (Choice.map f) m) : ChoiceT<'``Monad<'Choice<('T -> 'U),'E>>``>
    #endif

type ChoiceT<'``monad<'choice<'t,'e>>``> with
    #if !FABLE_COMPILER
    static member inline Return (x: 'T) = ChoiceT (result (Choice1Of2 x))                                                                   : ChoiceT<'``Monad<'Choice<'T,'E>>``>

    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member inline Map   (x: ChoiceT<'``Monad<'Choice<'T,'E>>``>, f: 'T->'U) = ChoiceT.map f x                                        : ChoiceT<'``Monad<'Choice<'U,'E>>``>

    static member inline (<*>) (f: ChoiceT<'``Monad<'Choice<('T -> 'U),'E>>``>, x: ChoiceT<'``Monad<'Choice<'T,'E>>``>) = ChoiceT.apply f x : ChoiceT<'``Monad<'Choice<'U,'E>>``>
    static member inline (>>=) (x: ChoiceT<'``Monad<'Choice<'T,'E>>``>, f: 'T->ChoiceT<'``Monad<'Choice<'U,'E>>``>)     = ChoiceT.bind f x

    static member inline Lift (x: '``Monad<'T>``) = x |> liftM Choice1Of2 |> ChoiceT : ChoiceT<'``Monad<Choice<'T,'E>>``>

    static member inline Throw (x: 'E) = x |> Choice2Of2 |> result |> ChoiceT : ChoiceT<'``Monad<Choice<'T,'E>>``>
    static member inline Catch (ChoiceT x: ChoiceT<'``MonadError<'E1,'T>``>, f: 'E1 -> _) = (ChoiceT (x >>= (fun a -> match a with Choice2Of2 l -> ChoiceT.run (f l) | Choice1Of2 r -> result (Choice1Of2 r)))) : ChoiceT<'``Monad<Choice<'T,'E2>>``>
    #endif

    static member inline LiftAsync (x: Async<'T>) = lift (liftAsync x) : '``ChoiceT<'MonadAsync<'T>>``

    static member inline CallCC (f: ('T -> ChoiceT<'``MonadCont<'R,Choice<'U,'E>>``>) -> _) : ChoiceT<'``MonadCont<'R, Choice<'T,'E>>``> = ChoiceT (callCC <| fun c -> ChoiceT.run (f (ChoiceT << c << Choice1Of2)))

    #if !FABLE_COMPILER
    static member inline get_Ask () = (ChoiceT << (map Choice1Of2)) ask : ChoiceT<'``MonadReader<'R,Choice<'R,'E>>``>
    #endif

    static member inline Local (ChoiceT m: ChoiceT<'``MonadReader<'R2,Choice<'R2,'E>>``>, f: 'R1->'R2) = ChoiceT (local f m)

    static member inline Tell (w: 'Monoid) = w |> tell |> lift : '``ChoiceT<Writer<'Monoid,Choice<unit,'E>>>``

    #if !FABLE_COMPILER
    static member inline Listen m : ChoiceT<'``MonadWriter<'Monoid,Choice<'T*'Monoid,'E>>``> =
        let liftError (m, w) = Choice.map (fun x -> (x, w)) m
        ChoiceT (listen (ChoiceT.run m) >>= (result << liftError))

    static member inline Pass m = ChoiceT (ChoiceT.run m >>= either (map Choice1Of2 << pass << result) (result << Error)) : ChoiceT<'``MonadWriter<'Monoid,Choice<'T,'E>>``>
    #endif

    static member inline get_Get ()  = lift get         : '``ChoiceT<'MonadState<'S,Choice<_,'E>>>``
    static member inline Put (x: 'S) = x |> put |> lift : '``ChoiceT<'MonadState<'S,Choice<_,'E>>>``