namespace FSharpPlus.Data

#nowarn "0193"

open System
open System.ComponentModel
open FSharpPlus
open FSharpPlus.Internals.Prelude
open FSharpPlus.Control


#if !FABLE_COMPILER || FABLE_COMPILER_3
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


#if !FABLE_COMPILER || FABLE_COMPILER_3

/// Monad Transformer for Result<'T, 'E>
[<Struct>]
type ResultT<'e, 'monad, 't> =
    /// Represented as 'monad<'result<'t, 'e>>
    Value of obj

type [<AutoOpen>]ResultTOperations =
    [<GeneralizableValue>]
    static member inline ResultT< ^``monad<Result<'t, 'e>>``, ^monad, 'e, 't when (Map or  ^``monad<Result<'t, 'e>>`` or  ^monad) : (static member Map: ( ^``monad<Result<'t, 'e>>`` * (Result<'t, 'e> -> __)) * Map ->  ^monad)
                                                                        and  (Map or  ^monad or  ^``monad<Result<'t, 'e>>``) : (static member Map: ( ^monad * (__ -> Result<'t, 'e>)) * Map ->  ^``monad<Result<'t, 'e>>``)
                                                                        > (x: '``monad<Result<'t, 'e>>``) : ResultT<'e, 'monad, 't> =
        if opaqueId false then
            let _: 'monad = Unchecked.defaultof<'``monad<Result<'t, 'e>>``> |> map (fun (_: Result<'t, 'e>) -> Unchecked.defaultof<__>)
            let _: '``monad<Result<'t, 'e>>`` = Unchecked.defaultof<'monad> |> map (fun (_: __) -> Unchecked.defaultof<Result<'t, 'e>>)
            ()
        Value (box x)

module [<AutoOpen>]ResultTOperations =
    let inline resultT (x: '``monad<Result<'t, 'e>>``) : ResultT<'e, 'monad, 't> = ResultT x
    let inline (|ResultT|) (Value x: ResultT<'E, 'Monad, 'T>) =
        if opaqueId false then
            let _: '``Monad<Result<'T, 'E>>`` = map (fun (_: __) -> Unchecked.defaultof<Result<'T, 'E>>) Unchecked.defaultof<'Monad>
            ()
        x |> unbox : '``Monad<Result<'T, 'E>>``


/// Basic operations on ResultT
[<RequireQualifiedAccess>]
module ResultT =

    let inline run (ResultT (x: '``Monad<Result<'T, 'E>>``) : ResultT<'E, 'Monad, 'T>) = x

    /// Embed a Monad<'T> into a ResultT<'Monad<Result<'T,'Error>>>
    let inline lift<'T, 'E, .. > (x: '``Monad<'T>``) : ResultT<'E, 'Monad, 'T> =
        (x |> (if opaqueId false then liftM else map) Result<'T, 'E>.Ok : '``Monad<Result<'T, 'E>>``) |> ResultT

    /// Transform a Result<'T, 'E> to a ResultT<'Monad<Result<'T, 'E>>>
    let inline hoist (x: Result<'T, 'E>) : ResultT<'E, 'Monad, 'T> =
        let _: '``Monad<Result<'T, 'E>>`` = 
            if opaqueId false then
                map (fun _ -> Unchecked.defaultof<Result<'T, 'E>>) Unchecked.defaultof<'Monad>
            else Unchecked.defaultof<_>
        ResultT (result x : '``Monad<Result<'T, 'E>>``)


    let inline bind<'T, 'U, 'E, .. > (f: 'T -> ResultT<'E, 'Monad, 'U>) (ResultT (m: '``Monad<Result<'T, 'E>>``) : ResultT<'E, 'Monad, 'T>) : ResultT<'E, 'Monad, 'U> =
        (ResultT (m >>= (fun (a: Result<'T, 'E>) -> match a with Error l -> result (Error l: Result<'U, 'E>) | Ok r -> (run (f r) : '``Monad<Result<'U, 'E>>``))))

    let inline apply (ResultT (f: '``Monad<Result<('T -> 'U), 'E>>``) : ResultT<'E, 'Monad, 'T -> 'U>) (ResultT (x : '``Monad<Result<'T, 'E>>``) : ResultT<'E, 'Monad, 'T>) : ResultT<'E, 'Monad, 'U> =
        ResultT ((map: (Result<'T -> 'U, 'E> -> _) -> _ -> '``Monad<(Result<'T,'E> -> Result<'U,'E>>)``) Result.apply f <*> x : '``Monad<Result<'U, 'E>>``)

    let inline map (f: 'T -> 'U) (ResultT (m: '``Monad<Result<'T, 'E>>``) : ResultT<'E, 'Monad, 'T>) : ResultT<'E, 'Monad, 'U> =
        ResultT (map (Result.map f) m : '``Monad<Result<'U, 'E>>``)

    let inline map2 (f: 'T -> 'U -> 'V) (ResultT (x: '``Monad<Result<'T, 'E>>``) : ResultT<'E, 'Monad, 'T>) (ResultT (y: '``Monad<Result<'U, 'E>>``) : ResultT<'E, 'Monad, 'U>) : ResultT<'E, 'Monad, 'V> =
        ResultT (lift2 (Result.map2 f: _ -> _ -> Result<'V, 'E>) x y : '``Monad<Result<'V, 'E>>``)

    let inline map3 (f: 'T -> 'U -> 'V -> 'W) (ResultT (x: '``Monad<Result<'T, 'E>>``) : ResultT<'E, 'Monad, 'T>) (ResultT (y: '``Monad<Result<'U, 'E>>``) : ResultT<'E, 'Monad, 'U>) (ResultT (z: '``Monad<Result<'V, 'E>>``) : ResultT<'E, 'Monad, 'V>) : ResultT<'E, 'Monad, 'W> =
        ResultT (lift3 (Result.map3 f: _ -> _ -> _ -> Result<'W, 'E>) x y z : '``Monad<Result<'W, 'E>>``)



type ResultT<'e, 'monad, 't> with

    static member inline Return (x: 'T) : ResultT<'E, 'Monad, 'T> =
        let _: '``Monad<Result<'T, 'E>>`` =
            if opaqueId false then
                result Unchecked.defaultof<Result<'T, 'E>>
            else Unchecked.defaultof<_>
        let _: '``Monad<Result<'T, 'E>>`` = 
            if opaqueId false then
                map (fun (_: __) -> Unchecked.defaultof<Result<'T, 'E>>) Unchecked.defaultof<'Monad>
            else Unchecked.defaultof<_>
        Value (result (Ok x) : '``Monad<Result<'T, 'E>>``)


    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member inline Map   (x: ResultT<'E, 'Monad, 'T>, f: 'T->'U) : ResultT<'E, 'Monad, 'U> = ResultT.map f x

    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member inline Lift2 (f: 'T -> 'U -> 'V, x: ResultT<'E, 'Monad, 'T>, y: ResultT<'E, 'Monad, 'U>) : ResultT<'E, 'Monad, 'V> =
        ResultT.map2 f x y
    
    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member inline Lift3 (f: 'T -> 'U -> 'V -> 'W, x: ResultT<'E, 'Monad, 'T>, y: ResultT<'E, 'Monad, 'U>, z: ResultT<'E, 'Monad, 'V>) : ResultT<'E, 'Monad, 'W> =
        ResultT.map3 f x y z

    static member inline (<*>) (f: ResultT<'E, 'Monad, 'T -> 'U>, x: ResultT<'E, 'Monad, 'T>) = ResultT.apply f x : ResultT<'E, 'Monad, 'U>

    static member inline (>>=) (x: ResultT<'E, 'Monad, 'T>, f: 'T -> ResultT<'E, 'Monad, 'U>) =
        ResultT.bind<'T, 'U, 'E, 'Monad, '``Monad<Result<'T, 'E>>``, '``Monad<Result<'U, 'E>>``> f x : ResultT<'E, 'Monad, 'U>

    static member inline TryWith (source: unit -> ResultT<'E, 'Monad, 'T>, f: exn -> ResultT<'E, 'Monad, 'T>) = ResultTOperations.ResultT< '``Monad<Result<'T, 'E>>``, 'Monad, 'E, 'T> <| (TryWith.Invoke  (fun () -> ResultT.run (source ())) (ResultT.run << f))
    static member inline TryFinally (computation: unit -> ResultT<'E, 'Monad, 'T>, f) = ResultTOperations.ResultT< '``Monad<Result<'T, 'E>>``, 'Monad, 'E, 'T> (TryFinally.Invoke (fun () -> ResultT.run (computation ())) f)
    static member inline Using (resource, f: _ -> ResultT<'E, 'Monad, 'T>)    = ResultTOperations.ResultT< '``Monad<Result<'T, 'E>>``, 'Monad, 'E, 'T> (Using.Invoke resource (ResultT.run << f))
    static member inline Delay (body: unit -> ResultT<'E, 'Monad, 'T>) = Value ((Delay.Invoke (fun () -> ResultT.run (body ()) : '``Monad<Result<'T, 'E>>``)) |> box<'``Monad<Result<'T, 'E>>``>)


    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member inline Lift (x: '``Monad<'T>``) : ResultT<'E, 'Monad, 'T> = ResultT.lift<_, _, _, ^``Monad<Result<'T, 'E>>``, 'Monad> x

    static member inline Throw x = ((x |> Error : Result<'T, 'E>) |> result : '``Monad<Result<'T, 'E>>``)  |> ResultTOperations.ResultT : ResultT<'E, 'Monad, 'T>
    static member inline Catch (ResultT (x: '``Monad<Result<'T, 'E1>>``) : ResultT<'E1, 'Monad, 'T>, f: 'E1 -> ResultT<'E2, 'Monad, 'T>) : ResultT<'E2, 'Monad, 'T> =
        ResultTOperations.ResultT (x >>= fun a -> match a with Error l -> ResultT.run (f l) | Ok r -> (result (Result<'T, 'E2>.Ok r) : '``Monad<Result<'T, 'E2>>``))

    static member inline LiftAsync (x: Async<'T>) : ResultT<'E, 'MonadAsync, 'T> =
        ResultT.lift<_, _, _, '``Monad<Result<'T, 'E>>``, _> (liftAsync x: '``MonadAsync<'T>``)

    // 'Monad : MonadCont<'R, 'Monad>
    static member inline CallCC (f: ('T -> ResultT<'E, 'Monad, 'U>) -> ResultT<'E, 'Monad, 'T>) : ResultT<'E, 'Monad, 'T> =
        resultT ((callCC <| fun (c: _ -> '``Monad<Result<'U, 'E>>``) -> ResultT.run (f (ResultTOperations.ResultT << c << Result<'T, 'E>.Ok))) : '``Monad<Result<'T, 'E>>``)
    
    // 'Monad : MonadReader<'R, 'Monad>
    static member inline get_Ask () : ResultT<'E, '``MonadReader<'R>``, 'R> = ResultT.lift<_, _, '``MonadReader<'R, 'R>``, '``MonadReader<'R, Result<'R, 'E>>``, '``MonadReader<'R>``> ask
    static member inline Local (ResultT m : ResultT<'E, '``MonadReader<'R2>``, 'T>, f: 'R1 -> 'R2) : ResultT<'E, '``MonadReader<'R1>``, 'T> =
        ResultTOperations.ResultT (local f (m: '``MonadReader<'R2, Result<'T, 'E>>``) : '``MonadReader<'R1, Result<'T, 'E>>``)

    static member inline Tell (w: 'Monoid) : (*MonadWriter<'Monoid, *)ResultT<'E, '``MonadWriter<'Monoid>``, unit> =
        (w |> tell : '``MonadWriter<'Monoid, unit>``) |> ResultT.lift<_, _, _, '``MonadWriter<'Monoid, Result<unit, 'E>>``, '``MonadWriter<'Monoid>``>

    static member inline Listen (m: ResultT<'E, '``MonadWriter<'Monoid>``, 'T>) : ResultT<'E, '``MonadWriter<'Monoid>``, ('T * 'Monoid)> =
        let liftError (m, w) = Result.map (fun x -> (x, w)) m
        ResultTOperations.ResultT<'``MonadWriter<'Monoid, Result<('T * 'Monoid), 'E>>``, _, _, _> ((listen (ResultT.run m: '``MonadWriter<'Monoid, Result<'T, 'E>>``) : '``MonadWriter<'Monoid, Result<'T, 'E> * 'Monoid>``) >>= ((result: Result<('T * 'Monoid), 'E> -> '``MonadWriter<'Monoid, Result<('T * 'Monoid), 'E>>``) << liftError))

    static member inline Pass (m: ResultT<'E, '``MonadWriter<'Monoid>``, ('T * ('Monoid -> 'Monoid))>) : ResultT<'E, '``MonadWriter<'Monoid>``, 'T> =
        ResultTOperations.ResultT<'``MonadWriter<'Monoid, Result<'T, 'E>>``, _, _, _> ((ResultT.run m: '``MonadWriter<'Monoid, Result<('T * ('Monoid -> 'Monoid)), 'E>>``) >>= either (map Result<'T, 'E>.Ok << (pass: '``MonadWriter<'Monoid, ('T * ('Monoid -> 'Monoid))>`` -> '``MonadWriter<'Monoid, 'T>``) << (result: ('T * ('Monoid -> 'Monoid)) -> _)) (result << Result<'T, 'E>.Error))
    
    static member inline get_Get () : ResultT<'E, '``MonadState<'S>``, 'S> = ResultT.lift<_, _, '``MonadState<'S, 'S>``, '``MonadState<'S, Result<'S, 'E>>``, '``MonadState<'S>``> get
    static member inline Put (x: 'S) : ResultT<'E, '``MonadState<'S>``, unit> = x |> put |> ResultT.lift<_, _, '``MonadState<'S, unit>``, '``MonadState<'S, Result<unit, 'E>>``, '``MonadState<'S>``>




/// Monad Transformer for Choice<'T, 'E>
[<Struct>]
type ChoiceT<'e, 'monad, 't> =
    /// Represented as 'monad<'choice<'t, 'e>>
    Value of obj

type [<AutoOpen>]ChoiceTOperations =
    [<GeneralizableValue>]
    static member inline ChoiceT< ^``monad<Choice<'t, 'e>>``, ^monad, 'e, 't when (Map or  ^``monad<Choice<'t, 'e>>`` or  ^monad) : (static member Map: ( ^``monad<Choice<'t, 'e>>`` * (Choice<'t, 'e> -> __)) * Map ->  ^monad)
                                                                        and  (Map or  ^monad or  ^``monad<Choice<'t, 'e>>``) : (static member Map: ( ^monad * (__ -> Choice<'t, 'e>)) * Map ->  ^``monad<Choice<'t, 'e>>``)
                                                                        > (x: '``monad<Choice<'t, 'e>>``) : ChoiceT<'e, 'monad, 't> =
        if opaqueId false then
            let _: 'monad = Unchecked.defaultof<'``monad<Choice<'t, 'e>>``> |> map (fun (_: Choice<'t, 'e>) -> Unchecked.defaultof<__>)
            let _: '``monad<Choice<'t, 'e>>`` = Unchecked.defaultof<'monad> |> map (fun (_: __) -> Unchecked.defaultof<Choice<'t, 'e>>)
            ()
        Value (box x)

module [<AutoOpen>]ChoiceTOperations =
    let inline resultT (x: '``monad<Choice<'t, 'e>>``) : ChoiceT<'e, 'monad, 't> = ChoiceT x
    let inline (|ChoiceT|) (Value x: ChoiceT<'E, 'Monad, 'T>) =
        if opaqueId false then
            let _: '``Monad<Choice<'T, 'E>>`` = map (fun (_: __) -> Unchecked.defaultof<Choice<'T, 'E>>) Unchecked.defaultof<'Monad>
            ()
        x |> unbox : '``Monad<Choice<'T, 'E>>``


/// Basic operations on ChoiceT
[<RequireQualifiedAccess>]
module ChoiceT =

    let inline run (ChoiceT (x: '``Monad<Choice<'T, 'E>>``) : ChoiceT<'E, 'Monad, 'T>) = x

    /// Embed a Monad<'T> into a ChoiceT<'Monad<Choice<'T,'Error>>>
    let inline lift<'T, 'E, .. > (x: '``Monad<'T>``) : ChoiceT<'E, 'Monad, 'T> =
        (x |> (if opaqueId false then liftM else map) Choice<'T, 'E>.Choice1Of2 : '``Monad<Choice<'T, 'E>>``) |> ChoiceT

    /// Transform a Choice<'T, 'E> to a ChoiceT<'Monad<Choice<'T, 'E>>>
    let inline hoist (x: Choice<'T, 'E>) : ChoiceT<'E, 'Monad, 'T> =
        let _: '``Monad<Choice<'T, 'E>>`` = 
            if opaqueId false then
                map (fun _ -> Unchecked.defaultof<Choice<'T, 'E>>) Unchecked.defaultof<'Monad>
            else Unchecked.defaultof<_>
        ChoiceT (result x : '``Monad<Choice<'T, 'E>>``)


    let inline bind<'T, 'U, 'E, .. > (f: 'T -> ChoiceT<'E, 'Monad, 'U>) (ChoiceT (m: '``Monad<Choice<'T, 'E>>``) : ChoiceT<'E, 'Monad, 'T>) : ChoiceT<'E, 'Monad, 'U> =
        (ChoiceT (m >>= (fun (a: Choice<'T, 'E>) -> match a with Choice2Of2 l -> result (Choice2Of2 l: Choice<'U, 'E>) | Choice1Of2 r -> (run (f r) : '``Monad<Choice<'U, 'E>>``))))

    let inline apply (ChoiceT (f: '``Monad<Choice<('T -> 'U), 'E>>``) : ChoiceT<'E, 'Monad, 'T -> 'U>) (ChoiceT (x : '``Monad<Choice<'T, 'E>>``) : ChoiceT<'E, 'Monad, 'T>) : ChoiceT<'E, 'Monad, 'U> =
        ChoiceT ((map: (Choice<'T -> 'U, 'E> -> _) -> _ -> '``Monad<(Choice<'T,'E> -> Choice<'U,'E>>)``) Choice.apply f <*> x : '``Monad<Choice<'U, 'E>>``)

    let inline map (f: 'T -> 'U) (ChoiceT (m: '``Monad<Choice<'T, 'E>>``) : ChoiceT<'E, 'Monad, 'T>) : ChoiceT<'E, 'Monad, 'U> =
        ChoiceT (map (Choice.map f) m : '``Monad<Choice<'U, 'E>>``)

    let inline map2 (f: 'T -> 'U -> 'V) (ChoiceT (x: '``Monad<Choice<'T, 'E>>``) : ChoiceT<'E, 'Monad, 'T>) (ChoiceT (y: '``Monad<Choice<'U, 'E>>``) : ChoiceT<'E, 'Monad, 'U>) : ChoiceT<'E, 'Monad, 'V> =
        ChoiceT (lift2 (Choice.map2 f: _ -> _ -> Choice<'V, 'E>) x y : '``Monad<Choice<'V, 'E>>``)

    let inline map3 (f: 'T -> 'U -> 'V -> 'W) (ChoiceT (x: '``Monad<Choice<'T, 'E>>``) : ChoiceT<'E, 'Monad, 'T>) (ChoiceT (y: '``Monad<Choice<'U, 'E>>``) : ChoiceT<'E, 'Monad, 'U>) (ChoiceT (z: '``Monad<Choice<'V, 'E>>``) : ChoiceT<'E, 'Monad, 'V>) : ChoiceT<'E, 'Monad, 'W> =
        ChoiceT (lift3 (Choice.map3 f: _ -> _ -> _ -> Choice<'W, 'E>) x y z : '``Monad<Choice<'W, 'E>>``)



type ChoiceT<'e, 'monad, 't> with

    static member inline Return (x: 'T) : ChoiceT<'E, 'Monad, 'T> =
        let _: '``Monad<Choice<'T, 'E>>`` =
            if opaqueId false then
                result Unchecked.defaultof<Choice<'T, 'E>>
            else Unchecked.defaultof<_>
        let _: '``Monad<Choice<'T, 'E>>`` = 
            if opaqueId false then
                map (fun (_: __) -> Unchecked.defaultof<Choice<'T, 'E>>) Unchecked.defaultof<'Monad>
            else Unchecked.defaultof<_>
        Value (result (Choice1Of2 x) : '``Monad<Choice<'T, 'E>>``)


    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member inline Map   (x: ChoiceT<'E, 'Monad, 'T>, f: 'T->'U) : ChoiceT<'E, 'Monad, 'U> = ChoiceT.map f x

    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member inline Lift2 (f: 'T -> 'U -> 'V, x: ChoiceT<'E, 'Monad, 'T>, y: ChoiceT<'E, 'Monad, 'U>) : ChoiceT<'E, 'Monad, 'V> =
        ChoiceT.map2 f x y
    
    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member inline Lift3 (f: 'T -> 'U -> 'V -> 'W, x: ChoiceT<'E, 'Monad, 'T>, y: ChoiceT<'E, 'Monad, 'U>, z: ChoiceT<'E, 'Monad, 'V>) : ChoiceT<'E, 'Monad, 'W> =
        ChoiceT.map3 f x y z

    static member inline (<*>) (f: ChoiceT<'E, 'Monad, 'T -> 'U>, x: ChoiceT<'E, 'Monad, 'T>) = ChoiceT.apply f x : ChoiceT<'E, 'Monad, 'U>

    static member inline (>>=) (x: ChoiceT<'E, 'Monad, 'T>, f: 'T -> ChoiceT<'E, 'Monad, 'U>) =
        ChoiceT.bind<'T, 'U, 'E, 'Monad, '``Monad<Choice<'T, 'E>>``, '``Monad<Choice<'U, 'E>>``> f x : ChoiceT<'E, 'Monad, 'U>

    static member inline TryWith (source: unit -> ChoiceT<'E, 'Monad, 'T>, f: exn -> ChoiceT<'E, 'Monad, 'T>) = ChoiceTOperations.ChoiceT< '``Monad<Choice<'T, 'E>>``, 'Monad, 'E, 'T> <| (TryWith.Invoke  (fun () -> ChoiceT.run (source ())) (ChoiceT.run << f))
    static member inline TryFinally (computation: unit -> ChoiceT<'E, 'Monad, 'T>, f) = ChoiceTOperations.ChoiceT< '``Monad<Choice<'T, 'E>>``, 'Monad, 'E, 'T> (TryFinally.Invoke (fun () -> ChoiceT.run (computation ())) f)
    static member inline Using (resource, f: _ -> ChoiceT<'E, 'Monad, 'T>)    = ChoiceTOperations.ChoiceT< '``Monad<Choice<'T, 'E>>``, 'Monad, 'E, 'T> (Using.Invoke resource (ChoiceT.run << f))
    static member inline Delay (body: unit -> ChoiceT<'E, 'Monad, 'T>) = Value ((Delay.Invoke (fun () -> ChoiceT.run (body ()) : '``Monad<Choice<'T, 'E>>``)) |> box<'``Monad<Choice<'T, 'E>>``>)


    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member inline Lift (x: '``Monad<'T>``) : ChoiceT<'E, 'Monad, 'T> = ChoiceT.lift<_, _, _, ^``Monad<Choice<'T, 'E>>``, 'Monad> x

    static member inline Throw x = ((x |> Choice2Of2 : Choice<'T, 'E>) |> result : '``Monad<Choice<'T, 'E>>``)  |> ChoiceTOperations.ChoiceT : ChoiceT<'E, 'Monad, 'T>
    static member inline Catch (ChoiceT (x: '``Monad<Choice<'T, 'E1>>``) : ChoiceT<'E1, 'Monad, 'T>, f: 'E1 -> ChoiceT<'E2, 'Monad, 'T>) : ChoiceT<'E2, 'Monad, 'T> =
        ChoiceTOperations.ChoiceT (x >>= fun a -> match a with Choice2Of2 l -> ChoiceT.run (f l) | Choice1Of2 r -> (result (Choice<'T, 'E2>.Choice1Of2 r) : '``Monad<Choice<'T, 'E2>>``))

    static member inline LiftAsync (x: Async<'T>) : ChoiceT<'E, 'MonadAsync, 'T> =
        ChoiceT.lift<_, _, _, '``Monad<Choice<'T, 'E>>``, _> (liftAsync x: '``MonadAsync<'T>``)

    // 'Monad : MonadCont<'R, 'Monad>
    static member inline CallCC (f: ('T -> ChoiceT<'E, 'Monad, 'U>) -> ChoiceT<'E, 'Monad, 'T>) : ChoiceT<'E, 'Monad, 'T> =
        resultT ((callCC <| fun (c: _ -> '``Monad<Choice<'U, 'E>>``) -> ChoiceT.run (f (ChoiceTOperations.ChoiceT << c << Choice<'T, 'E>.Choice1Of2))) : '``Monad<Choice<'T, 'E>>``)
    
    // 'Monad : MonadReader<'R, 'Monad>
    static member inline get_Ask () : ChoiceT<'E, '``MonadReader<'R>``, 'R> = ChoiceT.lift<_, _, '``MonadReader<'R, 'R>``, '``MonadReader<'R, Choice<'R, 'E>>``, '``MonadReader<'R>``> ask
    static member inline Local (ChoiceT m : ChoiceT<'E, '``MonadReader<'R2>``, 'T>, f: 'R1 -> 'R2) : ChoiceT<'E, '``MonadReader<'R1>``, 'T> =
        ChoiceTOperations.ChoiceT (local f (m: '``MonadReader<'R2, Choice<'T, 'E>>``) : '``MonadReader<'R1, Choice<'T, 'E>>``)

    static member inline Tell (w: 'Monoid) : (*MonadWriter<'Monoid, *)ChoiceT<'E, '``MonadWriter<'Monoid>``, unit> =
        (w |> tell : '``MonadWriter<'Monoid, unit>``) |> ChoiceT.lift<_, _, _, '``MonadWriter<'Monoid, Choice<unit, 'E>>``, '``MonadWriter<'Monoid>``>

    static member inline Listen (m: ChoiceT<'E, '``MonadWriter<'Monoid>``, 'T>) : ChoiceT<'E, '``MonadWriter<'Monoid>``, ('T * 'Monoid)> =
        let liftError (m, w) = Choice.map (fun x -> (x, w)) m
        ChoiceTOperations.ChoiceT<'``MonadWriter<'Monoid, Choice<('T * 'Monoid), 'E>>``, _, _, _> ((listen (ChoiceT.run m: '``MonadWriter<'Monoid, Choice<'T, 'E>>``) : '``MonadWriter<'Monoid, Choice<'T, 'E> * 'Monoid>``) >>= ((result: Choice<('T * 'Monoid), 'E> -> '``MonadWriter<'Monoid, Choice<('T * 'Monoid), 'E>>``) << liftError))

    static member inline Pass (m: ChoiceT<'E, '``MonadWriter<'Monoid>``, ('T * ('Monoid -> 'Monoid))>) : ChoiceT<'E, '``MonadWriter<'Monoid>``, 'T> =
        ChoiceTOperations.ChoiceT<'``MonadWriter<'Monoid, Choice<'T, 'E>>``, _, _, _> ((ChoiceT.run m: '``MonadWriter<'Monoid, Choice<('T * ('Monoid -> 'Monoid)), 'E>>``) >>= either (map Choice<'T, 'E>.Choice1Of2 << (pass: '``MonadWriter<'Monoid, ('T * ('Monoid -> 'Monoid))>`` -> '``MonadWriter<'Monoid, 'T>``) << (result: ('T * ('Monoid -> 'Monoid)) -> _)) (result << Choice<'T, 'E>.Choice2Of2))
    
    static member inline get_Get () : ChoiceT<'E, '``MonadState<'S>``, 'S> = ChoiceT.lift<_, _, '``MonadState<'S, 'S>``, '``MonadState<'S, Choice<'S, 'E>>``, '``MonadState<'S>``> get
    static member inline Put (x: 'S) : ChoiceT<'E, '``MonadState<'S>``, unit> = x |> put |> ChoiceT.lift<_, _, '``MonadState<'S, unit>``, '``MonadState<'S, Choice<unit, 'E>>``, '``MonadState<'S>``>

#endif
