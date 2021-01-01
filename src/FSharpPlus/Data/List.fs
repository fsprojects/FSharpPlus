namespace FSharpPlus.Data

#if !FABLE_COMPILER || FABLE_COMPILER_3

open FSharpPlus
open System.ComponentModel
open FSharpPlus.Internals.Prelude


/// Additional operations on List
module List =
    let inline sequence (ms: list<'``Applicative<'T>``>) : '``Applicative<list<'T>>`` = sequence ms

    let inline traverse (f: 'T->'``Applicative<'U>``) (xs:list<'T>) : '``Applicative<list<'U>>`` = traverse f xs
    
    let inline foldM (f: 'T->'U->'``Monad<'T>``) (a: 'T) (bx:list<'U>) : '``Monad<'T>`` =
        let f = OptimizedClosures.FSharpFunc<_,_,_>.Adapt f
        let rec loopM a = function
            | x::xs -> (f.Invoke (a, x)) >>= fun fax -> loopM fax xs 
            | [] -> result a
        loopM a bx

    let inline filterM (f: 'T -> '``Monad<Bool>``) (xs: list<'T>) : '``Monad<list<'T>>`` =
        let rec loopM = function
            | []   -> result []
            | h::t -> 
                f h >>= (fun flg ->
                    loopM t >>= (fun ys ->
                        result (if flg then (h::ys) else ys)))
        loopM xs

    let inline replicateM count (initial: '``Applicative<'T>``)  = sequence (List.replicate count initial)


open FSharpPlus.Control

/// Monad Transformer for list<'T>
[<Struct>]
type ListT<'``monad<list<'t>>``> = ListT of '``monad<list<'t>>``

/// Basic operations on ListT
[<RequireQualifiedAccess>]
module ListT =
    let run (ListT m) = m : '``Monad<list<'T>>``

    /// Embed a Monad<'T> into a ListT<'Monad<list<'T>>>
    let inline lift (x: '``Monad<'T>``) : ListT<'``Monad<list<'T>>``> =
        if opaqueId false then x |> liftM List.singleton |> ListT
        else x |> map List.singleton |> ListT

    let inline internal sequence ms =
        let k m m' = m >>= fun (x: 'a) -> m' >>= fun xs -> (result: list<'a> -> 'M) (x::xs)
        List.foldBack k ms ((result :list<'a> -> 'M) [])

    let inline internal mapM f as' = sequence (List.map f as')

    let inline bind (f: 'T-> ListT<'``Monad<list<'U>``>) (ListT m: ListT<'``Monad<list<'T>``>) = (ListT (m >>= mapM (run << f) >>= ((List.concat: list<_>->_) >> result)))
    let inline apply (ListT f: ListT<'``Monad<list<('T -> 'U)>``>) (ListT x: ListT<'``Monad<list<'T>``>) = ListT (map List.apply f <*> x) : ListT<'``Monad<list<'U>``>
    let inline lift2 (f: 'T->'U->'V) (ListT x: ListT<'``Monad<list<'T>``>) (ListT y: ListT<'``Monad<list<'U>``>) = ListT (lift2 (List.lift2 f) x y)  : ListT<'``Monad<list<'V>``>
    let inline map  (f: 'T->'U) (ListT m: ListT<'``Monad<list<'T>``>) =  ListT (map (List.map f) m) : ListT<'``Monad<list<'U>``>

type ListT<'``monad<list<'t>>``> with

    static member inline Return (x: 'T) = [x] |> result |> ListT                        : ListT<'``Monad<list<'T>``>

    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member inline Map (x: ListT<'``Monad<list<'T>``>, f: 'T->'U) = ListT.map f x : ListT<'``Monad<list<'U>``>

    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member inline Lift2 (f: 'T->'U->'V, x: ListT<'``Monad<list<'T>``>, y: ListT<'``Monad<list<'U>``>) = ListT.lift2 f x y : ListT<'``Monad<list<'V>``>

    static member inline (<*>) (f: ListT<'``Monad<list<('T -> 'U)>``>, x: ListT<'``Monad<list<'T>``>) = ListT.apply f x : ListT<'``Monad<list<'U>``>
    static member inline (>>=) (x: ListT<'``Monad<list<'T>``>, f: 'T -> ListT<'``Monad<list<'U>``>)   = ListT.bind f x

    static member inline get_Empty () = ListT <| result [] : ListT<'``MonadPlus<list<'T>``>
    static member inline (<|>) (ListT x, ListT y) = ListT (x >>= (fun a -> y >>= (fun b -> result (a @ b)))) : ListT<'``MonadPlus<list<'T>``>

    static member inline TryWith (source: ListT<'``Monad<list<'T>>``>, f: exn -> ListT<'``Monad<list<'T>>``>) = ListT (TryWith.Invoke (ListT.run source) (ListT.run << f))
    static member inline TryFinally (computation: ListT<'``Monad<list<'T>>``>, f) = ListT (TryFinally.Invoke     (ListT.run computation) f)
    static member inline Using (resource, f: _ -> ListT<'``Monad<list<'T>>``>)    = ListT (Using.Invoke resource (ListT.run << f))
    static member inline Delay (body : unit   ->  ListT<'``Monad<list<'T>>``>)    = ListT (Delay.Invoke (fun _ -> ListT.run (body ()))) : ListT<'``Monad<list<'T>>``>

    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member inline Lift (x: '``Monad<'T>``) : ListT<'``Monad<list<'T>>``> = ListT.lift x
    
    static member inline LiftAsync (x: Async<'T>) = ListT.lift (liftAsync x) : ListT<'``MonadAsync<'T>``>
    
    static member inline Throw (x: 'E) = x |> throw |> ListT.lift
    static member inline Catch (m: ListT<'``MonadError<'E1,'T>``>, h: 'E1 -> ListT<'``MonadError<'E2,'T>``>) = ListT ((fun v h -> Catch.Invoke v h) (ListT.run m) (ListT.run << h)) : ListT<'``MonadError<'E2,'T>``>
    
    static member inline CallCC (f: (('T -> ListT<'``MonadCont<'R,list<'U>>``>) -> _)) = ListT (callCC <| fun c -> ListT.run (f (ListT << c << List.singleton))) : ListT<'``MonadCont<'R, list<'T>>``>
    
    static member inline get_Get ()  = ListT.lift get         : ListT<'``MonadState<'S,'S>``>
    static member inline Put (x: 'S) = x |> put |> ListT.lift : ListT<'``MonadState<unit,'S>``>
    
    static member inline get_Ask () = ListT.lift ask          : ListT<'``MonadReader<'R,  list<'R>>``>
    static member inline Local (ListT (m: '``MonadReader<'R2,'T>``), f: 'R1->'R2) = ListT (local f m)

#endif
