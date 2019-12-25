namespace FSharpPlus.Data

open FSharpPlus
open FSharpPlus.Control

[<NoComparison>]
type FreeNode<'ft,'t> = Pure of 't | Roll of obj

type FreeBase<'ft,'t> (f: FreeNode<'ft,'t>) =
    let free = f
    member __.getFree () = free

/// Free Monad
type Free<[<EqualityConditionalOn; ComparisonConditionalOn >]'ft,'t> (f: FreeNode<'ft,'t>) =
    inherit FreeBase<'ft,'t> (f)
    override x.GetHashCode () = Unchecked.hash (x.getFree ())
    override x.Equals o =
        match o with
        | :? Free<'ft,'t> as y -> Unchecked.equals (x.getFree ()) (y.getFree ())
        | _                    -> false


module FreeInternals =

    let inline _roll (f: 'fFreeFT)  : Free<'ft,'t> =
        let (_: 'ft) = Map.InvokeOnInstance (fun (_: Free<'ft,'t>) -> Unchecked.defaultof<'t> ) f
        Free (FreeNode<'ft,'t>.Roll f)  

    let inline _unroll (f: Free<'ft,'t>) : 'fFreeFT when ^ft : (static member Map : ^ft * ('t -> Free< ^ft, 't>) -> ^fFreeFT) =
        match f.getFree () with
        | Roll s -> unbox s
        | Pure _ -> failwith "It was Pure"

    let inline mapWithMapOnInstance (f: 'T -> 'U) (x: Free<'``Functor<'T>``,'T>) : Free<'``Functor<'U>``,'U> =
        let rec loop f (x: Free<_,_>) =
            match x.getFree () with
            | Pure x -> Free (Pure (f x))
            | _      -> let x = _unroll x in _roll (Map.InvokeOnInstance (loop f) x) : Free<'``Functor<'U>``,'U>
        loop f x

    let inline applyWithMapOnInstance (f: Free<'``Functor<'T->'U>``,'T->'U>) (x: Free<'``Functor<'T>``,'T>) : Free<'``Functor<'U>``,'U> =
        let rec loop (f: Free<_,_>) (x: Free<_,_>) =
            match f.getFree () with
            | Pure f -> mapWithMapOnInstance f x
            | _      -> let f = _unroll f in _roll (flip loop x </Map.InvokeOnInstance/> f)
        loop f x

    let inline bindWithMapOnInstance (f: 'T -> Free<'``Functor<'U>``,'U>) (x: Free<'``Functor<'T>``,'T>) : Free<'``Functor<'U>``,'U> =
        let rec loop f (x: Free<_,_>) =
            match x.getFree () with
            | Pure r -> f r
            | _      -> let x = _unroll x in _roll (Map.InvokeOnInstance (loop f) x) : Free<'``Functor<'U>``,'U>
        loop f x

    let inline unroll (f: Free<'``Functor<'T>``,'T>) : '``Functor<Free<'Functor<'T>,'T>>`` when (Map or ^``Functor<'T>`` or ^``Functor<Free<'Functor<'T>,'T>>``) : (static member Map : (^``Functor<'T>`` * ('T -> Free< ^``Functor<'T>``, 'T>)) * Map -> ^``Functor<Free<'Functor<'T>,'T>>``) =
        match f.getFree () with
        | Pure _ -> failwith "It was Pure"
        | Roll x -> unbox x


open FreeInternals

[<AutoOpen>]
module FreePrimitives =
    let inline Pure x = Free (Pure x)
    let inline Roll (f: '``Functor<Free<'Functor<'T>,'T>>``) : Free<'``Functor<'T>``,'T> =
        let (_: '``Functor<'T>``) = Map.Invoke (fun (_: Free<'``Functor<'T>``,'T>) -> Unchecked.defaultof<'T>) f
        Free (FreeNode<'``Functor<'T>``,'T>.Roll f)

    let inline (|Pure|Roll|) (f: Free<_,_>) =
        match f.getFree () with
        | FreeNode.Pure x -> Pure x
        | FreeNode.Roll _ -> let x = unroll f in Roll x

/// Basic operations on Free Monads
[<RequireQualifiedAccess>]
module Free =

    let inline map f x =
        let rec loop f (x: Free<_,_>) =
            match x.getFree () with
            | FreeNode.Pure x  -> Pure (f x)
            | _ -> let x = unroll x in Roll (map (loop f) x)
        loop f x

    let inline bind (f: 'T -> Free<'``Functor<'U>``,'U>) (x: Free<'``Functor<'T>``,'T>) : Free<'``Functor<'U>``,'U> =
        let rec loop f (x: Free<_,_>) =
            match x.getFree () with
            | FreeNode.Pure r -> f r
            | _ -> let x = unroll x in Roll (Map.Invoke(loop f) x) : Free<'``Functor<'U>``,'U>
        loop f x

    let inline apply (f: Free<'``Functor<'T->'U>``,'T->'U>) (x: Free<'``Functor<'T>``,'T>) : Free<'``Functor<'U>``,'U> =
        let rec loop (f: Free<_,_>) (x: Free<_,_>)  =
            match f.getFree () with
            | FreeNode.Pure f -> map f x
            | _  -> let f = unroll f in Roll (flip loop x </Map.Invoke/> f)
        loop f x
        
    /// Folds the Free structure into a Monad
    let inline fold (f: '``Functor<'T>`` -> '``Monad<'T>``) (x: Free<'``Functor<'T>``,'T>) : '``Monad<'T>`` =
        let rec loop f = function
            | Roll x -> f x >>= loop f
            | Pure a -> result a
        loop f x

    /// Lift any Functor into a Free structure
    let inline liftF (x: '``Functor<'T>``) : Free<'``Functor<'T>``,'T> = Roll (Map.InvokeOnInstance (Pure: 'T -> Free<'``Functor<'T>``,'T>) x)


type FreeBase<'FT,'T> with
    static member inline Map   (x: FreeBase<'``Functor<'T>``,'T>, f: 'T -> 'U) = Free.map f (x :?> Free<'``Functor<'T>``,'T>) : Free<'``Functor<'U>``,'U>
    static member inline (>>=) (x: FreeBase<'``Functor<'T>``,'T>, f: 'T -> Free<'``Functor<'U>``,'U>) = Free.bind f (x :?> Free<'``Functor<'T>``,'T>) : Free<'``Functor<'U>``,'U>
    static member inline (<*>) (f: Free<'``Functor<'T->'U>``,'T->'U>, x: FreeBase<'``Functor<'T>``,'T>) = Free.apply f (x :?> Free<'``Functor<'T>``,'T>) : Free<'``Functor<'U>``,'U>


type Free<'FT,'T> with
    static member Return x = Pure x
    static member inline Map   (x: Free<'``Functor<'T>``,'T>, f: 'T -> 'U) = mapWithMapOnInstance f x : Free<'``Functor<'U>``,'U>
    static member inline (>>=) (x: Free<'``Functor<'T>``,'T>, f: 'T -> Free<'``Functor<'U>``,'U>) = bindWithMapOnInstance f x : Free<'``Functor<'U>``,'U>
    static member inline (<*>) (f: Free<'``Functor<'T->'U>``,'T->'U>, x: Free<'``Functor<'T>``,'T>) = applyWithMapOnInstance f x : Free<'``Functor<'U>``,'U>