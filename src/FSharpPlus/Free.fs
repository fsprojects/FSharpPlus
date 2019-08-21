namespace FSharpPlus.Data

open FSharpPlus
open FSharpPlus.Control

type FreeNode<'ft,'t> = Pure of 't | Roll of obj
type FreeParent<'ft,'t> (f: FreeNode<'ft,'t>) =
    let free = f
    member __.getFree () = free

/// Free Monad
[<NoEquality; NoComparison>]
type Free<'ft,'t> (f: FreeNode<'ft,'t>) =
    inherit FreeParent<'ft,'t> (f)


module FreeInternals =

    let inline bindWithMapOnInstance (f: 'T -> Free<'``Functor<'U>``,'U>) (x: Free<'``Functor<'T>``,'T>) : Free<'``Functor<'U>``,'U> =

        let inline Roll (f: 'fFreeFT)  : Free<'ft,'t> =
            let (r: 'ft) = Map.InvokeOnInstance (fun (x: Free<'ft,'t>) -> Unchecked.defaultof<'t> ) f
            Free (FreeNode<'ft,'t>.Roll f)  

        let inline unroll (f: Free<'ft,'t>) : 'fFreeFT when ^ft : (static member Map : ^ft * ('t -> Free< ^ft, 't>)  -> ^fFreeFT) =
            match f.getFree () with
            | Roll s -> unbox s
            | Pure x -> failwith "It was Pure"

        let rec loop f (x:Free<_,_>) =
            match x.getFree () with
            | Pure r -> f r
            | _ -> let x = unroll x in Roll (Map.InvokeOnInstance (loop f) x) : Free<'``Functor<'U>``,'U>
        loop f x

    let inline unroll (f: Free<'``Functor<'T>``,'T>) : '``Functor<Free<'Functor<'T>,'T>>`` when (Map or ^``Functor<'T>`` or ^``Functor<Free<'Functor<'T>,'T>>``) : (static member Map : (^``Functor<'T>`` * ('T -> Free< ^``Functor<'T>``, 'T>)) * Map -> ^``Functor<Free<'Functor<'T>,'T>>``) =
        match f.getFree () with
        | Pure x -> failwith "It was Pure"
        | Roll x -> unbox x


open FreeInternals

[<AutoOpen>]
module FreePrimitives =
    let inline Pure x = Free (Pure x)
    let inline Roll (f: '``Functor<Free<'Functor<'T>,'T>>``) : Free<'``Functor<'T>``,'T> =
        let (r: '``Functor<'T>``) = Map.Invoke (fun (x: Free<'``Functor<'T>``,'T>) -> Unchecked.defaultof<'T>) f
        Free (FreeNode<'``Functor<'T>``,'T>.Roll f)

    let inline (|Pure|Roll|) (f: Free<_,_>) =
        match f.getFree () with
        | FreeNode.Pure x -> Choice1Of2 x
        | FreeNode.Roll x -> let x = unroll f in Choice2Of2 x

/// Basic operations on Free Monads
[<RequireQualifiedAccess>]
module Free =

    let inline bind (f: 'T -> Free<'``Functor<'U>``,'U>) (x: Free<'``Functor<'T>``,'T>) : Free<'``Functor<'U>``,'U> =
        let rec loop f (x:Free<_,_>) =
            match x.getFree () with
            | FreeNode.Pure r -> f r
            | _ -> let x = unroll x in Roll (Map.Invoke(loop f) x) : Free<'``Functor<'U>``,'U>
        loop f x


type FreeParent<'FT,'T> with
    static member inline (>>=) (x: FreeParent<'``Functor<'T>``,'T>, f: 'T -> Free<'``Functor<'U>``,'U>) = Free.bind f ( x :?> Free<'``Functor<'T>``,'T>) : Free<'``Functor<'U>``,'U>

type Free<'FT,'T> with
    static member Return x = Pure x
    static member inline (>>=) (x: Free<'``Functor<'T>``,'T>, f: 'T -> Free<'``Functor<'U>``,'U>) = bindWithMapOnInstance f x : Free<'``Functor<'U>``,'U>
