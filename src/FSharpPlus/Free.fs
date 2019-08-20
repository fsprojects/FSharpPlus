namespace FSharpPlus.Data

open FSharpPlus
open FSharpPlus.Control

/// Free Monad
[<NoEquality; NoComparison>]
type Free<'``functor<'t>``,'t> = Pure of 't | Roll of obj

[<AutoOpen>]
module FreePrimitives =

    let inline Roll (f: '``Functor<Free<'Functor<'T>>>``) : Free<'``Functor<'T>``,'T> =
        let (_: '``Functor<'T>``) = Map.InvokeOnInstance (fun (_: Free<'``Functor<'T>``,'T>) -> Unchecked.defaultof<'T>) f
        Free<'``Functor<'T>``,'T>.Roll f

    let inline (|Pure|Roll|) (f: Free<'``Functor<'T>``,'T>) =
        match f with
        | Pure x -> Choice1Of2 x
        | Roll x -> Choice2Of2 (unbox x: '``Functor<Free<'Functor<'T>>>`` when ^``Functor<'T>`` : (static member Map : ^``Functor<'T>`` * ('T -> Free< ^``Functor<'T>``, 'T>) -> ^``Functor<Free<'Functor<'T>>>``))

/// Basic operations on Free Monads
[<RequireQualifiedAccess>]
module Free =

    let inline bind (f: 'T -> Free<'``Functor<'U>``,'U>) (x: Free<'``Functor<'T>``,'T>) : Free<'``Functor<'U>``,'U> =
        let rec loop f = function
            | Pure r -> f r
            | Roll (x: '``Functor<Free<'Functor<'T>>>``) -> Roll (Map.InvokeOnInstance (loop f) x) : Free<'``Functor<'U>``,'U>
        loop f x

type Free<'FT,'T> with
    static member Return x = Pure x
    static member inline (>>=) (x: Free<'``Functor<'T>``,'T>, f: 'T -> Free<'``Functor<'U>``,'U>) = Free.bind f x : Free<'``Functor<'U>``,'U>
