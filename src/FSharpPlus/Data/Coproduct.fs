namespace FSharpPlus.Data

#if !FABLE_COMPILER || FABLE_COMPILER_3

open FSharpPlus
open FSharpPlus.Control
open FSharpPlus.Internals.Prelude


[<AbstractClass>]
type CoproductBase<'functorL, 'functorR, 't> (left: obj, right: obj, isLeft: bool) =
    let (left, right, isLeft)    = left, right, isLeft
    with
        member __.getContents () = left, right, isLeft
        override x.GetHashCode () = Unchecked.hash (x.getContents ())
        override x.Equals o =
            match o with
            | :? CoproductBase<'functorL, 'functorR, 't> as y -> Unchecked.equals (x.getContents ()) (y.getContents ())
            | _ -> false

type CoproductL<[<EqualityConditionalOn>]'functorL, 'functorR, 't> (left: obj, right: obj, isLeft: bool) =
    inherit CoproductBase<'functorL, 'functorR, 't> (left, right, isLeft)

type CoproductR<[<EqualityConditionalOn>]'functorL, 'functorR, 't> (left: obj, right: obj, isLeft: bool) =
    inherit CoproductL<'functorL, 'functorR, 't> (left, right, isLeft)

type Coproduct<[<EqualityConditionalOn>]'functorL, 'functorR, 't> (left: obj, right: obj, isLeft: bool) =
    inherit CoproductR<'functorL, 'functorR, 't> (left, right, isLeft)

[<AutoOpen>]
module CoproductPrimitives =
    [<GeneralizableValue>]
    let inline InL (x: '``FunctorL<'T>``) : Coproduct<'FunctorL, 'FunctorR, 'T> =        
        if opaqueId false then
            let (_: 'FunctorL) = Map.Invoke (fun (_: 'T) -> Unchecked.defaultof<__>) Unchecked.defaultof<'``FunctorL<'T>``>
            ()  
        Coproduct<'FunctorL, 'FunctorR, 'T> (box x, null, true)
    
    [<GeneralizableValue>]
    let inline InR (x: '``FunctorR<'T>``) : Coproduct<'FunctorL, 'FunctorR, 'T> =   
        if opaqueId false then
            let (_: 'FunctorR) = Map.Invoke (fun (_: 'T) -> Unchecked.defaultof<__>) Unchecked.defaultof<'``FunctorR<'T>``>
            ()
        Coproduct<'FunctorL, 'FunctorR, 'T> (null, box x, false)


    let inline (|InL|InR|) (x: Coproduct<'FunctorL, 'FunctorR, 'T>) : Choice<'``FunctorL<'T>``, '``FunctorR<'T>``> =
        if opaqueId false then
            let (_: '``FunctorL<'T>``) = Map.Invoke (fun (_: __) -> Unchecked.defaultof<'T>) Unchecked.defaultof<'FunctorL>
            let (_: '``FunctorR<'T>``) = Map.Invoke (fun (_: __) -> Unchecked.defaultof<'T>) Unchecked.defaultof<'FunctorR>
            ()
        let (l, r, isL) = x.getContents ()
        if isL then InL (unbox<'``FunctorL<'T>``> l)
        else        InR (unbox<'``FunctorR<'T>``> r)


type CoproductBase<'functorL, 'functorR, 't> with
    static member inline Map (x: CoproductBase<'FunctorL, 'FunctorR, 'T>, f: 'T -> 'U) : Coproduct<'FunctorL, 'FunctorR, 'U> =
        let (l, r, isL) = x.getContents ()
        if isL then InL (Map.Invoke f (unbox l: '``FunctorL<'T>``) : '``FunctorL<'U>``)
        else        InR (Map.Invoke f (unbox r: '``FunctorR<'T>``) : '``FunctorR<'U>``)

type CoproductL<'functorL, 'functorR, 't> with
    static member inline Map (x: CoproductL<'FunctorL, 'FunctorR, 'T>, f: 'T -> 'U) : Coproduct<'FunctorL, 'FunctorR, 'U> =
        let inline _CXs (_: '``FunctorL<'T>``* '``FunctorR<'T>``) =
            let (_: '``FunctorR<'T>``) = Map.InvokeOnInstance (fun (_: __) -> Unchecked.defaultof<'T>) Unchecked.defaultof<'FunctorR>
            ()
        let (l, r, isL) =  x.getContents ()
        if isL then InL (Map.Invoke f (unbox l: '``FunctorL<'T>``) : '``FunctorL<'U>``)
        else        Coproduct<_, _, _> (null, box (Map.InvokeOnInstance f (unbox r: '``FunctorR<'T>``) : ^``FunctorR<'U>`` ), false)

type CoproductL<'functorL, 'functorR, 't> with
    static member inline Map (x: CoproductL<'FunctorL, 'FunctorR, 'T>, f: 'T -> 'U) : Coproduct<'FunctorL, 'FunctorR, 'U> =
        let inline _CXs (_: '``FunctorL<'T>``* '``FunctorR<'T>``) =
            let (_: '``FunctorL<'T>``) = Map.InvokeOnInstance (fun (_: __) -> Unchecked.defaultof<'T>) Unchecked.defaultof<'FunctorL>
            ()
        let (l, r, isL) =  x.getContents ()
        if isL then Coproduct<_, _, _> (box (Map.InvokeOnInstance f (unbox l: '``FunctorL<'T>``) : ^``FunctorL<'U>`` ), null, true )
        else        InR (Map.Invoke f (unbox r: '``FunctorR<'T>``) : '``FunctorR<'U>``)

type Coproduct<'functorL, 'functorR, 't> with
    static member inline Map (x: Coproduct<'FunctorL, 'FunctorR, 'T>, f: 'T -> 'U) : Coproduct<'FunctorL, 'FunctorR, 'U> =
        let inline _CXs (_: '``FunctorL<'T>``* '``FunctorR<'T>``) =
            let (_: '``FunctorL<'T>``) = Map.InvokeOnInstance (fun (_: __) -> Unchecked.defaultof<'T>) Unchecked.defaultof<'FunctorL>
            let (_: '``FunctorR<'T>``) = Map.InvokeOnInstance (fun (_: __) -> Unchecked.defaultof<'T>) Unchecked.defaultof<'FunctorR>
            ()
        let (l, r, isL) =  x.getContents ()
        if isL then Coproduct<_, _, _> (box (Map.InvokeOnInstance f (unbox l: '``FunctorL<'T>``) : ^``FunctorL<'U>`` ), null, true )
        else        Coproduct<_, _, _> (null, box (Map.InvokeOnInstance f (unbox r: '``FunctorR<'T>``) : ^``FunctorR<'U>`` ), false)

#endif