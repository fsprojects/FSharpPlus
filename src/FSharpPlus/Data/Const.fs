namespace FSharpPlus.Data

open FSharpPlus
open FSharpPlus.Internals.Prelude

#if (!FABLE_COMPILER || FABLE_COMPILER_3) && !FABLE_COMPILER_4

/// <summary> The Const functor, defined as Const&lt;&#39;T, &#39;U&gt; where &#39;U is a phantom type. Useful for: Lens getters Its applicative instance plays a fundamental role in Lens.
/// <para/>   Useful for: Lens getters.
/// <para/>   Its applicative instance plays a fundamental role in Lens. </summary>
[<Struct>]
type Const<'t, 'u> = Const of 't with

    // Monoid
    static member inline get_Zero () = Const (getZero ()) : Const<'T, 'U>
    static member inline (+) (Const x: Const<'T, 'U>, Const y: Const<'T, 'U>) : Const<'T, 'U> = Const (plus x y)

/// Basic operations on Const
[<RequireQualifiedAccess>]
module Const =
    let run (Const t) = t
    let map (_: 'T -> 'U) (Const x: Const<_, 'T>) : Const<'C, 'U> = Const x
    let inline apply (Const f: Const<'C, 'T -> 'U>) (Const x: Const<'C, 'T>) : Const<'C, 'U> = Const (plus f x)

type Const<'t, 'u> with

    // Functor
    static member Map (Const x: Const<_, 'T>, _: 'T -> 'U) : Const<'C, 'U> = Const x

    /// <summary>Lifts a function into a Const. Same as map.
    /// To be used in Applicative Style expressions, combined with &lt;*&gt;
    /// </summary>
    /// <category index="1">Functor</category>
    static member (<!>) (_: 'T -> 'U, Const x: Const<_, 'T>) : Const<'C, 'U> = Const x

    // Applicative
    static member inline Return (_: 'U) = Const (getZero ()) : Const<'T, 'U>
    static member inline (<*>) (Const f: Const<'C, 'T -> 'U>, Const x: Const<'C, 'T>) : Const<'C, 'U> = Const (plus f x)

    /// <summary>
    /// Sequences two Consts left-to-right, discarding the value of the first argument.
    /// </summary>
    /// <category index="2">Applicative</category>
    static member inline ( *>) (Const x: Const<'C, 'T>, Const y: Const<'C, 'U>) : Const<'C, 'U> = Const (plus x y)
    
    /// <summary>
    /// Sequences two Consts left-to-right, discarding the value of the second argument.
    /// </summary>
    /// <category index="2">Applicative</category>
    static member inline (<* ) (Const x: Const<'C, 'U>, Const y: Const<'C, 'T>) : Const<'C, 'U> = Const (plus x y)

    static member inline Lift2 (_: 'T -> 'U -> 'V, Const x: Const<'C, 'T>, Const y: Const<'C, 'U>) : Const<'C, 'V> = Const (plus x y)
    static member inline Lift3 (_: 'T -> 'U -> 'V -> 'W, Const x: Const<'C, 'T>, Const y: Const<'C, 'U>, Const z: Const<'C, 'V>) : Const<'C, 'W> = Const (x ++ y ++ z)

    // Contravariant
    static member Contramap (Const x: Const<'C, 'T>, _: 'U -> 'T) : Const<'C, 'U> = Const x

    // Bifunctor
    static member Bimap (Const x: Const<'T, 'V>, f: 'T -> 'U, _: 'V -> 'W) : Const<'U, 'W> = Const (f x)
    static member First (Const x: Const<'T, 'V>, f: 'T -> 'U) : Const<'U, 'V> = Const (f x)

    // Bifoldable
    static member BifoldMap  (Const x: Const<'T, 'V>, f: 'T -> 'U, _: 'V -> 'W) = f x
    static member BifoldBack (Const x: Const<'T, 'V>, f: 'T -> 'U -> 'U, _: 'V -> 'W -> 'W, z: 'U) = f x z
    static member Bifold     (Const x: Const<'T, 'V>, f: 'U -> 'T -> 'U, _: 'W -> 'V -> 'W, z: 'U) = f z x

    // Bitraversable
    static member inline Bitraverse (Const x: Const<'T1, 'U1>, f: 'T1 -> '``Functor<'T2>``, g: 'U1 -> '``Functor<'U2>``) : '``Functor<Const<'T2, 'U2>>`` =
        let mid x = map (id: 'U2 -> 'U2) x
        if opaqueId false then
            let (a: '``Functor<'U2>``) = mid (g Unchecked.defaultof<'U1>)
            let (_: '``Functor<Const<'T2, 'U2>>``) = map (Unchecked.defaultof<'U2 -> Const<'T2, 'U2>>) a
            ()
        (Const: _ -> Const<'T2, 'U2>) <!> f x

#endif
