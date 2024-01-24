namespace FSharpPlus.Data

open FSharpPlus
open FSharpPlus.Internals.Prelude

#if (!FABLE_COMPILER || FABLE_COMPILER_3) && !FABLE_COMPILER_4
/// The dual of a monoid, obtained by swapping the arguments of append.
[<Struct>]
type Dual<'t> = Dual of 't with
    static member inline get_Zero () = Dual (getZero ())        : Dual<'T>
    static member inline (+) (Dual x, Dual y) = Dual (plus y x) : Dual<'T>

/// Basic operations on Dual
[<RequireQualifiedAccess>]
module Dual = let run (Dual x) = x : 'T

#endif

/// The monoid of endomorphisms under composition.
[<Struct; NoEquality; NoComparison>]
type Endo<'t> = Endo of ('t -> 't) with
    static member get_Zero () = Endo id                : Endo<'T>
    static member (+) (Endo f, Endo g) = Endo (f << g) : Endo<'T>

/// Basic operations on Endo
[<RequireQualifiedAccess>]
module Endo = let run (Endo f) = f : 'T -> 'T


/// Boolean monoid under conjunction.
[<Struct>]
type All = All of bool with
    static member Zero = All true
    static member (+) (All x, All y) = All (x && y)

/// Boolean monoid under disjunction.
[<Struct>]
type Any = Any of bool with
    static member Zero = Any false
    static member (+) (Any x, Any y) = Any (x || y)


#if (!FABLE_COMPILER || FABLE_COMPILER_3) && !FABLE_COMPILER_4

/// <summary> The Const functor, defined as Const&lt;&#39;T, &#39;U&gt; where &#39;U is a phantom type. Useful for: Lens getters Its applicative instance plays a fundamental role in Lens.
/// <para/>   Useful for: Lens getters.
/// <para/>   Its applicative instance plays a fundamental role in Lens. </summary>
[<Struct>]
type Const<'t,'u> = Const of 't with

    // Monoid
    static member inline get_Zero () = Const (getZero ()) : Const<'T,'U>
    static member inline (+) (Const x: Const<'T,'U>, Const y: Const<'T,'U>) = Const (plus x y) : Const<'T,'U>

/// Basic operations on Const
[<RequireQualifiedAccess>]
module Const =
    let run (Const t) = t
    let map (_: 'T -> 'U) (Const x: Const<_, 'T>) : Const<'C, 'U> = Const x
    let inline apply (Const f: Const<'C, 'T -> 'U>) (Const x: Const<'C, 'T>) : Const<'C, 'U> = Const (plus f x)

type Const<'t,'u> with

    // Functor
    static member Map (Const x: Const<_, 'T>, _: 'T->'U) = Const x : Const<'C,'U>

    /// <summary>Lifts a function into a Const. Same as map.
    /// To be used in Applicative Style expressions, combined with &lt;*&gt;
    /// </summary>
    /// <category index="1">Functor</category>
    static member (<!>) (_: 'T->'U, Const x: Const<_,'T>) : Const<'C, 'U> = Const x

    // Applicative
    static member inline Return (_: 'U) = Const (getZero ()) : Const<'T,'U>
    static member inline (<*>) (Const f: Const<'C,'T->'U>, Const x: Const<'C,'T>) = Const (plus f x) : Const<'C,'U>

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

    static member inline Lift2 (_: 'T->'U->'V, Const x: Const<'C,'T>, Const y: Const<'C,'U>) = Const (plus x y) : Const<'C,'V>
    static member inline Lift3 (_: 'T->'U->'V->'W, Const x: Const<'C,'T>, Const y: Const<'C,'U>, Const z: Const<'C,'V>) = Const (x ++ y ++ z) : Const<'C,'W>

    // Contravariant
    static member Contramap (Const x: Const<'C,'T>, _: 'U->'T) = Const x     : Const<'C,'U>

    // Bifunctor
    static member Bimap     (Const x: Const<'T,'V>, f: 'T->'U, _: 'V->'W) = Const (f x) : Const<'U,'W>
    static member First     (Const x: Const<'T,'V>, f: 'T->'U) = Const (f x) : Const<'U,'V>

    // Bifoldable
    static member BifoldMap  (Const x: Const<'T,'V>, f: 'T->'U, _: 'V->'W) = f x
    static member BifoldBack (Const x: Const<'T,'V>, f: 'T->'U->'U, _: 'V->'W->'W, z: 'U) = f x z
    static member Bifold     (Const x: Const<'T,'V>, f: 'U->'T->'U, _: 'W->'V->'W, z: 'U) = f z x

    // Bitraversable
    static member inline Bitraverse (Const x: Const<'T1,'U1>, f: 'T1->'``Functor<'T2>``, g: 'U1->'``Functor<'U2>``) : '``Functor<Const<'T2,'U2>>`` =
        let mid x = map (id: 'U2 -> 'U2) x
        if opaqueId false then
            let (a: '``Functor<'U2>``) = mid (g Unchecked.defaultof<'U1>)
            let (_: '``Functor<Const<'T2,'U2>>``) = map (Unchecked.defaultof<'U2 -> Const<'T2,'U2>>) a
            ()
        (Const : _ -> Const<'T2,'U2>) <!> f x


#endif

/// Option<'T> monoid returning the leftmost non-None value.
[<Struct>]
type First<'t> = First of Option<'t> with
    static member get_Zero () = First None                                    : First<'t>
    static member (+) (x, y) = match x, y with First None, r -> r | l, _ -> l : First<'t>
    static member run (First a) = a                                           : 't option

/// Option<'T> monoid returning the rightmost non-None value.
[<Struct>]
type Last<'t> = Last of Option<'t> with
    static member get_Zero () = Last None                                     : Last<'t>
    static member (+) (x, y) = match x, y with l, Last None -> l | _, r -> r  : Last<'t>
    static member run (Last a) = a                                            : 't option


#if (!FABLE_COMPILER || FABLE_COMPILER_3) && !FABLE_COMPILER_4

/// Numeric wrapper for multiplication monoid (*, 1)
[<Struct>]
type Mult<'a> = Mult of 'a with    
    static member inline get_Zero () = Mult one
    static member inline (+) (Mult (x: 'n), Mult (y: 'n)) = Mult (x * y)


open FSharpPlus.Control

/// Right-to-left composition of functors. The composition of applicative functors is always applicative, but the composition of monads is not always a monad.
[<Struct>]
type Compose<'``functorF<'functorG<'t>>``> = Compose of '``functorF<'functorG<'t>>`` with

    // Functor
    static member inline Map (Compose (x: '``FunctorF<'FunctorG<'T>>``), f: 'T->'U) = Compose (map (map f: '``FunctorG<'T>`` -> '``FunctorG<'U>``) x : '``FunctorF<'FunctorG<'U>>``)

    /// <summary>Lifts a function into a Composed Applicative Functor. Same as map.
    /// To be used in Applicative Style expressions, combined with &lt;*&gt;
    /// </summary>
    /// <category index="1">Functor</category>
    static member inline (<!>) (f: 'T->'U, x: '``FunctorF<'FunctorG<'T>>``) = Compose (map (map f: '``FunctorG<'T>`` -> '``FunctorG<'U>``) x : '``FunctorF<'FunctorG<'U>>``)

    // Applicative
    static member inline Return (x: 'T) = Compose (result (result x: '``ApplicativeG<'T>``)) : Compose<'``ApplicativeF<'ApplicativeG<'T>``>

    static member inline (<*>) (Compose (f: '``ApplicativeF<'ApplicativeG<'T->'U>``), Compose (x: '``ApplicativeF<'ApplicativeG<'T>``)) =
        Compose ((((<*>) : '``ApplicativeG<'T->'U>`` -> '``ApplicativeG<'T>`` -> '``ApplicativeG<'U>``) <!> f: '``ApplicativeF<'ApplicativeG<'T>->'ApplicativeG<'U>`` ) <*> x: '``ApplicativeF<'ApplicativeG<'U>``)

    /// <summary>
    /// Sequences two composed applicatives left-to-right, discarding the value of the first argument.
    /// </summary>
    /// <category index="2">Applicative</category>
    static member inline ( *>) (x: '``FunctorF<'FunctorG<'T>>``, y: '``FunctorF<'FunctorG<'U>>``) : '``FunctorF<'FunctorG<'U>>`` = ((fun (_: 'T) (k: 'U) -> k) <!>  x : '``FunctorF<'FunctorG<'U->'U>>``) <*> y
    
    /// <summary>
    /// Sequences two composed applicatives left-to-right, discarding the value of the second argument.
    /// </summary>
    /// <category index="2">Applicative</category>
    static member inline (<*  ) (x: '``FunctorF<'FunctorG<'U>>``, y: '``FunctorF<'FunctorG<'T>>``): '``FunctorF<'FunctorG<'U>>`` = ((fun (k: 'U) (_: 'T) -> k ) <!> x : '``FunctorF<'FunctorG<'T->'U>>``) <*> y
    
    static member inline Lift2 (f: 'T -> 'U -> 'V, Compose (x: '``ApplicativeF<'ApplicativeG<'T>``), Compose (y: '``ApplicativeF<'ApplicativeG<'U>``)) =
        Compose (Lift2.Invoke (Lift2.Invoke f: '``ApplicativeG<'T>`` -> '``ApplicativeG<'U>`` -> '``ApplicativeG<'V>``) x y: '``ApplicativeF<'ApplicativeG<'V>``)

    static member inline Lift3 (f: 'T -> 'U -> 'V -> 'W, Compose (x: '``ApplicativeF<'ApplicativeG<'T>``), Compose (y: '``ApplicativeF<'ApplicativeG<'U>``), Compose (z: '``ApplicativeF<'ApplicativeG<'V>``)) =
        Compose (Lift3.Invoke (Lift3.Invoke f: '``ApplicativeG<'T>`` -> '``ApplicativeG<'U>`` -> '``ApplicativeG<'V>`` -> '``ApplicativeG<'W>``) x y z: '``ApplicativeF<'ApplicativeG<'W>``)

    // Alternative
    static member inline get_Empty ()                 = Compose (getEmpty ()) : Compose<'``AlternativeF<'ApplicativeG<'T>``>
    static member inline (<|>) (Compose x, Compose y) = Compose (x <|> y)     : Compose<'``AlternativeF<'ApplicativeG<'T>``>

    // ZipApplicative
    static member inline (<.>) (Compose (f: '``ApplicativeF<'ApplicativeG<'T->'U>``), Compose (x: '``ApplicativeF<'ApplicativeG<'T>``)) =
        Compose ((((<.>) : '``ApplicativeG<'T->'U>`` -> '``ApplicativeG<'T>`` -> '``ApplicativeG<'U>``) <!> f: '``ApplicativeF<'ApplicativeG<'T>->'ApplicativeG<'U>`` ) <.> x: '``ApplicativeF<'ApplicativeG<'U>``)

    static member inline Map2 (f: 'T -> 'U -> 'V, Compose (x: '``ApplicativeF<'ApplicativeG<'T>``), Compose (y: '``ApplicativeF<'ApplicativeG<'U>``)) =
        Compose (Map2.Invoke (Map2.Invoke f: '``ApplicativeG<'T>`` -> '``ApplicativeG<'U>`` -> '``ApplicativeG<'V>``) x y: '``ApplicativeF<'ApplicativeG<'V>``)

    static member inline Map3 (f: 'T -> 'U -> 'V -> 'W, Compose (x: '``ApplicativeF<'ApplicativeG<'T>``), Compose (y: '``ApplicativeF<'ApplicativeG<'U>``), Compose (z: '``ApplicativeF<'ApplicativeG<'V>``)) =
        Compose (Map3.Invoke (Map3.Invoke f: '``ApplicativeG<'T>`` -> '``ApplicativeG<'U>`` -> '``ApplicativeG<'V>`` -> '``ApplicativeG<'W>``) x y z: '``ApplicativeF<'ApplicativeG<'W>``)

/// Basic operations on Compose
[<RequireQualifiedAccess>]
module Compose =
    let run (Compose t) = t

#endif
