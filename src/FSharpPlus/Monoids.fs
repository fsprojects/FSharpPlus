namespace FSharpPlus.Data

open FSharpPlus.Operators

/// The dual of a monoid, obtained by swapping the arguments of append.
type Dual<'t> = Dual of 't with
    static member inline get_MEmpty() = Dual (getMEmpty())                : Dual<'T>
    static member inline MAppend (Dual x, Dual y) = Dual (mappend y x)    : Dual<'T>

/// Basic operations on Dual
[<RequireQualifiedAccess>]
module Dual = let run (Dual x) = x          : 'T

/// The monoid of endomorphisms under composition.
type Endo<'t> = Endo of ('t -> 't) with
    static member get_MEmpty() = Endo id                                 : Endo<'T>
    static member MAppend (Endo f, Endo g) = Endo (f << g)               : Endo<'T>

/// Basic operations on Endo
[<RequireQualifiedAccess>]
module Endo = let run (Endo f) = f          : 'T -> 'T


/// Boolean monoid under conjunction.
type All = All of bool with
    static member MEmpty = All true
    static member MAppend (All x, All y) = All (x && y)

/// Boolean monoid under disjunction.
type Any = Any of bool with
    static member MEmpty = Any false
    static member MAppend (Any x, Any y) = Any (x || y)


/// <summary> The Const functor, defined as Const&lt;&#39;T, &#39;U&gt; where &#39;U is a phantom type. Useful for: Lens getters Its applicative instance plays a fundamental role in Lens.
/// <para/>   Useful for: Lens getters.
/// <para/>   Its applicative instance plays a fundamental role in Lens. </summary>
type Const<'t,'u> = Const of 't with

    // Monoid
    static member inline get_MEmpty() = Const (getMEmpty()) : Const<'T,'U>
    static member inline MAppend (Const x : Const<'T,'U>, Const y : Const<'T,'U>) = Const (mappend x y) : Const<'T,'U>

    // Functor
    static member Map (Const x : Const<_,'T>, _:'T->'U) = Const x : Const<'C,'U>

    // Applicative
    static member inline Return (_:'U) = Const (getMEmpty()) : Const<'T,'U>
    static member inline (<*>) (Const f : Const<'C,'T->'U>, Const x : Const<'C,'T>) = Const (mappend f x) : Const<'C,'U>

    // Contravariant
    static member Contramap (Const x : Const<'C,'T>, _:'U->'T) = Const x        : Const<'C,'U>

    // Bifunctor
    static member First     (Const x : Const<'T,'V>, f:'T->'U) = Const (f x)    : Const<'U,'V>
    static member Second    (Const x : Const<'T,'V>, _:'V->'W) = Const x        : Const<'T,'W>

/// Basic operations on Const
[<RequireQualifiedAccess>]
module Const =
    let run (Const t) = t


/// Option<'T> monoid returning the leftmost non-None value.
type First<'t> = First of Option<'t> with
    static member get_MEmpty() = First None                                          : First<'T>
    static member MAppend (x, y) = match x, y with First None, r -> r | l, _ -> l    : First<'T>
    static member run (First a) = a                                                 : 'T option

/// Option<'T> monoid returning the rightmost non-None value.
type Last<'t> = Last of Option<'t> with
    static member get_MEmpty() = Last None                                           : Last<'T>
    static member MAppend (x, y) = match x, y with l, Last None -> l | _, r -> r     : Last<'T>
    static member run (Last a) = a                                                  : 'T option


/// Right-to-left composition of functors. The composition of applicative functors is always applicative, but the composition of monads is not always a monad.
type Compose<'``f<'g<'t>>``> = Compose of '``f<'g<'t>>`` with

    // Functor
    static member inline Map  (Compose x, f:'T->'U) = Compose (map (map f) x)

    // Applicative
    static member inline Return (x:'T) = Compose (result (result x)) : Compose<'``F<'G<'T>``>
    static member inline (<*>)  (Compose (f: '``F<'G<'T->'U>>``), Compose (x: '``F<'G<'T>>``)) = Compose ((<*>) <!> f <*> x: '``F<'G<'U>>``)

    // Alternative
    static member inline get_MZero()                  = Compose (getMZero()) : Compose<'``F<'G<'T>``>
    static member inline MPlus (Compose x, Compose y) = Compose (x <|> y)    : Compose<'``F<'G<'T>``>


/// Basic operations on Compose
[<RequireQualifiedAccess>]
module Compose =
    let run (Compose t) = t