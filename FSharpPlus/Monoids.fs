namespace FSharpPlus.Data

open FSharpPlus.Operators

/// The dual of a monoid, obtained by swapping the arguments of append.
type Dual<'t> = Dual of 't with
    static member inline get_Empty() = Dual (getEmpty())                : Dual<'T>
    static member inline Append (Dual x, Dual y) = Dual (append y x)    : Dual<'T>

[<RequireQualifiedAccess>]
module Dual = let run (Dual x) = x          : 'T

/// The monoid of endomorphisms under composition.
type Endo<'t> = Endo of ('t -> 't) with
    static member get_Empty() = Endo id                                 : Endo<'T>
    static member Append (Endo f, Endo g) = Endo (f << g)               : Endo<'T>

[<RequireQualifiedAccess>]
module Endo = let run (Endo f) = f          : 'T -> 'T


/// Boolean monoid under conjunction.
type All = All of bool with
    static member Empty = All true
    static member Append (All x, All y) = All (x && y)

/// Boolean monoid under disjunction.
type Any = Any of bool with
    static member Empty = Any false
    static member Append (Any x, Any y) = Any (x || y)


type Const<'t,'u> = Const of 't with

    // Monoid
    static member inline get_Empty() = Const (getEmpty()) : Const<'T,'U>
    static member inline Append (Const x : Const<'T,'U>, Const y : Const<'T,'U>) = Const (append x y) : Const<'T,'U>

    // Functor
    static member Map (Const x : Const<_,'T>, f:'T->'U) = Const x : Const<'C,'U>

    // Applicative
    static member inline Return (_:'U) = Const (getEmpty()) : Const<'T,'U>
    static member inline (<*>) (Const f : Const<'C,'T->'U>, Const x : Const<'C,'T>) = Const (append f x) : Const<'C,'U>

    // Contravariant
    static member Contramap (Const x : Const<'C,'T>, _:'U->'T) = Const x        : Const<'C,'U>

    // Bifunctor
    static member First     (Const x : Const<'T,'V>, f:'T->'U) = Const (f x)    : Const<'U,'V>
    static member Second    (Const x : Const<'T,'V>, _:'V->'W) = Const x        : Const<'T,'W>

[<RequireQualifiedAccess>]
module Const =
    let run (Const t) = t


/// Option<'T> monoid returning the leftmost non-None value.
type First<'t> = First of Option<'t> with
    static member get_Empty() = First None                                          : First<'T>
    static member Append (x, y) = match x, y with First None, r -> r | l, _ -> l    : First<'T>
    static member run (First a) = a                                                 : 'T option

/// Option<'T> monoid returning the rightmost non-None value.
type Last<'t> = Last of Option<'t> with
    static member get_Empty() = Last None                                           : Last<'T>
    static member Append (x, y) = match x, y with l, Last None -> l | _, r -> r     : Last<'T>
    static member run (Last a) = a                                                  : 'T option