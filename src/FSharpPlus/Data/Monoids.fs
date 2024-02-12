namespace FSharpPlus.Data

open FSharpPlus

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

#endif
