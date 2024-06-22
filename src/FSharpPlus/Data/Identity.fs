namespace FSharpPlus.Data

/// <summary> Computation type: Simple function application.
/// <para/>   Binding strategy: The bound function is applied to the input value. Identity x >>= f = Identity (f x)
/// <para/>   Useful for: Lens setters and updaters - Monads can be derived from monad transformers applied to the Identity monad.
/// <para/>   The Identity monad is a monad that does not embody any computational strategy. 
///           It simply applies the bound function to its input without any modification. 
///           Computationally, there is no reason to use the Identity monad instead of the much simpler act of simply applying functions to their arguments.
///           The purpose of the Identity monad is its fundamental role in the theory of monad transformers.
///           Any monad transformer applied to the Identity monad yields a non-transformer version of that monad.
///           Its applicative instance plays a fundamental role in Lens. </summary> 
[<Struct>]
type Identity<'t> = Identity of 't

/// Basic operations on Identity
[<RequireQualifiedAccess>]
module Identity = 
    let run (Identity x) = x  : 'T
    let apply (Identity (f : 'T -> 'U)) (Identity (x: 'T)) : Identity<'U> = Identity (f x)
    let bind  (f: 'T -> Identity<'U>) (Identity x) : Identity<'U> = f x

type Identity<'t> with
    
    static member Return x = Identity x                                             : Identity<'T>
    static member Pure x = Identity x                                               : Identity<'T>
    
    static member Lift2 (f, Identity (x: 'T), Identity (y: 'U)) = Identity (f x y)  : Identity<'V>
    static member Lift3 (f, Identity (x: 'T), Identity (y: 'U), Identity (z: 'V))   : Identity<'W> = Identity (f x y z)
    static member Map   (Identity x, f : 'T->'U) = Identity (f x)                   : Identity<'U>
    static member Map2  (f, Identity (x: 'T), Identity (y: 'U)) = Identity (f x y)  : Identity<'V>
    static member Map3  (f, Identity (x: 'T), Identity (y: 'U), Identity (z: 'V))   : Identity<'W> = Identity (f x y z)
    static member Zip   (Identity x, Identity y) = Identity (x, y)                  : Identity<'T * 'U>

    static member (<*>) (Identity (f: 'T -> 'U), Identity (x: 'T)) : Identity<'U> = Identity (f x)
    static member (<.>) (Identity (f: 'T -> 'U), Identity (x: 'T)) : Identity<'U> = Identity (f x)
    
    static member (>>=) (Identity x, f: 'T -> Identity<'U>) : Identity<'U> = f x

    /// <summary>
    /// Composes left-to-right two Id functions (Kleisli composition).
    /// </summary>
    /// <category index="2">Monad</category>
    static member (>=>) (f, (g: 'U -> _)) : 'T -> Identity<'V> = fun x -> Identity.bind g (f x)