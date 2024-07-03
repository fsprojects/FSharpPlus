namespace FSharpPlus.Data

open FSharpPlus
open FSharpPlus.Control


#if !FABLE_COMPILER

/// Right-to-left composition of functors. The composition of applicative functors is always applicative, but the composition of monads is not always a monad.
[<Struct>]
type Compose<'``functorF<'functorG<'t>>``> = Compose of '``functorF<'functorG<'t>>`` with

    // Functor
    static member inline Map (Compose (x: '``FunctorF<'FunctorG<'T>>``), f: 'T -> 'U) =
        Compose (map (map f: '``FunctorG<'T>`` -> '``FunctorG<'U>``) x : '``FunctorF<'FunctorG<'U>>``)

    /// <summary>Lifts a function into a Composed Applicative Functor. Same as map.
    /// To be used in Applicative Style expressions, combined with &lt;*&gt;
    /// </summary>
    /// <category index="1">Functor</category>
    static member inline (<!>) (f: 'T -> 'U, x: '``FunctorF<'FunctorG<'T>>``) =
        Compose (map (map f: '``FunctorG<'T>`` -> '``FunctorG<'U>``) x : '``FunctorF<'FunctorG<'U>>``)

    // Applicative
    static member inline Return (x: 'T) : Compose<'``ApplicativeF<'ApplicativeG<'T>``> =
        Compose (result (result x: '``ApplicativeG<'T>``))

    static member inline (<*>) (Compose (f: '``ApplicativeF<'ApplicativeG<'T -> 'U>``), Compose (x: '``ApplicativeF<'ApplicativeG<'T>``)) =
        Compose ((((<*>) : '``ApplicativeG<'T -> 'U>`` -> '``ApplicativeG<'T>`` -> '``ApplicativeG<'U>``) <!> f: '``ApplicativeF<'ApplicativeG<'T> -> 'ApplicativeG<'U>`` ) <*> x: '``ApplicativeF<'ApplicativeG<'U>``)

    /// <summary>
    /// Sequences two composed applicatives left-to-right, discarding the value of the first argument.
    /// </summary>
    /// <category index="2">Applicative</category>
    static member inline ( *>) (x: '``FunctorF<'FunctorG<'T>>``, y: '``FunctorF<'FunctorG<'U>>``) : '``FunctorF<'FunctorG<'U>>`` =
        ((fun (_: 'T) (k: 'U) -> k) <!> x : '``FunctorF<'FunctorG<'U -> 'U>>``) <*> y
    
    /// <summary>
    /// Sequences two composed applicatives left-to-right, discarding the value of the second argument.
    /// </summary>
    /// <category index="2">Applicative</category>
    static member inline (<*  ) (x: '``FunctorF<'FunctorG<'U>>``, y: '``FunctorF<'FunctorG<'T>>``): '``FunctorF<'FunctorG<'U>>`` =
        ((fun (k: 'U) (_: 'T) -> k) <!> x : '``FunctorF<'FunctorG<'T -> 'U>>``) <*> y
    
    static member inline Lift2 (f: 'T -> 'U -> 'V, Compose (x: '``ApplicativeF<'ApplicativeG<'T>``), Compose (y: '``ApplicativeF<'ApplicativeG<'U>``)) =
        Compose (Lift2.Invoke (Lift2.Invoke f: '``ApplicativeG<'T>`` -> '``ApplicativeG<'U>`` -> '``ApplicativeG<'V>``) x y: '``ApplicativeF<'ApplicativeG<'V>``)

    static member inline Lift3 (f: 'T -> 'U -> 'V -> 'W, Compose (x: '``ApplicativeF<'ApplicativeG<'T>``), Compose (y: '``ApplicativeF<'ApplicativeG<'U>``), Compose (z: '``ApplicativeF<'ApplicativeG<'V>``)) =
        Compose (Lift3.Invoke (Lift3.Invoke f: '``ApplicativeG<'T>`` -> '``ApplicativeG<'U>`` -> '``ApplicativeG<'V>`` -> '``ApplicativeG<'W>``) x y z: '``ApplicativeF<'ApplicativeG<'W>``)

    // Alternative
    static member inline get_Empty ()                 : Compose<'``AlternativeF<'ApplicativeG<'T>``> = Compose (getEmpty ())
    static member inline (<|>) (Compose x, Compose y) : Compose<'``AlternativeF<'ApplicativeG<'T>``> = Compose (x <|> y)

    // ZipApplicative
    static member inline (<.>) (Compose (f: '``ApplicativeF<'ApplicativeG<'T -> 'U>``), Compose (x: '``ApplicativeF<'ApplicativeG<'T>``)) =
        Compose ((((<.>) : '``ApplicativeG<'T -> 'U>`` -> '``ApplicativeG<'T>`` -> '``ApplicativeG<'U>``) <!> f: '``ApplicativeF<'ApplicativeG<'T> -> 'ApplicativeG<'U>`` ) <.> x: '``ApplicativeF<'ApplicativeG<'U>``)

    static member inline Map2 (f: 'T -> 'U -> 'V, Compose (x: '``ApplicativeF<'ApplicativeG<'T>``), Compose (y: '``ApplicativeF<'ApplicativeG<'U>``)) =
        Compose (Map2.Invoke (Map2.Invoke f: '``ApplicativeG<'T>`` -> '``ApplicativeG<'U>`` -> '``ApplicativeG<'V>``) x y: '``ApplicativeF<'ApplicativeG<'V>``)

    static member inline Map3 (f: 'T -> 'U -> 'V -> 'W, Compose (x: '``ApplicativeF<'ApplicativeG<'T>``), Compose (y: '``ApplicativeF<'ApplicativeG<'U>``), Compose (z: '``ApplicativeF<'ApplicativeG<'V>``)) =
        Compose (Map3.Invoke (Map3.Invoke f: '``ApplicativeG<'T>`` -> '``ApplicativeG<'U>`` -> '``ApplicativeG<'V>`` -> '``ApplicativeG<'W>``) x y z: '``ApplicativeF<'ApplicativeG<'W>``)


/// Basic operations on Compose
[<RequireQualifiedAccess>]
module Compose =
    let run (Compose t) = t

#endif
