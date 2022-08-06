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
    type Identity<'t> =
        | Identity of 't
        
        static member
          (<*>) : Identity<('T -> 'U)> * Identity<'T> -> Identity<'U>
        
        static member
          (>>=) : Identity<'T> * f: ('T -> Identity<'U>) -> Identity<'U>
        
        static member
          Lift2: f: ('T -> 'U -> 'V) * Identity<'T> * Identity<'U>
                   -> Identity<'V>
        
        static member
          Lift3: f: ('T -> 'U -> 'V -> 'W) * Identity<'T> * Identity<'U> *
                 Identity<'V> -> Identity<'W>
        
        static member Map: Identity<'T> * f: ('T -> 'U) -> Identity<'U>
        
        static member Return: x: 'T -> Identity<'T>
        
        static member Zip: Identity<'T> * Identity<'U> -> Identity<'T * 'U>
    
    /// Basic operations on Identity
    module Identity =
        
        val run: Identity<'T> -> 'T

