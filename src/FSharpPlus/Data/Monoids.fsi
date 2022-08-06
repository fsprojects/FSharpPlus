namespace FSharpPlus.Data
    
    /// The dual of a monoid, obtained by swapping the arguments of append.
    [<Struct>]
    type Dual<'t> =
        | Dual of 't
        
        static member
          inline (+) : Dual< ^T> * Dual< ^T> -> Dual< ^T>
                         when (Control.Plus or  ^T) :
                                (static member ``+`` :
                                    ^T *  ^T * Control.Plus ->  ^T)
        
        static member
          inline get_Zero: unit -> Dual< ^T>
                             when (Control.Zero or  ^T) :
                                    (static member Zero:
                                        ^T * Control.Zero ->  ^T)
    
    /// Basic operations on Dual
    module Dual =
        
        val run: Dual<'T> -> 'T
    
    /// The monoid of endomorphisms under composition.
    [<NoEquality; NoComparison; Struct>]
    type Endo<'t> =
        | Endo of ('t -> 't)
        
        static member (+) : Endo<'T> * Endo<'T> -> Endo<'T>
        
        static member get_Zero: unit -> Endo<'T>
    
    /// Basic operations on Endo
    module Endo =
        
        val run: Endo<'T> -> ('T -> 'T)
    
    /// Boolean monoid under conjunction.
    [<Struct>]
    type All =
        | All of bool
        
        static member (+) : All * All -> All
        
        static member Zero: All
    
    /// Boolean monoid under disjunction.
    [<Struct>]
    type Any =
        | Any of bool
        
        static member (+) : Any * Any -> Any
        
        static member Zero: Any
    
    /// <summary> The Const functor, defined as Const&lt;&#39;T, &#39;U&gt; where &#39;U is a phantom type. Useful for: Lens getters Its applicative instance plays a fundamental role in Lens.
    /// <para/>   Useful for: Lens getters.
    /// <para/>   Its applicative instance plays a fundamental role in Lens. </summary>
    [<Struct>]
    type Const<'t,'u> =
        | Const of 't
        
        /// <summary>
        /// Sequences two Consts left-to-right, discarding the value of the first argument.
        /// </summary>
        /// <category index="2">Applicative</category>
        static member
          inline ( *> ) : Const< ^C,'T> * Const< ^C,'U> -> Const< ^C,'U>
                            when (Control.Plus or  ^C) :
                                   (static member ``+`` :
                                       ^C *  ^C * Control.Plus ->  ^C)
        
        /// <summary>
        /// Sequences two Consts left-to-right, discarding the value of the second argument.
        /// </summary>
        /// <category index="2">Applicative</category>
        static member
          inline ( <* ) : Const< ^C,'U> * Const< ^C,'T> -> Const< ^C,'U>
                            when (Control.Plus or  ^C) :
                                   (static member ``+`` :
                                       ^C *  ^C * Control.Plus ->  ^C)
        
        static member
          inline (+) : Const< ^T,'U> * Const< ^T,'U> -> Const< ^T,'U>
                         when (Control.Plus or  ^T) :
                                (static member ``+`` :
                                    ^T *  ^T * Control.Plus ->  ^T)
        
        /// <summary>Lifts a function into a Const. Same as map.
        /// To be used in Applicative Style expressions, combined with &lt;*&gt;
        /// </summary>
        /// <category index="1">Functor</category>
        static member (<!>) : ('T -> 'U) * Const<'C,'T> -> Const<'C,'U>
        
        static member
          inline (<*>) : Const< ^C,('T -> 'U)> * Const< ^C,'T> -> Const< ^C,'U>
                           when (Control.Plus or  ^C) :
                                  (static member ``+`` :
                                      ^C *  ^C * Control.Plus ->  ^C)
        
        static member
          Bifold: Const<'T,'V> * f: ('U -> 'T -> 'U) * ('W -> 'V -> 'W) * z: 'U
                    -> 'U
        
        static member
          BifoldBack: Const<'T,'V> * f: ('T -> 'U -> 'U) * ('V -> 'W -> 'W) *
                      z: 'U -> 'U
        
        static member BifoldMap: Const<'T,'V> * f: ('T -> 'U) * ('V -> 'W) -> 'U
        
        static member
          Bimap: Const<'T,'V> * f: ('T -> 'U) * ('V -> 'W) -> Const<'U,'W>
        
        static member
          inline Bitraverse: Const<'T1,'U1> * f: ('T1 ->  ^Functor<'T2>) *
                             g: ('U1 ->  ^Functor<'U2>)
                               ->  ^Functor<Const<'T2,'U2>>
                               when (Control.Map or  ^Functor<'T2> or
                                      ^Functor<Const<'T2,'U2>>) :
                                      (static member Map:
                                         ( ^Functor<'T2> *
                                          ('T20 -> Const<'T20,'U2>)) *
                                         Control.Map
                                           ->  ^Functor<Const<'T2,'U2>>) and
                                    (Control.Map or  ^Functor<'U2> or
                                      ^Functor<Const<'T2,'U2>>) :
                                      (static member Map:
                                         ( ^Functor<'U2> *
                                          ('U2 -> Const<'T2,'U2>)) * Control.Map
                                           ->  ^Functor<Const<'T2,'U2>>) and
                                    (Control.Map or  ^Functor<'U2>) :
                                      (static member Map:
                                         ( ^Functor<'U2> * ('U2 -> 'U2)) *
                                         Control.Map ->  ^Functor<'U2>)
        
        static member Contramap: Const<'C,'T> * ('U -> 'T) -> Const<'C,'U>
        
        static member First: Const<'T,'V> * f: ('T -> 'U) -> Const<'U,'V>
        
        static member
          inline Lift2: ('T -> 'U -> 'V) * Const< ^C,'T> * Const< ^C,'U>
                          -> Const< ^C,'V>
                          when (Control.Plus or  ^C) :
                                 (static member ``+`` :
                                     ^C *  ^C * Control.Plus ->  ^C)
        
        static member
          inline Lift3: ('T -> 'U -> 'V -> 'W) * Const< ^C,'T> * Const< ^C,'U> *
                        Const< ^C,'V> -> Const< ^C,'W>
                          when (Control.Plus or  ^C) :
                                 (static member ``+`` :
                                     ^C *  ^C * Control.Plus ->  ^C)
        
        static member Map: Const<'C,'T> * ('T -> 'U) -> Const<'C,'U>
        
        static member
          inline Return: 'U -> Const< ^T,'U>
                           when (Control.Zero or  ^T) :
                                  (static member Zero:  ^T * Control.Zero ->  ^T)
        
        static member
          inline get_Zero: unit -> Const< ^T,'U>
                             when (Control.Zero or  ^T) :
                                    (static member Zero:
                                        ^T * Control.Zero ->  ^T)
    
    /// Basic operations on Const
    module Const =
        
        val run: Const<'a,'b> -> 'a
        
        val map: ('T -> 'U) -> Const<'C,'T> -> Const<'C,'U>
        
        val inline apply:
          Const< ^C,('T -> 'U)> -> Const< ^C,'T> -> Const< ^C,'U>
            when (Control.Plus or  ^C) :
                   (static member ``+`` :  ^C *  ^C * Control.Plus ->  ^C)
    
    /// Option<'T> monoid returning the leftmost non-None value.
    [<Struct>]
    type First<'t> =
        | First of Option<'t>
        
        static member (+) : x: First<'t> * y: First<'t> -> First<'t>
        
        static member get_Zero: unit -> First<'t>
        
        static member run: First<'t> -> 't option
    
    /// Option<'T> monoid returning the rightmost non-None value.
    [<Struct>]
    type Last<'t> =
        | Last of Option<'t>
        
        static member (+) : x: Last<'t> * y: Last<'t> -> Last<'t>
        
        static member get_Zero: unit -> Last<'t>
        
        static member run: Last<'t> -> 't option
    
    /// Numeric wrapper for multiplication monoid (*, 1)
    [<Struct>]
    type Mult<'a> =
        | Mult of 'a
        
        static member
          inline (+) : Mult< ^n> * Mult< ^n> -> Mult< ^a0>
                         when  ^n: (static member ( * ) :  ^n *  ^n ->  ^a0)
        
        static member
          inline get_Zero: unit -> Mult< ^a0>
                             when (Control.One or  ^a0) :
                                    (static member One:
                                        ^a0 * Control.One ->  ^a0)
    
    /// Right-to-left composition of functors. The composition of applicative functors is always applicative, but the composition of monads is not always a monad.
    [<Struct>]
    type Compose<'functorF<'functorG<'t>>> =
        | Compose of 'functorF<'functorG<'t>>
        
        /// <summary>
        /// Sequences two composed applicatives left-to-right, discarding the value of the first argument.
        /// </summary>
        /// <category index="2">Applicative</category>
        static member
          inline ( *> ) : x:  ^FunctorF<'FunctorG<'T>> *
                          y:  ^FunctorF<'FunctorG<'U>>
                            ->  ^FunctorF<'FunctorG<'U>>
                            when (Control.Map or  ^FunctorF<'FunctorG<'T>> or
                                   ^FunctorF<'FunctorG<'U->'U>>) :
                                   (static member Map:
                                      ( ^FunctorF<'FunctorG<'T>> *
                                       ('T -> 'U -> 'U)) * Control.Map
                                        ->  ^FunctorF<'FunctorG<'U->'U>>) and
                                 (Control.Apply or  ^FunctorF<'FunctorG<'U->'U>> or
                                   ^FunctorF<'FunctorG<'U>>) :
                                   (static member ``<*>`` :
                                       ^FunctorF<'FunctorG<'U->'U>> *
                                       ^FunctorF<'FunctorG<'U>> *
                                       ^FunctorF<'FunctorG<'U>> * Control.Apply
                                        ->  ^FunctorF<'FunctorG<'U>>)
        
        /// <summary>
        /// Sequences two composed applicatives left-to-right, discarding the value of the second argument.
        /// </summary>
        /// <category index="2">Applicative</category>
        static member
          inline ( <* ) : x:  ^FunctorF<'FunctorG<'U>> *
                          y:  ^FunctorF<'FunctorG<'T>>
                            ->  ^FunctorF<'FunctorG<'U>>
                            when (Control.Apply or  ^FunctorF<'FunctorG<'T->'U>> or
                                   ^FunctorF<'FunctorG<'T>> or
                                   ^FunctorF<'FunctorG<'U>>) :
                                   (static member ``<*>`` :
                                       ^FunctorF<'FunctorG<'T->'U>> *
                                       ^FunctorF<'FunctorG<'T>> *
                                       ^FunctorF<'FunctorG<'U>> * Control.Apply
                                        ->  ^FunctorF<'FunctorG<'U>>) and
                                 (Control.Map or  ^FunctorF<'FunctorG<'U>> or
                                   ^FunctorF<'FunctorG<'T->'U>>) :
                                   (static member Map:
                                      ( ^FunctorF<'FunctorG<'U>> *
                                       ('U -> 'T -> 'U)) * Control.Map
                                        ->  ^FunctorF<'FunctorG<'T->'U>>)
        
        /// <summary>Lifts a function into a Composed Applicative Functor. Same as map.
        /// To be used in Applicative Style expressions, combined with &lt;*&gt;
        /// </summary>
        /// <category index="1">Functor</category>
        static member
          inline (<!>) : f: ('T -> 'U) * x:  ^FunctorF<'FunctorG<'T>>
                           -> Compose< ^FunctorF<'FunctorG<'U>>>
                           when (Control.Map or  ^FunctorF<'FunctorG<'T>> or
                                  ^FunctorF<'FunctorG<'U>>) :
                                  (static member Map:
                                     ( ^FunctorF<'FunctorG<'T>> *
                                      ( ^FunctorG<'T> ->  ^FunctorG<'U>)) *
                                     Control.Map ->  ^FunctorF<'FunctorG<'U>>) and
                                (Control.Map or  ^FunctorG<'T> or  ^FunctorG<'U>) :
                                  (static member Map:
                                     ( ^FunctorG<'T> * ('T -> 'U)) * Control.Map
                                       ->  ^FunctorG<'U>)
        
        static member
          inline (<*>) : Compose< ^ApplicativeF<'ApplicativeG<'T->'U>> *
                         Compose< ^ApplicativeF<'ApplicativeG<'T>>
                           -> Compose< ^ApplicativeF<'ApplicativeG<'U>>
                           when (Control.Map or
                                  ^ApplicativeF<'ApplicativeG<'T->'U> or
                                  ^ApplicativeF<'ApplicativeG<'T>->'ApplicativeG<'U>) :
                                  (static member Map:
                                     ( ^ApplicativeF<'ApplicativeG<'T->'U> *
                                      ( ^ApplicativeG<'T->'U>
                                       ->  ^ApplicativeG<'T>
                                         ->  ^ApplicativeG<'U>)) * Control.Map
                                       ->  ^ApplicativeF<'ApplicativeG<'T>->'ApplicativeG<'U>) and
                                (Control.Apply or
                                  ^ApplicativeF<'ApplicativeG<'T>->'ApplicativeG<'U> or
                                  ^ApplicativeF<'ApplicativeG<'T> or
                                  ^ApplicativeF<'ApplicativeG<'U>) :
                                  (static member ``<*>`` :
                                      ^ApplicativeF<'ApplicativeG<'T>->'ApplicativeG<'U> *
                                      ^ApplicativeF<'ApplicativeG<'T> *
                                      ^ApplicativeF<'ApplicativeG<'U> *
                                     Control.Apply
                                       ->  ^ApplicativeF<'ApplicativeG<'U>) and
                                (Control.Apply or  ^ApplicativeG<'T->'U> or
                                  ^ApplicativeG<'T> or  ^ApplicativeG<'U>) :
                                  (static member ``<*>`` :
                                      ^ApplicativeG<'T->'U> *  ^ApplicativeG<'T> *
                                      ^ApplicativeG<'U> * Control.Apply
                                       ->  ^ApplicativeG<'U>)
        
        static member
          inline (<|>) : Compose< ^AlternativeF<'ApplicativeG<'T>> *
                         Compose< ^AlternativeF<'ApplicativeG<'T>>
                           -> Compose< ^AlternativeF<'ApplicativeG<'T>>
                           when (Control.Append or
                                  ^AlternativeF<'ApplicativeG<'T>) :
                                  (static member ``<|>`` :
                                      ^AlternativeF<'ApplicativeG<'T> *
                                      ^AlternativeF<'ApplicativeG<'T> *
                                     Control.Append
                                       ->  ^AlternativeF<'ApplicativeG<'T>)
        
        static member
          inline Lift2: f: ('T -> 'U -> 'V) *
                        Compose< ^ApplicativeF<'ApplicativeG<'T>> *
                        Compose< ^ApplicativeF<'ApplicativeG<'U>>
                          -> Compose< ^ApplicativeF<'ApplicativeG<'V>>
                          when (Control.Lift2 or
                                 ^ApplicativeF<'ApplicativeG<'T> or
                                 ^ApplicativeF<'ApplicativeG<'U> or
                                 ^ApplicativeF<'ApplicativeG<'V>) :
                                 (static member Lift2:
                                    ( ^ApplicativeG<'T> ->  ^ApplicativeG<'U>
                                       ->  ^ApplicativeG<'V>) *
                                    ( ^ApplicativeF<'ApplicativeG<'T> *
                                      ^ApplicativeF<'ApplicativeG<'U>) *
                                    Control.Lift2
                                      ->  ^ApplicativeF<'ApplicativeG<'V>) and
                               (Control.Lift2 or  ^ApplicativeG<'T> or
                                 ^ApplicativeG<'U> or  ^ApplicativeG<'V>) :
                                 (static member Lift2:
                                    ('T -> 'U -> 'V) *
                                    ( ^ApplicativeG<'T> *  ^ApplicativeG<'U>) *
                                    Control.Lift2 ->  ^ApplicativeG<'V>)
        
        static member
          inline Lift3: f: ('T -> 'U -> 'V -> 'W) *
                        Compose< ^ApplicativeF<'ApplicativeG<'T>> *
                        Compose< ^ApplicativeF<'ApplicativeG<'U>> *
                        Compose< ^ApplicativeF<'ApplicativeG<'V>>
                          -> Compose< ^ApplicativeF<'ApplicativeG<'W>>
                          when (Control.Lift3 or
                                 ^ApplicativeF<'ApplicativeG<'T> or
                                 ^ApplicativeF<'ApplicativeG<'U> or
                                 ^ApplicativeF<'ApplicativeG<'V> or
                                 ^ApplicativeF<'ApplicativeG<'W>) :
                                 (static member Lift3:
                                    ( ^ApplicativeG<'T> ->  ^ApplicativeG<'U>
                                     ->  ^ApplicativeG<'V> ->  ^ApplicativeG<'W>) *
                                    ( ^ApplicativeF<'ApplicativeG<'T> *
                                      ^ApplicativeF<'ApplicativeG<'U> *
                                      ^ApplicativeF<'ApplicativeG<'V>) *
                                    Control.Lift3
                                      ->  ^ApplicativeF<'ApplicativeG<'W>) and
                               (Control.Lift3 or  ^ApplicativeG<'T> or
                                 ^ApplicativeG<'U> or  ^ApplicativeG<'V> or
                                 ^ApplicativeG<'W>) :
                                 (static member Lift3:
                                    ('T -> 'U -> 'V -> 'W) *
                                    ( ^ApplicativeG<'T> *  ^ApplicativeG<'U> *
                                      ^ApplicativeG<'V>) * Control.Lift3
                                      ->  ^ApplicativeG<'W>)
        
        static member
          inline Map: Compose< ^FunctorF<'FunctorG<'T>>> * f: ('T -> 'U)
                        -> Compose< ^FunctorF<'FunctorG<'U>>>
                        when (Control.Map or  ^FunctorF<'FunctorG<'T>> or
                               ^FunctorF<'FunctorG<'U>>) :
                               (static member Map:
                                  ( ^FunctorF<'FunctorG<'T>> *
                                   ( ^FunctorG<'T> ->  ^FunctorG<'U>)) *
                                  Control.Map ->  ^FunctorF<'FunctorG<'U>>) and
                             (Control.Map or  ^FunctorG<'T> or  ^FunctorG<'U>) :
                               (static member Map:
                                  ( ^FunctorG<'T> * ('T -> 'U)) * Control.Map
                                    ->  ^FunctorG<'U>)
        
        static member
          inline Return: x: 'T -> Compose< ^ApplicativeF<'ApplicativeG<'T>>
                           when (Control.Return or
                                  ^ApplicativeF<'ApplicativeG<'T>) :
                                  (static member Return:
                                      ^ApplicativeF<'ApplicativeG<'T> *
                                     Control.Return
                                       -> ( ^ApplicativeG<'T>
                                             ->  ^ApplicativeF<'ApplicativeG<'T>)) and
                                (Control.Return or  ^ApplicativeG<'T>) :
                                  (static member Return:
                                      ^ApplicativeG<'T> * Control.Return
                                       -> ('T ->  ^ApplicativeG<'T>))
        
        static member
          inline get_Empty: unit -> Compose< ^AlternativeF<'ApplicativeG<'T>>
                              when (Control.Empty or
                                     ^AlternativeF<'ApplicativeG<'T>) :
                                     (static member Empty:
                                         ^AlternativeF<'ApplicativeG<'T> *
                                        Control.Empty
                                          ->  ^AlternativeF<'ApplicativeG<'T>)
    
    /// Basic operations on Compose
    module Compose =
        
        val run: Compose<'a> -> 'a

