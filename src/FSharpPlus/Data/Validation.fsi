namespace FSharpPlus.Data
    
    /// A 'Validation' is either a value of the type 'error or 't, similar to 'Result'. However,
    /// the 'Applicative' instance for 'Validation' accumulates errors using a 'Semigroup' on 'error.
    /// In contrast, the Applicative for 'Result' returns only the first error.
    ///
    /// A consequence of this is that 'Validation' is not a monad. There is no F#+ 'Bind' method since
    /// that would violate monad rules.
    type Validation<'error,'t> =
        | Failure of 'error
        | Success of 't
        
        /// <summary>
        /// Sequences two Validations left-to-right, discarding the value of the first argument.
        /// </summary>
        /// <category index="2">Applicative</category>
        static member
          inline ( *> ) : x: Validation< ^Error,'T> * y: Validation< ^Error,'U>
                            -> Validation< ^Error,'U>
                            when (Control.Plus or  ^Error) :
                                   (static member ``+`` :
                                       ^Error *  ^Error * Control.Plus
                                        ->  ^Error)
        
        /// <summary>
        /// Sequences two Validations left-to-right, discarding the value of the second argument.
        /// </summary>
        /// <category index="2">Applicative</category>
        static member
          inline ( <* ) : x: Validation< ^Error,'U> * y: Validation< ^Error,'T>
                            -> Validation< ^Error,'U>
                            when (Control.Plus or  ^Error) :
                                   (static member ``+`` :
                                       ^Error *  ^Error * Control.Plus
                                        ->  ^Error)
        
        /// <summary>Lifts a function into a Validator. Same as map.
        /// To be used in Applicative Style expressions, combined with &lt;*&gt;
        /// </summary>
        /// <category index="1">Functor</category>
        static member
          (<!>) : f: ('a -> 'b) * x: Validation<'c,'a> -> Validation<'c,'b>
        
        static member
          inline (<*>) : f: Validation< ^a,('T -> 'U)> * x: Validation< ^a,'T>
                           -> Validation< ^a,'U>
                           when (Control.Plus or  ^a) :
                                  (static member ``+`` :
                                      ^a *  ^a * Control.Plus ->  ^a)
        
        static member
          inline (<|>) : x: Validation< ^a,'b> * y: Validation< ^a,'b>
                           -> Validation< ^a,'b>
                           when (Control.Append or  ^a) :
                                  (static member ``<|>`` :
                                      ^a *  ^a * Control.Append ->  ^a)
        
        [<System.ComponentModel.EditorBrowsable
          (enum<System.ComponentModel.EditorBrowsableState> (1))>]
        static member
          inline Bifold: t: Validation<'err,'a> * f: ('b -> 'err -> 'b) *
                         g: ('b -> 'a -> 'b) * z: 'b -> 'b
        
        [<System.ComponentModel.EditorBrowsable
          (enum<System.ComponentModel.EditorBrowsableState> (1))>]
        static member
          inline BifoldBack: t: Validation<'err,'a> * f: ('err -> 'b -> 'b) *
                             g: ('a -> 'b -> 'b) * z: 'b -> 'b
        
        [<System.ComponentModel.EditorBrowsable
          (enum<System.ComponentModel.EditorBrowsableState> (1))>]
        static member
          inline BifoldMap: t: Validation<'err,'a> * f: ('err -> 'b) *
                            g: ('a -> 'b) -> 'b
        
        [<System.ComponentModel.EditorBrowsable
          (enum<System.ComponentModel.EditorBrowsableState> (1))>]
        static member
          Bimap: x: Validation<'T,'V> * f: ('T -> 'U) * g: ('V -> 'W)
                   -> Validation<'U,'W>
        
        [<System.ComponentModel.EditorBrowsable
          (enum<System.ComponentModel.EditorBrowsableState> (1))>]
        static member
          inline Bisequence: t: Validation< ^err, ^a0> ->  ^a
                               when (Control.Map or  ^err or  ^a) :
                                      (static member Map:
                                         ( ^err * ('c -> Validation<'c,'b>)) *
                                         Control.Map ->  ^a) and
                                    (Control.Map or  ^a0 or  ^a) :
                                      (static member Map:
                                         ( ^a0 * ('b -> Validation<'c,'b>)) *
                                         Control.Map ->  ^a)
        
        [<System.ComponentModel.EditorBrowsable
          (enum<System.ComponentModel.EditorBrowsableState> (1))>]
        static member
          inline Bitraverse: t: Validation<'err,'a> * f: ('err ->  ^a0) *
                             g: ('a ->  ^c) ->  ^b
                               when (Control.Map or  ^a0 or  ^b) :
                                      (static member Map:
                                         ( ^a0 * ('e -> Validation<'e,'d>)) *
                                         Control.Map ->  ^b) and
                                    (Control.Map or  ^c or  ^b) :
                                      (static member Map:
                                         ( ^c * ('d -> Validation<'e,'d>)) *
                                         Control.Map ->  ^b)
        
        [<System.ComponentModel.EditorBrowsable
          (enum<System.ComponentModel.EditorBrowsableState> (1))>]
        static member
          inline Lift2: f: ('T -> 'U -> 'V) * x: Validation< ^a,'T> *
                        y: Validation< ^a,'U> -> Validation< ^a,'V>
                          when (Control.Plus or  ^a) :
                                 (static member ``+`` :
                                     ^a *  ^a * Control.Plus ->  ^a)
        
        [<System.ComponentModel.EditorBrowsable
          (enum<System.ComponentModel.EditorBrowsableState> (1))>]
        static member
          inline Lift3: f: ('T -> 'U -> 'V -> 'W) * x: Validation< ^a,'T> *
                        y: Validation< ^a,'U> * z: Validation< ^a,'V>
                          -> Validation< ^a,'W>
                          when (Control.Plus or  ^a) :
                                 (static member ``+`` :
                                     ^a *  ^a * Control.Plus ->  ^a)
        
        [<System.ComponentModel.EditorBrowsable
          (enum<System.ComponentModel.EditorBrowsableState> (1))>]
        static member
          Map: x: Validation<'a,'b> * f: ('b -> 'c) -> Validation<'a,'c>
        
        static member Return: x: 'a -> Validation<'b,'a>
        
        [<System.ComponentModel.EditorBrowsable
          (enum<System.ComponentModel.EditorBrowsableState> (1))>]
        static member
          inline Sequence: t: Validation<'err, ^a> ->  ^c
                             when (Control.Map or  ^a or  ^c) :
                                    (static member Map:
                                       ( ^a * ('a0 -> Validation<'err,'a0>)) *
                                       Control.Map ->  ^c) and
                                  (Control.Return or  ^c) :
                                    (static member Return:
                                        ^c * Control.Return
                                         -> (Validation<'err,'a0> ->  ^c))
        
        /// Creates an array with either all Success values or the Failure ones.
        static member
          SequenceBiApply: t: Validation<'Error,'T>[]
                             -> Validation<'Error[],'T[]>
        
        /// Creates a list with either all Success values or the Failure ones.
        static member
          SequenceBiApply: t: Validation<'Error,'T> list
                             -> Validation<'Error list,'T list>
        
        [<System.ComponentModel.EditorBrowsable
          (enum<System.ComponentModel.EditorBrowsableState> (1))>]
        static member
          inline Traverse: t: Validation<'err,'a> * f: ('a ->  ^b) ->  ^c
                             when (Control.Map or  ^b or  ^c) :
                                    (static member Map:
                                       ( ^b * ('a0 -> Validation<'err,'a0>)) *
                                       Control.Map ->  ^c) and
                                  (Control.Return or  ^c) :
                                    (static member Return:
                                        ^c * Control.Return
                                         -> (Validation<'err,'a0> ->  ^c))
        
        static member
          inline get_Empty: unit -> Validation< ^a,'b>
                              when (Control.Empty or  ^a) :
                                     (static member Empty:
                                         ^a * Control.Empty ->  ^a)
    
    module Validation =
        
        val map:
          f: ('T -> 'U) -> source: Validation<'Error,'T>
            -> Validation<'Error,'U>
        
        /// Applies the wrapped value to the wrapped function when both are Success and returns a wrapped result or the Failure(s).
        /// <param name="f">The function wrapped in a Success or a Failure.</param>
        /// <param name="x">The value wrapped in a Success or a Failure.</param>
        /// <returns>A Success of the function applied to the value when both are Success, or the Failure(s) if more than one, combined with the Semigroup (++) operation of the Error type.</returns>
        val inline apply:
          f: Validation< ^Error,('T -> 'U)> -> x: Validation< ^Error,'T>
            -> Validation< ^Error,'U>
            when (Control.Plus or  ^Error) :
                   (static member ``+`` :
                       ^Error *  ^Error * Control.Plus ->  ^Error)
        
        val inline map2:
          f: ('T -> 'U -> 'V) -> x: Validation< ^Error,'T>
          -> y: Validation< ^Error,'U> -> Validation< ^Error,'V>
            when (Control.Plus or  ^Error) :
                   (static member ``+`` :
                       ^Error *  ^Error * Control.Plus ->  ^Error)
        
        val inline map3:
          f: ('T -> 'U -> 'V -> 'W) -> x: Validation< ^Error,'T>
          -> y: Validation< ^Error,'U> -> z: Validation< ^Error,'V>
            -> Validation< ^Error,'W>
            when (Control.Plus or  ^Error) :
                   (static member ``+`` :
                       ^Error *  ^Error * Control.Plus ->  ^Error)
        
        val inline foldBack:
          folder: ('T -> 'State -> 'State) -> source: Validation<'Error,'T>
          -> state: 'State -> 'State
        
        /// Traverse the Success case with the supplied function.
        val inline traverse:
          f: ('T ->  ^Functor<'U>) -> source: Validation<'Error,'T>
            ->  ^Functor<Validation<'Error,'U>>
            when (Control.Map or  ^Functor<'U> or
                   ^Functor<Validation<'Error,'U>>) :
                   (static member Map:
                      ( ^Functor<'U> * ('U -> Validation<'Error,'U>)) *
                      Control.Map ->  ^Functor<Validation<'Error,'U>>) and
                 (Control.Return or  ^Functor<Validation<'Error,'U>>) :
                   (static member Return:
                       ^Functor<Validation<'Error,'U>> * Control.Return
                        -> (Validation<'Error,'U>
                              ->  ^Functor<Validation<'Error,'U>>))
        
        /// Traverse the Success case.
        val inline sequence:
          source: Validation<'Error, ^Functor<'T>>
            ->  ^Functor<Validation<'Error,'T>>
            when (Control.Map or  ^Functor<'T> or
                   ^Functor<Validation<'Error,'T>>) :
                   (static member Map:
                      ( ^Functor<'T> * ('a -> Validation<'Error,'a>)) *
                      Control.Map ->  ^Functor<Validation<'Error,'T>>) and
                 (Control.Return or  ^Functor<Validation<'Error,'T>>) :
                   (static member Return:
                       ^Functor<Validation<'Error,'T>> * Control.Return
                        -> (Validation<'Error,'a>
                              ->  ^Functor<Validation<'Error,'T>>))
        
        val bimap:
          f: ('T1 -> 'U1) -> g: ('T2 -> 'U2) -> _arg1: Validation<'T1,'T2>
            -> Validation<'U1,'U2>
        
        val bifoldBack:
          f: ('Error -> 'State -> 'State) -> g: ('T -> 'State -> 'State)
          -> source: Validation<'Error,'T> -> state: 'State -> 'State
        
        [<System.Obsolete ("Use Validation.bifoldBack instead.")>]
        val biFoldBack:
          f: ('a -> 'b -> 'c) -> g: ('d -> 'b -> 'c) -> state: Validation<'a,'d>
          -> x: 'b -> 'c
        
        /// Like traverse but taking an additional function to traverse the Failure part as well.
        val inline bitraverse:
          f: ('Error1 ->  ^Functor<'Error2>) -> g: ('T1 ->  ^Functor<'T2>)
          -> source: Validation<'Error1,'T1>
            ->  ^Functor<Validation<'Error2,'T2>>
            when (Control.Map or  ^Functor<'Error2> or
                   ^Functor<Validation<'Error2,'T2>>) :
                   (static member Map:
                      ( ^Functor<'Error2> * ('Error2 -> Validation<'Error2,'T2>)) *
                      Control.Map ->  ^Functor<Validation<'Error2,'T2>>) and
                 (Control.Map or  ^Functor<'T2> or
                   ^Functor<Validation<'Error2,'T2>>) :
                   (static member Map:
                      ( ^Functor<'T2> * ('T2 -> Validation<'Error2,'T2>)) *
                      Control.Map ->  ^Functor<Validation<'Error2,'T2>>)
        
        /// Like sequence but traversing the Failure part as well.
        val inline bisequence:
          source: Validation< ^Functor<'Error>, ^Functor<'T>>
            ->  ^Functor<Validation<'Error,'T>>
            when (Control.Map or  ^Functor<'Error> or
                   ^Functor<Validation<'Error,'T>>) :
                   (static member Map:
                      ( ^Functor<'Error> * ('b -> Validation<'b,'a>)) *
                      Control.Map ->  ^Functor<Validation<'Error,'T>>) and
                 (Control.Map or  ^Functor<'T> or
                   ^Functor<Validation<'Error,'T>>) :
                   (static member Map:
                      ( ^Functor<'T> * ('a -> Validation<'b,'a>)) * Control.Map
                        ->  ^Functor<Validation<'Error,'T>>)
        
        /// Binds through a Validation, which is useful for
        /// composing Validations sequentially. Note that despite having a bind
        /// function of the correct type, Validation is not a monad.
        /// The reason is, this bind does not accumulate errors, so it does not
        /// agree with the Applicative instance.
        ///
        /// There is nothing wrong with using this function, it just does not make a
        /// valid Monad instance.
        val bind:
          f: ('T -> Validation<'Error,'U>) -> x: Validation<'Error,'T>
            -> Validation<'Error,'U>
        
        [<System.Obsolete ("Use Validation.defaultValue instead.")>]
        val orElse: v: Validation<'a,'a0> -> a: 'a0 -> 'a0
        
        /// Extracts the Success value or use the supplied default value when it's a Failure.
        val defaultValue: value: 'T -> source: Validation<'Error,'T> -> 'T
        
        /// Extracts the Success value or applies the compensation function over the Failure.
        val defaultWith:
          compensation: ('Error -> 'T) -> source: Validation<'Error,'T> -> 'T
        
        [<System.Obsolete ("Use Validation.defaultWith instead.")>]
        val valueOr: ea: ('e -> 'a) -> v: Validation<'e,'a> -> 'a
        
        /// Converts a 'Result' to a 'Validation'
        /// when the 'Error' of the 'Result' needs to be lifted into a 'Semigroup'.
        val liftResult:
          f: ('Error -> 'Semigroup) -> _arg1: Result<'T,'Error>
            -> Validation<'Semigroup,'T>
        
        /// Converting a 'Choice' to a 'Validation'
        /// when the 'Choice2Of2' of the 'Choice' needs to be lifted into a 'Semigroup'.
        val liftChoice:
          f: ('b -> 'Semigroup) -> (Choice<'b,'T> -> Validation<'Semigroup,'T>)
        
        /// Takes two Validations and returns the first Success.
        /// If both are Failures it returns both Failures combined with the supplied function.
        val appValidation:
          combine: ('err -> 'err -> 'err) -> e1': Validation<'err,'a>
          -> e2': Validation<'err,'a> -> Validation<'err,'a>
        
        /// Converts a Validation<'Error,'T> to a Result<'T,'Error>.
        val toResult: x: Validation<'Error,'T> -> Result<'T,'Error>
        
        /// Creates a Validation<'Error,'T> from a Result<'T,'Error>.
        val ofResult: x: Result<'T,'Error> -> Validation<'Error,'T>
        
        /// Converts a Validation<'Error,'T> to a Choice<'T,'Error>.
        val toChoice: x: Validation<'Error,'T> -> Choice<'T,'Error>
        
        /// Creates a Validation<'Error,'T> from a Choice<'T,'Error>.
        val ofChoice: x: Choice<'T,'Error> -> Validation<'Error,'T>
        
        /// <summary> Extracts a value from either side of a Validation.</summary>
        /// <param name="fSuccess">Function to be applied to source, if it contains a Success value.</param>
        /// <param name="fFailure">Function to be applied to source, if it contains a Failure value.</param>
        /// <param name="source">The source value, containing a Success or a Failure.</param>
        /// <returns>The result of applying either functions.</returns>
        val either:
          fSuccess: ('T -> 'U) -> fFailure: ('Error -> 'U)
          -> source: Validation<'Error,'T> -> 'U
        
        [<System.Obsolete
          ("This function will not be supported in future versions.")>]
        val validate: e: 'e -> p: ('a -> bool) -> a: 'a -> Validation<'e,'a>
        
        /// validationNel : Result<'a,'e> -> Validation (NonEmptyList<'e>) a
        /// This is 'liftError' specialized to 'NonEmptyList', since
        /// they are a common semigroup to use.
        val validationNel: x: Result<'a,'e> -> Validation<NonEmptyList<'e>,'a>
        
        [<System.Obsolete
          ("This function will not be supported in future versions.")>]
        val ensure:
          e: 'e -> p: ('a -> bool) -> _arg1: Validation<'e,'a>
            -> Validation<'e,'a>
        
        /// Creates a safe version of the supplied function, which returns a Validation<exn,'U> instead of throwing exceptions.
        val protect: f: ('T -> 'U) -> x: 'T -> Validation<exn,'U>
        
        val inline _Success:
          x: ('a ->  ^b) -> (Validation<'d,'a> ->  ^e)
            when  ^b:
                   (static member Map:  ^b * ('c -> Validation<'d,'c>) ->  ^e) and
                  ^e: (static member Return: Validation<'d,'c> ->  ^e)
        
        val inline _Failure:
          x: ('a ->  ^b) -> (Validation<'a,'c> ->  ^e)
            when  ^b:
                   (static member Map:  ^b * ('c -> Validation<'c,'d>) ->  ^e) and
                  ^e: (static member Return: Validation<'c,'d> ->  ^e)
        
        val inline isoValidationResult:
          x:  ^a ->  ^b
            when (Control.Dimap or  ^a or  ^b) :
                   (static member Dimap:
                       ^a * (Validation<'c,'d> -> Result<'d,'c>) * ( ^e -> 'h) *
                      Control.Dimap ->  ^b) and
                  ^e:
                   (static member Map:
                       ^e * (Result<'f,'g> -> Validation<'g,'f>) -> 'h)
        
        /// <summary>
        /// Creates two lists by classifying the values depending on whether they were wrapped with Success or Failure.
        /// </summary>
        /// <returns>
        /// A tuple with both resulting lists, Success are in the first list.
        /// </returns>
        val partition:
          source: Validation<'Error,'T> list -> 'T list * 'Error list

