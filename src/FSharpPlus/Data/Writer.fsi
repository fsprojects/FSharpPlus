namespace FSharpPlus.Data
    
    /// <summary> Computation type: Computations which produce a stream of data in addition to the computed values.
    /// <para/>   Binding strategy: Combines the outputs of the subcomputations using <c>mappend</c>.
    /// <para/>   Useful for: Logging, or other computations that produce output "on the side". </summary>
    [<Struct>]
    type Writer<'monoid,'t> =
        | Writer of ('t * 'monoid)
        
        /// <summary>
        /// Sequences two Writers left-to-right, discarding the value of the first argument.
        /// </summary>
        /// <category index="2">Applicative</category>
        static member
          inline ( *> ) : x: Writer< ^Monoid,'T> * y: Writer< ^Monoid,'U>
                            -> Writer< ^Monoid,'U>
                            when (Control.Plus or  ^Monoid) :
                                   (static member ``+`` :
                                       ^Monoid *  ^Monoid * Control.Plus
                                        ->  ^Monoid)
        
        /// <summary>
        /// Sequences two Writers left-to-right, discarding the value of the second argument.
        /// </summary>
        /// <category index="2">Applicative</category>
        static member
          inline ( <* ) : x: Writer< ^Monoid,'U> * y: Writer< ^Monoid,'T>
                            -> Writer< ^Monoid,'U>
                            when (Control.Plus or  ^Monoid) :
                                   (static member ``+`` :
                                       ^Monoid *  ^Monoid * Control.Plus
                                        ->  ^Monoid)
        
        /// <summary>Lifts a function into a Writer. Same as map.
        /// To be used in Applicative Style expressions, combined with &lt;*&gt;
        /// </summary>
        /// <category index="1">Functor</category>
        static member
          (<!>) : f: ('T -> 'U) * x: Writer<'Monoid,'T> -> Writer<'Monoid,'U>
        
        static member
          inline (<*>) : f: Writer< ^Monoid,('T -> 'U)> * x: Writer< ^Monoid,'T>
                           -> Writer< ^Monoid,'U>
                           when (Control.Plus or  ^Monoid) :
                                  (static member ``+`` :
                                      ^Monoid *  ^Monoid * Control.Plus
                                       ->  ^Monoid)
        
        static member
          (=>>) : g: Writer<'T,'W> * f: (Writer<'T,'W> -> 'U) -> Writer<'U,'W>
        
        static member
          inline (>>=) : x: Writer< ^Monoid,'T> * f: ('T -> Writer< ^Monoid,'U>)
                           -> Writer< ^Monoid,'U>
                           when (Control.Plus or  ^Monoid) :
                                  (static member ``+`` :
                                      ^Monoid *  ^Monoid * Control.Plus
                                       ->  ^Monoid)
        
        static member Extract: Writer<'T,'W> -> 'T
        
        static member
          Listen: m: Writer<'Monoid,'T> -> Writer<'Monoid,('T * 'Monoid)>
        
        [<System.ComponentModel.EditorBrowsable
          (enum<System.ComponentModel.EditorBrowsableState> (1))>]
        static member
          Map: x: Writer<'Monoid,'T> * f: ('T -> 'U) -> Writer<'Monoid,'U>
        
        static member
          Pass: m: Writer<'Monoid,('T * ('Monoid -> 'Monoid))>
                  -> Writer<'Monoid,'T>
        
        static member
          inline Return: x: 'T -> Writer< ^Monoid,'T>
                           when (Control.Zero or  ^Monoid) :
                                  (static member Zero:
                                      ^Monoid * Control.Zero ->  ^Monoid)
        
        static member Tell: w: 'Monoid -> Writer<'Monoid,unit>
    
    /// Basic operations on Writer
    module Writer =
        
        /// Unwraps a writer computation as a (result, output) pair. (The inverse of Writer.)
        val run: Writer<'Monoid,'T> -> 'T * 'Monoid
        
        val map: f: ('T -> 'U) -> Writer<'Monoid,'T> -> Writer<'Monoid,'U>
        
        /// Combines two Writers into one by applying a mapping function.
        val inline map2:
          f: ('T -> 'U -> 'V) -> Writer< ^Monoid,'T> -> Writer< ^Monoid,'U>
            -> Writer< ^Monoid,'V>
            when (Control.Plus or  ^Monoid) :
                   (static member ``+`` :
                       ^Monoid *  ^Monoid * Control.Plus ->  ^Monoid)
        
        /// Combines three Writers into one by applying a mapping function.
        val inline map3:
          f: ('T -> 'U -> 'V -> 'W) -> Writer< ^Monoid,'T>
          -> Writer< ^Monoid,'U> -> Writer< ^Monoid,'V> -> Writer< ^Monoid,'W>
            when (Control.Plus or  ^Monoid) :
                   (static member ``+`` :
                       ^Monoid *  ^Monoid * Control.Plus ->  ^Monoid)
        
        val inline bind:
          f: ('T -> Writer< ^Monoid,'U>) -> Writer< ^Monoid,'T>
            -> Writer< ^Monoid,'U>
            when (Control.Plus or  ^Monoid) :
                   (static member ``+`` :
                       ^Monoid *  ^Monoid * Control.Plus ->  ^Monoid)
        
        val inline apply:
          Writer< ^Monoid,('T -> 'U)> -> Writer< ^Monoid,'T>
            -> Writer< ^Monoid,'U>
            when (Control.Plus or  ^Monoid) :
                   (static member ``+`` :
                       ^Monoid *  ^Monoid * Control.Plus ->  ^Monoid)
        
        /// Extract the output from a writer computation.
        val exec: Writer<'Monoid,'T> -> 'Monoid
        
        /// Embeds a simple writer action.
        val tell: w: 'Monoid -> Writer<'Monoid,unit>
        
        /// <summary> An action that executes the action <paramref name="m"/> and adds its output
        /// to the value of the computation. </summary>
        /// <param name="m">The action to be executed.</param>
        val listen: m: Writer<'Monoid,'T> -> Writer<'Monoid,('T * 'Monoid)>
        
        /// Action that executes the action m, which returns a value and a function, and returns the value, applying the function to the output.
        val pass:
          m: Writer<'Monoid,('T * ('Monoid -> 'Monoid))> -> Writer<'Monoid,'T>
    
    /// Monad Transformer for Writer<'Monoid, 'T>
    [<Struct>]
    type WriterT<'monad<'t * 'monoid>> =
        | WriterT of 'monad<'t * 'monoid>
        
        /// <summary>
        /// Sequences two Writers left-to-right, discarding the value of the first argument.
        /// </summary>
        /// <category index="2">Applicative</category>
        static member
          inline ( *> ) : x: WriterT< ^Monad<'T * 'Monoid>> *
                          y: WriterT< ^Monad<'U * 'Monoid>>
                            -> WriterT< ^Monad<'U * 'Monoid>>
                            when (Control.Map or  ^Monad<'T * 'Monoid> or
                                   ^Monad<('U -> 'U) * 'Monoid>) :
                                   (static member Map:
                                      ( ^Monad<'T * 'Monoid> *
                                       ('T * 'a -> ('U -> 'U) * 'a)) *
                                      Control.Map
                                        ->  ^Monad<('U -> 'U) * 'Monoid>) and
                                 (Control.Apply or  ^b or
                                   ^Monad<('U -> 'U) * 'Monoid> or  ^f) :
                                   (static member ``<*>`` :
                                       ^b *  ^Monad<('U -> 'U) * 'Monoid> *  ^f *
                                      Control.Apply ->  ^f) and
                                 (Control.Return or  ^b) :
                                   (static member Return:
                                       ^b * Control.Return
                                        -> ((('c -> 'd) *  ^e -> 'c *  ^e
                                               -> 'd *  ^e) ->  ^b)) and
                                 (Control.Plus or  ^e) :
                                   (static member ``+`` :
                                       ^e *  ^e * Control.Plus ->  ^e) and
                                 (Control.Apply or  ^f or  ^Monad<'U * 'Monoid>) :
                                   (static member ``<*>`` :
                                       ^f *  ^Monad<'U * 'Monoid> *
                                       ^Monad<'U * 'Monoid> * Control.Apply
                                        ->  ^Monad<'U * 'Monoid>)
        
        /// <summary>
        /// Sequences two Writers left-to-right, discarding the value of the second argument.
        /// </summary>
        /// <category index="2">Applicative</category>
        static member
          inline ( <* ) : x: WriterT< ^Monad<'U * 'Monoid>> *
                          y: WriterT< ^Monad<'T * 'Monoid>>
                            -> WriterT< ^Monad<'U * 'Monoid>>
                            when (Control.Map or  ^Monad<'U * 'Monoid> or
                                   ^Monad<('T -> 'U) * 'Monoid>) :
                                   (static member Map:
                                      ( ^Monad<'U * 'Monoid> *
                                       ('U * 'a -> ('T -> 'U) * 'a)) *
                                      Control.Map
                                        ->  ^Monad<('T -> 'U) * 'Monoid>) and
                                 (Control.Apply or  ^f or  ^Monad<'T * 'Monoid> or
                                   ^Monad<'U * 'Monoid>) :
                                   (static member ``<*>`` :
                                       ^f *  ^Monad<'T * 'Monoid> *
                                       ^Monad<'U * 'Monoid> * Control.Apply
                                        ->  ^Monad<'U * 'Monoid>) and
                                 (Control.Apply or  ^b or
                                   ^Monad<('T -> 'U) * 'Monoid> or  ^f) :
                                   (static member ``<*>`` :
                                       ^b *  ^Monad<('T -> 'U) * 'Monoid> *  ^f *
                                      Control.Apply ->  ^f) and
                                 (Control.Return or  ^b) :
                                   (static member Return:
                                       ^b * Control.Return
                                        -> ((('c -> 'd) *  ^e -> 'c *  ^e
                                               -> 'd *  ^e) ->  ^b)) and
                                 (Control.Plus or  ^e) :
                                   (static member ``+`` :
                                       ^e *  ^e * Control.Plus ->  ^e)
        
        /// <summary>Lifts a function into a WriterT. Same as map.
        /// To be used in Applicative Style expressions, combined with &lt;*&gt;
        /// </summary>
        /// <category index="1">Functor</category>
        static member
          inline (<!>) : f: ('T -> 'U) * x: WriterT< ^Monad<'T * 'Monoid>>
                           -> WriterT< ^Monad<'U * 'Monoid>>
                           when (Control.Map or  ^Monad<'T * 'Monoid> or
                                  ^Monad<'U * 'Monoid>) :
                                  (static member Map:
                                     ( ^Monad<'T * 'Monoid> *
                                      ('T * 'a -> 'U * 'a)) * Control.Map
                                       ->  ^Monad<'U * 'Monoid>)
        
        static member
          inline (<*>) : f: WriterT< ^Monad<('T -> 'U) * 'Monoid>> *
                         x: WriterT< ^Monad<'T * 'Monoid>>
                           -> WriterT< ^Monad<'U * 'Monoid>>
                           when (Control.Apply or  ^a or
                                  ^Monad<('T -> 'U) * 'Monoid> or  ^e) :
                                  (static member ``<*>`` :
                                      ^a *  ^Monad<('T -> 'U) * 'Monoid> *  ^e *
                                     Control.Apply ->  ^e) and
                                (Control.Return or  ^a) :
                                  (static member Return:
                                      ^a * Control.Return
                                       -> ((('b -> 'c) *  ^d -> 'b *  ^d
                                              -> 'c *  ^d) ->  ^a)) and
                                (Control.Plus or  ^d) :
                                  (static member ``+`` :
                                      ^d *  ^d * Control.Plus ->  ^d) and
                                (Control.Apply or  ^e or  ^Monad<'T * 'Monoid> or
                                  ^Monad<'U * 'Monoid>) :
                                  (static member ``<*>`` :
                                      ^e *  ^Monad<'T * 'Monoid> *
                                      ^Monad<'U * 'Monoid> * Control.Apply
                                       ->  ^Monad<'U * 'Monoid>)
        
        static member
          inline (<|>) : WriterT< ^MonadPlus<'T * 'Monoid>> *
                         WriterT< ^MonadPlus<'T * 'Monoid>>
                           -> WriterT< ^MonadPlus<'T * 'Monoid>>
                           when (Control.Append or  ^MonadPlus<'T * 'Monoid>) :
                                  (static member ``<|>`` :
                                      ^MonadPlus<'T * 'Monoid> *
                                      ^MonadPlus<'T * 'Monoid> * Control.Append
                                       ->  ^MonadPlus<'T * 'Monoid>)
        
        static member
          inline (>>=) : x: WriterT< ^Monad<'T * 'Monoid>> *
                         f: ('T -> WriterT< ^Monad<'U * 'Monoid>>)
                           -> WriterT< ^Monad<'U * 'Monoid>>
                           when (Control.Bind or  ^Monad<'T * 'Monoid> or
                                  ^Monad<'U * 'Monoid>) :
                                  (static member (>>=) :
                                      ^Monad<'T * 'Monoid> *
                                     ('T *  ^a ->  ^Monad<'U * 'Monoid>)
                                       ->  ^Monad<'U * 'Monoid>) and
                                (Control.Bind or  ^Monad<'U * 'Monoid>) :
                                  (static member (>>=) :
                                      ^Monad<'U * 'Monoid> *
                                     ('b *  ^a ->  ^Monad<'U * 'Monoid>)
                                       ->  ^Monad<'U * 'Monoid>) and
                                (Control.Return or  ^Monad<'U * 'Monoid>) :
                                  (static member Return:
                                      ^Monad<'U * 'Monoid> * Control.Return
                                       -> ('b *  ^a ->  ^Monad<'U * 'Monoid>)) and
                                (Control.Plus or  ^a) :
                                  (static member ``+`` :
                                      ^a *  ^a * Control.Plus ->  ^a)
        
        static member
          inline CallCC: f: (('a -> WriterT<Cont<'r,'t>>)
                               -> WriterT< ^MonadCont<'r,'a*'b>>)
                           -> WriterT< ^MonadCont<'r,'a*'b>>
                           when  ^MonadCont<'r,'a*'b> :
                                  (static member CallCC:
                                     (('a *  ^a0 -> Cont<'r,'t>)
                                        ->  ^MonadCont<'r,'a*'b>)
                                       ->  ^MonadCont<'r,'a*'b>) and
                                (Control.Zero or  ^a0) :
                                  (static member Zero:
                                      ^a0 * Control.Zero ->  ^a0)
        
        static member
          inline Catch: m: WriterT< ^MonadError<'E1, 'T * 'Monoid>> *
                        h: ('E1 -> WriterT< ^MonadError<'E2, 'T * 'Monoid>>)
                          -> WriterT< ^MonadError<'E2, 'T * 'Monoid>>
                          when (Control.Catch or  ^MonadError<'E1, 'T * 'Monoid> or
                                 ^MonadError<'E2, 'T * 'Monoid>) :
                                 (static member Catch:
                                     ^MonadError<'E1, 'T * 'Monoid> *
                                    ('E1 ->  ^MonadError<'E2, 'T * 'Monoid>)
                                      ->  ^MonadError<'E2, 'T * 'Monoid>)
        
        static member
          inline Delay: body: (unit -> WriterT< ^Monad<'T * 'Monoid>>)
                          -> WriterT< ^Monad<'T * 'Monoid>>
                          when (Control.Delay or  ^Monad<'T * 'Monoid>) :
                                 (static member Delay:
                                    Control.Delay *
                                    (unit ->  ^Monad<'T * 'Monoid>) *
                                    Control.Delay ->  ^Monad<'T * 'Monoid>)
        
        [<System.ComponentModel.EditorBrowsable
          (enum<System.ComponentModel.EditorBrowsableState> (1))>]
        static member
          inline Lift: m:  ^Monad<'T> -> WriterT< ^Monad<'T * 'Monoid>>
                         when (Control.Map or  ^Monad<'T> or
                                ^Monad<'T * 'Monoid>) :
                                (static member Map:
                                   ( ^Monad<'T> * ('c -> 'c *  ^d)) *
                                   Control.Map ->  ^Monad<'T * 'Monoid>) and
                              (Control.Bind or  ^Monad<'T> or
                                ^Monad<'T * 'Monoid>) :
                                (static member (>>=) :
                                    ^Monad<'T> * ('a ->  ^Monad<'T * 'Monoid>)
                                     ->  ^Monad<'T * 'Monoid>) and
                              (Control.Return or  ^Monad<'T * 'Monoid>) :
                                (static member Return:
                                    ^Monad<'T * 'Monoid> * Control.Return
                                     -> ('a *  ^b ->  ^Monad<'T * 'Monoid>)) and
                              (Control.Zero or  ^b) :
                                (static member Zero:  ^b * Control.Zero ->  ^b) and
                              (Control.Zero or  ^d) :
                                (static member Zero:  ^d * Control.Zero ->  ^d)
        
        [<System.ComponentModel.EditorBrowsable
          (enum<System.ComponentModel.EditorBrowsableState> (1))>]
        static member
          inline Lift2: f: ('T -> 'U -> 'V) * x: WriterT< ^Monad<'T * 'Monoid>> *
                        y: WriterT< ^Monad<'U * 'Monoid>>
                          -> WriterT< ^Monad<'V * 'Monoid>>
                          when (Control.Lift2 or  ^Monad<'T * 'Monoid> or
                                 ^Monad<'U * 'Monoid> or  ^Monad<'V * 'Monoid>) :
                                 (static member Lift2:
                                    ('T *  ^a -> 'U *  ^a -> 'V *  ^a) *
                                    ( ^Monad<'T * 'Monoid> *
                                      ^Monad<'U * 'Monoid>) * Control.Lift2
                                      ->  ^Monad<'V * 'Monoid>) and
                               (Control.Plus or  ^a) :
                                 (static member ``+`` :
                                     ^a *  ^a * Control.Plus ->  ^a)
        
        [<System.ComponentModel.EditorBrowsable
          (enum<System.ComponentModel.EditorBrowsableState> (1))>]
        static member
          inline Lift3: f: ('T -> 'U -> 'V -> 'W) *
                        x: WriterT< ^Monad<'T * 'Monoid>> *
                        y: WriterT< ^Monad<'U * 'Monoid>> *
                        z: WriterT< ^Monad<'V * 'Monoid>>
                          -> WriterT< ^Monad<'W * 'Monoid>>
                          when (Control.Lift3 or  ^Monad<'T * 'Monoid> or
                                 ^Monad<'U * 'Monoid> or  ^Monad<'V * 'Monoid> or
                                 ^Monad<'W * 'Monoid>) :
                                 (static member Lift3:
                                    ('T *  ^a -> 'U *  ^a -> 'V *  ^a
                                       -> 'W *  ^a) *
                                    ( ^Monad<'T * 'Monoid> *
                                      ^Monad<'U * 'Monoid> *
                                      ^Monad<'V * 'Monoid>) * Control.Lift3
                                      ->  ^Monad<'W * 'Monoid>) and
                               (Control.Plus or  ^a) :
                                 (static member ``+`` :
                                     ^a *  ^a * Control.Plus ->  ^a)
        
        static member
          inline LiftAsync: x: Async<'T> -> WriterT< ^MonadAsync<'T>>
                              when (Control.Return or  ^MonadAsync<'T>) :
                                     (static member Return:
                                         ^MonadAsync<'T> * Control.Return
                                          -> ('a *  ^b ->  ^MonadAsync<'T>)) and
                                   (Control.Bind or  ^c or  ^MonadAsync<'T>) :
                                     (static member (>>=) :
                                         ^c * ('a ->  ^MonadAsync<'T>)
                                          ->  ^MonadAsync<'T>) and
                                   (Control.Map or  ^c or  ^MonadAsync<'T>) :
                                     (static member Map:
                                        ( ^c * ('d -> 'd *  ^e)) * Control.Map
                                          ->  ^MonadAsync<'T>) and
                                   (Control.Zero or  ^b) :
                                     (static member Zero:
                                         ^b * Control.Zero ->  ^b) and
                                   (Control.LiftAsync or  ^c) :
                                     (static member LiftAsync:
                                         ^c -> (Async<'T> ->  ^c)) and
                                   (Control.Zero or  ^e) :
                                     (static member Zero:
                                         ^e * Control.Zero ->  ^e)
        
        static member
          inline Listen: WriterT< ^Monad<'T * 'Monoid>>
                           -> WriterT< ^Monad<('T * 'Monoid) * 'Monoid>>
                           when (Control.Bind or  ^Monad<'T * 'Monoid> or
                                  ^Monad<('T * 'Monoid) * 'Monoid>) :
                                  (static member (>>=) :
                                      ^Monad<'T * 'Monoid> *
                                     ('a * 'b
                                        ->  ^Monad<('T * 'Monoid) * 'Monoid>)
                                       ->  ^Monad<('T * 'Monoid) * 'Monoid>) and
                                (Control.Return or
                                  ^Monad<('T * 'Monoid) * 'Monoid>) :
                                  (static member Return:
                                      ^Monad<('T * 'Monoid) * 'Monoid> *
                                     Control.Return
                                       -> (('a * 'b) * 'b
                                             ->  ^Monad<('T * 'Monoid) * 'Monoid>))
        
        static member
          inline Local: WriterT< ^a> * f: ('R1 -> 'R2)
                          -> WriterT< ^MonadReader<'R1,'T*'Monoid>>
                          when  ^MonadReader<'R1,'T*'Monoid> :
                                 (static member Local:
                                     ^a * ('R1 -> 'R2)
                                      ->  ^MonadReader<'R1,'T*'Monoid>)
        
        [<System.ComponentModel.EditorBrowsable
          (enum<System.ComponentModel.EditorBrowsableState> (1))>]
        static member
          inline Map: x: WriterT< ^Monad<'T * 'Monoid>> * f: ('T -> 'U)
                        -> WriterT< ^Monad<'U * 'Monoid>>
                        when (Control.Map or  ^Monad<'T * 'Monoid> or
                               ^Monad<'U * 'Monoid>) :
                               (static member Map:
                                  ( ^Monad<'T * 'Monoid> * ('T * 'a -> 'U * 'a)) *
                                  Control.Map ->  ^Monad<'U * 'Monoid>)
        
        static member
          inline Pass: WriterT< ^Monad<('T * ('Monoid -> 'Monoid)) * 'Monoid>>
                         -> WriterT< ^Monad<'T * 'Monoid>>
                         when (Control.Bind or
                                ^Monad<('T * ('Monoid -> 'Monoid)) * 'Monoid> or
                                ^Monad<'T * 'Monoid>) :
                                (static member (>>=) :
                                    ^Monad<('T * ('Monoid -> 'Monoid)) * 'Monoid> *
                                   (('a * ('b -> 'c)) * 'b
                                      ->  ^Monad<'T * 'Monoid>)
                                     ->  ^Monad<'T * 'Monoid>) and
                              (Control.Return or  ^Monad<'T * 'Monoid>) :
                                (static member Return:
                                    ^Monad<'T * 'Monoid> * Control.Return
                                     -> ('a * 'c ->  ^Monad<'T * 'Monoid>))
        
        static member
          inline Put: x: 'S -> WriterT< ^MonadState<'S,unit*'Monoid>>
                        when (Control.Return or  ^MonadState<'S,unit*'Monoid>) :
                               (static member Return:
                                   ^MonadState<'S,unit*'Monoid> * Control.Return
                                    -> ('a *  ^b
                                          ->  ^MonadState<'S,unit*'Monoid>)) and
                             (Control.Bind or  ^c or
                               ^MonadState<'S,unit*'Monoid>) :
                               (static member (>>=) :
                                   ^c * ('a ->  ^MonadState<'S,unit*'Monoid>)
                                    ->  ^MonadState<'S,unit*'Monoid>) and
                             (Control.Map or  ^c or
                               ^MonadState<'S,unit*'Monoid>) :
                               (static member Map:
                                  ( ^c * ('d -> 'd *  ^e)) * Control.Map
                                    ->  ^MonadState<'S,unit*'Monoid>) and
                             (Control.Zero or  ^b) :
                               (static member Zero:  ^b * Control.Zero ->  ^b) and
                              ^c: (static member Put: 'S ->  ^c) and
                             (Control.Zero or  ^e) :
                               (static member Zero:  ^e * Control.Zero ->  ^e)
        
        static member
          inline Return: x: 'T -> WriterT< ^Monad<'T * 'Monoid>>
                           when (Control.Return or  ^Monad<'T * 'Monoid>) :
                                  (static member Return:
                                      ^Monad<'T * 'Monoid> * Control.Return
                                       -> ('T *  ^a ->  ^Monad<'T * 'Monoid>)) and
                                (Control.Zero or  ^a) :
                                  (static member Zero:  ^a * Control.Zero ->  ^a)
        
        static member
          inline Tell: w: 'Monoid -> WriterT< ^Monad<unit * 'Monoid>>
                         when (Control.Return or  ^Monad<unit * 'Monoid>) :
                                (static member Return:
                                    ^Monad<unit * 'Monoid> * Control.Return
                                     -> (unit * 'Monoid
                                           ->  ^Monad<unit * 'Monoid>))
        
        static member
          inline Throw: x: 'E -> WriterT< ^a>
                          when (Control.Return or  ^a) :
                                 (static member Return:
                                     ^a * Control.Return -> ('b *  ^c ->  ^a)) and
                               (Control.Bind or  ^d or  ^a) :
                                 (static member (>>=) :  ^d * ('b ->  ^a) ->  ^a) and
                               (Control.Map or  ^d or  ^a) :
                                 (static member Map:
                                    ( ^d * ('e -> 'e *  ^f)) * Control.Map
                                      ->  ^a) and
                               (Control.Zero or  ^c) :
                                 (static member Zero:  ^c * Control.Zero ->  ^c) and
                               (Control.Throw or  ^d) :
                                 (static member Throw:  ^d * 'E ->  ^d) and
                               (Control.Zero or  ^f) :
                                 (static member Zero:  ^f * Control.Zero ->  ^f)
        
        static member
          inline TryFinally: computation: WriterT< ^Monad<'T * 'Monoid>> *
                             f: (unit -> unit) -> WriterT< ^Monad<'T * 'Monoid>>
                               when (Control.TryFinally or  ^Monad<'T * 'Monoid>) :
                                      (static member TryFinally:
                                         ((unit ->  ^Monad<'T * 'Monoid>) *
                                          (unit -> unit)) * Control.TryFinally *
                                         Control.TryFinally *
                                         Control.TryBlock.False
                                           ->  ^Monad<'T * 'Monoid>)
        
        static member
          inline TryWith: source: WriterT< ^Monad<'T * 'Monoid>> *
                          f: (exn -> WriterT< ^Monad<'T * 'Monoid>>)
                            -> WriterT< ^Monad<'T * 'Monoid>>
                            when (Control.TryWith or  ^Monad<'T * 'Monoid>) :
                                   (static member TryWith:
                                      (unit ->  ^Monad<'T * 'Monoid>) *
                                      ('a ->  ^Monad<'T * 'Monoid>) *
                                      Control.TryWith * Control.TryBlock.False
                                        ->  ^Monad<'T * 'Monoid>) and 'a :> exn
        
        static member
          inline Using: resource: 'a * f: ('a -> WriterT< ^Monad<'T * 'Monoid>>)
                          -> WriterT< ^Monad<'T * 'Monoid>>
                          when 'a :> System.IDisposable and
                               (Control.Using or  ^Monad<'T * 'Monoid>) :
                                 (static member Using:
                                    'a * ('a ->  ^Monad<'T * 'Monoid>) *
                                    Control.Using ->  ^Monad<'T * 'Monoid>)
        
        static member
          inline get_Ask: unit -> WriterT< ^MonadReader<'R,'R*'Monoid>>
                            when (Control.Return or  ^MonadReader<'R,'R*'Monoid>) :
                                   (static member Return:
                                       ^MonadReader<'R,'R*'Monoid> *
                                      Control.Return
                                        -> ('a *  ^b
                                              ->  ^MonadReader<'R,'R*'Monoid>)) and
                                 (Control.Bind or  ^c or
                                   ^MonadReader<'R,'R*'Monoid>) :
                                   (static member (>>=) :
                                       ^c * ('a ->  ^MonadReader<'R,'R*'Monoid>)
                                        ->  ^MonadReader<'R,'R*'Monoid>) and
                                 (Control.Map or  ^c or
                                   ^MonadReader<'R,'R*'Monoid>) :
                                   (static member Map:
                                      ( ^c * ('d -> 'd *  ^e)) * Control.Map
                                        ->  ^MonadReader<'R,'R*'Monoid>) and
                                 (Control.Zero or  ^b) :
                                   (static member Zero:
                                       ^b * Control.Zero ->  ^b) and
                                  ^c: (static member get_Ask: ->  ^c) and
                                 (Control.Zero or  ^e) :
                                   (static member Zero:
                                       ^e * Control.Zero ->  ^e)
        
        static member
          inline get_Empty: unit -> WriterT< ^MonadPlus<'T * 'Monoid>>
                              when (Control.Empty or  ^MonadPlus<'T * 'Monoid>) :
                                     (static member Empty:
                                         ^MonadPlus<'T * 'Monoid> *
                                        Control.Empty
                                          ->  ^MonadPlus<'T * 'Monoid>)
        
        static member
          inline get_Get: unit -> WriterT< ^MonadState<'S,'S*'Monoid>>
                            when (Control.Return or  ^MonadState<'S,'S*'Monoid>) :
                                   (static member Return:
                                       ^MonadState<'S,'S*'Monoid> *
                                      Control.Return
                                        -> ('a *  ^b
                                              ->  ^MonadState<'S,'S*'Monoid>)) and
                                 (Control.Bind or  ^c or
                                   ^MonadState<'S,'S*'Monoid>) :
                                   (static member (>>=) :
                                       ^c * ('a ->  ^MonadState<'S,'S*'Monoid>)
                                        ->  ^MonadState<'S,'S*'Monoid>) and
                                 (Control.Map or  ^c or
                                   ^MonadState<'S,'S*'Monoid>) :
                                   (static member Map:
                                      ( ^c * ('d -> 'd *  ^e)) * Control.Map
                                        ->  ^MonadState<'S,'S*'Monoid>) and
                                 (Control.Zero or  ^b) :
                                   (static member Zero:
                                       ^b * Control.Zero ->  ^b) and
                                  ^c: (static member get_Get: ->  ^c) and
                                 (Control.Zero or  ^e) :
                                   (static member Zero:
                                       ^e * Control.Zero ->  ^e)
    
    /// Basic operations on WriterT
    module WriterT =
        
        val run: WriterT<'Monad<'T * 'Monoid>> -> 'Monad<'T * 'Monoid>
        
        /// Embed a Monad<'T> into a WriterT<'Monad<'T * 'Monoid>>
        val inline lift:
          m:  ^Monad<'T> -> WriterT< ^Monad<'T * 'Monoid>>
            when (Control.Bind or  ^Monad<'T> or  ^Monad<'T * 'Monoid>) :
                   (static member (>>=) :
                       ^Monad<'T> * ('a ->  ^Monad<'T * 'Monoid>)
                        ->  ^Monad<'T * 'Monoid>) and
                 (Control.Map or  ^Monad<'T> or  ^Monad<'T * 'Monoid>) :
                   (static member Map:
                      ( ^Monad<'T> * ('c -> 'c *  ^d)) * Control.Map
                        ->  ^Monad<'T * 'Monoid>) and
                 (Control.Return or  ^Monad<'T * 'Monoid>) :
                   (static member Return:
                       ^Monad<'T * 'Monoid> * Control.Return
                        -> ('a *  ^b ->  ^Monad<'T * 'Monoid>)) and
                 (Control.Zero or  ^b) :
                   (static member Zero:  ^b * Control.Zero ->  ^b) and
                 (Control.Zero or  ^d) :
                   (static member Zero:  ^d * Control.Zero ->  ^d)
        
        val inline map:
          f: ('T -> 'U) -> WriterT< ^Monad<'T * 'Monoid>>
            -> WriterT< ^Monad<'U * 'Monoid>>
            when (Control.Map or  ^Monad<'T * 'Monoid> or  ^Monad<'U * 'Monoid>) :
                   (static member Map:
                      ( ^Monad<'T * 'Monoid> * ('T * 'a -> 'U * 'a)) *
                      Control.Map ->  ^Monad<'U * 'Monoid>)
        
        /// Combines two WriterTs into one by applying a mapping function.
        val inline map2:
          f: ('T -> 'U -> 'V) -> WriterT< ^Monad<'T * 'Monoid>>
          -> WriterT< ^Monad<'U * 'Monoid>> -> WriterT< ^Monad<'V * 'Monoid>>
            when (Control.Lift2 or  ^Monad<'T * 'Monoid> or
                   ^Monad<'U * 'Monoid> or  ^Monad<'V * 'Monoid>) :
                   (static member Lift2:
                      ('T *  ^a -> 'U *  ^a -> 'V *  ^a) *
                      ( ^Monad<'T * 'Monoid> *  ^Monad<'U * 'Monoid>) *
                      Control.Lift2 ->  ^Monad<'V * 'Monoid>) and
                 (Control.Plus or  ^a) :
                   (static member ``+`` :  ^a *  ^a * Control.Plus ->  ^a)
        
        /// Combines three WriterTs into one by applying a mapping function.
        val inline map3:
          f: ('T -> 'U -> 'V -> 'W) -> WriterT< ^Monad<'T * 'Monoid>>
          -> WriterT< ^Monad<'U * 'Monoid>> -> WriterT< ^Monad<'V * 'Monoid>>
            -> WriterT< ^Monad<'W * 'Monoid>>
            when (Control.Lift3 or  ^Monad<'T * 'Monoid> or
                   ^Monad<'U * 'Monoid> or  ^Monad<'V * 'Monoid> or
                   ^Monad<'W * 'Monoid>) :
                   (static member Lift3:
                      ('T *  ^a -> 'U *  ^a -> 'V *  ^a -> 'W *  ^a) *
                      ( ^Monad<'T * 'Monoid> *  ^Monad<'U * 'Monoid> *
                        ^Monad<'V * 'Monoid>) * Control.Lift3
                        ->  ^Monad<'W * 'Monoid>) and
                 (Control.Plus or  ^a) :
                   (static member ``+`` :  ^a *  ^a * Control.Plus ->  ^a)
        
        val inline apply:
          WriterT< ^Monad<('T -> 'U) * 'Monoid>>
          -> WriterT< ^Monad<'T * 'Monoid>> -> WriterT< ^Monad<'U * 'Monoid>>
            when (Control.Apply or  ^a or  ^Monad<('T -> 'U) * 'Monoid> or  ^e) :
                   (static member ``<*>`` :
                       ^a *  ^Monad<('T -> 'U) * 'Monoid> *  ^e * Control.Apply
                        ->  ^e) and
                 (Control.Return or  ^a) :
                   (static member Return:
                       ^a * Control.Return
                        -> ((('b -> 'c) *  ^d -> 'b *  ^d -> 'c *  ^d) ->  ^a)) and
                 (Control.Plus or  ^d) :
                   (static member ``+`` :  ^d *  ^d * Control.Plus ->  ^d) and
                 (Control.Apply or  ^e or  ^Monad<'T * 'Monoid> or
                   ^Monad<'U * 'Monoid>) :
                   (static member ``<*>`` :
                       ^e *  ^Monad<'T * 'Monoid> *  ^Monad<'U * 'Monoid> *
                      Control.Apply ->  ^Monad<'U * 'Monoid>)
        
        val inline bind:
          f: ('T -> WriterT< ^Monad<'U * 'Monoid>>)
          -> WriterT< ^Monad<'T * 'Monoid>> -> WriterT< ^Monad<'U * 'Monoid>>
            when (Control.Bind or  ^Monad<'T * 'Monoid> or  ^Monad<'U * 'Monoid>) :
                   (static member (>>=) :
                       ^Monad<'T * 'Monoid> *
                      ('T *  ^a ->  ^Monad<'U * 'Monoid>)
                        ->  ^Monad<'U * 'Monoid>) and
                 (Control.Bind or  ^Monad<'U * 'Monoid>) :
                   (static member (>>=) :
                       ^Monad<'U * 'Monoid> *
                      ('b *  ^a ->  ^Monad<'U * 'Monoid>)
                        ->  ^Monad<'U * 'Monoid>) and
                 (Control.Return or  ^Monad<'U * 'Monoid>) :
                   (static member Return:
                       ^Monad<'U * 'Monoid> * Control.Return
                        -> ('b *  ^a ->  ^Monad<'U * 'Monoid>)) and
                 (Control.Plus or  ^a) :
                   (static member ``+`` :  ^a *  ^a * Control.Plus ->  ^a)

