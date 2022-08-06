namespace FSharpPlus.Data
    
    /// <summary> Computation type: Computations which maintain state.
    /// <para>   Binding strategy: Threads a state parameter through the sequence of bound functions so that the same state value is never used twice, giving the illusion of in-place update.</para>
    /// <para>   Useful for: Building computations from sequences of operations that require a shared state.</para>
    /// The <typeparamref name="'s"/> indicates the computation state, while <typeparamref name="'t"/> indicates the result.</summary>
    [<Struct>]
    type State<'s,'t> =
        | State of ('s -> 't * 's)
        
        /// <summary>
        /// Sequences two States left-to-right, discarding the value of the first argument.
        /// </summary>
        /// <category index="2">Applicative</category>
        static member ( *> ) : x: State<'S,'T> * y: State<'S,'U> -> State<'S,'U>
        
        /// <summary>
        /// Sequences two States left-to-right, discarding the value of the second argument.
        /// </summary>
        /// <category index="2">Applicative</category>
        static member ( <* ) : x: State<'S,'U> * y: State<'S,'T> -> State<'S,'U>
        
        /// <summary>Lifts a function into a State. Same as map.
        /// To be used in Applicative Style expressions, combined with &lt;*&gt;
        /// </summary>
        /// <category index="1">Functor</category>
        static member (<!>) : f: ('T -> 'U) * x: State<'S,'T> -> State<'S,'U>
        
        static member
          (<*>) : f: State<'S,('T -> 'U)> * x: State<'S,'T> -> State<'S,'U>
        
        static member
          (>>=) : x: State<'S,'T> * f: ('T -> State<'S,'U>) -> State<'S,'U>
        
        static member Delay: body: (unit -> State<'S,'T>) -> State<'S,'T>
        
        [<System.ComponentModel.EditorBrowsable
          (enum<System.ComponentModel.EditorBrowsableState> (1))>]
        static member
          Lift2: f: ('T -> 'U -> 'V) * x: State<'S,'T> * y: State<'S,'U>
                   -> State<'S,'V>
        
        [<System.ComponentModel.EditorBrowsable
          (enum<System.ComponentModel.EditorBrowsableState> (1))>]
        static member
          Lift3: f: ('T -> 'U -> 'V -> 'W) * x: State<'S,'T> * y: State<'S,'U> *
                 z: State<'S,'V> -> State<'S,'W>
        
        [<System.ComponentModel.EditorBrowsable
          (enum<System.ComponentModel.EditorBrowsableState> (1))>]
        static member Map: x: State<'S,'T> * f: ('T -> 'U) -> State<'S,'U>
        
        [<System.ComponentModel.EditorBrowsable
          (enum<System.ComponentModel.EditorBrowsableState> (1))>]
        static member Put: x: 'S -> State<'S,unit>
        
        static member Return: a: 'T -> State<'S,'T>
        
        static member
          TryFinally: State<'S,'T> * f: (unit -> unit) -> State<'S,'T>
        
        static member
          TryWith: State<'S,'T> * h: (exn -> State<'S,'T>) -> State<'S,'T>
        
        static member
          Using: resource: 'a * f: ('a -> State<'S,'T>) -> State<'S,'T>
                   when 'a :> System.IDisposable
        
        [<System.ComponentModel.EditorBrowsable
          (enum<System.ComponentModel.EditorBrowsableState> (1))>]
        static member
          Zip: x: State<'a,'b> * y: State<'a,'c> -> State<'a,('b * 'c)>
        
        static member get_Get: unit -> State<'S,'S>
    
    /// Basic operations on State
    module State =
        
        /// Runs the state with an inital state to get back the result and the new state.
        val run: State<'S,'T> -> ('S -> 'T * 'S)
        
        val map: f: ('T -> 'U) -> State<'S,'T> -> State<'S,'U>
        
        /// Combines two States into one by applying a mapping function.
        val map2:
          f: ('T -> 'U -> 'V) -> State<'S,'T> -> State<'S,'U> -> State<'S,'V>
        
        /// Combines three States into one by applying a mapping function.
        val map3:
          f: ('T -> 'U -> 'V -> 'W) -> State<'S,'T> -> State<'S,'U>
          -> State<'S,'V> -> State<'S,'W>
        
        val bind: f: ('T -> State<'S,'U>) -> State<'S,'T> -> State<'S,'U>
        
        val apply: State<'S,('T -> 'U)> -> State<'S,'T> -> State<'S,'U>
        
        /// Evaluates a <paramref name="sa">state computation</paramref> with the <paramref name="s">initial value</paramref> and return only the result value of the computation. Ignore the final state.
        val eval: State<'s,'T> -> s: 's -> 'T
        
        /// Evaluates a <paramref name="sa">state computation</paramref> with the <paramref name="s">initial value</paramref> and return only the final state of the computation. Ignore the result value.
        val exec: State<'S,'A> -> s: 'S -> 'S
        
        /// Return the state from the internals of the monad.
        val get: State<'S,'S>
        
        /// Get a value which depends on the current state.
        val gets: f: ('S -> 'T) -> State<'S,'T>
        
        /// Replace the state inside the monad.
        val put: x: 'S -> State<'S,unit>
        
        /// Modify the state inside the monad by applying a function.
        val modify: f: ('S -> 'S) -> State<'S,unit>
        
        /// Zips two States into one.
        val zip: x: State<'S,'T> -> y: State<'S,'U> -> State<'S,('T * 'U)>
    
    /// Monad Transformer for State<'S, 'T>
    [<Struct>]
    type StateT<'s,'monad<'t * 's>> =
        | StateT of ('s -> 'monad<'t * 's>)
        
        /// <summary>
        /// Sequences two States left-to-right, discarding the value of the first argument.
        /// </summary>
        /// <category index="2">Applicative</category>
        static member
          inline ( *> ) : x: StateT<'S, ^Monad<'T * 'S>> *
                          y: StateT<'S, ^Monad<'U * 'S>>
                            -> StateT<'S, ^Monad<'U * 'S>>
                            when (Control.Map or  ^Monad<'T * 'S> or
                                   ^Monad<('U->'U) * 'S>) :
                                   (static member Map:
                                      ( ^Monad<'T * 'S> *
                                       ('T * 'a -> ('U -> 'U) * 'a)) *
                                      Control.Map ->  ^Monad<('U->'U) * 'S>) and
                                 (Control.Bind or  ^Monad<('U->'U) * 'S> or
                                   ^Monad<'U * 'S>) :
                                   (static member (>>=) :
                                       ^Monad<('U->'U) * 'S> *
                                      (('b -> 'c) * 'S ->  ^Monad<'U * 'S>)
                                        ->  ^Monad<'U * 'S>) and
                                 (Control.Map or  ^Monad<'U * 'S>) :
                                   (static member Map:
                                      ( ^Monad<'U * 'S> * ('b * 'S -> 'c * 'S)) *
                                      Control.Map ->  ^Monad<'U * 'S>)
        
        /// <summary>
        /// Sequences two States left-to-right, discarding the value of the second argument.
        /// </summary>
        /// <category index="2">Applicative</category>
        static member
          inline ( <* ) : x: StateT<'S, ^Monad<'U * 'S>> *
                          y: StateT<'S, ^Monad<'T * 'S>>
                            -> StateT<'S, ^Monad<'U * 'S>>
                            when (Control.Map or  ^Monad<'U * 'S> or
                                   ^Monad<('T->'U) * 'S>) :
                                   (static member Map:
                                      ( ^Monad<'U * 'S> *
                                       ('U * 'a -> ('T -> 'U) * 'a)) *
                                      Control.Map ->  ^Monad<('T->'U) * 'S>) and
                                 (Control.Bind or  ^Monad<('T->'U) * 'S> or
                                   ^Monad<'U * 'S>) :
                                   (static member (>>=) :
                                       ^Monad<('T->'U) * 'S> *
                                      (('b -> 'c) * 'S ->  ^Monad<'U * 'S>)
                                        ->  ^Monad<'U * 'S>) and
                                 (Control.Map or  ^Monad<'T * 'S> or
                                   ^Monad<'U * 'S>) :
                                   (static member Map:
                                      ( ^Monad<'T * 'S> * ('b * 'S -> 'c * 'S)) *
                                      Control.Map ->  ^Monad<'U * 'S>)
        
        /// <summary>Lifts a function into a StateT. Same as map.
        /// To be used in Applicative Style expressions, combined with &lt;*&gt;
        /// </summary>
        /// <category index="1">Functor</category>
        static member
          inline (<!>) : f: ('T -> 'U) * x: StateT<'S, ^Monad<'T * 'S>>
                           -> StateT<'S, ^Monad<'U * 'S>>
                           when (Control.Map or  ^Monad<'T * 'S> or
                                  ^Monad<'U * 'S>) :
                                  (static member Map:
                                     ( ^Monad<'T * 'S> * ('T * 'a -> 'U * 'a)) *
                                     Control.Map ->  ^Monad<'U * 'S>)
        
        static member
          inline (<*>) : f: StateT<'S, ^Monad<('T -> 'U) * 'S>> *
                         x: StateT<'S, ^Monad<'T * 'S>>
                           -> StateT<'S, ^Monad<'U * 'S>>
                           when (Control.Bind or  ^Monad<('T -> 'U) * 'S> or
                                  ^Monad<'U * 'S>) :
                                  (static member (>>=) :
                                      ^Monad<('T -> 'U) * 'S> *
                                     (('a -> 'b) * 'S ->  ^Monad<'U * 'S>)
                                       ->  ^Monad<'U * 'S>) and
                                (Control.Map or  ^Monad<'T * 'S> or
                                  ^Monad<'U * 'S>) :
                                  (static member Map:
                                     ( ^Monad<'T * 'S> * ('a * 'S -> 'b * 'S)) *
                                     Control.Map ->  ^Monad<'U * 'S>)
        
        static member
          inline (<|>) : StateT<'S, ^MonadPlus<'T * 'S>> *
                         StateT<'S, ^MonadPlus<'T * 'S>>
                           -> StateT<'S, ^MonadPlus<'T * 'S>>
                           when (Control.Append or  ^MonadPlus<'T * 'S>) :
                                  (static member ``<|>`` :
                                      ^MonadPlus<'T * 'S> *  ^MonadPlus<'T * 'S> *
                                     Control.Append ->  ^MonadPlus<'T * 'S>)
        
        static member
          inline (>>=) : x: StateT<'S, ^Monad<'T * 'S>> *
                         f: ('T -> StateT<'S, ^Monad<'U * 'S>>)
                           -> StateT<'S, ^Monad<'U * 'S>>
                           when (Control.Bind or  ^Monad<'T * 'S> or
                                  ^Monad<'U * 'S>) :
                                  (static member (>>=) :
                                      ^Monad<'T * 'S> *
                                     ('T * 'S ->  ^Monad<'U * 'S>)
                                       ->  ^Monad<'U * 'S>)
        
        static member
          inline Catch: m: StateT<'S, ^MonadError<'E1,'T * 'S>> *
                        h: ('E1 -> StateT<'S, ^MonadError<'E2, 'T * 'S>>)
                          -> StateT<'S, ^MonadError<'E2, 'T * 'S>>
                          when (Control.Catch or  ^MonadError<'E1,'T * 'S> or
                                 ^MonadError<'E2, 'T * 'S>) :
                                 (static member Catch:
                                     ^MonadError<'E1,'T * 'S> *
                                    ('E1 ->  ^MonadError<'E2, 'T * 'S>)
                                      ->  ^MonadError<'E2, 'T * 'S>)
        
        static member
          inline Delay: body: (unit -> StateT<'S, ^Monad<'T * 'S>>)
                          -> StateT<'S, ^Monad<'T * 'S>>
                          when (Control.Delay or  ^Monad<'T * 'S>) :
                                 (static member Delay:
                                    Control.Delay * (unit ->  ^Monad<'T * 'S>) *
                                    Control.Delay ->  ^Monad<'T * 'S>)
        
        [<System.ComponentModel.EditorBrowsable
          (enum<System.ComponentModel.EditorBrowsableState> (1))>]
        static member
          inline Lift: m:  ^Monad<'T> -> StateT<'S, ^Monad<'T * 'S>>
                         when (Control.Map or  ^Monad<'T> or  ^Monad<'T * 'S>) :
                                (static member Map:
                                   ( ^Monad<'T> * ('b -> 'b * 'S)) * Control.Map
                                     ->  ^Monad<'T * 'S>) and
                              (Control.Bind or  ^Monad<'T> or  ^Monad<'T * 'S>) :
                                (static member (>>=) :
                                    ^Monad<'T> * ('a ->  ^Monad<'T * 'S>)
                                     ->  ^Monad<'T * 'S>) and
                              (Control.Return or  ^Monad<'T * 'S>) :
                                (static member Return:
                                    ^Monad<'T * 'S> * Control.Return
                                     -> ('a * 'S ->  ^Monad<'T * 'S>))
        
        [<System.ComponentModel.EditorBrowsable
          (enum<System.ComponentModel.EditorBrowsableState> (1))>]
        static member
          inline Lift2: f: ('T -> 'U -> 'V) * x: StateT<'S, ^Monad<'T * 'S>> *
                        y: StateT<'S, ^Monad<'U * 'S>>
                          -> StateT<'S, ^Monad<'V * 'S>>
                          when (Control.Bind or  ^Monad<'T * 'S> or
                                 ^Monad<'V * 'S>) :
                                 (static member (>>=) :
                                     ^Monad<'T * 'S> *
                                    ('T * 'S ->  ^Monad<'V * 'S>)
                                      ->  ^Monad<'V * 'S>) and
                               (Control.Bind or  ^Monad<'U * 'S> or
                                 ^Monad<'V * 'S>) :
                                 (static member (>>=) :
                                     ^Monad<'U * 'S> *
                                    ('U * 'S ->  ^Monad<'V * 'S>)
                                      ->  ^Monad<'V * 'S>) and
                               (Control.Return or  ^Monad<'V * 'S>) :
                                 (static member Return:
                                     ^Monad<'V * 'S> * Control.Return
                                      -> ('V * 'S ->  ^Monad<'V * 'S>))
        
        [<System.ComponentModel.EditorBrowsable
          (enum<System.ComponentModel.EditorBrowsableState> (1))>]
        static member
          inline Lift3: f: ('T -> 'U -> 'V -> 'W) *
                        x: StateT<'S, ^Monad<'T * 'S>> *
                        y: StateT<'S, ^Monad<'U * 'S>> *
                        z: StateT<'S, ^Monad<'V * 'S>>
                          -> StateT<'S, ^Monad<'W * 'S>>
                          when (Control.Bind or  ^Monad<'T * 'S> or
                                 ^Monad<'W * 'S>) :
                                 (static member (>>=) :
                                     ^Monad<'T * 'S> *
                                    ('T * 'S ->  ^Monad<'W * 'S>)
                                      ->  ^Monad<'W * 'S>) and
                               (Control.Bind or  ^Monad<'U * 'S> or
                                 ^Monad<'W * 'S>) :
                                 (static member (>>=) :
                                     ^Monad<'U * 'S> *
                                    ('U * 'S ->  ^Monad<'W * 'S>)
                                      ->  ^Monad<'W * 'S>) and
                               (Control.Bind or  ^Monad<'V * 'S> or
                                 ^Monad<'W * 'S>) :
                                 (static member (>>=) :
                                     ^Monad<'V * 'S> *
                                    ('V * 'a ->  ^Monad<'W * 'S>)
                                      ->  ^Monad<'W * 'S>) and
                               (Control.Return or  ^Monad<'W * 'S>) :
                                 (static member Return:
                                     ^Monad<'W * 'S> * Control.Return
                                      -> ('W * 'a ->  ^Monad<'W * 'S>))
        
        static member
          inline LiftAsync: x: Async<'T> -> StateT<'S, ^MonadAsync<'T>>
                              when (Control.Bind or  ^a or  ^MonadAsync<'T>) :
                                     (static member (>>=) :
                                         ^a * ('c ->  ^MonadAsync<'T>)
                                          ->  ^MonadAsync<'T>) and
                                   (Control.Return or  ^MonadAsync<'T>) :
                                     (static member Return:
                                         ^MonadAsync<'T> * Control.Return
                                          -> ('c * 'S ->  ^MonadAsync<'T>)) and
                                   (Control.Map or  ^a or  ^MonadAsync<'T>) :
                                     (static member Map:
                                        ( ^a * ('b -> 'b * 'S)) * Control.Map
                                          ->  ^MonadAsync<'T>) and
                                   (Control.LiftAsync or  ^a) :
                                     (static member LiftAsync:
                                         ^a -> (Async<'T> ->  ^a))
        
        static member
          inline Local: StateT<'S, ^a> * f: ('R1 -> 'R2)
                          -> StateT<'S, ^MonadReader<'R1, 'T>>
                          when  ^MonadReader<'R1, 'T> :
                                 (static member Local:
                                     ^a * ('R1 -> 'R2) ->  ^MonadReader<'R1, 'T>)
        
        [<System.ComponentModel.EditorBrowsable
          (enum<System.ComponentModel.EditorBrowsableState> (1))>]
        static member
          inline Map: x: StateT<'S, ^Monad<'T * 'S>> * f: ('T -> 'U)
                        -> StateT<'S, ^Monad<'U * 'S>>
                        when (Control.Map or  ^Monad<'T * 'S> or
                               ^Monad<'U * 'S>) :
                               (static member Map:
                                  ( ^Monad<'T * 'S> * ('T * 'a -> 'U * 'a)) *
                                  Control.Map ->  ^Monad<'U * 'S>)
        
        static member
          inline Put: x: 'S -> StateT<'S, ^Monad<unit * 'S>>
                        when (Control.Return or  ^Monad<unit * 'S>) :
                               (static member Return:
                                   ^Monad<unit * 'S> * Control.Return
                                    -> (unit * 'S ->  ^Monad<unit * 'S>))
        
        static member
          inline Return: x: 'T -> StateT<'S, ^Monad<'T * 'S>>
                           when (Control.Return or  ^Monad<'T * 'S>) :
                                  (static member Return:
                                      ^Monad<'T * 'S> * Control.Return
                                       -> ('T * 'S ->  ^Monad<'T * 'S>))
        
        static member
          inline Throw: x: 'E -> StateT<'a, ^b>
                          when (Control.Bind or  ^c or  ^b) :
                                 (static member (>>=) :  ^c * ('d ->  ^b) ->  ^b) and
                               (Control.Return or  ^b) :
                                 (static member Return:
                                     ^b * Control.Return -> ('d * 'a ->  ^b)) and
                               (Control.Map or  ^c or  ^b) :
                                 (static member Map:
                                    ( ^c * ('e -> 'e * 'a)) * Control.Map ->  ^b) and
                               (Control.Throw or  ^c) :
                                 (static member Throw:  ^c * 'E ->  ^c)
        
        static member
          inline TryFinally: computation: StateT<'S, ^Monad<'T * 'S>> *
                             f: (unit -> unit) -> StateT<'S, ^Monad<'T * 'S>>
                               when (Control.TryFinally or  ^Monad<'T * 'S>) :
                                      (static member TryFinally:
                                         ((unit ->  ^Monad<'T * 'S>) *
                                          (unit -> unit)) * Control.TryFinally *
                                         Control.TryFinally *
                                         Control.TryBlock.True
                                           ->  ^Monad<'T * 'S>)
        
        static member
          inline TryWith: source: StateT<'S, ^Monad<'T * 'S>> *
                          f: (exn -> StateT<'S, ^Monad<'T * 'S>>)
                            -> StateT<'S, ^Monad<'T * 'S>>
                            when (Control.TryWith or  ^Monad<'T * 'S>) :
                                   (static member TryWith:
                                      (unit ->  ^Monad<'T * 'S>) *
                                      ('a ->  ^Monad<'T * 'S>) * Control.TryWith *
                                      Control.TryBlock.True ->  ^Monad<'T * 'S>) and
                                 'a :> exn
        
        static member
          inline Using: resource: 'a * f: ('a -> StateT<'S, ^Monad<'T * 'S>>)
                          -> StateT<'S, ^Monad<'T * 'S>>
                          when 'a :> System.IDisposable and
                               (Control.Using or  ^Monad<'T * 'S>) :
                                 (static member Using:
                                    'a * ('a ->  ^Monad<'T * 'S>) *
                                    Control.Using ->  ^Monad<'T * 'S>)
        
        [<System.ComponentModel.EditorBrowsable
          (enum<System.ComponentModel.EditorBrowsableState> (1))>]
        static member
          inline Zip: x: StateT<'S, ^Monad<'T * 'S>> *
                      y: StateT<'S, ^Monad<'U * 'S>> -> StateT<'S, ^e>
                        when (Control.Map or  ^Monad<'T * 'S> or  ^a) :
                               (static member Map:
                                  ( ^Monad<'T * 'S> *
                                   ('b * 'c -> ('d -> 'b * 'd) * 'c)) *
                                  Control.Map ->  ^a) and
                             (Control.Bind or  ^a or  ^e) :
                               (static member (>>=) :
                                   ^a * (('f -> 'g) * 'S ->  ^e) ->  ^e) and
                             (Control.Map or  ^Monad<'U * 'S> or  ^e) :
                               (static member Map:
                                  ( ^Monad<'U * 'S> * ('f * 'S -> 'g * 'S)) *
                                  Control.Map ->  ^e)
        
        static member
          inline get_Ask: unit -> StateT<'S, ^MonadReader<'R, 'R>>
                            when (Control.Bind or  ^a or  ^MonadReader<'R, 'R>) :
                                   (static member (>>=) :
                                       ^a * ('c ->  ^MonadReader<'R, 'R>)
                                        ->  ^MonadReader<'R, 'R>) and
                                 (Control.Return or  ^MonadReader<'R, 'R>) :
                                   (static member Return:
                                       ^MonadReader<'R, 'R> * Control.Return
                                        -> ('c * 'S ->  ^MonadReader<'R, 'R>)) and
                                 (Control.Map or  ^a or  ^MonadReader<'R, 'R>) :
                                   (static member Map:
                                      ( ^a * ('b -> 'b * 'S)) * Control.Map
                                        ->  ^MonadReader<'R, 'R>) and
                                  ^a: (static member get_Ask: ->  ^a)
        
        static member
          inline get_Empty: unit -> StateT<'S, ^MonadPlus<'T * 'S>>
                              when (Control.Empty or  ^MonadPlus<'T * 'S>) :
                                     (static member Empty:
                                         ^MonadPlus<'T * 'S> * Control.Empty
                                          ->  ^MonadPlus<'T * 'S>)
        
        static member
          inline get_Get: unit -> StateT<'S, ^Monad<'S * 'S>>
                            when (Control.Return or  ^Monad<'S * 'S>) :
                                   (static member Return:
                                       ^Monad<'S * 'S> * Control.Return
                                        -> ('S * 'S ->  ^Monad<'S * 'S>))
    
    /// Basic operations on StateT
    module StateT =
        
        /// Runs the state with an inital state to get back the result and the new state wrapped in an inner monad.
        val run: StateT<'S,'Monad<'T * 'S>> -> ('S -> 'Monad<'T * 'S>)
        
        /// Embed a Monad<'T> into a StateT<'S,'``Monad<'T * 'S>``>
        val inline lift:
          m:  ^Monad<'T> -> StateT<'S, ^Monad<'T * 'S>>
            when (Control.Bind or  ^Monad<'T> or  ^Monad<'T * 'S>) :
                   (static member (>>=) :
                       ^Monad<'T> * ('a ->  ^Monad<'T * 'S>) ->  ^Monad<'T * 'S>) and
                 (Control.Map or  ^Monad<'T> or  ^Monad<'T * 'S>) :
                   (static member Map:
                      ( ^Monad<'T> * ('b -> 'b * 'S)) * Control.Map
                        ->  ^Monad<'T * 'S>) and
                 (Control.Return or  ^Monad<'T * 'S>) :
                   (static member Return:
                       ^Monad<'T * 'S> * Control.Return
                        -> ('a * 'S ->  ^Monad<'T * 'S>))
        
        /// Transform a State<'S, 'T> to a StateT<'S, '``Monad<'T * 'S>``>
        val inline hoist:
          x: State<'S,'T> -> StateT<'S, ^Monad<'T * 'S>>
            when (Control.Return or  ^Monad<'T * 'S>) :
                   (static member Return:
                       ^Monad<'T * 'S> * Control.Return
                        -> ('T * 'S ->  ^Monad<'T * 'S>))
        
        val inline map:
          f: ('T -> 'U) -> StateT<'S, ^Monad<'T * 'S>>
            -> StateT<'S, ^Monad<'U * 'S>>
            when (Control.Map or  ^Monad<'T * 'S> or  ^Monad<'U * 'S>) :
                   (static member Map:
                      ( ^Monad<'T * 'S> * ('T * 'a -> 'U * 'a)) * Control.Map
                        ->  ^Monad<'U * 'S>)
        
        /// Combines two StateTs into one by applying a mapping function.
        val inline map2:
          f: ('T -> 'U -> 'V) -> StateT<'S, ^Monad<'T * 'S>>
          -> StateT<'S, ^Monad<'U * 'S>> -> StateT<'S, ^Monad<'V * 'S>>
            when (Control.Bind or  ^Monad<'T * 'S> or  ^Monad<'V * 'S>) :
                   (static member (>>=) :
                       ^Monad<'T * 'S> * ('T * 'S ->  ^Monad<'V * 'S>)
                        ->  ^Monad<'V * 'S>) and
                 (Control.Bind or  ^Monad<'U * 'S> or  ^Monad<'V * 'S>) :
                   (static member (>>=) :
                       ^Monad<'U * 'S> * ('U * 'S ->  ^Monad<'V * 'S>)
                        ->  ^Monad<'V * 'S>) and
                 (Control.Return or  ^Monad<'V * 'S>) :
                   (static member Return:
                       ^Monad<'V * 'S> * Control.Return
                        -> ('V * 'S ->  ^Monad<'V * 'S>))
        
        /// Combines three StateTs into one by applying a mapping function.
        val inline map3:
          f: ('T -> 'U -> 'V -> 'W) -> StateT<'S, ^Monad<'T * 'S>>
          -> StateT<'S, ^Monad<'U * 'S>> -> StateT<'S, ^Monad<'V * 'S>>
            -> StateT<'S, ^Monad<'W * 'S>>
            when (Control.Bind or  ^Monad<'T * 'S> or  ^Monad<'W * 'S>) :
                   (static member (>>=) :
                       ^Monad<'T * 'S> * ('T * 'S ->  ^Monad<'W * 'S>)
                        ->  ^Monad<'W * 'S>) and
                 (Control.Bind or  ^Monad<'U * 'S> or  ^Monad<'W * 'S>) :
                   (static member (>>=) :
                       ^Monad<'U * 'S> * ('U * 'S ->  ^Monad<'W * 'S>)
                        ->  ^Monad<'W * 'S>) and
                 (Control.Bind or  ^Monad<'V * 'S> or  ^Monad<'W * 'S>) :
                   (static member (>>=) :
                       ^Monad<'V * 'S> * ('V * 'a ->  ^Monad<'W * 'S>)
                        ->  ^Monad<'W * 'S>) and
                 (Control.Return or  ^Monad<'W * 'S>) :
                   (static member Return:
                       ^Monad<'W * 'S> * Control.Return
                        -> ('W * 'a ->  ^Monad<'W * 'S>))
        
        val inline apply:
          StateT<'S, ^Monad<('T -> 'U) * 'S>> -> StateT<'S, ^Monad<'T * 'S>>
            -> StateT<'S, ^Monad<'U * 'S>>
            when (Control.Bind or  ^Monad<('T -> 'U) * 'S> or  ^Monad<'U * 'S>) :
                   (static member (>>=) :
                       ^Monad<('T -> 'U) * 'S> *
                      (('T -> 'U) * 'S ->  ^Monad<'U * 'S>) ->  ^Monad<'U * 'S>) and
                 (Control.Map or  ^Monad<'T * 'S> or  ^Monad<'U * 'S>) :
                   (static member Map:
                      ( ^Monad<'T * 'S> * ('T * 'S -> 'U * 'S)) * Control.Map
                        ->  ^Monad<'U * 'S>)
        
        /// Zips two StateTs into one.
        val inline zip:
          x: StateT<'S, ^Monad<'T * 'S>> -> y: StateT<'S, ^Monad<'U * 'S>>
            -> StateT<'S, ^Monad<('T * 'U) * 'S>>
            when (Control.Map or  ^Monad<'T * 'S> or  ^a) :
                   (static member Map:
                      ( ^Monad<'T * 'S> * ('b * 'c -> ('d -> 'b * 'd) * 'c)) *
                      Control.Map ->  ^a) and
                 (Control.Bind or  ^a or  ^Monad<('T * 'U) * 'S>) :
                   (static member (>>=) :
                       ^a * (('e -> 'f) * 'S ->  ^Monad<('T * 'U) * 'S>)
                        ->  ^Monad<('T * 'U) * 'S>) and
                 (Control.Map or  ^Monad<'U * 'S> or  ^Monad<('T * 'U) * 'S>) :
                   (static member Map:
                      ( ^Monad<'U * 'S> * ('e * 'S -> 'f * 'S)) * Control.Map
                        ->  ^Monad<('T * 'U) * 'S>)
        
        val inline bind:
          f: ('T -> StateT<'S, ^Monad<'U * 'S>>) -> StateT<'S, ^Monad<'T * 'S>>
            -> StateT<'S, ^Monad<'U * 'S>>
            when (Control.Bind or  ^Monad<'T * 'S> or  ^Monad<'U * 'S>) :
                   (static member (>>=) :
                       ^Monad<'T * 'S> * ('T * 'S ->  ^Monad<'U * 'S>)
                        ->  ^Monad<'U * 'S>)

