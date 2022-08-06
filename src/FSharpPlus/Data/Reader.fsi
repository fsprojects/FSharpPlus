namespace FSharpPlus.Data
    
    /// <summary> Computation type: Computations which read values from a shared environment.
    /// <para/>   Binding strategy: Monad values are functions from the environment to a value. The bound function is applied to the bound value, and both have access to the shared environment.
    /// <para/>   Useful for: Maintaining variable bindings, or other shared environment.</summary>
    [<Struct>]
    type Reader<'r,'t> =
        | Reader of ('r -> 't)
        
        /// <summary>
        /// Sequences two Readers left-to-right, discarding the value of the first argument.
        /// </summary>
        /// <category index="2">Applicative</category>
        static member
          ( *> ) : x: Reader<'R,'T> * y: Reader<'R,'U> -> Reader<'R,'U>
        
        /// <summary>
        /// Sequences two Readers left-to-right, discarding the value of the second argument.
        /// </summary>
        /// <category index="2">Applicative</category>
        static member
          ( <* ) : x: Reader<'R,'U> * y: Reader<'R,'T> -> Reader<'R,'U>
        
        /// <summary>Lifts a function into a Reader. Same as map.
        /// To be used in Applicative Style expressions, combined with &lt;*&gt;
        /// </summary>
        /// <category index="1">Functor</category>
        static member (<!>) : f: ('T -> 'U) * x: Reader<'R,'T> -> Reader<'R,'U>
        
        static member
          (<*>) : f: Reader<'R,('T -> 'U)> * x: Reader<'R,'T> -> Reader<'R,'U>
        
        static member
          inline (=>>) : Reader< ^Monoid,'T> * f: (Reader< ^Monoid,'T> -> 'U)
                           -> Reader< ^Monoid,'U>
                           when (Control.Plus or  ^Monoid) :
                                  (static member ``+`` :
                                      ^Monoid *  ^Monoid * Control.Plus
                                       ->  ^Monoid)
        
        static member
          (>>=) : x: Reader<'R,'T> * f: ('T -> Reader<'R,'U>) -> Reader<'R,'U>
        
        static member Delay: body: (unit -> Reader<'R,'T>) -> Reader<'R,'T>
        
        static member
          inline Extract: Reader< ^Monoid,'T> -> 'T
                            when (Control.Zero or  ^Monoid) :
                                   (static member Zero:
                                       ^Monoid * Control.Zero ->  ^Monoid)
        
        [<System.ComponentModel.EditorBrowsable
          (enum<System.ComponentModel.EditorBrowsableState> (1))>]
        static member
          Lift2: f: ('T -> 'U -> 'V) * x: Reader<'R,'T> * y: Reader<'R,'U>
                   -> Reader<'R,'V>
        
        [<System.ComponentModel.EditorBrowsable
          (enum<System.ComponentModel.EditorBrowsableState> (1))>]
        static member
          Lift3: f: ('T -> 'U -> 'V -> 'W) * x: Reader<'R,'T> * y: Reader<'R,'U> *
                 z: Reader<'R,'V> -> Reader<'R,'W>
        
        [<System.ComponentModel.EditorBrowsable
          (enum<System.ComponentModel.EditorBrowsableState> (1))>]
        static member
          Local: m: Reader<'R2,'T> * f: ('R1 -> 'R2) -> Reader<'R1,'T>
        
        [<System.ComponentModel.EditorBrowsable
          (enum<System.ComponentModel.EditorBrowsableState> (1))>]
        static member Map: x: Reader<'R,'T> * f: ('T -> 'U) -> Reader<'R,'U>
        
        static member Return: x: 'T -> Reader<'R,'T>
        
        static member
          TryFinally: Reader<'a,'b> * f: (unit -> unit) -> Reader<'a,'b>
        
        static member
          TryWith: Reader<'R,'T> * h: (exn -> Reader<'R,'T>) -> Reader<'R,'T>
        
        static member
          Using: resource: 'a * f: ('a -> Reader<'R,'T>) -> Reader<'R,'T>
                   when 'a :> System.IDisposable
        
        [<System.ComponentModel.EditorBrowsable
          (enum<System.ComponentModel.EditorBrowsableState> (1))>]
        static member
          Zip: x: Reader<'a,'b> * y: Reader<'a,'c> -> Reader<'a,('b * 'c)>
        
        static member get_Ask: unit -> Reader<'R,'R>
    
    /// Basic operations on Reader
    module Reader =
        
        val run: Reader<'R,'T> -> ('R -> 'T)
        
        val map: f: ('T -> 'U) -> Reader<'R,'T> -> Reader<'R,'U>
        
        val bind: f: ('T -> Reader<'R,'U>) -> Reader<'R,'T> -> Reader<'R,'U>
        
        val apply: Reader<'R,('T -> 'U)> -> Reader<'R,'T> -> Reader<'R,'U>
        
        /// Combines two Readers into one by applying a mapping function.
        val map2:
          mapping: ('T -> 'U -> 'V) -> Reader<'R,'T> -> Reader<'R,'U>
            -> Reader<'R,'V>
        
        /// Combines three Readers into one by applying a mapping function.
        val map3:
          mapping: ('T -> 'U -> 'V -> 'W) -> Reader<'R,'T> -> Reader<'R,'U>
          -> Reader<'R,'V> -> Reader<'R,'W>
        
        /// Zips two Readers into one.
        val zip: x: Reader<'R,'T> -> y: Reader<'R,'U> -> Reader<'R,('T * 'U)>
        
        /// Retrieves the monad environment.
        val ask: Reader<'R,'R>
        
        /// <summary> Executes a computation in a modified environment. </summary>
        /// <param name="f"> The function to modify the environment.    </param>
        /// <param name="m"> Reader to run in the modified environment. </param>
        val local: f: ('R1 -> 'R2) -> m: Reader<'R2,'T> -> Reader<'R1,'T>
    
    /// Monad Transformer for Reader<'R, 'T>
    [<Struct>]
    type ReaderT<'r,'monad<'t>> =
        | ReaderT of ('r -> 'monad<'t>)
        
        /// <summary>
        /// Sequences two Readers left-to-right, discarding the value of the first argument.
        /// </summary>
        /// <category index="2">Applicative</category>
        static member
          inline ( *> ) : x: ReaderT<'R, ^Monad<'T>> *
                          y: ReaderT<'R, ^Monad<'U>> -> ReaderT<'R, ^Monad<'U>>
                            when (Control.Map or  ^Monad<'T> or  ^Monad<'U->'U>) :
                                   (static member Map:
                                      ( ^Monad<'T> * ('T -> 'U -> 'U)) *
                                      Control.Map ->  ^Monad<'U->'U>) and
                                 (Control.Apply or  ^Monad<'U->'U> or
                                   ^Monad<'U>) :
                                   (static member ``<*>`` :
                                       ^Monad<'U->'U> *  ^Monad<'U> *
                                       ^Monad<'U> * Control.Apply ->  ^Monad<'U>)
        
        /// <summary>
        /// Sequences two Readers left-to-right, discarding the value of the second argument.
        /// </summary>
        /// <category index="2">Applicative</category>
        static member
          inline ( <* ) : x: ReaderT<'R, ^Monad<'U>> *
                          y: ReaderT<'R, ^Monad<'T>> -> ReaderT<'R, ^Monad<'U>>
                            when (Control.Map or  ^Monad<'U> or  ^Monad<'T->'U>) :
                                   (static member Map:
                                      ( ^Monad<'U> * ('U -> 'T -> 'U)) *
                                      Control.Map ->  ^Monad<'T->'U>) and
                                 (Control.Apply or  ^Monad<'T->'U> or
                                   ^Monad<'T> or  ^Monad<'U>) :
                                   (static member ``<*>`` :
                                       ^Monad<'T->'U> *  ^Monad<'T> *
                                       ^Monad<'U> * Control.Apply ->  ^Monad<'U>)
        
        /// <summary>Lifts a function into a ReaderT. Same as map.
        /// To be used in Applicative Style expressions, combined with &lt;*&gt;
        /// </summary>
        /// <category index="1">Functor</category>
        static member
          inline (<!>) : f: ('T -> 'U) * x: ReaderT<'R, ^Monad<'T>>
                           -> ReaderT<'R, ^Monad<'U>>
                           when (Control.Map or  ^Monad<'T> or  ^Monad<'U>) :
                                  (static member Map:
                                     ( ^Monad<'T> * ('T -> 'U)) * Control.Map
                                       ->  ^Monad<'U>)
        
        static member
          inline (<*>) : f: ReaderT<'R, ^Monad<'T -> 'U>> *
                         x: ReaderT<'R, ^Monad<'T>> -> ReaderT<'R, ^Monad<'U>>
                           when (Control.Apply or  ^Monad<'T -> 'U> or
                                  ^Monad<'T> or  ^Monad<'U>) :
                                  (static member ``<*>`` :
                                      ^Monad<'T -> 'U> *  ^Monad<'T> *
                                      ^Monad<'U> * Control.Apply ->  ^Monad<'U>)
        
        static member
          inline (<|>) : ReaderT<'R, ^MonadPlus<'T>> *
                         ReaderT<'R, ^MonadPlus<'T>>
                           -> ReaderT<'R, ^MonadPlus<'T>>
                           when (Control.Append or  ^MonadPlus<'T>) :
                                  (static member ``<|>`` :
                                      ^MonadPlus<'T> *  ^MonadPlus<'T> *
                                     Control.Append ->  ^MonadPlus<'T>)
        
        static member
          inline (>>=) : x: ReaderT<'R, ^Monad<'T>> *
                         f: ('T -> ReaderT<'R, ^Monad<'U>>)
                           -> ReaderT<'R, ^Monad<'U>>
                           when (Control.Bind or  ^Monad<'T> or  ^Monad<'U>) :
                                  (static member (>>=) :
                                      ^Monad<'T> * ('T ->  ^Monad<'U>)
                                       ->  ^Monad<'U>)
        
        static member
          inline CallCC: f: (('T -> ReaderT<'R,'MonadCont<'C,'U>>)
                               -> ReaderT<'R, ^MonadCont<'C,'T>>)
                           -> ReaderT<'R, ^MonadCont<'C,'T>>
                           when  ^MonadCont<'C,'T> :
                                  (static member CallCC:
                                     (('T -> 'MonadCont<'C,'U>)
                                        ->  ^MonadCont<'C,'T>)
                                       ->  ^MonadCont<'C,'T>)
        
        static member
          inline Catch: m: ReaderT<'R, ^MonadError<'E1,'T>> *
                        h: ('E1 -> ReaderT<'R, ^MonadError<'E2,'T>>)
                          -> ReaderT<'R, ^MonadError<'E2,'T>>
                          when (Control.Catch or  ^MonadError<'E1,'T> or
                                 ^MonadError<'E2,'T>) :
                                 (static member Catch:
                                     ^MonadError<'E1,'T> *
                                    ('E1 ->  ^MonadError<'E2,'T>)
                                      ->  ^MonadError<'E2,'T>)
        
        static member
          inline Delay: body: (unit -> ReaderT<'R, ^Monad<'T>>)
                          -> ReaderT<'R, ^Monad<'T>>
                          when (Control.Delay or  ^Monad<'T>) :
                                 (static member Delay:
                                    Control.Delay * (unit ->  ^Monad<'T>) *
                                    Control.Delay ->  ^Monad<'T>)
        
        [<System.ComponentModel.EditorBrowsable
          (enum<System.ComponentModel.EditorBrowsableState> (1))>]
        static member Lift: m: 'Monad<'T> -> ReaderT<'R,'Monad<'T>>
        
        [<System.ComponentModel.EditorBrowsable
          (enum<System.ComponentModel.EditorBrowsableState> (1))>]
        static member
          inline Lift2: f: ('T -> 'U -> 'V) * x: ReaderT<'R, ^Monad<'T>> *
                        y: ReaderT<'R, ^Monad<'U>> -> ReaderT<'R, ^Monad<'V>>
                          when (Control.Lift2 or  ^Monad<'T> or  ^Monad<'U> or
                                 ^Monad<'V>) :
                                 (static member Lift2:
                                    ('T -> 'U -> 'V) *
                                    ( ^Monad<'T> *  ^Monad<'U>) * Control.Lift2
                                      ->  ^Monad<'V>)
        
        [<System.ComponentModel.EditorBrowsable
          (enum<System.ComponentModel.EditorBrowsableState> (1))>]
        static member
          inline Lift3: f: ('T -> 'U -> 'V -> 'W) * x: ReaderT<'R, ^Monad<'T>> *
                        y: ReaderT<'R, ^Monad<'U>> * z: ReaderT<'R, ^Monad<'V>>
                          -> ReaderT<'R, ^Monad<'W>>
                          when (Control.Lift3 or  ^Monad<'T> or  ^Monad<'U> or
                                 ^Monad<'V> or  ^Monad<'W>) :
                                 (static member Lift3:
                                    ('T -> 'U -> 'V -> 'W) *
                                    ( ^Monad<'T> *  ^Monad<'U> *  ^Monad<'V>) *
                                    Control.Lift3 ->  ^Monad<'W>)
        
        static member
          inline LiftAsync: x: Async<'T> -> ReaderT<'R, ^MonadAsync<'T>>
                              when (Control.LiftAsync or  ^MonadAsync<'T>) :
                                     (static member LiftAsync:
                                         ^MonadAsync<'T>
                                          -> (Async<'T> ->  ^MonadAsync<'T>))
        
        static member
          inline Listen: ReaderT<'R,'a>
                           -> ReaderT<'R, ^MonadWriter<'Monoid,'T*'Monoid>>
                           when  ^MonadWriter<'Monoid,'T*'Monoid> :
                                  (static member Listen:
                                     'a ->  ^MonadWriter<'Monoid,'T*'Monoid>)
        
        static member
          Local: ReaderT<'R2,'Monad<'T>> * f: ('R1 -> 'R2)
                   -> ReaderT<'R1,'Monad<'T>>
        
        [<System.ComponentModel.EditorBrowsable
          (enum<System.ComponentModel.EditorBrowsableState> (1))>]
        static member
          inline Map: x: ReaderT<'R, ^Monad<'T>> * f: ('T -> 'U)
                        -> ReaderT<'R, ^Monad<'U>>
                        when (Control.Map or  ^Monad<'T> or  ^Monad<'U>) :
                               (static member Map:
                                  ( ^Monad<'T> * ('T -> 'U)) * Control.Map
                                    ->  ^Monad<'U>)
        
        static member
          inline Pass: ReaderT<'R,'a> -> ReaderT<'R, ^MonadWriter<'Monoid,'T>>
                         when  ^MonadWriter<'Monoid,'T> :
                                (static member Pass:
                                   'a ->  ^MonadWriter<'Monoid,'T>)
        
        static member
          inline Put: x: 'S -> ReaderT<'R, ^MonadState<'S, unit>>
                        when  ^MonadState<'S, unit> :
                               (static member Put: 'S ->  ^MonadState<'S, unit>)
        
        static member
          inline Return: x: 'T -> ReaderT<'R, ^Monad<'T>>
                           when (Control.Return or  ^Monad<'T>) :
                                  (static member Return:
                                      ^Monad<'T> * Control.Return
                                       -> ('T ->  ^Monad<'T>))
        
        static member
          inline Tell: w: 'Monoid -> ReaderT<'R, ^MonadWriter<'Monoid,unit>>
                         when  ^MonadWriter<'Monoid,unit> :
                                (static member Tell:
                                   'Monoid ->  ^MonadWriter<'Monoid,unit>)
        
        static member
          inline Throw: x: 'E -> ReaderT<'R, ^MonadError<'E,'T>>
                          when (Control.Throw or  ^MonadError<'E,'T>) :
                                 (static member Throw:
                                     ^MonadError<'E,'T> * 'E
                                      ->  ^MonadError<'E,'T>)
        
        static member
          inline TryFinally: computation: ReaderT<'R, ^Monad<'T>> *
                             f: (unit -> unit) -> ReaderT<'R, ^Monad<'T>>
                               when (Control.TryFinally or  ^Monad<'T>) :
                                      (static member TryFinally:
                                         ((unit ->  ^Monad<'T>) * (unit -> unit)) *
                                         Control.TryFinally * Control.TryFinally *
                                         Control.TryBlock.True ->  ^Monad<'T>)
        
        static member
          inline TryWith: source: ReaderT<'R, ^Monad<'T>> *
                          f: (exn -> ReaderT<'R, ^Monad<'T>>)
                            -> ReaderT<'R, ^Monad<'T>>
                            when (Control.TryWith or  ^Monad<'T>) :
                                   (static member TryWith:
                                      (unit ->  ^Monad<'T>) *
                                      ('a ->  ^Monad<'T>) * Control.TryWith *
                                      Control.TryBlock.True ->  ^Monad<'T>) and
                                 'a :> exn
        
        static member
          inline Using: resource: 'a * f: ('a -> ReaderT<'R, ^Monad<'T>>)
                          -> ReaderT<'R, ^Monad<'T>>
                          when 'a :> System.IDisposable and
                               (Control.Using or  ^Monad<'T>) :
                                 (static member Using:
                                    'a * ('a ->  ^Monad<'T>) * Control.Using
                                      ->  ^Monad<'T>)
        
        [<System.ComponentModel.EditorBrowsable
          (enum<System.ComponentModel.EditorBrowsableState> (1))>]
        static member
          inline Zip: x: ReaderT<'S, ^Monad<'T>> * y: ReaderT<'S, ^Monad<'U>>
                        -> ReaderT<'S, ^d>
                        when (Control.Map or  ^Monad<'T> or  ^a) :
                               (static member Map:
                                  ( ^Monad<'T> * ('b -> 'c -> 'b * 'c)) *
                                  Control.Map ->  ^a) and
                             (Control.Apply or  ^a or  ^Monad<'U> or  ^d) :
                               (static member ``<*>`` :
                                   ^a *  ^Monad<'U> *  ^d * Control.Apply ->  ^d)
        
        static member
          inline get_Ask: unit -> ReaderT<'R, ^Monad<'T>>
                            when (Control.Return or  ^Monad<'T>) :
                                   (static member Return:
                                       ^Monad<'T> * Control.Return
                                        -> ('R ->  ^Monad<'T>))
        
        static member
          inline get_Empty: unit -> ReaderT<'R, ^MonadPlus<'T>>
                              when (Control.Empty or  ^MonadPlus<'T>) :
                                     (static member Empty:
                                         ^MonadPlus<'T> * Control.Empty
                                          ->  ^MonadPlus<'T>)
        
        static member
          inline get_Get: unit -> ReaderT<'R, ^MonadState<'S, 'S>>
                            when  ^MonadState<'S, 'S> :
                                   (static member get_Get:
                                      ->  ^MonadState<'S, 'S>)
    
    /// Basic operations on Reader
    module ReaderT =
        
        val run: ReaderT<'R,'Monad<'T>> -> ('R -> 'Monad<'T>)
        
        val inline hoist:
          x: Reader<'R,'T> -> ReaderT<'R, ^Monad<'T>>
            when (Control.Return or  ^Monad<'T>) :
                   (static member Return:
                       ^Monad<'T> * Control.Return -> ('T ->  ^Monad<'T>))
        
        val inline map:
          f: ('T -> 'U) -> ReaderT<'R, ^Monad<'T>> -> ReaderT<'R, ^Monad<'U>>
            when (Control.Map or  ^Monad<'T> or  ^Monad<'U>) :
                   (static member Map:
                      ( ^Monad<'T> * ('T -> 'U)) * Control.Map ->  ^Monad<'U>)
        
        /// Combines two ReaderTs into one by applying a mapping function.
        val inline map2:
          f: ('T -> 'U -> 'V) -> ReaderT<'R, ^Monad<'T>>
          -> ReaderT<'R, ^Monad<'U>> -> ReaderT<'R, ^Monad<'V>>
            when (Control.Lift2 or  ^Monad<'T> or  ^Monad<'U> or  ^Monad<'V>) :
                   (static member Lift2:
                      ('T -> 'U -> 'V) * ( ^Monad<'T> *  ^Monad<'U>) *
                      Control.Lift2 ->  ^Monad<'V>)
        
        /// Combines three ReaderTs into one by applying a mapping function.
        val inline map3:
          f: ('T -> 'U -> 'V -> 'W) -> ReaderT<'R, ^Monad<'T>>
          -> ReaderT<'R, ^Monad<'U>> -> ReaderT<'R, ^Monad<'V>>
            -> ReaderT<'R, ^Monad<'W>>
            when (Control.Lift3 or  ^Monad<'T> or  ^Monad<'U> or  ^Monad<'V> or
                   ^Monad<'W>) :
                   (static member Lift3:
                      ('T -> 'U -> 'V -> 'W) *
                      ( ^Monad<'T> *  ^Monad<'U> *  ^Monad<'V>) * Control.Lift3
                        ->  ^Monad<'W>)
        
        val inline apply:
          ReaderT<'R, ^Monad<'T -> 'U>> -> ReaderT<'R, ^Monad<'T>>
            -> ReaderT<'R, ^Monad<'U>>
            when (Control.Apply or  ^Monad<'T -> 'U> or  ^Monad<'T> or
                   ^Monad<'U>) :
                   (static member ``<*>`` :
                       ^Monad<'T -> 'U> *  ^Monad<'T> *  ^Monad<'U> *
                      Control.Apply ->  ^Monad<'U>)
        
        /// Zips two ReaderTs into one.
        val inline zip:
          x: ReaderT<'S, ^Monad<'T>> -> y: ReaderT<'S, ^Monad<'U>>
            -> ReaderT<'S, ^Monad<'T * 'U>>
            when (Control.Map or  ^Monad<'T> or  ^a) :
                   (static member Map:
                      ( ^Monad<'T> * ('b -> 'c -> 'b * 'c)) * Control.Map ->  ^a) and
                 (Control.Apply or  ^a or  ^Monad<'U> or  ^Monad<'T * 'U>) :
                   (static member ``<*>`` :
                       ^a *  ^Monad<'U> *  ^Monad<'T * 'U> * Control.Apply
                        ->  ^Monad<'T * 'U>)
        
        val inline bind:
          f: ('T -> ReaderT<'R, ^Monad<'U>>) -> ReaderT<'R, ^Monad<'T>>
            -> ReaderT<'R, ^Monad<'U>>
            when (Control.Bind or  ^Monad<'T> or  ^Monad<'U>) :
                   (static member (>>=) :
                       ^Monad<'T> * ('T ->  ^Monad<'U>) ->  ^Monad<'U>)
        
        /// Embed a Monad<'T> into an ReaderT<'R, 'Monad<'T>>
        val lift: m: 'Monad<'T> -> ReaderT<'R,'Monad<'T>>

