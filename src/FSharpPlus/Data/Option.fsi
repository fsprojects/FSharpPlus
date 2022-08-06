namespace FSharpPlus.Data
    
    /// Additional operations on Option
    module Option =
        
        val inline traverse:
          f: ('a ->  ^b) -> _arg1: 'a option ->  ^c
            when (Control.Map or  ^b or  ^c) :
                   (static member Map:
                      ( ^b * ('e -> 'e option)) * Control.Map ->  ^c) and
                 (Control.Return or  ^c) :
                   (static member Return:
                       ^c * Control.Return -> ('d option ->  ^c))
    
    /// Monad Transformer for Option<'T>
    [<Struct>]
    type OptionT<'monad<option<'t>>> =
        | OptionT of 'monad<option<'t>>
        
        static member
          inline (<*>) : f: OptionT< ^Monad<option<('T -> 'U)>> *
                         x: OptionT< ^Monad<option<'T>>
                           -> OptionT< ^Monad<option<'U>>
                           when (Control.Map or  ^Monad<option<('T -> 'U)> or
                                  ^a) :
                                  (static member Map:
                                     ( ^Monad<option<('T -> 'U)> *
                                      (('b -> 'c) option -> 'b option
                                         -> 'c option)) * Control.Map ->  ^a) and
                                (Control.Apply or  ^a or  ^Monad<option<'T> or
                                  ^Monad<option<'U>) :
                                  (static member ``<*>`` :
                                      ^a *  ^Monad<option<'T> *
                                      ^Monad<option<'U> * Control.Apply
                                       ->  ^Monad<option<'U>)
        
        static member
          inline (<|>) : OptionT< ^a> * OptionT< ^MonadPlus<option<'T>>
                           -> OptionT< ^MonadPlus<option<'T>>
                           when (Control.Bind or  ^a or  ^MonadPlus<option<'T>) :
                                  (static member (>>=) :
                                      ^a * ('b option ->  ^MonadPlus<option<'T>)
                                       ->  ^MonadPlus<option<'T>) and
                                (Control.Return or  ^MonadPlus<option<'T>) :
                                  (static member Return:
                                      ^MonadPlus<option<'T> * Control.Return
                                       -> ('b option ->  ^MonadPlus<option<'T>))
        
        static member
          inline (>>=) : x: OptionT< ^Monad<option<'T>> *
                         f: ('T -> OptionT< ^Monad<option<'U>>)
                           -> OptionT< ^Monad<option<'U>>
                           when (Control.Bind or  ^Monad<option<'T> or
                                  ^Monad<option<'U>) :
                                  (static member (>>=) :
                                      ^Monad<option<'T> *
                                     ('T option ->  ^Monad<option<'U>)
                                       ->  ^Monad<option<'U>) and
                                (Control.Return or  ^Monad<option<'U>) :
                                  (static member Return:
                                      ^Monad<option<'U> * Control.Return
                                       -> ('a option ->  ^Monad<option<'U>))
        
        static member
          inline CallCC: f: (('T -> OptionT<'MonadCont<'R,option<'U>>>)
                               -> OptionT< ^MonadCont<'R,option<'T>>>)
                           -> OptionT< ^MonadCont<'R,option<'T>>>
                           when  ^MonadCont<'R,option<'T>> :
                                  (static member CallCC:
                                     (('T option -> 'MonadCont<'R,option<'U>>)
                                        ->  ^MonadCont<'R,option<'T>>)
                                       ->  ^MonadCont<'R,option<'T>>)
        
        static member
          inline Catch: m: OptionT< ^MonadError<'E1,'T>> *
                        h: ('E1 -> OptionT< ^MonadError<'E2,'T>>)
                          -> OptionT< ^MonadError<'E2,'T>>
                          when (Control.Catch or  ^MonadError<'E1,'T> or
                                 ^MonadError<'E2,'T>) :
                                 (static member Catch:
                                     ^MonadError<'E1,'T> *
                                    ('E1 ->  ^MonadError<'E2,'T>)
                                      ->  ^MonadError<'E2,'T>)
        
        static member
          inline Delay: body: (unit -> OptionT< ^Monad<option<'T>>>)
                          -> OptionT< ^Monad<option<'T>>>
                          when (Control.Delay or  ^Monad<option<'T>>) :
                                 (static member Delay:
                                    Control.Delay *
                                    (unit ->  ^Monad<option<'T>>) *
                                    Control.Delay ->  ^Monad<option<'T>>)
        
        [<System.ComponentModel.EditorBrowsable
          (enum<System.ComponentModel.EditorBrowsableState> (1))>]
        static member
          inline Lift: x:  ^Monad<'T> -> OptionT< ^Monad<option<'T>>>
                         when (Control.Map or  ^Monad<'T> or  ^Monad<option<'T>>) :
                                (static member Map:
                                   ( ^Monad<'T> * ('b -> 'b option)) *
                                   Control.Map ->  ^Monad<option<'T>>) and
                              (Control.Bind or  ^Monad<'T> or
                                ^Monad<option<'T>>) :
                                (static member (>>=) :
                                    ^Monad<'T> * ('a ->  ^Monad<option<'T>>)
                                     ->  ^Monad<option<'T>>) and
                              (Control.Return or  ^Monad<option<'T>>) :
                                (static member Return:
                                    ^Monad<option<'T>> * Control.Return
                                     -> ('a option ->  ^Monad<option<'T>>))
        
        [<System.ComponentModel.EditorBrowsable
          (enum<System.ComponentModel.EditorBrowsableState> (1))>]
        static member
          inline Lift2: f: ('T -> 'U -> 'V) * x: OptionT< ^Monad<option<'T>> *
                        y: OptionT< ^Monad<option<'U>>
                          -> OptionT< ^Monad<option<'V>>
                          when (Control.Lift2 or  ^Monad<option<'T> or
                                 ^Monad<option<'U> or  ^Monad<option<'V>) :
                                 (static member Lift2:
                                    ('T option -> 'U option -> 'V option) *
                                    ( ^Monad<option<'T> *  ^Monad<option<'U>) *
                                    Control.Lift2 ->  ^Monad<option<'V>)
        
        [<System.ComponentModel.EditorBrowsable
          (enum<System.ComponentModel.EditorBrowsableState> (1))>]
        static member
          inline Lift3: f: ('T -> 'U -> 'V -> 'W) *
                        x: OptionT< ^Monad<option<'T>> *
                        y: OptionT< ^Monad<option<'U>> *
                        z: OptionT< ^Monad<option<'W>>
                          -> OptionT< ^Monad<option<'W>>
                          when (Control.Lift3 or  ^Monad<option<'T> or
                                 ^Monad<option<'U> or  ^Monad<option<'W>) :
                                 (static member Lift3:
                                    ('T option -> 'U option -> 'V option
                                       -> 'W option) *
                                    ( ^Monad<option<'T> *  ^Monad<option<'U> *
                                      ^Monad<option<'W>) * Control.Lift3
                                      ->  ^Monad<option<'W>)
        
        static member
          inline LiftAsync: x: Async<'T> -> OptionT< ^MonadAsync<'T>>
                              when (Control.Return or  ^MonadAsync<'T>) :
                                     (static member Return:
                                         ^MonadAsync<'T> * Control.Return
                                          -> ('a option ->  ^MonadAsync<'T>)) and
                                   (Control.Bind or  ^b or  ^MonadAsync<'T>) :
                                     (static member (>>=) :
                                         ^b * ('a ->  ^MonadAsync<'T>)
                                          ->  ^MonadAsync<'T>) and
                                   (Control.Map or  ^b or  ^MonadAsync<'T>) :
                                     (static member Map:
                                        ( ^b * ('c -> 'c option)) * Control.Map
                                          ->  ^MonadAsync<'T>) and
                                   (Control.LiftAsync or  ^b) :
                                     (static member LiftAsync:
                                         ^b -> (Async<'T> ->  ^b))
        
        static member
          inline Listen: m: OptionT<'a>
                           -> OptionT< ^'MonadWriter<'Monoid, option<'T>>>
                           when (Control.Bind or  ^b or
                                  ^'MonadWriter<'Monoid, option<'T>>) :
                                  (static member (>>=) :
                                      ^b *
                                     ('c option * 'd
                                        ->  ^'MonadWriter<'Monoid, option<'T>>)
                                       ->  ^'MonadWriter<'Monoid, option<'T>>) and
                                (Control.Return or
                                  ^'MonadWriter<'Monoid, option<'T>>) :
                                  (static member Return:
                                      ^'MonadWriter<'Monoid, option<'T>> *
                                     Control.Return
                                       -> (('c * 'd) option
                                             ->  ^'MonadWriter<'Monoid, option<'T>>)) and
                                 ^b: (static member Listen: 'a ->  ^b)
        
        static member
          inline Local: OptionT< ^MonadReader<'R2,'T>> * f: ('R1 -> 'R2)
                          -> OptionT< ^a>
                          when  ^a:
                                 (static member Local:
                                     ^MonadReader<'R2,'T> * ('R1 -> 'R2) ->  ^a)
        
        [<System.ComponentModel.EditorBrowsable
          (enum<System.ComponentModel.EditorBrowsableState> (1))>]
        static member
          inline Map: x: OptionT< ^Monad<option<'T>> * f: ('T -> 'U)
                        -> OptionT< ^Monad<option<'U>>
                        when (Control.Map or  ^Monad<option<'T> or
                               ^Monad<option<'U>) :
                               (static member Map:
                                  ( ^Monad<option<'T> * ('T option -> 'U option)) *
                                  Control.Map ->  ^Monad<option<'U>)
        
        static member
          inline Pass: m: OptionT< ^a>
                         -> OptionT< ^MonadWriter<'Monoid, option<'T>>>
                         when (Control.Bind or  ^a or
                                ^MonadWriter<'Monoid, option<'T>>) :
                                (static member (>>=) :
                                    ^a *
                                   ('b option
                                      ->  ^MonadWriter<'Monoid, option<'T>>)
                                     ->  ^MonadWriter<'Monoid, option<'T>>) and
                              (Control.Map or  ^c or
                                ^MonadWriter<'Monoid, option<'T>>) :
                                (static member Map:
                                   ( ^c * ('e -> 'e option)) * Control.Map
                                     ->  ^MonadWriter<'Monoid, option<'T>>) and
                              (Control.Return or
                                ^MonadWriter<'Monoid, option<'T>>) :
                                (static member Return:
                                    ^MonadWriter<'Monoid, option<'T>> *
                                   Control.Return
                                     -> ('f option
                                           ->  ^MonadWriter<'Monoid, option<'T>>)) and
                               ^c: (static member Pass:  ^d ->  ^c) and
                              (Control.Return or  ^d) :
                                (static member Return:
                                    ^d * Control.Return -> ('b ->  ^d))
        
        static member
          inline Put: x: 'S -> OptionT< ^MonadState<unit,'S>>
                        when (Control.Return or  ^MonadState<unit,'S>) :
                               (static member Return:
                                   ^MonadState<unit,'S> * Control.Return
                                    -> ('a option ->  ^MonadState<unit,'S>)) and
                             (Control.Bind or  ^b or  ^MonadState<unit,'S>) :
                               (static member (>>=) :
                                   ^b * ('a ->  ^MonadState<unit,'S>)
                                    ->  ^MonadState<unit,'S>) and
                             (Control.Map or  ^b or  ^MonadState<unit,'S>) :
                               (static member Map:
                                  ( ^b * ('c -> 'c option)) * Control.Map
                                    ->  ^MonadState<unit,'S>) and
                              ^b: (static member Put: 'S ->  ^b)
        
        static member
          inline Return: x: 'T -> OptionT< ^Monad<option<'T>>
                           when (Control.Return or  ^Monad<option<'T>) :
                                  (static member Return:
                                      ^Monad<option<'T> * Control.Return
                                       -> ('T option ->  ^Monad<option<'T>))
        
        static member
          inline Tell: w: 'Monoid -> OptionT< ^MonadWriter<'Monoid, unit>>
                         when (Control.Return or  ^MonadWriter<'Monoid, unit>) :
                                (static member Return:
                                    ^MonadWriter<'Monoid, unit> * Control.Return
                                     -> ('a option
                                           ->  ^MonadWriter<'Monoid, unit>)) and
                              (Control.Bind or  ^b or
                                ^MonadWriter<'Monoid, unit>) :
                                (static member (>>=) :
                                    ^b * ('a ->  ^MonadWriter<'Monoid, unit>)
                                     ->  ^MonadWriter<'Monoid, unit>) and
                              (Control.Map or  ^b or
                                ^MonadWriter<'Monoid, unit>) :
                                (static member Map:
                                   ( ^b * ('c -> 'c option)) * Control.Map
                                     ->  ^MonadWriter<'Monoid, unit>) and
                               ^b: (static member Tell: 'Monoid ->  ^b)
        
        static member
          inline Throw: x: 'E -> OptionT< ^a>
                          when (Control.Return or  ^a) :
                                 (static member Return:
                                     ^a * Control.Return -> ('b option ->  ^a)) and
                               (Control.Bind or  ^c or  ^a) :
                                 (static member (>>=) :  ^c * ('b ->  ^a) ->  ^a) and
                               (Control.Map or  ^c or  ^a) :
                                 (static member Map:
                                    ( ^c * ('d -> 'd option)) * Control.Map
                                      ->  ^a) and
                               (Control.Throw or  ^c) :
                                 (static member Throw:  ^c * 'E ->  ^c)
        
        static member
          inline TryFinally: computation: OptionT< ^Monad<option<'T>>> *
                             f: (unit -> unit) -> OptionT< ^Monad<option<'T>>>
                               when (Control.TryFinally or  ^Monad<option<'T>>) :
                                      (static member TryFinally:
                                         ((unit ->  ^Monad<option<'T>>) *
                                          (unit -> unit)) * Control.TryFinally *
                                         Control.TryFinally *
                                         Control.TryBlock.False
                                           ->  ^Monad<option<'T>>)
        
        static member
          inline TryWith: source: OptionT< ^Monad<option<'T>>> *
                          f: (exn -> OptionT< ^Monad<option<'T>>>)
                            -> OptionT< ^Monad<option<'T>>>
                            when (Control.TryWith or  ^Monad<option<'T>>) :
                                   (static member TryWith:
                                      (unit ->  ^Monad<option<'T>>) *
                                      ('a ->  ^Monad<option<'T>>) *
                                      Control.TryWith * Control.TryBlock.False
                                        ->  ^Monad<option<'T>>) and 'a :> exn
        
        static member
          inline Using: resource: 'a * f: ('a -> OptionT< ^Monad<option<'T>>>)
                          -> OptionT< ^Monad<option<'T>>>
                          when 'a :> System.IDisposable and
                               (Control.Using or  ^Monad<option<'T>>) :
                                 (static member Using:
                                    'a * ('a ->  ^Monad<option<'T>>) *
                                    Control.Using ->  ^Monad<option<'T>>)
        
        static member
          inline get_Ask: unit -> OptionT< ^MonadReader<'R,option<'R>>>
                            when (Control.Return or  ^MonadReader<'R,option<'R>>) :
                                   (static member Return:
                                       ^MonadReader<'R,option<'R>> *
                                      Control.Return
                                        -> ('a option
                                              ->  ^MonadReader<'R,option<'R>>)) and
                                 (Control.Bind or  ^b or
                                   ^MonadReader<'R,option<'R>>) :
                                   (static member (>>=) :
                                       ^b * ('a ->  ^MonadReader<'R,option<'R>>)
                                        ->  ^MonadReader<'R,option<'R>>) and
                                 (Control.Map or  ^b or
                                   ^MonadReader<'R,option<'R>>) :
                                   (static member Map:
                                      ( ^b * ('c -> 'c option)) * Control.Map
                                        ->  ^MonadReader<'R,option<'R>>) and
                                  ^b: (static member get_Ask: ->  ^b)
        
        static member
          inline get_Empty: unit -> OptionT< ^MonadPlus<option<'T>>
                              when (Control.Return or  ^MonadPlus<option<'T>) :
                                     (static member Return:
                                         ^MonadPlus<option<'T> * Control.Return
                                          -> ('a option
                                                ->  ^MonadPlus<option<'T>))
        
        static member
          inline get_Get: unit -> OptionT< ^MonadState<'S,'S>>
                            when (Control.Return or  ^MonadState<'S,'S>) :
                                   (static member Return:
                                       ^MonadState<'S,'S> * Control.Return
                                        -> ('a option ->  ^MonadState<'S,'S>)) and
                                 (Control.Bind or  ^b or  ^MonadState<'S,'S>) :
                                   (static member (>>=) :
                                       ^b * ('a ->  ^MonadState<'S,'S>)
                                        ->  ^MonadState<'S,'S>) and
                                 (Control.Map or  ^b or  ^MonadState<'S,'S>) :
                                   (static member Map:
                                      ( ^b * ('c -> 'c option)) * Control.Map
                                        ->  ^MonadState<'S,'S>) and
                                  ^b: (static member get_Get: ->  ^b)
    
    /// Basic operations on OptionT
    module OptionT =
        
        val run: OptionT<'Monad<option<'T>>> -> 'Monad<option<'T>>
        
        /// Embed a Monad<'T> into an OptionT<'Monad<option<'T>>>
        val inline lift:
          x:  ^Monad<'T> -> OptionT< ^Monad<option<'T>>>
            when (Control.Bind or  ^Monad<'T> or  ^Monad<option<'T>>) :
                   (static member (>>=) :
                       ^Monad<'T> * ('a ->  ^Monad<option<'T>>)
                        ->  ^Monad<option<'T>>) and
                 (Control.Map or  ^Monad<'T> or  ^Monad<option<'T>>) :
                   (static member Map:
                      ( ^Monad<'T> * ('b -> 'b option)) * Control.Map
                        ->  ^Monad<option<'T>>) and
                 (Control.Return or  ^Monad<option<'T>>) :
                   (static member Return:
                       ^Monad<option<'T>> * Control.Return
                        -> ('a option ->  ^Monad<option<'T>>))
        
        /// Transform an option<'T,'Error> to an OptionT<'Monad<option<'T,'Error>>>
        val inline hoist:
          x: 'T option -> OptionT< ^Monad<option<'T>>>
            when (Control.Return or  ^Monad<option<'T>>) :
                   (static member Return:
                       ^Monad<option<'T>> * Control.Return
                        -> ('T option ->  ^Monad<option<'T>>))
        
        val inline bind:
          f: ('T -> OptionT< ^Monad<option<'U>>) -> OptionT< ^Monad<option<'T>>
            -> OptionT< ^Monad<option<'U>>
            when (Control.Bind or  ^Monad<option<'T> or  ^Monad<option<'U>) :
                   (static member (>>=) :
                       ^Monad<option<'T> * ('T option ->  ^Monad<option<'U>)
                        ->  ^Monad<option<'U>) and
                 (Control.Return or  ^Monad<option<'U>) :
                   (static member Return:
                       ^Monad<option<'U> * Control.Return
                        -> ('a option ->  ^Monad<option<'U>))
        
        val inline apply:
          OptionT< ^Monad<option<('T -> 'U)>> -> OptionT< ^Monad<option<'T>>
            -> OptionT< ^Monad<option<'U>>
            when (Control.Map or  ^Monad<option<('T -> 'U)> or  ^a) :
                   (static member Map:
                      ( ^Monad<option<('T -> 'U)> *
                       (('b -> 'c) option -> 'b option -> 'c option)) *
                      Control.Map ->  ^a) and
                 (Control.Apply or  ^a or  ^Monad<option<'T> or
                   ^Monad<option<'U>) :
                   (static member ``<*>`` :
                       ^a *  ^Monad<option<'T> *  ^Monad<option<'U> *
                      Control.Apply ->  ^Monad<option<'U>)
        
        val inline map:
          f: ('T -> 'U) -> OptionT< ^Monad<option<'T>>
            -> OptionT< ^Monad<option<'U>>
            when (Control.Map or  ^Monad<option<'T> or  ^Monad<option<'U>) :
                   (static member Map:
                      ( ^Monad<option<'T> * ('T option -> 'U option)) *
                      Control.Map ->  ^Monad<option<'U>)
        
        val inline map2:
          f: ('T -> 'U -> 'V) -> OptionT< ^Monad<option<'T>>>
          -> OptionT< ^Monad<option<'U>>> -> OptionT< ^Monad<option<'V>>>
            when (Control.Lift2 or  ^Monad<option<'T>> or  ^Monad<option<'U>> or
                   ^Monad<option<'V>>) :
                   (static member Lift2:
                      ('T option -> 'U option -> 'V option) *
                      ( ^Monad<option<'T>> *  ^Monad<option<'U>>) *
                      Control.Lift2 ->  ^Monad<option<'V>>)
        
        val inline map3:
          f: ('T -> 'U -> 'V -> 'W) -> OptionT< ^Monad<option<'T>>>
          -> OptionT< ^Monad<option<'U>>> -> OptionT< ^Monad<option<'V>>>
            -> OptionT< ^Monad<option<'W>>>
            when (Control.Lift3 or  ^Monad<option<'T>> or  ^Monad<option<'U>> or
                   ^Monad<option<'V>> or  ^Monad<option<'W>>) :
                   (static member Lift3:
                      ('T option -> 'U option -> 'V option -> 'W option) *
                      ( ^Monad<option<'T>> *  ^Monad<option<'U>> *
                        ^Monad<option<'V>>) * Control.Lift3
                        ->  ^Monad<option<'W>>)

