namespace FSharpPlus.Data
    
    [<NoComparison>]
    type Free<'functor<'t>,'t> =
        | Pure of 't
        | Roll of obj
        
        static member
          inline (<*>) : f: Free< ^Functor<'T->'U>,('T -> 'U)> *
                         x: Free< ^Functor<'T>,'T> -> Free< ^Functor<'U>,'U>
                           when (Control.Map or  ^Functor<'T->'U> or  ^a) :
                                  (static member Map:
                                     ( ^Functor<'T->'U> *
                                      (('T -> 'U)
                                         -> Free< ^Functor<'T->'U>,('T -> 'U)>)) *
                                     Control.Map ->  ^a) and
                                (Control.Map or  ^a or  ^b) :
                                  (static member Map:
                                     ( ^a *
                                      (Free< ^Functor<'T->'U>,('T -> 'U)>
                                         -> Free< ^Functor<'U>,'U>)) *
                                     Control.Map ->  ^b) and
                                (Control.Map or  ^c or  ^b) :
                                  (static member Map:
                                     ( ^c *
                                      (Free< ^Functor<'T>,'T>
                                         -> Free< ^Functor<'U>,'U>)) *
                                     Control.Map ->  ^b) and
                                (Control.Map or  ^b or  ^Functor<'U>) :
                                  (static member Map:
                                     ( ^b * (Free< ^Functor<'U>,'U> -> 'U)) *
                                     Control.Map ->  ^Functor<'U>) and
                                (Control.Map or  ^Functor<'T> or  ^c) :
                                  (static member Map:
                                     ( ^Functor<'T> *
                                      ('T -> Free< ^Functor<'T>,'T>)) *
                                     Control.Map ->  ^c)
        
        static member
          inline (>>=) : x: Free< ^Functor<'T>,'T> *
                         f: ('T -> Free< ^Functor<'U>,'U>)
                           -> Free< ^Functor<'U>,'U>
                           when (Control.Map or  ^Functor<'T> or  ^a) :
                                  (static member Map:
                                     ( ^Functor<'T> *
                                      ('T -> Free< ^Functor<'T>,'T>)) *
                                     Control.Map ->  ^a) and
                                (Control.Map or  ^a or  ^b) :
                                  (static member Map:
                                     ( ^a *
                                      (Free< ^Functor<'T>,'T>
                                         -> Free< ^Functor<'U>,'U>)) *
                                     Control.Map ->  ^b) and
                                (Control.Map or  ^b or  ^Functor<'U>) :
                                  (static member Map:
                                     ( ^b * (Free< ^Functor<'U>,'U> -> 'U)) *
                                     Control.Map ->  ^Functor<'U>)
        
        static member
          Delay: x: (unit -> Free<'Functor<'T>,'T>) -> Free<'Functor<'T>,'T>
        
        [<System.ComponentModel.EditorBrowsable
          (enum<System.ComponentModel.EditorBrowsableState> (1))>]
        static member
          inline Lift2: f: ('T -> 'U -> 'V) * x: Free< ^Functor<'T>,'T> *
                        y: Free< ^Functor<'U>,'U> -> Free< ^Functor<'V>,'V>
                          when (Control.Map or  ^Functor<'T> or  ^a) :
                                 (static member Map:
                                    ( ^Functor<'T> *
                                     ('T -> Free< ^Functor<'T>,'T>)) *
                                    Control.Map ->  ^a) and
                               (Control.Map or  ^a or  ^b) :
                                 (static member Map:
                                    ( ^a *
                                     (Free< ^Functor<'T>,'T>
                                        -> Free< ^Functor<'V>,'V>)) *
                                    Control.Map ->  ^b) and
                               (Control.Map or  ^c or  ^b) :
                                 (static member Map:
                                    ( ^c *
                                     (Free< ^Functor<'U>,'U>
                                        -> Free< ^Functor<'V>,'V>)) *
                                    Control.Map ->  ^b) and
                               (Control.Map or  ^b or  ^Functor<'V>) :
                                 (static member Map:
                                    ( ^b * (Free< ^Functor<'V>,'V> -> 'V)) *
                                    Control.Map ->  ^Functor<'V>) and
                               (Control.Map or  ^Functor<'U> or  ^c) :
                                 (static member Map:
                                    ( ^Functor<'U> *
                                     ('U -> Free< ^Functor<'U>,'U>)) *
                                    Control.Map ->  ^c)
        
        [<System.ComponentModel.EditorBrowsable
          (enum<System.ComponentModel.EditorBrowsableState> (1))>]
        static member
          inline Lift3: f: ('T -> 'U -> 'V -> 'W) * x: Free< ^Functor<'T>,'T> *
                        y: Free< ^Functor<'U>,'U> * z: Free< ^Functor<'V>,'V>
                          -> Free< ^Functor<'W>,'W>
                          when (Control.Map or  ^Functor<'T> or  ^a) :
                                 (static member Map:
                                    ( ^Functor<'T> *
                                     ('T -> Free< ^Functor<'T>,'T>)) *
                                    Control.Map ->  ^a) and
                               (Control.Map or  ^a or  ^b) :
                                 (static member Map:
                                    ( ^a *
                                     (Free< ^Functor<'T>,'T>
                                      -> Free< ^Functor<'V>,'V>
                                        -> Free< ^Functor<'W>,'W>)) *
                                    Control.Map ->  ^b) and
                               (Control.Map or  ^b or  ^c) :
                                 (static member Map:
                                    ( ^b *
                                     (Free< ^Functor<'V>,'V>
                                        -> Free< ^Functor<'W>,'W>)) *
                                    Control.Map ->  ^c) and
                               (Control.Map or  ^Functor<'V> or  ^b) :
                                 (static member Map:
                                    ( ^Functor<'V> *
                                     ('V -> Free< ^Functor<'V>,'V>)) *
                                    Control.Map ->  ^b) and
                               (Control.Map or  ^b or  ^Functor<'W>) :
                                 (static member Map:
                                    ( ^b * (Free< ^Functor<'W>,'W> -> 'W)) *
                                    Control.Map ->  ^Functor<'W>) and
                               (Control.Map or  ^d or  ^c) :
                                 (static member Map:
                                    ( ^d *
                                     (Free< ^Functor<'U>,'U>
                                        -> Free< ^Functor<'W>,'W>)) *
                                    Control.Map ->  ^c) and
                               (Control.Map or  ^c or  ^Functor<'W>) :
                                 (static member Map:
                                    ( ^c * (Free< ^Functor<'W>,'W> -> 'W)) *
                                    Control.Map ->  ^Functor<'W>) and
                               (Control.Map or  ^Functor<'U> or  ^d) :
                                 (static member Map:
                                    ( ^Functor<'U> *
                                     ('U -> Free< ^Functor<'U>,'U>)) *
                                    Control.Map ->  ^d)
        
        [<System.ComponentModel.EditorBrowsable
          (enum<System.ComponentModel.EditorBrowsableState> (1))>]
        static member
          inline Map: x: Free< ^Functor<'T>,'T> * f: ('T -> 'U)
                        -> Free< ^Functor<'U>,'U>
                        when (Control.Map or  ^Functor<'T> or  ^a) :
                               (static member Map:
                                  ( ^Functor<'T> *
                                   ('T -> Free< ^Functor<'T>,'T>)) * Control.Map
                                    ->  ^a) and
                             (Control.Map or  ^a or  ^b) :
                               (static member Map:
                                  ( ^a *
                                   (Free< ^Functor<'T>,'T>
                                      -> Free< ^Functor<'U>,'U>)) * Control.Map
                                    ->  ^b) and
                             (Control.Map or  ^b or  ^Functor<'U>) :
                               (static member Map:
                                  ( ^b * (Free< ^Functor<'U>,'U> -> 'U)) *
                                  Control.Map ->  ^Functor<'U>)
        
        static member Return: x: 'a -> Free<'b,'a>
    
    module FreePrimitives =
        
        val inline Roll:
          f:  ^Functor<Free<'Functor<'T>,'T>> -> Free< ^Functor<'T>,'T>
            when (Control.Map or  ^Functor<Free<'Functor<'T>,'T>> or
                   ^Functor<'T>) :
                   (static member Map:
                      ( ^Functor<Free<'Functor<'T>,'T>> *
                       (Free< ^Functor<'T>,'T> -> 'T)) * Control.Map
                        ->  ^Functor<'T>)
        
        val (|Pure|Roll|) : x: Choice<'a,'b> -> Choice<'a,'b>
    
    /// Basic operations on Free Monads
    module Free =
        
        val inline run:
          f: Free< ^Functor<'T>,'T>
            -> Choice<'T, ^Functor<Free<'Functor<'T>,'T>>>
            when (Control.Map or  ^Functor<'T> or
                   ^Functor<Free<'Functor<'T>,'T>>) :
                   (static member Map:
                      ( ^Functor<'T> * ('T -> Free< ^Functor<'T>,'T>)) *
                      Control.Map ->  ^Functor<Free<'Functor<'T>,'T>>)
        
        val inline map:
          f: ('T -> 'U) -> x: Free< ^Functor<'T>,'T> -> Free< ^Functor<'U>,'U>
            when (Control.Map or  ^Functor<'T> or
                   ^Functor<Free<'Functor<'T>,'T>>) :
                   (static member Map:
                      ( ^Functor<'T> * ('T -> Free< ^Functor<'T>,'T>)) *
                      Control.Map ->  ^Functor<Free<'Functor<'T>,'T>>) and
                 (Control.Map or  ^Functor<Free<'Functor<'T>,'T>> or
                   ^Functor<Free<'Functor<'U>,'U>>) :
                   (static member Map:
                      ( ^Functor<Free<'Functor<'T>,'T>> *
                       (Free< ^Functor<'T>,'T> -> Free< ^Functor<'U>,'U>)) *
                      Control.Map ->  ^Functor<Free<'Functor<'U>,'U>>) and
                 (Control.Map or  ^Functor<Free<'Functor<'U>,'U>> or
                   ^Functor<'U>) :
                   (static member Map:
                      ( ^Functor<Free<'Functor<'U>,'U>> *
                       (Free< ^Functor<'U>,'U> -> 'U)) * Control.Map
                        ->  ^Functor<'U>)
        
        val inline bind:
          f: ('T -> Free< ^Functor<'U>,'U>) -> x: Free< ^Functor<'T>,'T>
            -> Free< ^Functor<'U>,'U>
            when (Control.Map or  ^Functor<Free<'Functor<'U>,'U>> or
                   ^Functor<'U>) :
                   (static member Map:
                      ( ^Functor<Free<'Functor<'U>,'U>> *
                       (Free< ^Functor<'U>,'U> -> 'U)) * Control.Map
                        ->  ^Functor<'U>) and
                 (Control.Map or  ^Functor<Free<'Functor<'T>,'T>> or
                   ^Functor<Free<'Functor<'U>,'U>>) :
                   (static member Map:
                      ( ^Functor<Free<'Functor<'T>,'T>> *
                       (Free< ^Functor<'T>,'T> -> Free< ^Functor<'U>,'U>)) *
                      Control.Map ->  ^Functor<Free<'Functor<'U>,'U>>) and
                 (Control.Map or  ^Functor<'T> or
                   ^Functor<Free<'Functor<'T>,'T>>) :
                   (static member Map:
                      ( ^Functor<'T> * ('T -> Free< ^Functor<'T>,'T>)) *
                      Control.Map ->  ^Functor<Free<'Functor<'T>,'T>>)
        
        val inline apply:
          f: Free< ^Functor<'T->'U>,('T -> 'U)> -> x: Free< ^Functor<'T>,'T>
            -> Free< ^Functor<'U>,'U>
            when (Control.Map or  ^Functor<'T->'U> or
                   ^Functor<Free<'Functor<'T->'U>,'T->'U>>) :
                   (static member Map:
                      ( ^Functor<'T->'U> *
                       (('T -> 'U) -> Free< ^Functor<'T->'U>,('T -> 'U)>)) *
                      Control.Map ->  ^Functor<Free<'Functor<'T->'U>,'T->'U>>) and
                 (Control.Map or  ^Functor<Free<'Functor<'T->'U>,'T->'U>> or
                   ^Functor<Free<'Functor<'U>,'U>>) :
                   (static member Map:
                      ( ^Functor<Free<'Functor<'T->'U>,'T->'U>> *
                       (Free< ^Functor<'T->'U>,('T -> 'U)>
                          -> Free< ^Functor<'U>,'U>)) * Control.Map
                        ->  ^Functor<Free<'Functor<'U>,'U>>) and
                 (Control.Map or  ^Functor<Free<'Functor<'T>,'T>> or
                   ^Functor<Free<'Functor<'U>,'U>>) :
                   (static member Map:
                      ( ^Functor<Free<'Functor<'T>,'T>> *
                       (Free< ^Functor<'T>,'T> -> Free< ^Functor<'U>,'U>)) *
                      Control.Map ->  ^Functor<Free<'Functor<'U>,'U>>) and
                 (Control.Map or  ^Functor<Free<'Functor<'U>,'U>> or
                   ^Functor<'U>) :
                   (static member Map:
                      ( ^Functor<Free<'Functor<'U>,'U>> *
                       (Free< ^Functor<'U>,'U> -> 'U)) * Control.Map
                        ->  ^Functor<'U>) and
                 (Control.Map or  ^Functor<'T> or
                   ^Functor<Free<'Functor<'T>,'T>>) :
                   (static member Map:
                      ( ^Functor<'T> * ('T -> Free< ^Functor<'T>,'T>)) *
                      Control.Map ->  ^Functor<Free<'Functor<'T>,'T>>)
        
        val inline map2:
          f: ('T -> 'U -> 'V) -> x: Free< ^Functor<'T>,'T>
          -> y: Free< ^Functor<'U>,'U> -> Free< ^Functor<'V>,'V>
            when (Control.Map or  ^Functor<'T> or
                   ^Functor<Free<'Functor<'T>,'T>>) :
                   (static member Map:
                      ( ^Functor<'T> * ('T -> Free< ^Functor<'T>,'T>)) *
                      Control.Map ->  ^Functor<Free<'Functor<'T>,'T>>) and
                 (Control.Map or  ^Functor<Free<'Functor<'T>,'T>> or
                   ^Functor<Free<'Functor<'V>,'V>>) :
                   (static member Map:
                      ( ^Functor<Free<'Functor<'T>,'T>> *
                       (Free< ^Functor<'T>,'T> -> Free< ^Functor<'V>,'V>)) *
                      Control.Map ->  ^Functor<Free<'Functor<'V>,'V>>) and
                 (Control.Map or  ^Functor<Free<'Functor<'U>,'U>> or
                   ^Functor<Free<'Functor<'V>,'V>>) :
                   (static member Map:
                      ( ^Functor<Free<'Functor<'U>,'U>> *
                       (Free< ^Functor<'U>,'U> -> Free< ^Functor<'V>,'V>)) *
                      Control.Map ->  ^Functor<Free<'Functor<'V>,'V>>) and
                 (Control.Map or  ^Functor<Free<'Functor<'V>,'V>> or
                   ^Functor<'V>) :
                   (static member Map:
                      ( ^Functor<Free<'Functor<'V>,'V>> *
                       (Free< ^Functor<'V>,'V> -> 'V)) * Control.Map
                        ->  ^Functor<'V>) and
                 (Control.Map or  ^Functor<'U> or
                   ^Functor<Free<'Functor<'U>,'U>>) :
                   (static member Map:
                      ( ^Functor<'U> * ('U -> Free< ^Functor<'U>,'U>)) *
                      Control.Map ->  ^Functor<Free<'Functor<'U>,'U>>)
        
        val inline map3:
          f: ('T -> 'U -> 'V -> 'W) -> x: Free< ^Functor<'T>,'T>
          -> y: Free< ^Functor<'U>,'U> -> z: Free< ^Functor<'V>,'V>
            -> Free< ^Functor<'W>,'W>
            when (Control.Map or  ^Functor<'T> or
                   ^Functor<Free<'Functor<'T>,'T>>) :
                   (static member Map:
                      ( ^Functor<'T> * ('T -> Free< ^Functor<'T>,'T>)) *
                      Control.Map ->  ^Functor<Free<'Functor<'T>,'T>>) and
                 (Control.Map or  ^Functor<Free<'Functor<'T>,'T>> or
                   ^Functor<Free<'Functor<'W>,'W>>) :
                   (static member Map:
                      ( ^Functor<Free<'Functor<'T>,'T>> *
                       (Free< ^Functor<'T>,'T> -> Free< ^Functor<'V>,'V>
                          -> Free< ^Functor<'W>,'W>)) * Control.Map
                        ->  ^Functor<Free<'Functor<'W>,'W>>) and
                 (Control.Map or  ^Functor<Free<'Functor<'W>,'W>> or
                   ^Functor<Free<'Functor<'V>,'V>>) :
                   (static member Map:
                      ( ^Functor<Free<'Functor<'W>,'W>> *
                       (Free< ^Functor<'V>,'V> -> Free< ^Functor<'W>,'W>)) *
                      Control.Map ->  ^Functor<Free<'Functor<'V>,'V>>) and
                 (Control.Map or  ^Functor<'V> or
                   ^Functor<Free<'Functor<'W>,'W>>) :
                   (static member Map:
                      ( ^Functor<'V> * ('V -> Free< ^Functor<'V>,'V>)) *
                      Control.Map ->  ^Functor<Free<'Functor<'W>,'W>>) and
                 (Control.Map or  ^Functor<Free<'Functor<'W>,'W>> or
                   ^Functor<'W>) :
                   (static member Map:
                      ( ^Functor<Free<'Functor<'W>,'W>> *
                       (Free< ^Functor<'W>,'W> -> 'W)) * Control.Map
                        ->  ^Functor<'W>) and
                 (Control.Map or  ^Functor<Free<'Functor<'U>,'U>> or
                   ^Functor<Free<'Functor<'V>,'V>>) :
                   (static member Map:
                      ( ^Functor<Free<'Functor<'U>,'U>> *
                       (Free< ^Functor<'U>,'U> -> Free< ^Functor<'W>,'W>)) *
                      Control.Map ->  ^Functor<Free<'Functor<'V>,'V>>) and
                 (Control.Map or  ^Functor<Free<'Functor<'V>,'V>> or
                   ^Functor<'W>) :
                   (static member Map:
                      ( ^Functor<Free<'Functor<'V>,'V>> *
                       (Free< ^Functor<'W>,'W> -> 'W)) * Control.Map
                        ->  ^Functor<'W>) and
                 (Control.Map or  ^Functor<'U> or
                   ^Functor<Free<'Functor<'U>,'U>>) :
                   (static member Map:
                      ( ^Functor<'U> * ('U -> Free< ^Functor<'U>,'U>)) *
                      Control.Map ->  ^Functor<Free<'Functor<'U>,'U>>)
        
        /// Folds the Free structure into a Monad
        val inline fold:
          f: ( ^Functor<'T> ->  ^Monad<'T>) -> x: Free< ^Functor<'U>,'U>
            ->  ^Monad<'U>
            when (Control.Map or  ^Functor<'U> or  ^Functor<'T>) :
                   (static member Map:
                      ( ^Functor<'U> * ('U -> Free< ^Functor<'U>,'U>)) *
                      Control.Map ->  ^Functor<'T>) and
                 (Control.Bind or  ^Monad<'T> or  ^Monad<'U>) :
                   (static member (>>=) :
                       ^Monad<'T> * (Free< ^Functor<'U>,'U> ->  ^Monad<'U>)
                        ->  ^Monad<'U>) and
                 (Control.Return or  ^Monad<'U>) :
                   (static member Return:
                       ^Monad<'U> * Control.Return -> ('U ->  ^Monad<'U>))
        
        /// Tear down a Free monad using iteration.
        val inline iterM:
          f: ( ^Functor<'Monad<'T>> ->  ^Monad<'T>) -> x: Free< ^Functor<'T>,'T>
            ->  ^Monad<'T>
            when (Control.Map or  ^Functor<Free<'Functor<'T>,'T>> or
                   ^Functor<'Monad<'T>>) :
                   (static member Map:
                      ( ^Functor<Free<'Functor<'T>,'T>> *
                       (Free< ^Functor<'T>,'T> ->  ^Monad<'T>)) * Control.Map
                        ->  ^Functor<'Monad<'T>>) and
                 (Control.Map or  ^Functor<'T> or
                   ^Functor<Free<'Functor<'T>,'T>>) :
                   (static member Map:
                      ( ^Functor<'T> * ('T -> Free< ^Functor<'T>,'T>)) *
                      Control.Map ->  ^Functor<Free<'Functor<'T>,'T>>) and
                 (Control.Return or  ^Monad<'T>) :
                   (static member Return:
                       ^Monad<'T> * Control.Return -> ('T ->  ^Monad<'T>))
        
        /// Lift any Functor into a Free structure
        val inline liftF:
          x:  ^Functor<'T> -> Free< ^Functor<'T>,'T>
            when (Control.Map or  ^Functor<Free<'Functor<'T>,'T>> or
                   ^Functor<'T>) :
                   (static member Map:
                      ( ^Functor<Free<'Functor<'T>,'T>> *
                       (Free< ^Functor<'T>,'T> -> 'T)) * Control.Map
                        ->  ^Functor<'T>) and
                 (Control.Map or  ^Functor<'T> or
                   ^Functor<Free<'Functor<'T>,'T>>) :
                   (static member Map:
                      ( ^Functor<'T> * ('T -> Free< ^Functor<'T>,'T>)) *
                      Control.Map ->  ^Functor<Free<'Functor<'T>,'T>>)

