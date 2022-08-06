namespace FSharpPlus.Data
    
    /// Additional operations on Seq
    module Seq =
        
        val inline sequence:
          ms: seq< ^Applicative<'T>> ->  ^Applicative<seq<'T>>
            when (Control.Apply or  ^a or  ^Applicative<'T> or  ^b) :
                   (static member ``<*>`` :
                       ^a *  ^Applicative<'T> *  ^b * Control.Apply ->  ^b) and
                 (Control.IsLeftZero or  ^Applicative<'T>) :
                   (static member IsLeftZero:
                       ^Applicative<'T> ref * Control.IsLeftZero -> bool) and
                 (Control.Map or  ^b or  ^a) :
                   (static member Map:
                      ( ^b * ('d list -> 'd -> 'd list)) * Control.Map ->  ^a) and
                 (Control.Return or  ^b) :
                   (static member Return:
                       ^b * Control.Return -> ('c list ->  ^b)) and
                 (Control.Map or  ^b or  ^Applicative<seq<'T>>) :
                   (static member Map:
                      ( ^b * ('e list -> seq<'e>)) * Control.Map
                        ->  ^Applicative<seq<'T>>) and
                 (Control.Sequence or seq< ^Applicative<'T>> or
                   ^Applicative<seq<'T>>) :
                   (static member Sequence:
                      seq< ^Applicative<'T>> *  ^Applicative<seq<'T>> *
                      Control.Sequence ->  ^Applicative<seq<'T>>)
        
        val inline traverse:
          f: ('T ->  ^Applicative<'U>) -> xs: seq<'T> ->  ^Applicative<seq<'U>>
            when (Control.Apply or  ^a or  ^Applicative<'U> or  ^b) :
                   (static member ``<*>`` :
                       ^a *  ^Applicative<'U> *  ^b * Control.Apply ->  ^b) and
                 (Control.IsLeftZero or  ^Applicative<'U>) :
                   (static member IsLeftZero:
                       ^Applicative<'U> ref * Control.IsLeftZero -> bool) and
                 (Control.Map or  ^b or  ^a) :
                   (static member Map:
                      ( ^b * ('d list -> 'd -> 'd list)) * Control.Map ->  ^a) and
                 (Control.Return or  ^b) :
                   (static member Return:
                       ^b * Control.Return -> ('c list ->  ^b)) and
                 (Control.Map or  ^b or  ^Applicative<seq<'U>>) :
                   (static member Map:
                      ( ^b * ('e list -> seq<'e>)) * Control.Map
                        ->  ^Applicative<seq<'U>>) and
                 (Control.Traverse or seq<'T> or  ^Applicative<seq<'U>>) :
                   (static member Traverse:
                      seq<'T> * ('T ->  ^Applicative<'U>) *
                       ^Applicative<seq<'U>> * Control.Traverse
                        ->  ^Applicative<seq<'U>>)
        
        val inline replicateM:
          count: int -> initial:  ^Applicative<'T> ->  ^e
            when (Control.IsLeftZero or  ^Applicative<'T>) :
                   (static member IsLeftZero:
                       ^Applicative<'T> ref * Control.IsLeftZero -> bool) and
                 (Control.Apply or  ^a or  ^Applicative<'T> or  ^b) :
                   (static member ``<*>`` :
                       ^a *  ^Applicative<'T> *  ^b * Control.Apply ->  ^b) and
                 (Control.Map or  ^b or  ^a) :
                   (static member Map:
                      ( ^b * ('d list -> 'd -> 'd list)) * Control.Map ->  ^a) and
                 (Control.Return or  ^b) :
                   (static member Return:
                       ^b * Control.Return -> ('c list ->  ^b)) and
                 (Control.Map or  ^b or  ^e) :
                   (static member Map:
                      ( ^b * ('f list -> seq<'f>)) * Control.Map ->  ^e) and
                 (Control.Sequence or seq< ^Applicative<'T>> or  ^e) :
                   (static member Sequence:
                      seq< ^Applicative<'T>> *  ^e * Control.Sequence ->  ^e)
    
    /// Monad Transformer for seq<'T>
    [<Struct>]
    type SeqT<'monad<seq<'t>>> =
        | SeqT of 'monad<seq<'t>>
        
        static member
          inline (<*>) : f: SeqT< ^Monad<seq<('T -> 'U)>> *
                         x: SeqT< ^Monad<seq<'T>> -> SeqT< ^Monad<seq<'U>>
                           when (Control.Map or  ^Monad<seq<('T -> 'U)> or  ^a) :
                                  (static member Map:
                                     ( ^Monad<seq<('T -> 'U)> *
                                      (seq<('b -> 'c)> -> seq<'b> -> seq<'c>)) *
                                     Control.Map ->  ^a) and
                                (Control.Apply or  ^a or  ^Monad<seq<'T> or
                                  ^Monad<seq<'U>) :
                                  (static member ``<*>`` :
                                      ^a *  ^Monad<seq<'T> *  ^Monad<seq<'U> *
                                     Control.Apply ->  ^Monad<seq<'U>)
        
        static member
          inline (<|>) : SeqT< ^a> * SeqT< ^c> -> SeqT< ^MonadPlus<seq<'T>>
                           when (Control.Bind or  ^a or  ^MonadPlus<seq<'T>) :
                                  (static member (>>=) :
                                      ^a * (seq<'b> ->  ^MonadPlus<seq<'T>)
                                       ->  ^MonadPlus<seq<'T>) and
                                (Control.Bind or  ^c or  ^MonadPlus<seq<'T>) :
                                  (static member (>>=) :
                                      ^c * (seq<'b> ->  ^MonadPlus<seq<'T>)
                                       ->  ^MonadPlus<seq<'T>) and
                                (Control.Return or  ^MonadPlus<seq<'T>) :
                                  (static member Return:
                                      ^MonadPlus<seq<'T> * Control.Return
                                       -> (seq<'b> ->  ^MonadPlus<seq<'T>))
        
        static member
          inline (>>=) : x: SeqT< ^Monad<seq<'T>> *
                         f: ('T -> SeqT< ^Monad<seq<'U>>) -> SeqT< ^c>
                           when (Control.Bind or  ^Monad<seq<'T> or  ^a) :
                                  (static member (>>=) :
                                      ^Monad<seq<'T> * (seq<'T> ->  ^a) ->  ^a) and
                                (Control.Return or  ^a) :
                                  (static member Return:
                                      ^a * Control.Return -> (seq<'b> ->  ^a)) and
                                (Control.Bind or  ^a) :
                                  (static member (>>=) :
                                      ^a * (seq<'b> ->  ^a) ->  ^a) and
                                (Control.Bind or  ^Monad<seq<'U> or  ^a) :
                                  (static member (>>=) :
                                      ^Monad<seq<'U> * ('b ->  ^a) ->  ^a) and
                                (Control.Bind or  ^a or  ^c) :
                                  (static member (>>=) :
                                      ^a * (seq<seq<'d>> ->  ^c) ->  ^c) and
                                (Control.Return or  ^c) :
                                  (static member Return:
                                      ^c * Control.Return -> (seq<'d> ->  ^c))
        
        static member
          inline CallCC: f: (('T -> SeqT<'MonadCont<'R,seq<'U>>>)
                               -> SeqT< ^MonadCont<'R, seq<'T>>>)
                           -> SeqT< ^MonadCont<'R, seq<'T>>>
                           when  ^MonadCont<'R, seq<'T>> :
                                  (static member CallCC:
                                     ((seq<'T> -> 'MonadCont<'R,seq<'U>>)
                                        ->  ^MonadCont<'R, seq<'T>>)
                                       ->  ^MonadCont<'R, seq<'T>>)
        
        static member
          inline Catch: m: SeqT< ^MonadError<'E1,'T>> *
                        h: ('E1 -> SeqT< ^MonadError<'E2,'T>>)
                          -> SeqT< ^MonadError<'E2,'T>>
                          when (Control.Catch or  ^MonadError<'E1,'T> or
                                 ^MonadError<'E2,'T>) :
                                 (static member Catch:
                                     ^MonadError<'E1,'T> *
                                    ('E1 ->  ^MonadError<'E2,'T>)
                                      ->  ^MonadError<'E2,'T>)
        
        static member
          inline Delay: body: (unit -> SeqT< ^Monad<seq<'T>>>)
                          -> SeqT< ^Monad<seq<'T>>>
                          when (Control.Delay or  ^Monad<seq<'T>>) :
                                 (static member Delay:
                                    Control.Delay * (unit ->  ^Monad<seq<'T>>) *
                                    Control.Delay ->  ^Monad<seq<'T>>)
        
        [<System.ComponentModel.EditorBrowsable
          (enum<System.ComponentModel.EditorBrowsableState> (1))>]
        static member
          inline Lift: x:  ^Monad<'T> -> SeqT< ^Monad<seq<'T>>>
                         when (Control.Map or  ^Monad<'T> or  ^Monad<seq<'T>>) :
                                (static member Map:
                                   ( ^Monad<'T> * ('b -> seq<'b>)) * Control.Map
                                     ->  ^Monad<seq<'T>>) and
                              (Control.Bind or  ^Monad<'T> or  ^Monad<seq<'T>>) :
                                (static member (>>=) :
                                    ^Monad<'T> * ('a ->  ^Monad<seq<'T>>)
                                     ->  ^Monad<seq<'T>>) and
                              (Control.Return or  ^Monad<seq<'T>>) :
                                (static member Return:
                                    ^Monad<seq<'T>> * Control.Return
                                     -> (seq<'a> ->  ^Monad<seq<'T>>))
        
        [<System.ComponentModel.EditorBrowsable
          (enum<System.ComponentModel.EditorBrowsableState> (1))>]
        static member
          inline Lift2: f: ('T -> 'U -> 'V) * x: SeqT< ^Monad<seq<'T>> *
                        y: SeqT< ^Monad<seq<'U>> -> SeqT< ^Monad<seq<'V>>
                          when (Control.Lift2 or  ^Monad<seq<'T> or
                                 ^Monad<seq<'U> or  ^Monad<seq<'V>) :
                                 (static member Lift2:
                                    ('a -> 'b -> seq<'V>) *
                                    ( ^Monad<seq<'T> *  ^Monad<seq<'U>) *
                                    Control.Lift2 ->  ^Monad<seq<'V>) and
                               'a :> seq<'T> and 'b :> seq<'U>
        
        [<System.ComponentModel.EditorBrowsable
          (enum<System.ComponentModel.EditorBrowsableState> (1))>]
        static member
          inline Lift3: f: ('T -> 'U -> 'V -> 'W) * x: SeqT< ^Monad<seq<'T>> *
                        y: SeqT< ^Monad<seq<'U>> * z: SeqT< ^Monad<seq<'V>>
                          -> SeqT< ^Monad<seq<'W>>
                          when (Control.Lift3 or  ^Monad<seq<'T> or
                                 ^Monad<seq<'U> or  ^Monad<seq<'V> or
                                 ^Monad<seq<'W>) :
                                 (static member Lift3:
                                    ('a -> 'b -> 'c -> seq<'W>) *
                                    ( ^Monad<seq<'T> *  ^Monad<seq<'U> *
                                      ^Monad<seq<'V>) * Control.Lift3
                                      ->  ^Monad<seq<'W>) and 'a :> seq<'V> and
                               'b :> seq<'T> and 'c :> seq<'U>
        
        static member
          inline LiftAsync: x: Async<'T> -> SeqT< ^MonadAsync<'T>>
                              when (Control.Return or  ^MonadAsync<'T>) :
                                     (static member Return:
                                         ^MonadAsync<'T> * Control.Return
                                          -> (seq<'a> ->  ^MonadAsync<'T>)) and
                                   (Control.Bind or  ^b or  ^MonadAsync<'T>) :
                                     (static member (>>=) :
                                         ^b * ('a ->  ^MonadAsync<'T>)
                                          ->  ^MonadAsync<'T>) and
                                   (Control.Map or  ^b or  ^MonadAsync<'T>) :
                                     (static member Map:
                                        ( ^b * ('c -> seq<'c>)) * Control.Map
                                          ->  ^MonadAsync<'T>) and
                                   (Control.LiftAsync or  ^b) :
                                     (static member LiftAsync:
                                         ^b -> (Async<'T> ->  ^b))
        
        static member
          inline Local: SeqT< ^MonadReader<'R2,'T>> * f: ('R1 -> 'R2)
                          -> SeqT< ^a>
                          when  ^a:
                                 (static member Local:
                                     ^MonadReader<'R2,'T> * ('R1 -> 'R2) ->  ^a)
        
        [<System.ComponentModel.EditorBrowsable
          (enum<System.ComponentModel.EditorBrowsableState> (1))>]
        static member
          inline Map: x: SeqT< ^Monad<seq<'T>> * f: ('T -> 'U)
                        -> SeqT< ^Monad<seq<'U>>
                        when (Control.Map or  ^Monad<seq<'T> or  ^Monad<seq<'U>) :
                               (static member Map:
                                  ( ^Monad<seq<'T> * (seq<'T> -> seq<'U>)) *
                                  Control.Map ->  ^Monad<seq<'U>)
        
        static member
          inline Put: x: 'S -> SeqT< ^MonadState<unit,'S>>
                        when (Control.Return or  ^MonadState<unit,'S>) :
                               (static member Return:
                                   ^MonadState<unit,'S> * Control.Return
                                    -> (seq<'a> ->  ^MonadState<unit,'S>)) and
                             (Control.Bind or  ^b or  ^MonadState<unit,'S>) :
                               (static member (>>=) :
                                   ^b * ('a ->  ^MonadState<unit,'S>)
                                    ->  ^MonadState<unit,'S>) and
                             (Control.Map or  ^b or  ^MonadState<unit,'S>) :
                               (static member Map:
                                  ( ^b * ('c -> seq<'c>)) * Control.Map
                                    ->  ^MonadState<unit,'S>) and
                              ^b: (static member Put: 'S ->  ^b)
        
        static member
          inline Return: x: 'T -> SeqT< ^Monad<seq<'T>>
                           when (Control.Return or  ^Monad<seq<'T>) :
                                  (static member Return:
                                      ^Monad<seq<'T> * Control.Return
                                       -> (seq<'T> ->  ^Monad<seq<'T>))
        
        static member
          inline Throw: x: 'E -> SeqT< ^a>
                          when (Control.Return or  ^a) :
                                 (static member Return:
                                     ^a * Control.Return -> (seq<'b> ->  ^a)) and
                               (Control.Bind or  ^c or  ^a) :
                                 (static member (>>=) :  ^c * ('b ->  ^a) ->  ^a) and
                               (Control.Map or  ^c or  ^a) :
                                 (static member Map:
                                    ( ^c * ('d -> seq<'d>)) * Control.Map ->  ^a) and
                               (Control.Throw or  ^c) :
                                 (static member Throw:  ^c * 'E ->  ^c)
        
        static member
          inline TryFinally: computation: SeqT< ^Monad<seq<'T>>> *
                             f: (unit -> unit) -> SeqT< ^Monad<seq<'T>>>
                               when (Control.TryFinally or  ^Monad<seq<'T>>) :
                                      (static member TryFinally:
                                         ((unit ->  ^Monad<seq<'T>>) *
                                          (unit -> unit)) * Control.TryFinally *
                                         Control.TryFinally *
                                         Control.TryBlock.False
                                           ->  ^Monad<seq<'T>>)
        
        static member
          inline TryWith: source: SeqT< ^Monad<seq<'T>>> *
                          f: (exn -> SeqT< ^Monad<seq<'T>>>)
                            -> SeqT< ^Monad<seq<'T>>>
                            when (Control.TryWith or  ^Monad<seq<'T>>) :
                                   (static member TryWith:
                                      (unit ->  ^Monad<seq<'T>>) *
                                      ('a ->  ^Monad<seq<'T>>) * Control.TryWith *
                                      Control.TryBlock.False ->  ^Monad<seq<'T>>) and
                                 'a :> exn
        
        static member
          inline Using: resource: 'a * f: ('a -> SeqT< ^Monad<seq<'T>>>)
                          -> SeqT< ^Monad<seq<'T>>>
                          when 'a :> System.IDisposable and
                               (Control.Using or  ^Monad<seq<'T>>) :
                                 (static member Using:
                                    'a * ('a ->  ^Monad<seq<'T>>) *
                                    Control.Using ->  ^Monad<seq<'T>>)
        
        static member
          inline get_Ask: unit -> SeqT< ^MonadReader<'R,seq<'R>>>
                            when (Control.Return or  ^MonadReader<'R,seq<'R>>) :
                                   (static member Return:
                                       ^MonadReader<'R,seq<'R>> * Control.Return
                                        -> (seq<'a> ->  ^MonadReader<'R,seq<'R>>)) and
                                 (Control.Bind or  ^b or
                                   ^MonadReader<'R,seq<'R>>) :
                                   (static member (>>=) :
                                       ^b * ('a ->  ^MonadReader<'R,seq<'R>>)
                                        ->  ^MonadReader<'R,seq<'R>>) and
                                 (Control.Map or  ^b or
                                   ^MonadReader<'R,seq<'R>>) :
                                   (static member Map:
                                      ( ^b * ('c -> seq<'c>)) * Control.Map
                                        ->  ^MonadReader<'R,seq<'R>>) and
                                  ^b: (static member get_Ask: ->  ^b)
        
        static member
          inline get_Empty: unit -> SeqT< ^MonadPlus<seq<'T>>
                              when (Control.Return or  ^MonadPlus<seq<'T>) :
                                     (static member Return:
                                         ^MonadPlus<seq<'T> * Control.Return
                                          -> (seq<'a> ->  ^MonadPlus<seq<'T>))
        
        static member
          inline get_Get: unit -> SeqT< ^MonadState<'S,'S>>
                            when (Control.Return or  ^MonadState<'S,'S>) :
                                   (static member Return:
                                       ^MonadState<'S,'S> * Control.Return
                                        -> (seq<'a> ->  ^MonadState<'S,'S>)) and
                                 (Control.Bind or  ^b or  ^MonadState<'S,'S>) :
                                   (static member (>>=) :
                                       ^b * ('a ->  ^MonadState<'S,'S>)
                                        ->  ^MonadState<'S,'S>) and
                                 (Control.Map or  ^b or  ^MonadState<'S,'S>) :
                                   (static member Map:
                                      ( ^b * ('c -> seq<'c>)) * Control.Map
                                        ->  ^MonadState<'S,'S>) and
                                  ^b: (static member get_Get: ->  ^b)
    
    /// Basic operations on SeqT
    module SeqT =
        
        val run: SeqT<'a> -> 'a
        
        /// Embed a Monad<'T> into a SeqT<'Monad<seq<'T>>>
        val inline lift:
          x:  ^Monad<'T> -> SeqT< ^Monad<seq<'T>>>
            when (Control.Bind or  ^Monad<'T> or  ^Monad<seq<'T>>) :
                   (static member (>>=) :
                       ^Monad<'T> * ('a ->  ^Monad<seq<'T>>) ->  ^Monad<seq<'T>>) and
                 (Control.Map or  ^Monad<'T> or  ^Monad<seq<'T>>) :
                   (static member Map:
                      ( ^Monad<'T> * ('b -> seq<'b>)) * Control.Map
                        ->  ^Monad<seq<'T>>) and
                 (Control.Return or  ^Monad<seq<'T>>) :
                   (static member Return:
                       ^Monad<seq<'T>> * Control.Return
                        -> (seq<'a> ->  ^Monad<seq<'T>>))
        
        val inline internal sequence:
          ms: seq< ^a> ->  ^M
            when (Control.Bind or  ^a or  ^M) :
                   (static member (>>=) :  ^a * ('a0 ->  ^M) ->  ^M) and
                 (Control.Return or  ^M) :
                   (static member Return:
                       ^M * Control.Return -> (seq<'a0> ->  ^M)) and
                 (Control.Bind or  ^M) :
                   (static member (>>=) :  ^M * (seq<'a0> ->  ^M) ->  ^M)
        
        val inline internal mapM:
          f: ('a ->  ^b) -> as': seq<'a> ->  ^c
            when (Control.Bind or  ^b or  ^c) :
                   (static member (>>=) :  ^b * ('d ->  ^c) ->  ^c) and
                 (Control.Return or  ^c) :
                   (static member Return:
                       ^c * Control.Return -> (seq<'d> ->  ^c)) and
                 (Control.Bind or  ^c) :
                   (static member (>>=) :  ^c * (seq<'d> ->  ^c) ->  ^c)
        
        val inline bind:
          f: ('T -> SeqT< ^Monad<seq<'U>>) -> SeqT< ^Monad<seq<'T>> -> SeqT< ^c>
            when (Control.Bind or  ^Monad<seq<'U> or  ^a) :
                   (static member (>>=) :  ^Monad<seq<'U> * ('b ->  ^a) ->  ^a) and
                 (Control.Return or  ^a) :
                   (static member Return:
                       ^a * Control.Return -> (seq<'b> ->  ^a)) and
                 (Control.Bind or  ^a) :
                   (static member (>>=) :  ^a * (seq<'b> ->  ^a) ->  ^a) and
                 (Control.Bind or  ^Monad<seq<'T> or  ^a) :
                   (static member (>>=) :
                       ^Monad<seq<'T> * (seq<'T> ->  ^a) ->  ^a) and
                 (Control.Bind or  ^a or  ^c) :
                   (static member (>>=) :  ^a * (seq<seq<'d>> ->  ^c) ->  ^c) and
                 (Control.Return or  ^c) :
                   (static member Return:
                       ^c * Control.Return -> (seq<'d> ->  ^c))
        
        val inline apply:
          SeqT< ^Monad<seq<('T -> 'U)>> -> SeqT< ^Monad<seq<'T>>
            -> SeqT< ^Monad<seq<'U>>
            when (Control.Map or  ^Monad<seq<('T -> 'U)> or  ^a) :
                   (static member Map:
                      ( ^Monad<seq<('T -> 'U)> *
                       (seq<('b -> 'c)> -> seq<'b> -> seq<'c>)) * Control.Map
                        ->  ^a) and
                 (Control.Apply or  ^a or  ^Monad<seq<'T> or  ^Monad<seq<'U>) :
                   (static member ``<*>`` :
                       ^a *  ^Monad<seq<'T> *  ^Monad<seq<'U> * Control.Apply
                        ->  ^Monad<seq<'U>)
        
        val inline lift2:
          f: ('T -> 'U -> 'V) -> SeqT< ^Monad<seq<'T>> -> SeqT< ^Monad<seq<'U>>
            -> SeqT< ^Monad<seq<'V>>
            when (Control.Lift2 or  ^Monad<seq<'T> or  ^Monad<seq<'U> or
                   ^Monad<seq<'V>) :
                   (static member Lift2:
                      ('a -> 'b -> seq<'V>) *
                      ( ^Monad<seq<'T> *  ^Monad<seq<'U>) * Control.Lift2
                        ->  ^Monad<seq<'V>) and 'a :> seq<'T> and 'b :> seq<'U>
        
        val inline lift3:
          f: ('T -> 'U -> 'V -> 'W) -> SeqT< ^Monad<seq<'T>>
          -> SeqT< ^Monad<seq<'U>> -> SeqT< ^Monad<seq<'V>>
            -> SeqT< ^Monad<seq<'W>>
            when (Control.Lift3 or  ^Monad<seq<'T> or  ^Monad<seq<'U> or
                   ^Monad<seq<'V> or  ^Monad<seq<'W>) :
                   (static member Lift3:
                      ('a -> 'b -> 'c -> seq<'W>) *
                      ( ^Monad<seq<'T> *  ^Monad<seq<'U> *  ^Monad<seq<'V>) *
                      Control.Lift3 ->  ^Monad<seq<'W>) and 'a :> seq<'V> and
                 'b :> seq<'T> and 'c :> seq<'U>
        
        val inline map:
          f: ('T -> 'U) -> SeqT< ^Monad<seq<'T>> -> SeqT< ^Monad<seq<'U>>
            when (Control.Map or  ^Monad<seq<'T> or  ^Monad<seq<'U>) :
                   (static member Map:
                      ( ^Monad<seq<'T> * (seq<'T> -> seq<'U>)) * Control.Map
                        ->  ^Monad<seq<'U>)

