namespace FSharpPlus.Data
    
    /// Additional operations on List
    module List =
        
        val inline sequence:
          ms:  ^Applicative<'T> list ->  ^Applicative<list<'T>>
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
                 (Control.Map or  ^b or  ^Applicative<list<'T>>) :
                   (static member Map:
                      ( ^b * ('e list -> 'e list)) * Control.Map
                        ->  ^Applicative<list<'T>>) and
                 (Control.Sequence or  ^Applicative<'T> list or
                   ^Applicative<list<'T>>) :
                   (static member Sequence:
                       ^Applicative<'T> list *  ^Applicative<list<'T>> *
                      Control.Sequence ->  ^Applicative<list<'T>>)
        
        val inline traverse:
          f: ('T ->  ^Applicative<'U>) -> xs: 'T list ->  ^Applicative<list<'U>>
            when (Control.Map or  ^Applicative<'U> or  ^a) :
                   (static member Map:
                      ( ^Applicative<'U> * ('b -> 'b list -> 'b list)) *
                      Control.Map ->  ^a) and
                 (Control.Apply or  ^a or  ^Applicative<list<'U>>) :
                   (static member ``<*>`` :
                       ^a *  ^Applicative<list<'U>> *  ^Applicative<list<'U>> *
                      Control.Apply ->  ^Applicative<list<'U>>) and
                 (Control.Traverse or 'T list or  ^Applicative<list<'U>>) :
                   (static member Traverse:
                      'T list * ('T ->  ^Applicative<'U>) *
                       ^Applicative<list<'U>> * Control.Traverse
                        ->  ^Applicative<list<'U>>) and
                 (Control.Return or  ^Applicative<list<'U>>) :
                   (static member Return:
                       ^Applicative<list<'U>> * Control.Return
                        -> ('c list ->  ^Applicative<list<'U>>))
        
        val inline foldM:
          f: ('T -> 'U ->  ^Monad<'T>) -> a: 'T -> bx: 'U list ->  ^Monad<'T>
            when (Control.Bind or  ^Monad<'T>) :
                   (static member (>>=) :
                       ^Monad<'T> * ('T ->  ^Monad<'T>) ->  ^Monad<'T>) and
                 (Control.Return or  ^Monad<'T>) :
                   (static member Return:
                       ^Monad<'T> * Control.Return -> ('T ->  ^Monad<'T>))
        
        val inline filterM:
          f: ('T ->  ^Monad<Bool>) -> xs: 'T list ->  ^Monad<list<'T>>
            when (Control.Bind or  ^Monad<Bool> or  ^Monad<list<'T>>) :
                   (static member (>>=) :
                       ^Monad<Bool> * (bool ->  ^Monad<list<'T>>)
                        ->  ^Monad<list<'T>>) and
                 (Control.Return or  ^Monad<list<'T>>) :
                   (static member Return:
                       ^Monad<list<'T>> * Control.Return
                        -> ('T list ->  ^Monad<list<'T>>)) and
                 (Control.Bind or  ^Monad<list<'T>>) :
                   (static member (>>=) :
                       ^Monad<list<'T>> * ('T list ->  ^Monad<list<'T>>)
                        ->  ^Monad<list<'T>>)
        
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
                      ( ^b * ('f list -> 'f list)) * Control.Map ->  ^e) and
                 (Control.Sequence or  ^Applicative<'T> list or  ^e) :
                   (static member Sequence:
                       ^Applicative<'T> list *  ^e * Control.Sequence ->  ^e)
    
    /// Monad Transformer for list<'T>
    [<Struct>]
    type ListT<'monad<list<'t>>> =
        | ListT of 'monad<list<'t>>
        
        static member
          inline (<*>) : f: ListT< ^Monad<list<('T -> 'U)>> *
                         x: ListT< ^Monad<list<'T>> -> ListT< ^Monad<list<'U>>
                           when (Control.Map or  ^Monad<list<('T -> 'U)> or  ^a) :
                                  (static member Map:
                                     ( ^Monad<list<('T -> 'U)> *
                                      (('b -> 'c) list -> 'b list -> 'c list)) *
                                     Control.Map ->  ^a) and
                                (Control.Apply or  ^a or  ^Monad<list<'T> or
                                  ^Monad<list<'U>) :
                                  (static member ``<*>`` :
                                      ^a *  ^Monad<list<'T> *  ^Monad<list<'U> *
                                     Control.Apply ->  ^Monad<list<'U>)
        
        static member
          inline (<|>) : ListT< ^a> * ListT< ^c> -> ListT< ^MonadPlus<list<'T>>
                           when (Control.Bind or  ^a or  ^MonadPlus<list<'T>) :
                                  (static member (>>=) :
                                      ^a * ('b list ->  ^MonadPlus<list<'T>)
                                       ->  ^MonadPlus<list<'T>) and
                                (Control.Bind or  ^c or  ^MonadPlus<list<'T>) :
                                  (static member (>>=) :
                                      ^c * ('b list ->  ^MonadPlus<list<'T>)
                                       ->  ^MonadPlus<list<'T>) and
                                (Control.Return or  ^MonadPlus<list<'T>) :
                                  (static member Return:
                                      ^MonadPlus<list<'T> * Control.Return
                                       -> ('b list ->  ^MonadPlus<list<'T>))
        
        static member
          inline (>>=) : x: ListT< ^Monad<list<'T>> *
                         f: ('T -> ListT< ^Monad<list<'U>>) -> ListT< ^c>
                           when (Control.Bind or  ^Monad<list<'T> or  ^a) :
                                  (static member (>>=) :
                                      ^Monad<list<'T> * ('T list ->  ^a) ->  ^a) and
                                (Control.Return or  ^a) :
                                  (static member Return:
                                      ^a * Control.Return -> ('b list ->  ^a)) and
                                (Control.Bind or  ^a) :
                                  (static member (>>=) :
                                      ^a * ('b list ->  ^a) ->  ^a) and
                                (Control.Bind or  ^Monad<list<'U> or  ^a) :
                                  (static member (>>=) :
                                      ^Monad<list<'U> * ('b ->  ^a) ->  ^a) and
                                (Control.Bind or  ^a or  ^c) :
                                  (static member (>>=) :
                                      ^a * ('d list list ->  ^c) ->  ^c) and
                                (Control.Return or  ^c) :
                                  (static member Return:
                                      ^c * Control.Return -> ('d list ->  ^c))
        
        static member
          inline CallCC: f: (('T -> ListT<'MonadCont<'R,list<'U>>>)
                               -> ListT< ^MonadCont<'R, list<'T>>>)
                           -> ListT< ^MonadCont<'R, list<'T>>>
                           when  ^MonadCont<'R, list<'T>> :
                                  (static member CallCC:
                                     (('T list -> 'MonadCont<'R,list<'U>>)
                                        ->  ^MonadCont<'R, list<'T>>)
                                       ->  ^MonadCont<'R, list<'T>>)
        
        static member
          inline Catch: m: ListT< ^MonadError<'E1,'T>> *
                        h: ('E1 -> ListT< ^MonadError<'E2,'T>>)
                          -> ListT< ^MonadError<'E2,'T>>
                          when (Control.Catch or  ^MonadError<'E1,'T> or
                                 ^MonadError<'E2,'T>) :
                                 (static member Catch:
                                     ^MonadError<'E1,'T> *
                                    ('E1 ->  ^MonadError<'E2,'T>)
                                      ->  ^MonadError<'E2,'T>)
        
        static member
          inline Delay: body: (unit -> ListT< ^Monad<list<'T>>>)
                          -> ListT< ^Monad<list<'T>>>
                          when (Control.Delay or  ^Monad<list<'T>>) :
                                 (static member Delay:
                                    Control.Delay * (unit ->  ^Monad<list<'T>>) *
                                    Control.Delay ->  ^Monad<list<'T>>)
        
        [<System.ComponentModel.EditorBrowsable
          (enum<System.ComponentModel.EditorBrowsableState> (1))>]
        static member
          inline Lift: x:  ^Monad<'T> -> ListT< ^Monad<list<'T>>>
                         when (Control.Map or  ^Monad<'T> or  ^Monad<list<'T>>) :
                                (static member Map:
                                   ( ^Monad<'T> * ('b -> 'b list)) * Control.Map
                                     ->  ^Monad<list<'T>>) and
                              (Control.Bind or  ^Monad<'T> or  ^Monad<list<'T>>) :
                                (static member (>>=) :
                                    ^Monad<'T> * ('a ->  ^Monad<list<'T>>)
                                     ->  ^Monad<list<'T>>) and
                              (Control.Return or  ^Monad<list<'T>>) :
                                (static member Return:
                                    ^Monad<list<'T>> * Control.Return
                                     -> ('a list ->  ^Monad<list<'T>>))
        
        [<System.ComponentModel.EditorBrowsable
          (enum<System.ComponentModel.EditorBrowsableState> (1))>]
        static member
          inline Lift2: f: ('T -> 'U -> 'V) * x: ListT< ^Monad<list<'T>> *
                        y: ListT< ^Monad<list<'U>> -> ListT< ^Monad<list<'V>>
                          when (Control.Lift2 or  ^Monad<list<'T> or
                                 ^Monad<list<'U> or  ^Monad<list<'V>) :
                                 (static member Lift2:
                                    ('T list -> 'U list -> 'V list) *
                                    ( ^Monad<list<'T> *  ^Monad<list<'U>) *
                                    Control.Lift2 ->  ^Monad<list<'V>)
        
        [<System.ComponentModel.EditorBrowsable
          (enum<System.ComponentModel.EditorBrowsableState> (1))>]
        static member
          inline Lift3: f: ('T -> 'U -> 'V -> 'W) * x: ListT< ^Monad<list<'T>> *
                        y: ListT< ^Monad<list<'U>> * z: ListT< ^Monad<list<'V>>
                          -> ListT< ^Monad<list<'W>>
                          when (Control.Lift3 or  ^Monad<list<'T> or
                                 ^Monad<list<'U> or  ^Monad<list<'V> or
                                 ^Monad<list<'W>) :
                                 (static member Lift3:
                                    ('V list -> 'T list -> 'U list -> 'W list) *
                                    ( ^Monad<list<'T> *  ^Monad<list<'U> *
                                      ^Monad<list<'V>) * Control.Lift3
                                      ->  ^Monad<list<'W>)
        
        static member
          inline LiftAsync: x: Async<'T> -> ListT< ^MonadAsync<'T>>
                              when (Control.Return or  ^MonadAsync<'T>) :
                                     (static member Return:
                                         ^MonadAsync<'T> * Control.Return
                                          -> ('a list ->  ^MonadAsync<'T>)) and
                                   (Control.Bind or  ^b or  ^MonadAsync<'T>) :
                                     (static member (>>=) :
                                         ^b * ('a ->  ^MonadAsync<'T>)
                                          ->  ^MonadAsync<'T>) and
                                   (Control.Map or  ^b or  ^MonadAsync<'T>) :
                                     (static member Map:
                                        ( ^b * ('c -> 'c list)) * Control.Map
                                          ->  ^MonadAsync<'T>) and
                                   (Control.LiftAsync or  ^b) :
                                     (static member LiftAsync:
                                         ^b -> (Async<'T> ->  ^b))
        
        static member
          inline Local: ListT< ^MonadReader<'R2,'T>> * f: ('R1 -> 'R2)
                          -> ListT< ^a>
                          when  ^a:
                                 (static member Local:
                                     ^MonadReader<'R2,'T> * ('R1 -> 'R2) ->  ^a)
        
        [<System.ComponentModel.EditorBrowsable
          (enum<System.ComponentModel.EditorBrowsableState> (1))>]
        static member
          inline Map: x: ListT< ^Monad<list<'T>> * f: ('T -> 'U)
                        -> ListT< ^Monad<list<'U>>
                        when (Control.Map or  ^Monad<list<'T> or
                               ^Monad<list<'U>) :
                               (static member Map:
                                  ( ^Monad<list<'T> * ('T list -> 'U list)) *
                                  Control.Map ->  ^Monad<list<'U>)
        
        static member
          inline Put: x: 'S -> ListT< ^MonadState<unit,'S>>
                        when (Control.Return or  ^MonadState<unit,'S>) :
                               (static member Return:
                                   ^MonadState<unit,'S> * Control.Return
                                    -> ('a list ->  ^MonadState<unit,'S>)) and
                             (Control.Bind or  ^b or  ^MonadState<unit,'S>) :
                               (static member (>>=) :
                                   ^b * ('a ->  ^MonadState<unit,'S>)
                                    ->  ^MonadState<unit,'S>) and
                             (Control.Map or  ^b or  ^MonadState<unit,'S>) :
                               (static member Map:
                                  ( ^b * ('c -> 'c list)) * Control.Map
                                    ->  ^MonadState<unit,'S>) and
                              ^b: (static member Put: 'S ->  ^b)
        
        static member
          inline Return: x: 'T -> ListT< ^Monad<list<'T>>
                           when (Control.Return or  ^Monad<list<'T>) :
                                  (static member Return:
                                      ^Monad<list<'T> * Control.Return
                                       -> ('T list ->  ^Monad<list<'T>))
        
        static member
          inline Throw: x: 'E -> ListT< ^a>
                          when (Control.Return or  ^a) :
                                 (static member Return:
                                     ^a * Control.Return -> ('b list ->  ^a)) and
                               (Control.Bind or  ^c or  ^a) :
                                 (static member (>>=) :  ^c * ('b ->  ^a) ->  ^a) and
                               (Control.Map or  ^c or  ^a) :
                                 (static member Map:
                                    ( ^c * ('d -> 'd list)) * Control.Map ->  ^a) and
                               (Control.Throw or  ^c) :
                                 (static member Throw:  ^c * 'E ->  ^c)
        
        static member
          inline TryFinally: computation: ListT< ^Monad<list<'T>>> *
                             f: (unit -> unit) -> ListT< ^Monad<list<'T>>>
                               when (Control.TryFinally or  ^Monad<list<'T>>) :
                                      (static member TryFinally:
                                         ((unit ->  ^Monad<list<'T>>) *
                                          (unit -> unit)) * Control.TryFinally *
                                         Control.TryFinally *
                                         Control.TryBlock.False
                                           ->  ^Monad<list<'T>>)
        
        static member
          inline TryWith: source: ListT< ^Monad<list<'T>>> *
                          f: (exn -> ListT< ^Monad<list<'T>>>)
                            -> ListT< ^Monad<list<'T>>>
                            when (Control.TryWith or  ^Monad<list<'T>>) :
                                   (static member TryWith:
                                      (unit ->  ^Monad<list<'T>>) *
                                      ('a ->  ^Monad<list<'T>>) *
                                      Control.TryWith * Control.TryBlock.False
                                        ->  ^Monad<list<'T>>) and 'a :> exn
        
        static member
          inline Using: resource: 'a * f: ('a -> ListT< ^Monad<list<'T>>>)
                          -> ListT< ^Monad<list<'T>>>
                          when 'a :> System.IDisposable and
                               (Control.Using or  ^Monad<list<'T>>) :
                                 (static member Using:
                                    'a * ('a ->  ^Monad<list<'T>>) *
                                    Control.Using ->  ^Monad<list<'T>>)
        
        static member
          inline get_Ask: unit -> ListT< ^MonadReader<'R,  list<'R>>>
                            when (Control.Return or  ^MonadReader<'R,  list<'R>>) :
                                   (static member Return:
                                       ^MonadReader<'R,  list<'R>> *
                                      Control.Return
                                        -> ('a list
                                              ->  ^MonadReader<'R,  list<'R>>)) and
                                 (Control.Bind or  ^b or
                                   ^MonadReader<'R,  list<'R>>) :
                                   (static member (>>=) :
                                       ^b * ('a ->  ^MonadReader<'R,  list<'R>>)
                                        ->  ^MonadReader<'R,  list<'R>>) and
                                 (Control.Map or  ^b or
                                   ^MonadReader<'R,  list<'R>>) :
                                   (static member Map:
                                      ( ^b * ('c -> 'c list)) * Control.Map
                                        ->  ^MonadReader<'R,  list<'R>>) and
                                  ^b: (static member get_Ask: ->  ^b)
        
        static member
          inline get_Empty: unit -> ListT< ^MonadPlus<list<'T>>
                              when (Control.Return or  ^MonadPlus<list<'T>) :
                                     (static member Return:
                                         ^MonadPlus<list<'T> * Control.Return
                                          -> ('a list ->  ^MonadPlus<list<'T>))
        
        static member
          inline get_Get: unit -> ListT< ^MonadState<'S,'S>>
                            when (Control.Return or  ^MonadState<'S,'S>) :
                                   (static member Return:
                                       ^MonadState<'S,'S> * Control.Return
                                        -> ('a list ->  ^MonadState<'S,'S>)) and
                                 (Control.Bind or  ^b or  ^MonadState<'S,'S>) :
                                   (static member (>>=) :
                                       ^b * ('a ->  ^MonadState<'S,'S>)
                                        ->  ^MonadState<'S,'S>) and
                                 (Control.Map or  ^b or  ^MonadState<'S,'S>) :
                                   (static member Map:
                                      ( ^b * ('c -> 'c list)) * Control.Map
                                        ->  ^MonadState<'S,'S>) and
                                  ^b: (static member get_Get: ->  ^b)
    
    /// Basic operations on ListT
    module ListT =
        
        val run: ListT<'Monad<list<'T>>> -> 'Monad<list<'T>>
        
        /// Embed a Monad<'T> into a ListT<'Monad<list<'T>>>
        val inline lift:
          x:  ^Monad<'T> -> ListT< ^Monad<list<'T>>>
            when (Control.Bind or  ^Monad<'T> or  ^Monad<list<'T>>) :
                   (static member (>>=) :
                       ^Monad<'T> * ('a ->  ^Monad<list<'T>>)
                        ->  ^Monad<list<'T>>) and
                 (Control.Map or  ^Monad<'T> or  ^Monad<list<'T>>) :
                   (static member Map:
                      ( ^Monad<'T> * ('b -> 'b list)) * Control.Map
                        ->  ^Monad<list<'T>>) and
                 (Control.Return or  ^Monad<list<'T>>) :
                   (static member Return:
                       ^Monad<list<'T>> * Control.Return
                        -> ('a list ->  ^Monad<list<'T>>))
        
        val inline internal sequence:
          ms:  ^a list ->  ^M
            when (Control.Bind or  ^a or  ^M) :
                   (static member (>>=) :  ^a * ('a0 ->  ^M) ->  ^M) and
                 (Control.Return or  ^M) :
                   (static member Return:
                       ^M * Control.Return -> ('a0 list ->  ^M)) and
                 (Control.Bind or  ^M) :
                   (static member (>>=) :  ^M * ('a0 list ->  ^M) ->  ^M)
        
        val inline internal mapM:
          f: ('a ->  ^b) -> as': 'a list ->  ^c
            when (Control.Bind or  ^b or  ^c) :
                   (static member (>>=) :  ^b * ('d ->  ^c) ->  ^c) and
                 (Control.Return or  ^c) :
                   (static member Return:
                       ^c * Control.Return -> ('d list ->  ^c)) and
                 (Control.Bind or  ^c) :
                   (static member (>>=) :  ^c * ('d list ->  ^c) ->  ^c)
        
        val inline bind:
          f: ('T -> ListT< ^Monad<list<'U>>) -> ListT< ^Monad<list<'T>>
            -> ListT< ^c>
            when (Control.Bind or  ^Monad<list<'U> or  ^a) :
                   (static member (>>=) :  ^Monad<list<'U> * ('b ->  ^a) ->  ^a) and
                 (Control.Return or  ^a) :
                   (static member Return:
                       ^a * Control.Return -> ('b list ->  ^a)) and
                 (Control.Bind or  ^a) :
                   (static member (>>=) :  ^a * ('b list ->  ^a) ->  ^a) and
                 (Control.Bind or  ^Monad<list<'T> or  ^a) :
                   (static member (>>=) :
                       ^Monad<list<'T> * ('T list ->  ^a) ->  ^a) and
                 (Control.Bind or  ^a or  ^c) :
                   (static member (>>=) :  ^a * ('d list list ->  ^c) ->  ^c) and
                 (Control.Return or  ^c) :
                   (static member Return:
                       ^c * Control.Return -> ('d list ->  ^c))
        
        val inline apply:
          ListT< ^Monad<list<('T -> 'U)>> -> ListT< ^Monad<list<'T>>
            -> ListT< ^Monad<list<'U>>
            when (Control.Map or  ^Monad<list<('T -> 'U)> or  ^a) :
                   (static member Map:
                      ( ^Monad<list<('T -> 'U)> *
                       (('b -> 'c) list -> 'b list -> 'c list)) * Control.Map
                        ->  ^a) and
                 (Control.Apply or  ^a or  ^Monad<list<'T> or  ^Monad<list<'U>) :
                   (static member ``<*>`` :
                       ^a *  ^Monad<list<'T> *  ^Monad<list<'U> * Control.Apply
                        ->  ^Monad<list<'U>)
        
        val inline lift2:
          f: ('T -> 'U -> 'V) -> ListT< ^Monad<list<'T>>
          -> ListT< ^Monad<list<'U>> -> ListT< ^Monad<list<'V>>
            when (Control.Lift2 or  ^Monad<list<'T> or  ^Monad<list<'U> or
                   ^Monad<list<'V>) :
                   (static member Lift2:
                      ('T list -> 'U list -> 'V list) *
                      ( ^Monad<list<'T> *  ^Monad<list<'U>) * Control.Lift2
                        ->  ^Monad<list<'V>)
        
        val inline lift3:
          f: ('T -> 'U -> 'V -> 'W) -> ListT< ^Monad<list<'T>>
          -> ListT< ^Monad<list<'U>> -> ListT< ^Monad<list<'V>>
            -> ListT< ^Monad<list<'W>>
            when (Control.Lift3 or  ^Monad<list<'T> or  ^Monad<list<'U> or
                   ^Monad<list<'V> or  ^Monad<list<'W>) :
                   (static member Lift3:
                      ('V list -> 'T list -> 'U list -> 'W list) *
                      ( ^Monad<list<'T> *  ^Monad<list<'U> *  ^Monad<list<'V>) *
                      Control.Lift3 ->  ^Monad<list<'W>)
        
        val inline map:
          f: ('T -> 'U) -> ListT< ^Monad<list<'T>> -> ListT< ^Monad<list<'U>>
            when (Control.Map or  ^Monad<list<'T> or  ^Monad<list<'U>) :
                   (static member Map:
                      ( ^Monad<list<'T> * ('T list -> 'U list)) * Control.Map
                        ->  ^Monad<list<'U>)

