namespace FSharpPlus.Data
    
    /// Additional operations on Result
    module Result =
        
        val inline traverse:
          f: ('a ->  ^b) -> _arg1: Result<'a,'e> ->  ^c
            when (Control.Map or  ^b or  ^c) :
                   (static member Map:
                      ( ^b * ('f -> Result<'f,'g>)) * Control.Map ->  ^c) and
                 (Control.Return or  ^c) :
                   (static member Return:
                       ^c * Control.Return -> (Result<'d,'e> ->  ^c))
    
    /// Result<'TSuccess,'TFailure> specialized in 'TFailure = Exception 
    module ResultOrException =
        
        [<System.Runtime.CompilerServices.Extension>]
        val IsResult: _arg1: Result<'a,exn> -> bool
        
        [<System.Runtime.CompilerServices.Extension>]
        val IsException: _arg1: Result<'a,exn> -> bool
        
        [<System.Runtime.CompilerServices.Extension>]
        val Result: _arg1: Result<'a,exn> -> 'a
        
        [<System.Runtime.CompilerServices.Extension>]
        val Exception: _arg1: Result<'a,exn> -> exn
    
    /// Monad Transformer for Result<'T, 'E>
    [<Struct>]
    type ResultT<'monad<Result<'t,'e>>> =
        | ResultT of 'monad<Result<'t,'e>>
        
        static member
          inline (<*>) : f: ResultT< ^Monad<Result<('T -> 'U),'E>>> *
                         x: ResultT< ^Monad<Result<'T,'E>>>
                           -> ResultT< ^Monad<Result<'U,'E>>>
                           when (Control.Map or  ^Monad<Result<('T -> 'U),'E>> or
                                  ^a) :
                                  (static member Map:
                                     ( ^Monad<Result<('T -> 'U),'E>> *
                                      (Result<('b -> 'c),'d> -> Result<'b,'d>
                                         -> Result<'c,'d>)) * Control.Map ->  ^a) and
                                (Control.Apply or  ^a or  ^Monad<Result<'T,'E>> or
                                  ^Monad<Result<'U,'E>>) :
                                  (static member ``<*>`` :
                                      ^a *  ^Monad<Result<'T,'E>> *
                                      ^Monad<Result<'U,'E>> * Control.Apply
                                       ->  ^Monad<Result<'U,'E>>)
        
        static member
          inline (>>=) : x: ResultT< ^Monad<Result<'T,'E>>> *
                         f: ('T -> ResultT< ^Monad<Result<'U,'E>>>)
                           -> ResultT< ^Monad<Result<'U,'E>>>
                           when (Control.Bind or  ^Monad<Result<'T,'E>> or
                                  ^Monad<Result<'U,'E>>) :
                                  (static member (>>=) :
                                      ^Monad<Result<'T,'E>> *
                                     (Result<'T,'b> ->  ^Monad<Result<'U,'E>>)
                                       ->  ^Monad<Result<'U,'E>>) and
                                (Control.Return or  ^Monad<Result<'U,'E>>) :
                                  (static member Return:
                                      ^Monad<Result<'U,'E>> * Control.Return
                                       -> (Result<'a,'b>
                                             ->  ^Monad<Result<'U,'E>>))
        
        static member
          inline CallCC: f: (('T -> ResultT<'MonadCont<'R,Result<'U,'E>>>)
                               -> ResultT< ^MonadCont<'R, Result<'T,'E>>>)
                           -> ResultT< ^MonadCont<'R, Result<'T,'E>>>
                           when  ^MonadCont<'R, Result<'T,'E>> :
                                  (static member CallCC:
                                     ((Result<'T,'E>
                                         -> 'MonadCont<'R,Result<'U,'E>>)
                                        ->  ^MonadCont<'R, Result<'T,'E>>)
                                       ->  ^MonadCont<'R, Result<'T,'E>>)
        
        static member
          inline Catch: ResultT< ^Monad<Result<'T, 'E1>>> *
                        f: ('E1 -> ResultT< ^Monad<Result<'T, 'E2>>>)
                          -> ResultT< ^Monad<Result<'T, 'E2>>>
                          when (Control.Bind or  ^Monad<Result<'T, 'E1>> or
                                 ^Monad<Result<'T, 'E2>>) :
                                 (static member (>>=) :
                                     ^Monad<Result<'T, 'E1>> *
                                    (Result<'T,'E1> ->  ^Monad<Result<'T, 'E2>>)
                                      ->  ^Monad<Result<'T, 'E2>>) and
                               (Control.Return or  ^Monad<Result<'T, 'E2>>) :
                                 (static member Return:
                                     ^Monad<Result<'T, 'E2>> * Control.Return
                                      -> (Result<'T,'a>
                                            ->  ^Monad<Result<'T, 'E2>>))
        
        static member
          inline Delay: body: (unit -> ResultT< ^Monad<Result<'T,'E>>>)
                          -> ResultT< ^Monad<Result<'T,'E>>>
                          when (Control.Delay or  ^Monad<Result<'T,'E>>) :
                                 (static member Delay:
                                    Control.Delay *
                                    (unit ->  ^Monad<Result<'T,'E>>) *
                                    Control.Delay ->  ^Monad<Result<'T,'E>>)
        
        [<System.ComponentModel.EditorBrowsable
          (enum<System.ComponentModel.EditorBrowsableState> (1))>]
        static member
          inline Lift: x:  ^Monad<'T> -> ResultT< ^Monad<Result<'T,'E>>>
                         when (Control.Map or  ^Monad<'T> or
                                ^Monad<Result<'T,'E>>) :
                                (static member Map:
                                   ( ^Monad<'T> * ('c -> Result<'c,'d>)) *
                                   Control.Map ->  ^Monad<Result<'T,'E>>) and
                              (Control.Bind or  ^Monad<'T> or
                                ^Monad<Result<'T,'E>>) :
                                (static member (>>=) :
                                    ^Monad<'T> * ('a ->  ^Monad<Result<'T,'E>>)
                                     ->  ^Monad<Result<'T,'E>>) and
                              (Control.Return or  ^Monad<Result<'T,'E>>) :
                                (static member Return:
                                    ^Monad<Result<'T,'E>> * Control.Return
                                     -> (Result<'a,'b> ->  ^Monad<Result<'T,'E>>))
        
        [<System.ComponentModel.EditorBrowsable
          (enum<System.ComponentModel.EditorBrowsableState> (1))>]
        static member
          inline Lift2: f: ('T -> 'U -> 'V) * x: ResultT< ^Monad<Result<'T,'E>> *
                        y: ResultT< ^Monad<Result<'U,'E>>
                          -> ResultT< ^Monad<Result<'V,'E>>
                          when (Control.Lift2 or  ^Monad<Result<'T,'E> or
                                 ^Monad<Result<'U,'E> or  ^Monad<Result<'V,'E>) :
                                 (static member Lift2:
                                    (Result<'T,'a> -> Result<'U,'a>
                                       -> Result<'V,'a>) *
                                    ( ^Monad<Result<'T,'E> *
                                      ^Monad<Result<'U,'E>) * Control.Lift2
                                      ->  ^Monad<Result<'V,'E>)
        
        [<System.ComponentModel.EditorBrowsable
          (enum<System.ComponentModel.EditorBrowsableState> (1))>]
        static member
          inline Lift3: f: ('T -> 'U -> 'V -> 'W) *
                        x: ResultT< ^Monad<Result<'T,'E>> *
                        y: ResultT< ^Monad<Result<'U,'E>> *
                        z: ResultT< ^Monad<Result<'V,'E>>
                          -> ResultT< ^Monad<Result<'W,'E>>
                          when (Control.Lift3 or  ^Monad<Result<'T,'E> or
                                 ^Monad<Result<'U,'E> or  ^Monad<Result<'V,'E> or
                                 ^Monad<Result<'W,'E>) :
                                 (static member Lift3:
                                    (Result<'T,'a> -> Result<'U,'a>
                                     -> Result<'V,'a> -> Result<'W,'a>) *
                                    ( ^Monad<Result<'T,'E> *
                                      ^Monad<Result<'U,'E> *
                                      ^Monad<Result<'V,'E>) * Control.Lift3
                                      ->  ^Monad<Result<'W,'E>)
        
        static member
          inline LiftAsync: x: Async<'T> -> ResultT< ^MonadAsync<'T>>
                              when (Control.Return or  ^MonadAsync<'T>) :
                                     (static member Return:
                                         ^MonadAsync<'T> * Control.Return
                                          -> (Result<'a,'b> ->  ^MonadAsync<'T>)) and
                                   (Control.Bind or  ^c or  ^MonadAsync<'T>) :
                                     (static member (>>=) :
                                         ^c * ('a ->  ^MonadAsync<'T>)
                                          ->  ^MonadAsync<'T>) and
                                   (Control.Map or  ^c or  ^MonadAsync<'T>) :
                                     (static member Map:
                                        ( ^c * ('d -> Result<'d,'e>)) *
                                        Control.Map ->  ^MonadAsync<'T>) and
                                   (Control.LiftAsync or  ^c) :
                                     (static member LiftAsync:
                                         ^c -> (Async<'T> ->  ^c))
        
        static member
          inline Listen: m: ResultT<'a>
                           -> ResultT< ^MonadWriter<'Monoid,Result<'T*'Monoid,'E>>>
                           when (Control.Bind or  ^b or
                                  ^MonadWriter<'Monoid,Result<'T*'Monoid,'E>>) :
                                  (static member (>>=) :
                                      ^b *
                                     (Result<'c,'d> * 'e
                                        ->  ^MonadWriter<'Monoid,Result<'T*'Monoid,'E>>)
                                       ->  ^MonadWriter<'Monoid,Result<'T*'Monoid,'E>>) and
                                (Control.Return or
                                  ^MonadWriter<'Monoid,Result<'T*'Monoid,'E>>) :
                                  (static member Return:
                                      ^MonadWriter<'Monoid,Result<'T*'Monoid,'E>> *
                                     Control.Return
                                       -> (Result<('c * 'e),'d>
                                             ->  ^MonadWriter<'Monoid,Result<'T*'Monoid,'E>>)) and
                                 ^b: (static member Listen: 'a ->  ^b)
        
        static member
          inline Local: ResultT< ^MonadReader<'R2,Result<'R2,'E>>> *
                        f: ('R1 -> 'R2) -> ResultT< ^a>
                          when  ^a:
                                 (static member Local:
                                     ^MonadReader<'R2,Result<'R2,'E>> *
                                    ('R1 -> 'R2) ->  ^a)
        
        [<System.ComponentModel.EditorBrowsable
          (enum<System.ComponentModel.EditorBrowsableState> (1))>]
        static member
          inline Map: x: ResultT< ^Monad<Result<'T,'E>>> * f: ('T -> 'U)
                        -> ResultT< ^Monad<Result<'U,'E>>>
                        when (Control.Map or  ^Monad<Result<'T,'E>> or
                               ^Monad<Result<'U,'E>>) :
                               (static member Map:
                                  ( ^Monad<Result<'T,'E>> *
                                   (Result<'T,'a> -> Result<'U,'a>)) *
                                  Control.Map ->  ^Monad<Result<'U,'E>>)
        
        static member
          inline Pass: m: ResultT< ^a>
                         -> ResultT< ^MonadWriter<'Monoid,Result<'T,'E>>>
                         when (Control.Bind or  ^a or
                                ^MonadWriter<'Monoid,Result<'T,'E>>) :
                                (static member (>>=) :
                                    ^a *
                                   (Result<'b,'c>
                                      ->  ^MonadWriter<'Monoid,Result<'T,'E>>)
                                     ->  ^MonadWriter<'Monoid,Result<'T,'E>>) and
                              (Control.Map or  ^d or
                                ^MonadWriter<'Monoid,Result<'T,'E>>) :
                                (static member Map:
                                   ( ^d * ('f -> Result<'f,'g>)) * Control.Map
                                     ->  ^MonadWriter<'Monoid,Result<'T,'E>>) and
                              (Control.Return or
                                ^MonadWriter<'Monoid,Result<'T,'E>>) :
                                (static member Return:
                                    ^MonadWriter<'Monoid,Result<'T,'E>> *
                                   Control.Return
                                     -> (Result<'h,'c>
                                           ->  ^MonadWriter<'Monoid,Result<'T,'E>>)) and
                               ^d: (static member Pass:  ^e ->  ^d) and
                              (Control.Return or  ^e) :
                                (static member Return:
                                    ^e * Control.Return -> ('b ->  ^e))
        
        static member
          inline Put: x: 'S -> ResultT< ^MonadState<'S, Result<_, 'E>>>
                        when (Control.Return or  ^MonadState<'S, Result<_, 'E>>) :
                               (static member Return:
                                   ^MonadState<'S, Result<_, 'E>> *
                                  Control.Return
                                    -> (Result<'a,'b>
                                          ->  ^MonadState<'S, Result<_, 'E>>)) and
                             (Control.Bind or  ^c or
                               ^MonadState<'S, Result<_, 'E>>) :
                               (static member (>>=) :
                                   ^c * ('a ->  ^MonadState<'S, Result<_, 'E>>)
                                    ->  ^MonadState<'S, Result<_, 'E>>) and
                             (Control.Map or  ^c or
                               ^MonadState<'S, Result<_, 'E>>) :
                               (static member Map:
                                  ( ^c * ('d -> Result<'d,'e>)) * Control.Map
                                    ->  ^MonadState<'S, Result<_, 'E>>) and
                              ^c: (static member Put: 'S ->  ^c)
        
        static member
          inline Return: x: 'T -> ResultT< ^Monad<Result<'T,'E>>>
                           when (Control.Return or  ^Monad<Result<'T,'E>>) :
                                  (static member Return:
                                      ^Monad<Result<'T,'E>> * Control.Return
                                       -> (Result<'T,'a>
                                             ->  ^Monad<Result<'T,'E>>))
        
        static member
          inline Tell: w: 'Monoid -> ResultT< ^Writer<'Monoid,Result<unit,'E>>>
                         when (Control.Return or
                                ^Writer<'Monoid,Result<unit,'E>>) :
                                (static member Return:
                                    ^Writer<'Monoid,Result<unit,'E>> *
                                   Control.Return
                                     -> (Result<'a,'b>
                                           ->  ^Writer<'Monoid,Result<unit,'E>>)) and
                              (Control.Bind or  ^c or
                                ^Writer<'Monoid,Result<unit,'E>>) :
                                (static member (>>=) :
                                    ^c *
                                   ('a ->  ^Writer<'Monoid,Result<unit,'E>>)
                                     ->  ^Writer<'Monoid,Result<unit,'E>>) and
                              (Control.Map or  ^c or
                                ^Writer<'Monoid,Result<unit,'E>>) :
                                (static member Map:
                                   ( ^c * ('d -> Result<'d,'e>)) * Control.Map
                                     ->  ^Writer<'Monoid,Result<unit,'E>>) and
                               ^c: (static member Tell: 'Monoid ->  ^c)
        
        static member
          inline Throw: x: 'E -> ResultT< ^Monad<Result<'T,'E>>>
                          when (Control.Return or  ^Monad<Result<'T,'E>>) :
                                 (static member Return:
                                     ^Monad<Result<'T,'E>> * Control.Return
                                      -> (Result<'a,'E>
                                            ->  ^Monad<Result<'T,'E>>))
        
        static member
          inline TryFinally: computation: ResultT< ^Monad<Result<'T,'E>>> *
                             f: (unit -> unit)
                               -> ResultT< ^Monad<Result<'T,'E>>>
                               when (Control.TryFinally or
                                      ^Monad<Result<'T,'E>>) :
                                      (static member TryFinally:
                                         ((unit ->  ^Monad<Result<'T,'E>>) *
                                          (unit -> unit)) * Control.TryFinally *
                                         Control.TryFinally *
                                         Control.TryBlock.False
                                           ->  ^Monad<Result<'T,'E>>)
        
        static member
          inline TryWith: source: ResultT< ^Monad<Result<'T,'E>>> *
                          f: (exn -> ResultT< ^Monad<Result<'T,'E>>>)
                            -> ResultT< ^Monad<Result<'T,'E>>>
                            when (Control.TryWith or  ^Monad<Result<'T,'E>>) :
                                   (static member TryWith:
                                      (unit ->  ^Monad<Result<'T,'E>>) *
                                      ('a ->  ^Monad<Result<'T,'E>>) *
                                      Control.TryWith * Control.TryBlock.False
                                        ->  ^Monad<Result<'T,'E>>) and 'a :> exn
        
        static member
          inline Using: resource: 'a *
                        f: ('a -> ResultT< ^Monad<Result<'T,'E>>>)
                          -> ResultT< ^Monad<Result<'T,'E>>>
                          when 'a :> System.IDisposable and
                               (Control.Using or  ^Monad<Result<'T,'E>>) :
                                 (static member Using:
                                    'a * ('a ->  ^Monad<Result<'T,'E>>) *
                                    Control.Using ->  ^Monad<Result<'T,'E>>)
        
        static member
          inline get_Ask: unit -> ResultT< ^MonadReader<'R,Result<'R,'E>>>
                            when (Control.Return or
                                   ^MonadReader<'R,Result<'R,'E>>) :
                                   (static member Return:
                                       ^MonadReader<'R,Result<'R,'E>> *
                                      Control.Return
                                        -> (Result<'a,'b>
                                              ->  ^MonadReader<'R,Result<'R,'E>>)) and
                                 (Control.Bind or  ^c or
                                   ^MonadReader<'R,Result<'R,'E>>) :
                                   (static member (>>=) :
                                       ^c *
                                      ('a ->  ^MonadReader<'R,Result<'R,'E>>)
                                        ->  ^MonadReader<'R,Result<'R,'E>>) and
                                 (Control.Map or  ^c or
                                   ^MonadReader<'R,Result<'R,'E>>) :
                                   (static member Map:
                                      ( ^c * ('d -> Result<'d,'e>)) *
                                      Control.Map
                                        ->  ^MonadReader<'R,Result<'R,'E>>) and
                                  ^c: (static member get_Ask: ->  ^c)
        
        static member
          inline get_Get: unit -> ResultT< ^MonadState<'S, Result<_, 'E>>>
                            when (Control.Return or
                                   ^MonadState<'S, Result<_, 'E>>) :
                                   (static member Return:
                                       ^MonadState<'S, Result<_, 'E>> *
                                      Control.Return
                                        -> (Result<'a,'b>
                                              ->  ^MonadState<'S, Result<_, 'E>>)) and
                                 (Control.Bind or  ^c or
                                   ^MonadState<'S, Result<_, 'E>>) :
                                   (static member (>>=) :
                                       ^c *
                                      ('a ->  ^MonadState<'S, Result<_, 'E>>)
                                        ->  ^MonadState<'S, Result<_, 'E>>) and
                                 (Control.Map or  ^c or
                                   ^MonadState<'S, Result<_, 'E>>) :
                                   (static member Map:
                                      ( ^c * ('d -> Result<'d,'e>)) *
                                      Control.Map
                                        ->  ^MonadState<'S, Result<_, 'E>>) and
                                  ^c: (static member get_Get: ->  ^c)
    
    /// Basic operations on ResultT
    module ResultT =
        
        val run: ResultT<'Monad<Result<'T,'E>>> -> 'Monad<Result<'T,'E>>
        
        /// Embed a Monad<'T> into a ResultT<'Monad<Result<'T, 'TError>>>
        val inline lift:
          x:  ^Monad<'T> -> ResultT< ^Monad<Result<'T,'TError>>>
            when (Control.Bind or  ^Monad<'T> or  ^Monad<Result<'T,'TError>>) :
                   (static member (>>=) :
                       ^Monad<'T> * ('a ->  ^Monad<Result<'T,'TError>>)
                        ->  ^Monad<Result<'T,'TError>>) and
                 (Control.Map or  ^Monad<'T> or  ^Monad<Result<'T,'TError>>) :
                   (static member Map:
                      ( ^Monad<'T> * ('c -> Result<'c,'d>)) * Control.Map
                        ->  ^Monad<Result<'T,'TError>>) and
                 (Control.Return or  ^Monad<Result<'T,'TError>>) :
                   (static member Return:
                       ^Monad<Result<'T,'TError>> * Control.Return
                        -> (Result<'a,'b> ->  ^Monad<Result<'T,'TError>>))
        
        /// Transform a Result<'T,'Error> to a ResultT<'Monad<Result<'T,'Error>>>
        val inline hoist:
          x: Result<'T,'TError> -> ResultT< ^Monad<Result<'T,'TError>>>
            when (Control.Return or  ^Monad<Result<'T,'TError>>) :
                   (static member Return:
                       ^Monad<Result<'T,'TError>> * Control.Return
                        -> (Result<'T,'TError> ->  ^Monad<Result<'T,'TError>>))
        
        val inline bind:
          f: ('T -> ResultT< ^Monad<Result<'U,'E>>>)
          -> ResultT< ^Monad<Result<'T,'E>>> -> ResultT< ^Monad<Result<'U,'E>>>
            when (Control.Return or  ^Monad<Result<'U,'E>>) :
                   (static member Return:
                       ^Monad<Result<'U,'E>> * Control.Return
                        -> (Result<'a,'b> ->  ^Monad<Result<'U,'E>>)) and
                 (Control.Bind or  ^Monad<Result<'T,'E>> or
                   ^Monad<Result<'U,'E>>) :
                   (static member (>>=) :
                       ^Monad<Result<'T,'E>> *
                      (Result<'T,'b> ->  ^Monad<Result<'U,'E>>)
                        ->  ^Monad<Result<'U,'E>>)
        
        val inline apply:
          ResultT< ^Monad<Result<('T -> 'U),'E>>>
          -> ResultT< ^Monad<Result<'T,'E>>> -> ResultT< ^Monad<Result<'U,'E>>>
            when (Control.Map or  ^Monad<Result<('T -> 'U),'E>> or  ^a) :
                   (static member Map:
                      ( ^Monad<Result<('T -> 'U),'E>> *
                       (Result<('b -> 'c),'d> -> Result<'b,'d> -> Result<'c,'d>)) *
                      Control.Map ->  ^a) and
                 (Control.Apply or  ^a or  ^Monad<Result<'T,'E>> or
                   ^Monad<Result<'U,'E>>) :
                   (static member ``<*>`` :
                       ^a *  ^Monad<Result<'T,'E>> *  ^Monad<Result<'U,'E>> *
                      Control.Apply ->  ^Monad<Result<'U,'E>>)
        
        val inline map:
          f: ('T -> 'U) -> ResultT< ^Monad<Result<'T,'E>>>
            -> ResultT< ^Monad<Result<('T -> 'U),'E>>>
            when (Control.Map or  ^Monad<Result<'T,'E>> or
                   ^Monad<Result<('T -> 'U),'E>>) :
                   (static member Map:
                      ( ^Monad<Result<'T,'E>> * (Result<'T,'a> -> Result<'U,'a>)) *
                      Control.Map ->  ^Monad<Result<('T -> 'U),'E>>)
        
        val inline map2:
          f: ('T -> 'U -> 'V) -> ResultT< ^Monad<Result<'T,'E>>>
          -> ResultT< ^Monad<Result<'U,'E>>> -> ResultT< ^Monad<Result<'V,'E>>>
            when (Control.Lift2 or  ^Monad<Result<'T,'E>> or
                   ^Monad<Result<'U,'E>> or  ^Monad<Result<'V,'E>>) :
                   (static member Lift2:
                      (Result<'T,'a> -> Result<'U,'a> -> Result<'V,'a>) *
                      ( ^Monad<Result<'T,'E>> *  ^Monad<Result<'U,'E>>) *
                      Control.Lift2 ->  ^Monad<Result<'V,'E>>)
        
        val inline map3:
          f: ('T -> 'U -> 'V -> 'W) -> ResultT< ^Monad<Result<'T,'E>>>
          -> ResultT< ^Monad<Result<'U,'E>>> -> ResultT< ^Monad<Result<'V,'E>>>
            -> ResultT< ^Monad<Result<'W,'E>>>
            when (Control.Lift3 or  ^Monad<Result<'T,'E>> or
                   ^Monad<Result<'U,'E>> or  ^Monad<Result<'V,'E>> or
                   ^Monad<Result<'W,'E>>) :
                   (static member Lift3:
                      (Result<'T,'a> -> Result<'U,'a> -> Result<'V,'a>
                         -> Result<'W,'a>) *
                      ( ^Monad<Result<'T,'E>> *  ^Monad<Result<'U,'E>> *
                        ^Monad<Result<'V,'E>>) * Control.Lift3
                        ->  ^Monad<Result<'W,'E>>)
    
    [<Struct>]
    type ChoiceT<'monad<Choice<'t,'e>>> =
        | ChoiceT of 'monad<Choice<'t,'e>>
        
        static member
          inline (<*>) : f: ChoiceT< ^Monad<Choice<('T -> 'U),'E>>> *
                         x: ChoiceT< ^Monad<Choice<'T,'E>>>
                           -> ChoiceT< ^Monad<Choice<'U,'E>>>
                           when (Control.Map or  ^Monad<Choice<('T -> 'U),'E>> or
                                  ^a) :
                                  (static member Map:
                                     ( ^Monad<Choice<('T -> 'U),'E>> *
                                      (Choice<('b -> 'c),'d> -> Choice<'b,'d>
                                         -> Choice<'c,'d>)) * Control.Map ->  ^a) and
                                (Control.Apply or  ^a or  ^Monad<Choice<'T,'E>> or
                                  ^Monad<Choice<'U,'E>>) :
                                  (static member ``<*>`` :
                                      ^a *  ^Monad<Choice<'T,'E>> *
                                      ^Monad<Choice<'U,'E>> * Control.Apply
                                       ->  ^Monad<Choice<'U,'E>>)
        
        static member
          inline (>>=) : x: ChoiceT< ^Monad<Choice<'T,'E>>> *
                         f: ('T -> ChoiceT< ^Monad<Choice<'U,'E>>>)
                           -> ChoiceT< ^Monad<Choice<'U,'E>>>
                           when (Control.Bind or  ^Monad<Choice<'T,'E>> or
                                  ^Monad<Choice<'U,'E>>) :
                                  (static member (>>=) :
                                      ^Monad<Choice<'T,'E>> *
                                     (Choice<'T,'b> ->  ^Monad<Choice<'U,'E>>)
                                       ->  ^Monad<Choice<'U,'E>>) and
                                (Control.Return or  ^Monad<Choice<'U,'E>>) :
                                  (static member Return:
                                      ^Monad<Choice<'U,'E>> * Control.Return
                                       -> (Choice<'a,'b>
                                             ->  ^Monad<Choice<'U,'E>>))
        
        static member
          inline CallCC: f: (('T -> ChoiceT<'MonadCont<'R,Choice<'U,'E>>>)
                               -> ChoiceT< ^MonadCont<'R, Choice<'T,'E>>>)
                           -> ChoiceT< ^MonadCont<'R, Choice<'T,'E>>>
                           when  ^MonadCont<'R, Choice<'T,'E>> :
                                  (static member CallCC:
                                     ((Choice<'T,'a>
                                         -> 'MonadCont<'R,Choice<'U,'E>>)
                                        ->  ^MonadCont<'R, Choice<'T,'E>>)
                                       ->  ^MonadCont<'R, Choice<'T,'E>>)
        
        static member
          inline Catch: ChoiceT< ^MonadError<'E1,'T>> *
                        f: ('E1 -> ChoiceT< ^Monad<Choice<'T,'E2>>>)
                          -> ChoiceT< ^Monad<Choice<'T,'E2>>>
                          when (Control.Bind or  ^MonadError<'E1,'T> or
                                 ^Monad<Choice<'T,'E2>>) :
                                 (static member (>>=) :
                                     ^MonadError<'E1,'T> *
                                    (Choice<'a,'E1> ->  ^Monad<Choice<'T,'E2>>)
                                      ->  ^Monad<Choice<'T,'E2>>) and
                               (Control.Return or  ^Monad<Choice<'T,'E2>>) :
                                 (static member Return:
                                     ^Monad<Choice<'T,'E2>> * Control.Return
                                      -> (Choice<'a,'b>
                                            ->  ^Monad<Choice<'T,'E2>>))
        
        [<System.ComponentModel.EditorBrowsable
          (enum<System.ComponentModel.EditorBrowsableState> (1))>]
        static member
          inline Lift: x:  ^Monad<'T> -> ChoiceT< ^Monad<Choice<'T,'E>>>
                         when (Control.Map or  ^Monad<'T> or
                                ^Monad<Choice<'T,'E>>) :
                                (static member Map:
                                   ( ^Monad<'T> * ('c -> Choice<'c,'d>)) *
                                   Control.Map ->  ^Monad<Choice<'T,'E>>) and
                              (Control.Bind or  ^Monad<'T> or
                                ^Monad<Choice<'T,'E>>) :
                                (static member (>>=) :
                                    ^Monad<'T> * ('a ->  ^Monad<Choice<'T,'E>>)
                                     ->  ^Monad<Choice<'T,'E>>) and
                              (Control.Return or  ^Monad<Choice<'T,'E>>) :
                                (static member Return:
                                    ^Monad<Choice<'T,'E>> * Control.Return
                                     -> (Choice<'a,'b> ->  ^Monad<Choice<'T,'E>>))
        
        [<System.ComponentModel.EditorBrowsable
          (enum<System.ComponentModel.EditorBrowsableState> (1))>]
        static member
          inline Lift2: f: ('T -> 'U -> 'V) * x: ChoiceT< ^Monad<Choice<'T,'E>> *
                        y: ChoiceT< ^Monad<Choice<'U,'E>>
                          -> ChoiceT< ^Monad<Choice<'V,'E>>
                          when (Control.Lift2 or  ^Monad<Choice<'T,'E> or
                                 ^Monad<Choice<'U,'E> or  ^Monad<Choice<'V,'E>) :
                                 (static member Lift2:
                                    (Choice<'T,'a> -> Choice<'U,'a>
                                       -> Choice<'V,'a>) *
                                    ( ^Monad<Choice<'T,'E> *
                                      ^Monad<Choice<'U,'E>) * Control.Lift2
                                      ->  ^Monad<Choice<'V,'E>)
        
        static member
          inline LiftAsync: x: Async<'T> -> ChoiceT< ^MonadAsync<'T>>
                              when (Control.Return or  ^MonadAsync<'T>) :
                                     (static member Return:
                                         ^MonadAsync<'T> * Control.Return
                                          -> (Choice<'a,'b> ->  ^MonadAsync<'T>)) and
                                   (Control.Bind or  ^c or  ^MonadAsync<'T>) :
                                     (static member (>>=) :
                                         ^c * ('a ->  ^MonadAsync<'T>)
                                          ->  ^MonadAsync<'T>) and
                                   (Control.Map or  ^c or  ^MonadAsync<'T>) :
                                     (static member Map:
                                        ( ^c * ('d -> Choice<'d,'e>)) *
                                        Control.Map ->  ^MonadAsync<'T>) and
                                   (Control.LiftAsync or  ^c) :
                                     (static member LiftAsync:
                                         ^c -> (Async<'T> ->  ^c))
        
        static member
          inline Listen: m: ChoiceT<'a>
                           -> ChoiceT< ^MonadWriter<'Monoid,Choice<'T*'Monoid,'E>>>
                           when (Control.Bind or  ^b or
                                  ^MonadWriter<'Monoid,Choice<'T*'Monoid,'E>>) :
                                  (static member (>>=) :
                                      ^b *
                                     (Choice<'c,'d> * 'e
                                        ->  ^MonadWriter<'Monoid,Choice<'T*'Monoid,'E>>)
                                       ->  ^MonadWriter<'Monoid,Choice<'T*'Monoid,'E>>) and
                                (Control.Return or
                                  ^MonadWriter<'Monoid,Choice<'T*'Monoid,'E>>) :
                                  (static member Return:
                                      ^MonadWriter<'Monoid,Choice<'T*'Monoid,'E>> *
                                     Control.Return
                                       -> (Choice<('c * 'e),'d>
                                             ->  ^MonadWriter<'Monoid,Choice<'T*'Monoid,'E>>)) and
                                 ^b: (static member Listen: 'a ->  ^b)
        
        static member
          inline Local: ChoiceT< ^MonadReader<'R2,Choice<'R2,'E>>> *
                        f: ('R1 -> 'R2) -> ChoiceT< ^a>
                          when  ^a:
                                 (static member Local:
                                     ^MonadReader<'R2,Choice<'R2,'E>> *
                                    ('R1 -> 'R2) ->  ^a)
        
        [<System.ComponentModel.EditorBrowsable
          (enum<System.ComponentModel.EditorBrowsableState> (1))>]
        static member
          inline Map: x: ChoiceT< ^Monad<Choice<'T,'E>>> * f: ('T -> 'U)
                        -> ChoiceT< ^Monad<Choice<'U,'E>>>
                        when (Control.Map or  ^Monad<Choice<'T,'E>> or
                               ^Monad<Choice<'U,'E>>) :
                               (static member Map:
                                  ( ^Monad<Choice<'T,'E>> *
                                   (Choice<'T,'a> -> Choice<'U,'a>)) *
                                  Control.Map ->  ^Monad<Choice<'U,'E>>)
        
        static member
          inline Pass: m: ChoiceT< ^a>
                         -> ChoiceT< ^MonadWriter<'Monoid,Choice<'T,'E>>>
                         when (Control.Bind or  ^a or
                                ^MonadWriter<'Monoid,Choice<'T,'E>>) :
                                (static member (>>=) :
                                    ^a *
                                   (Result<'b,'c>
                                      ->  ^MonadWriter<'Monoid,Choice<'T,'E>>)
                                     ->  ^MonadWriter<'Monoid,Choice<'T,'E>>) and
                              (Control.Map or  ^d or
                                ^MonadWriter<'Monoid,Choice<'T,'E>>) :
                                (static member Map:
                                   ( ^d * ('f -> Choice<'f,'g>)) * Control.Map
                                     ->  ^MonadWriter<'Monoid,Choice<'T,'E>>) and
                              (Control.Return or
                                ^MonadWriter<'Monoid,Choice<'T,'E>>) :
                                (static member Return:
                                    ^MonadWriter<'Monoid,Choice<'T,'E>> *
                                   Control.Return
                                     -> (Result<'h,'c>
                                           ->  ^MonadWriter<'Monoid,Choice<'T,'E>>)) and
                               ^d: (static member Pass:  ^e ->  ^d) and
                              (Control.Return or  ^e) :
                                (static member Return:
                                    ^e * Control.Return -> ('b ->  ^e))
        
        static member
          inline Put: x: 'S -> ChoiceT< ^MonadState<'S, Choice<_, 'E>>>
                        when (Control.Return or  ^MonadState<'S, Choice<_, 'E>>) :
                               (static member Return:
                                   ^MonadState<'S, Choice<_, 'E>> *
                                  Control.Return
                                    -> (Choice<'a,'b>
                                          ->  ^MonadState<'S, Choice<_, 'E>>)) and
                             (Control.Bind or  ^c or
                               ^MonadState<'S, Choice<_, 'E>>) :
                               (static member (>>=) :
                                   ^c * ('a ->  ^MonadState<'S, Choice<_, 'E>>)
                                    ->  ^MonadState<'S, Choice<_, 'E>>) and
                             (Control.Map or  ^c or
                               ^MonadState<'S, Choice<_, 'E>>) :
                               (static member Map:
                                  ( ^c * ('d -> Choice<'d,'e>)) * Control.Map
                                    ->  ^MonadState<'S, Choice<_, 'E>>) and
                              ^c: (static member Put: 'S ->  ^c)
        
        static member
          inline Return: x: 'T -> ChoiceT< ^Monad<Choice<'T,'E>>>
                           when (Control.Return or  ^Monad<Choice<'T,'E>>) :
                                  (static member Return:
                                      ^Monad<Choice<'T,'E>> * Control.Return
                                       -> (Choice<'T,'a>
                                             ->  ^Monad<Choice<'T,'E>>))
        
        static member
          inline Tell: w: 'Monoid -> ChoiceT< ^Writer<'Monoid,Choice<unit,'E>>>
                         when (Control.Return or
                                ^Writer<'Monoid,Choice<unit,'E>>) :
                                (static member Return:
                                    ^Writer<'Monoid,Choice<unit,'E>> *
                                   Control.Return
                                     -> (Choice<'a,'b>
                                           ->  ^Writer<'Monoid,Choice<unit,'E>>)) and
                              (Control.Bind or  ^c or
                                ^Writer<'Monoid,Choice<unit,'E>>) :
                                (static member (>>=) :
                                    ^c *
                                   ('a ->  ^Writer<'Monoid,Choice<unit,'E>>)
                                     ->  ^Writer<'Monoid,Choice<unit,'E>>) and
                              (Control.Map or  ^c or
                                ^Writer<'Monoid,Choice<unit,'E>>) :
                                (static member Map:
                                   ( ^c * ('d -> Choice<'d,'e>)) * Control.Map
                                     ->  ^Writer<'Monoid,Choice<unit,'E>>) and
                               ^c: (static member Tell: 'Monoid ->  ^c)
        
        static member
          inline Throw: x: 'E -> ChoiceT< ^Monad<Choice<'T,'E>>>
                          when (Control.Return or  ^Monad<Choice<'T,'E>>) :
                                 (static member Return:
                                     ^Monad<Choice<'T,'E>> * Control.Return
                                      -> (Choice<'a,'E>
                                            ->  ^Monad<Choice<'T,'E>>))
        
        static member
          inline get_Ask: unit -> ChoiceT< ^MonadReader<'R,Choice<'R,'E>>>
                            when (Control.Return or
                                   ^MonadReader<'R,Choice<'R,'E>>) :
                                   (static member Return:
                                       ^MonadReader<'R,Choice<'R,'E>> *
                                      Control.Return
                                        -> (Choice<'a,'b>
                                              ->  ^MonadReader<'R,Choice<'R,'E>>)) and
                                 (Control.Bind or  ^c or
                                   ^MonadReader<'R,Choice<'R,'E>>) :
                                   (static member (>>=) :
                                       ^c *
                                      ('a ->  ^MonadReader<'R,Choice<'R,'E>>)
                                        ->  ^MonadReader<'R,Choice<'R,'E>>) and
                                 (Control.Map or  ^c or
                                   ^MonadReader<'R,Choice<'R,'E>>) :
                                   (static member Map:
                                      ( ^c * ('d -> Choice<'d,'e>)) *
                                      Control.Map
                                        ->  ^MonadReader<'R,Choice<'R,'E>>) and
                                  ^c: (static member get_Ask: ->  ^c)
        
        static member
          inline get_Get: unit -> ChoiceT< ^MonadState<'S, Choice<_, 'E>>>
                            when (Control.Return or
                                   ^MonadState<'S, Choice<_, 'E>>) :
                                   (static member Return:
                                       ^MonadState<'S, Choice<_, 'E>> *
                                      Control.Return
                                        -> (Choice<'a,'b>
                                              ->  ^MonadState<'S, Choice<_, 'E>>)) and
                                 (Control.Bind or  ^c or
                                   ^MonadState<'S, Choice<_, 'E>>) :
                                   (static member (>>=) :
                                       ^c *
                                      ('a ->  ^MonadState<'S, Choice<_, 'E>>)
                                        ->  ^MonadState<'S, Choice<_, 'E>>) and
                                 (Control.Map or  ^c or
                                   ^MonadState<'S, Choice<_, 'E>>) :
                                   (static member Map:
                                      ( ^c * ('d -> Choice<'d,'e>)) *
                                      Control.Map
                                        ->  ^MonadState<'S, Choice<_, 'E>>) and
                                  ^c: (static member get_Get: ->  ^c)
    
    module ChoiceT =
        
        val run: ChoiceT<'Monad<Choice<'T,'E>>> -> 'Monad<Choice<'T,'E>>
        
        /// Embed a Monad<'T> into a ChoiceT<'Monad<Choice<'T,'Error>>>
        val inline lift:
          x:  ^Monad<'T> -> ChoiceT< ^Monad<Choice<'T,'Error>>>
            when (Control.Bind or  ^Monad<'T> or  ^Monad<Choice<'T,'Error>>) :
                   (static member (>>=) :
                       ^Monad<'T> * ('a ->  ^Monad<Choice<'T,'Error>>)
                        ->  ^Monad<Choice<'T,'Error>>) and
                 (Control.Map or  ^Monad<'T> or  ^Monad<Choice<'T,'Error>>) :
                   (static member Map:
                      ( ^Monad<'T> * ('c -> Choice<'c,'d>)) * Control.Map
                        ->  ^Monad<Choice<'T,'Error>>) and
                 (Control.Return or  ^Monad<Choice<'T,'Error>>) :
                   (static member Return:
                       ^Monad<Choice<'T,'Error>> * Control.Return
                        -> (Choice<'a,'b> ->  ^Monad<Choice<'T,'Error>>))
        
        /// Transform a Choice<'T,'TError> to a ChoiceT<'Monad<Choice<'T,'TError>>>
        val inline hoist:
          x: Choice<'T,'TError> -> ChoiceT< ^Monad<Choice<'T,'TError>>>
            when (Control.Return or  ^Monad<Choice<'T,'TError>>) :
                   (static member Return:
                       ^Monad<Choice<'T,'TError>> * Control.Return
                        -> (Choice<'T,'TError> ->  ^Monad<Choice<'T,'TError>>))
        
        val inline bind:
          f: ('T -> ChoiceT< ^Monad<ChoiceT<'U,'E>>>)
          -> ChoiceT< ^Monad<Choice<'T,'E>>> -> ChoiceT< ^Monad<ChoiceT<'U,'E>>>
            when (Control.Return or  ^Monad<ChoiceT<'U,'E>>) :
                   (static member Return:
                       ^Monad<ChoiceT<'U,'E>> * Control.Return
                        -> (Choice<'a,'b> ->  ^Monad<ChoiceT<'U,'E>>)) and
                 (Control.Bind or  ^Monad<Choice<'T,'E>> or
                   ^Monad<ChoiceT<'U,'E>>) :
                   (static member (>>=) :
                       ^Monad<Choice<'T,'E>> *
                      (Choice<'T,'b> ->  ^Monad<ChoiceT<'U,'E>>)
                        ->  ^Monad<ChoiceT<'U,'E>>)
        
        val inline apply:
          ChoiceT< ^Monad<Choice<('T -> 'U),'E>>>
          -> ChoiceT< ^Monad<Choice<'T,'E>>> -> ChoiceT< ^Monad<Choice<'U,'E>>>
            when (Control.Map or  ^Monad<Choice<('T -> 'U),'E>> or  ^a) :
                   (static member Map:
                      ( ^Monad<Choice<('T -> 'U),'E>> *
                       (Choice<('b -> 'c),'d> -> Choice<'b,'d> -> Choice<'c,'d>)) *
                      Control.Map ->  ^a) and
                 (Control.Apply or  ^a or  ^Monad<Choice<'T,'E>> or
                   ^Monad<Choice<'U,'E>>) :
                   (static member ``<*>`` :
                       ^a *  ^Monad<Choice<'T,'E>> *  ^Monad<Choice<'U,'E>> *
                      Control.Apply ->  ^Monad<Choice<'U,'E>>)
        
        val inline map:
          f: ('T -> 'U) -> ChoiceT< ^Monad<Choice<'T,'E>>>
            -> ChoiceT< ^Monad<Choice<('T -> 'U),'E>>>
            when (Control.Map or  ^Monad<Choice<'T,'E>> or
                   ^Monad<Choice<('T -> 'U),'E>>) :
                   (static member Map:
                      ( ^Monad<Choice<'T,'E>> * (Choice<'T,'a> -> Choice<'U,'a>)) *
                      Control.Map ->  ^Monad<Choice<('T -> 'U),'E>>)
        
        val inline map2:
          f: ('T -> 'U -> 'V) -> ChoiceT< ^Monad<Choice<'T,'E>>>
          -> ChoiceT< ^Monad<Choice<'U,'E>>> -> ChoiceT< ^Monad<Choice<'V,'E>>>
            when (Control.Lift2 or  ^Monad<Choice<'T,'E>> or
                   ^Monad<Choice<'U,'E>> or  ^Monad<Choice<'V,'E>>) :
                   (static member Lift2:
                      (Choice<'T,'a> -> Choice<'U,'a> -> Choice<'V,'a>) *
                      ( ^Monad<Choice<'T,'E>> *  ^Monad<Choice<'U,'E>>) *
                      Control.Lift2 ->  ^Monad<Choice<'V,'E>>)

