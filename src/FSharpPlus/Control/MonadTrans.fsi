namespace FSharpPlus.Control
    
    [<Class>]
    type Lift =
        
        static member
          inline Invoke: x: 'Monad<'T> ->  ^MonadTrans<'Monad<'T>>
                           when  ^MonadTrans<'Monad<'T>> :
                                  (static member Lift:
                                     'Monad<'T> ->  ^MonadTrans<'Monad<'T>>)
    
    [<Class>]
    type LiftAsync =
        
        static member
          inline Invoke: x: Async<'T> ->  ^MonadAsync<'T>
                           when (LiftAsync or  ^MonadAsync<'T>) :
                                  (static member LiftAsync:
                                      ^MonadAsync<'T>
                                       -> (Async<'T> ->  ^MonadAsync<'T>))
        
        static member LiftAsync: Async<'T> -> (Async<'T> -> Async<'T>)
        
        static member
          inline LiftAsync:  ^t -> unit when  ^t: null and  ^t: struct
        
        static member
          inline LiftAsync:  ^R -> (Async<'T> ->  ^R)
                              when  ^R:
                                     (static member LiftAsync: Async<'T> ->  ^R)
    
    [<Class>]
    type Throw =
        
        static member
          inline Invoke: x: 'E ->  ^'MonadError<'E,'T>
                           when (Throw or  ^'MonadError<'E,'T>) :
                                  (static member Throw:
                                      ^'MonadError<'E,'T> * 'E
                                       ->  ^'MonadError<'E,'T>)
        
        static member Throw: Choice<'T,'E> * x: 'E -> Choice<'T,'E>
        
        static member Throw: Result<'T,'E> * x: 'E -> Result<'T,'E>
        
        static member
          inline Throw:  ^t * 'a -> ('b -> 'b) when  ^t: null and  ^t: struct
        
        static member
          inline Throw:  ^R * x: 'E ->  ^R
                          when  ^R: (static member Throw: 'E ->  ^R)
    
    [<Class>]
    type Catch =
        
        static member
          Catch: x: Choice<'a,'e1> * k: ('e1 -> Choice<'a,'e2>)
                   -> Choice<'a,'e2>
        
        static member
          Catch: x: Result<'a,'e1> * k: ('e1 -> Result<'a,'e2>)
                   -> Result<'a,'e2>
        
        static member
          inline Invoke: x:  ^MonadError<'E1,'T>
                         -> f: ('E1 ->  ^MonadError<'E2,'T>)
                           ->  ^MonadError<'E2,'T>
                           when (Catch or  ^MonadError<'E1,'T> or
                                  ^MonadError<'E2,'T>) :
                                  (static member Catch:
                                      ^MonadError<'E1,'T> *
                                     ('E1 ->  ^MonadError<'E2,'T>)
                                       ->  ^MonadError<'E2,'T>)
    
    [<Class>]
    type CallCC =
        
        static member
          inline Invoke: f: (('T -> 'MonadCont<'U>) ->  ^MonadCont<'T>)
                           ->  ^MonadCont<'T>
                           when  ^MonadCont<'T> :
                                  (static member CallCC:
                                     (('T -> 'MonadCont<'U>) ->  ^MonadCont<'T>)
                                       ->  ^MonadCont<'T>)
    
    [<Class>]
    type Get =
        
        static member
          inline Invoke: unit ->  ^MonadState<'S, 'S>
                           when  ^MonadState<'S, 'S> :
                                  (static member get_Get:
                                     ->  ^MonadState<'S, 'S>)
    
    [<Class>]
    type Put =
        
        static member
          inline Invoke: x: 'S ->  ^MonadState<'S, unit>
                           when  ^MonadState<'S, unit> :
                                  (static member Put:
                                     'S ->  ^MonadState<'S, unit>)
    
    [<Class>]
    type Ask =
        
        static member
          inline Invoke: unit ->  ^MonadReader<'R, 'T>
                           when  ^MonadReader<'R, 'T> :
                                  (static member get_Ask:
                                     ->  ^MonadReader<'R, 'T>)
    
    [<Class>]
    type Local =
        
        static member
          inline Invoke: f: ('R1 -> 'R2) -> m:  ^MonadReader<'R2, 'T>
                           ->  ^MonadReader<'R1, 'T>
                           when  ^MonadReader<'R1, 'T> :
                                  (static member Local:
                                      ^MonadReader<'R2, 'T> * ('R1 -> 'R2)
                                       ->  ^MonadReader<'R1, 'T>)
    
    [<Class>]
    type Tell =
        
        static member
          inline Invoke: w: 'Monoid ->  ^MonadWriter<'Monoid,unit>
                           when  ^MonadWriter<'Monoid,unit> :
                                  (static member Tell:
                                     'Monoid ->  ^MonadWriter<'Monoid,unit>)
    
    [<Class>]
    type Listen =
        
        static member
          inline Invoke: m: 'MonadWriter<'Monoid,'T>
                           ->  ^MonadWriter<'Monoid,('T * 'Monoid)>
                           when  ^MonadWriter<'Monoid,('T * 'Monoid)> :
                                  (static member Listen:
                                     'MonadWriter<'Monoid,'T>
                                       ->  ^MonadWriter<'Monoid,('T * 'Monoid)>)
    
    [<Class>]
    type Pass =
        
        static member
          inline Invoke: m: 'MonadWriter<'Monoid,('T * ('Monoid -> 'Monoid))>
                           ->  ^MonadWriter<'Monoid,'T>
                           when  ^MonadWriter<'Monoid,'T> :
                                  (static member Pass:
                                     'MonadWriter<'Monoid,('T * ('Monoid -> 'Monoid))>
                                       ->  ^MonadWriter<'Monoid,'T>)

