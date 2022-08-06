namespace FSharpPlus.Control
    
    [<Class>]
    type Bitraverse =
        inherit Internals.Default1
        
        static member
          inline Bitraverse: 't * 'a * 'b * Internals.Default1 -> ('c -> 'c)
                               when 't: null and 't: struct
        
        static member
          inline Bitraverse: x:  ^Bitraversable<'T1,'U1> *
                             f: ('T1 -> 'Functor<'T2>) *
                             g: ('U1 -> 'Functor<'U2>) *
                             _impl: Internals.Default1
                               -> 'Functor<'Bitraversable<'T2,'U2>>
                               when  ^Bitraversable<'T1,'U1> :
                                      (static member Bitraverse:
                                          ^Bitraversable<'T1,'U1> *
                                         ('T1 -> 'Functor<'T2>) *
                                         ('U1 -> 'Functor<'U2>)
                                           -> 'Functor<'Bitraversable<'T2,'U2>>)
        
        static member
          inline Bitraverse: x:  ^Bitraversable<'T1,'U1> *
                             f: ('T1 -> 'Functor<'T2>) *
                             g: ('U1 -> 'Functor<'U2>) *
                             _impl: Internals.Default2
                               -> 'Functor<'Bitraversable<'T2,'U2>>
                               when  ^Bitraversable<'T1,'U1> :
                                      (static member Bimap:
                                          ^Bitraversable<'T1,'U1> *
                                         ('T1 -> 'Functor<'T2>) *
                                         ('U1 -> 'Functor<'U2>) ->  ^a) and
                                     ^a:
                                      (static member Bisequence:
                                          ^a
                                           -> 'Functor<'Bitraversable<'T2,'U2>>)
        
        static member
          inline Bitraverse: ('T1 * 'U1) * f: ('T1 ->  ^Functor<'T2>) *
                             g: ('U1 ->  ^Functor<'U2>) * _impl: Bitraverse
                               ->  ^Functor<'T2 * 'U2>
                               when (Lift2 or  ^Functor<'T2> or  ^Functor<'U2> or
                                      ^Functor<'T2 * 'U2>) :
                                      (static member Lift2:
                                         ('a -> 'b -> 'a * 'b) *
                                         ( ^Functor<'T2> *  ^Functor<'U2>) *
                                         Lift2 ->  ^Functor<'T2 * 'U2>)
        
        static member
          inline Bitraverse: x: Choice<'T1,'Error1> *
                             f: ('Error1 ->  ^Functor<'Error2>) *
                             g: ('T1 ->  ^Functor<'T2>) * _impl: Bitraverse
                               ->  ^Functor<Choice<'Error2,'T2>>
                               when (Map or  ^Functor<'Error2> or
                                      ^Functor<Choice<'Error2,'T2>>) :
                                      (static member Map:
                                         ( ^Functor<'Error2> *
                                          ('T2 -> Choice<'Error2,'T2>)) * Map
                                           ->  ^Functor<Choice<'Error2,'T2>>) and
                                    (Map or  ^Functor<'T2> or
                                      ^Functor<Choice<'Error2,'T2>>) :
                                      (static member Map:
                                         ( ^Functor<'T2> *
                                          ('Error2 -> Choice<'Error2,'T2>)) *
                                         Map ->  ^Functor<Choice<'Error2,'T2>>)
        
        static member
          inline Bitraverse: x: Result<'T1,'Error1> *
                             f: ('Error1 ->  ^Functor<'Error2>) *
                             g: ('T1 ->  ^Functor<'T2>) * _impl: Bitraverse
                               ->  ^Functor<Result<'Error2,'T2>>
                               when (Map or  ^Functor<'Error2> or
                                      ^Functor<Result<'Error2,'T2>>) :
                                      (static member Map:
                                         ( ^Functor<'Error2> *
                                          ('T2 -> Result<'Error2,'T2>)) * Map
                                           ->  ^Functor<Result<'Error2,'T2>>) and
                                    (Map or  ^Functor<'T2> or
                                      ^Functor<Result<'Error2,'T2>>) :
                                      (static member Map:
                                         ( ^Functor<'T2> *
                                          ('Error2 -> Result<'Error2,'T2>)) *
                                         Map ->  ^Functor<Result<'Error2,'T2>>)
        
        static member
          inline Invoke: f: ('T1 -> 'Functor<'T2>) -> g: ('U1 -> 'Functor<'U2>)
                         -> source:  ^Bitraversable<'T1,'U1>
                           ->  ^Functor<'Bitraversable<'T2,'U2>>
                           when (Bitraverse or  ^Bitraversable<'T1,'U1> or
                                  ^Functor<'Bitraversable<'T2,'U2>>) :
                                  (static member Bitraverse:
                                      ^Bitraversable<'T1,'U1> *
                                     ('T1 -> 'Functor<'T2>) *
                                     ('U1 -> 'Functor<'U2>) * Bitraverse
                                       ->  ^Functor<'Bitraversable<'T2,'U2>>)
        
        static member
          inline InvokeOnInstance: f: ('T1 -> 'Functor<'T2>)
                                   -> g: ('U1 -> 'Functor<'U2>)
                                   -> source:  ^Bitraversable<'T1,'U1>
                                     -> 'Functor<'Bitraversable<'T2,'U2>>
                                     when  ^Bitraversable<'T1,'U1> :
                                            (static member Bitraverse:
                                                ^Bitraversable<'T1,'U1> *
                                               ('T1 -> 'Functor<'T2>) *
                                               ('U1 -> 'Functor<'U2>)
                                                 -> 'Functor<'Bitraversable<'T2,'U2>>)
    
    [<Class>]
    type Bisequence =
        inherit Internals.Default1
        
        static member
          inline Bisequence: 't * Internals.Default1 -> unit
                               when 't: null and 't: struct
        
        static member
          inline Bisequence: x:  ^Bitraversable<'Functor<'T>,'Functor<'U>> *
                             _impl: Internals.Default1
                               -> 'Functor<'Bitraversable<'T,'U>>
                               when  ^Bitraversable<'Functor<'T>,'Functor<'U>> :
                                      (static member Bisequence:
                                          ^Bitraversable<'Functor<'T>,'Functor<'U>>
                                           -> 'Functor<'Bitraversable<'T,'U>>)
        
        static member
          inline Bisequence: x:  ^Bitraversable<'Functor<'T>,'Functor<'U>> *
                             _impl: Internals.Default2
                               -> 'Functor<'Bitraversable<'T,'U>>
                               when  ^Bitraversable<'Functor<'T>,'Functor<'U>> :
                                      (static member Bitraverse:
                                          ^Bitraversable<'Functor<'T>,'Functor<'U>> *
                                         ('a -> 'a) * ('b -> 'b)
                                           -> 'Functor<'Bitraversable<'T,'U>>)
        
        static member
          inline Bisequence: ( ^Functor<'T> *  ^Functor<'U>) * _impl: Bisequence
                               ->  ^Functor<'T2 * 'U>
                               when (Lift2 or  ^Functor<'T> or  ^Functor<'U> or
                                      ^Functor<'T2 * 'U>) :
                                      (static member Lift2:
                                         ('a -> 'b -> 'a * 'b) *
                                         ( ^Functor<'T> *  ^Functor<'U>) * Lift2
                                           ->  ^Functor<'T2 * 'U>)
        
        static member
          inline Bisequence: x: Choice< ^Functor<'Error>, ^Functor<'T>> *
                             _impl: Bisequence ->  ^Functor<Choice<'Error,'T>>
                               when (Map or  ^Functor<'Error> or
                                      ^Functor<Choice<'Error,'T>>) :
                                      (static member Map:
                                         ( ^Functor<'Error> *
                                          ('Error -> Choice<'Error,'T>)) * Map
                                           ->  ^Functor<Choice<'Error,'T>>) and
                                    (Map or  ^Functor<'T> or
                                      ^Functor<Choice<'Error,'T>>) :
                                      (static member Map:
                                         ( ^Functor<'T> *
                                          ('T -> Choice<'Error,'T>)) * Map
                                           ->  ^Functor<Choice<'Error,'T>>)
        
        static member
          inline Bisequence: x: Result< ^Functor<'Error>, ^Functor<'T>> *
                             _impl: Bisequence ->  ^Functor<Result<'Error,'T>>
                               when (Map or  ^Functor<'Error> or
                                      ^Functor<Result<'Error,'T>>) :
                                      (static member Map:
                                         ( ^Functor<'Error> *
                                          ('Error -> Result<'Error,'T>)) * Map
                                           ->  ^Functor<Result<'Error,'T>>) and
                                    (Map or  ^Functor<'T> or
                                      ^Functor<Result<'Error,'T>>) :
                                      (static member Map:
                                         ( ^Functor<'T> *
                                          ('T -> Result<'Error,'T>)) * Map
                                           ->  ^Functor<Result<'Error,'T>>)
        
        static member
          inline Invoke: source:  ^Bitraversable<'Functor<'T>,'Functor<'U>>
                           ->  ^Functor<'Bitraversable<'T,'U>>
                           when (Bisequence or
                                  ^Bitraversable<'Functor<'T>,'Functor<'U>> or
                                  ^Functor<'Bitraversable<'T,'U>>) :
                                  (static member Bisequence:
                                      ^Bitraversable<'Functor<'T>,'Functor<'U>> *
                                     Bisequence
                                       ->  ^Functor<'Bitraversable<'T,'U>>)
        
        static member
          inline InvokeOnInstance: source:  ^Bitraversable<'Functor<'T>,'Functor<'U>>
                                     -> 'Functor<'Bitraversable<'T,'U>>
                                     when  ^Bitraversable<'Functor<'T>,'Functor<'U>> :
                                            (static member Bisequence:
                                                ^Bitraversable<'Functor<'T>,'Functor<'U>>
                                                 -> 'Functor<'Bitraversable<'T,'U>>)

