namespace FSharpPlus.Control
    
    [<Class>]
    type Id =
        inherit Internals.Default1
        
        static member
          inline Id: _output:  ^t * _mthd: Internals.Default1 -> ('a -> 'a)
                       when  ^t: null and  ^t: struct
        
        static member
          inline Id: _output:  ^Category<'T,'T> * _mthd: Internals.Default1
                       ->  ^Category<'T,'T>
                       when  ^Category<'T,'T> :
                              (static member get_Id: ->  ^Category<'T,'T>)
        
        static member
          Id: _output: System.Func<'T,'T> * _mthd: Id -> System.Func<'T,'T>
        
        static member Id: _output: ('T -> 'T) * _mthd: Id -> ('T -> 'T)
        
        static member
          inline Invoke: unit ->  ^Category<'T,'T>
                           when (Id or  ^Category<'T,'T>) :
                                  (static member Id:
                                      ^Category<'T,'T> * Id ->  ^Category<'T,'T>)
        
        static member
          inline InvokeOnInstance: unit ->  ^Category<'T,'T>
                                     when  ^Category<'T,'T> :
                                            (static member get_Id:
                                               ->  ^Category<'T,'T>)
    
    [<Class>]
    type Comp =
        inherit Internals.Default1
        
        static member
          inline Invoke: f:  ^Category<'U,'V> -> g: 'Category<'T,'U>
                           -> 'Category<'T,'V>
                           when (Comp or  ^Category<'U,'V>) :
                                  (static member ``<<<`` :
                                      ^Category<'U,'V> * 'Category<'T,'U> * Comp *
                                     Comp -> 'Category<'T,'V>)
        
        static member
          inline InvokeOnInstance: f: 'Category<'U,'V> -> g: 'Category<'T,'U>
                                     ->  ^Category<'T,'V>
                                     when  ^Category<'T,'V> :
                                            (static member (<<<) :
                                               'Category<'U,'V> *
                                               'Category<'T,'U>
                                                 ->  ^Category<'T,'V>)
        
        static member
          inline InvokeOnInstance': f:  ^Category<'U,'V> -> g:  ^Category<'T,'U>
                                      -> 'Category<'T,'V>
                                      when ( ^Category<'U,'V> or
                                             ^Category<'T,'U>) :
                                             (static member (<<<) :
                                                 ^Category<'U,'V> *
                                                 ^Category<'T,'U>
                                                  -> 'Category<'T,'V>)
        
        static member
          inline ``<<<`` : f:  ^F * g:  ^G * 'd * _mthd: Internals.Default1
                             -> ComposedStaticInvokable< ^F, ^G>
                             when  ^F: (static member Invoke:  ^F * 'a -> 'b) and
                                   ^G: (static member Invoke:  ^G * 'b -> 'c)
        
        static member
          inline ``<<<`` : f:  ^Category<'U,'V> * g:  ^Category<'T,'U> *
                           _output: 'a * _mthd: Internals.Default1
                             -> 'Category<'T,'V>
                             when ( ^Category<'U,'V> or  ^Category<'T,'U>) :
                                    (static member (<<<) :
                                        ^Category<'U,'V> *  ^Category<'T,'U>
                                         -> 'Category<'T,'V>)
        
        static member
          ``<<<`` : f: System.Func<'U,'V> * g: System.Func<'T,'U> * _output: 'a *
                    _mthd: Comp -> System.Func<'T,'V>
        
        static member
          ``<<<`` : f: ('U -> 'V) * g: ('T -> 'U) * _output: 'a * _mthd: Comp
                      -> ('T -> 'V)

