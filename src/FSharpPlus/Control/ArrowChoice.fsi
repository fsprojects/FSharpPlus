namespace FSharpPlus.Control
    
    [<Class>]
    type Fanin =
        inherit Internals.Default1
        
        static member
          inline Invoke: f: 'ArrowChoice<'T,'V> -> g: 'ArrowChoice<'U,'V>
                           ->  ^ArrowChoice<Choice<'U,'T>,'V>
                           when (Fanin or  ^ArrowChoice<Choice<'U,'T>,'V>) :
                                  (static member ``|||`` :
                                     'ArrowChoice<'T,'V> * 'ArrowChoice<'U,'V> *
                                      ^ArrowChoice<Choice<'U,'T>,'V> * Fanin
                                       ->  ^ArrowChoice<Choice<'U,'T>,'V>)
        
        static member
          inline InvokeOnInstance: f: 'ArrowChoice<'T,'V>
                                   -> g: 'ArrowChoice<'U,'V>
                                     ->  ^ArrowChoice<Choice<'U,'T>,'V>
                                     when  ^ArrowChoice<Choice<'U,'T>,'V> :
                                            (static member (|||) :
                                               'ArrowChoice<'T,'V> *
                                               'ArrowChoice<'U,'V>
                                                 ->  ^ArrowChoice<Choice<'U,'T>,'V>)
        
        static member
          inline ``|||`` : 'ArrowChoice<'T,'V> * 'ArrowChoice<'U,'V> *
                           _output:  ^t * _mthd: Internals.Default1
                             -> ('a -> 'a) when  ^t: null and  ^t: struct
        
        static member
          inline ``|||`` : f: 'ArrowChoice<'T,'V> * g: 'ArrowChoice<'U,'V> *
                           _output:  ^ArrowChoice<Choice<'U,'T>,'V> *
                           _mthd: Internals.Default1
                             ->  ^ArrowChoice<Choice<'U,'T>,'V>
                             when  ^ArrowChoice<Choice<'U,'T>,'V> :
                                    (static member (|||) :
                                       'ArrowChoice<'T,'V> * 'ArrowChoice<'U,'V>
                                         ->  ^ArrowChoice<Choice<'U,'T>,'V>)
        
        static member
          ``|||`` : f: System.Func<'T,'V> * g: System.Func<'U,'V> *
                    _output: System.Func<Choice<'U,'T>,'V> * _mthd: Fanin
                      -> System.Func<Choice<'U,'T>,'V>
        
        static member
          ``|||`` : f: ('T -> 'V) * g: ('U -> 'V) *
                    _output: (Choice<'U,'T> -> 'V) * _mthd: Fanin
                      -> (Choice<'U,'T> -> 'V)
    
    [<Class>]
    type AcMerge =
        inherit Internals.Default1
        
        static member
          inline Invoke: f: 'ArrowChoice<'T1,'U1> -> g: 'ArrowChoice<'T2,'U2>
                           ->  ^ArrowChoice<Choice<'T2,'T1>,Choice<'U2,'U1>>
                           when (AcMerge or
                                  ^ArrowChoice<Choice<'T2,'T1>,Choice<'U2,'U1>>) :
                                  (static member ``+++`` :
                                     'ArrowChoice<'T1,'U1> *
                                     'ArrowChoice<'T2,'U2> *
                                      ^ArrowChoice<Choice<'T2,'T1>,Choice<'U2,'U1>> *
                                     AcMerge
                                       ->  ^ArrowChoice<Choice<'T2,'T1>,Choice<'U2,'U1>>)
        
        static member
          inline InvokeOnInstance: f: 'ArrowChoice<'T1,'U1>
                                   -> g: 'ArrowChoice<'T2,'U2>
                                     ->  ^ArrowChoice<Choice<'T2,'T1>,Choice<'U2,'U1>>
                                     when  ^ArrowChoice<Choice<'T2,'T1>,Choice<'U2,'U1>> :
                                            (static member (+++) :
                                               'ArrowChoice<'T1,'U1> *
                                               'ArrowChoice<'T2,'U2>
                                                 ->  ^ArrowChoice<Choice<'T2,'T1>,Choice<'U2,'U1>>)
        
        static member
          inline ``+++`` : 'ArrowChoice<'T1,'U1> * 'ArrowChoice<'T2,'U2> *
                           _output:  ^t * _mthd: Internals.Default1
                             -> ('a -> 'a) when  ^t: null and  ^t: struct
        
        static member
          inline ``+++`` : f: 'ArrowChoice<'T1,'U1> * g: 'ArrowChoice<'T2,'U2> *
                           _output:  ^ArrowChoice<Choice<'T2,'T1>,Choice<'U2,'U1>> *
                           _mthd: Internals.Default1
                             ->  ^ArrowChoice<Choice<'T2,'T1>,Choice<'U2,'U1>>
                             when  ^ArrowChoice<Choice<'T2,'T1>,Choice<'U2,'U1>> :
                                    (static member (+++) :
                                       'ArrowChoice<'T1,'U1> *
                                       'ArrowChoice<'T2,'U2>
                                         ->  ^ArrowChoice<Choice<'T2,'T1>,Choice<'U2,'U1>>)
        
        static member
          ``+++`` : f: System.Func<'T1,'U1> * g: System.Func<'T2,'U2> *
                    _output: System.Func<Choice<'T2,'T1>,Choice<'U2,'U1>> *
                    _mthd: AcMerge
                      -> System.Func<Choice<'T2,'T1>,Choice<'U2,'U1>>
        
        static member
          ``+++`` : f: ('T1 -> 'U1) * g: ('T2 -> 'U2) *
                    _output: (Choice<'T2,'T1> -> Choice<'U2,'U1>) *
                    _mthd: AcMerge -> (Choice<'T2,'T1> -> Choice<'U2,'U1>)
    
    [<Class>]
    type AcLeft =
        inherit Internals.Default1
        
        static member
          inline Invoke: f:  ^ArrowChoice<'T,'U>
                           ->  ^ArrowChoice<Choice<'V,'T>,Choice<'V,'U>>
                           when (AcLeft or  ^ArrowChoice<'T,'U> or
                                  ^ArrowChoice<Choice<'V,'T>,Choice<'V,'U>>) :
                                  (static member Left:
                                      ^ArrowChoice<'T,'U> *
                                      ^ArrowChoice<Choice<'V,'T>,Choice<'V,'U>> *
                                     AcLeft
                                       ->  ^ArrowChoice<Choice<'V,'T>,Choice<'V,'U>>)
        
        static member
          inline InvokeOnInstance: f:  ^ArrowChoice<'T,'U>
                                     ->  ^ArrowChoice<Choice<'V,'T>,Choice<'V,'U>>
                                     when ( ^ArrowChoice<'T,'U> or
                                            ^ArrowChoice<Choice<'V,'T>,Choice<'V,'U>>) :
                                            (static member Left:
                                                ^ArrowChoice<'T,'U>
                                                 ->  ^ArrowChoice<Choice<'V,'T>,Choice<'V,'U>>)
        
        static member
          inline Left: 'ArrowChoice<'T,'U> * _output:  ^t *
                       _mthd: Internals.Default1 -> ('a -> 'a)
                         when  ^t: null and  ^t: struct
        
        static member
          inline Left: f:  ^ArrowChoice<'T,'U> *
                       _output:  ^ArrowChoice<Choice<'V,'T>,Choice<'V,'U>> *
                       _mthd: Internals.Default1
                         ->  ^ArrowChoice<Choice<'V,'T>,Choice<'V,'U>>
                         when ( ^ArrowChoice<'T,'U> or
                                ^ArrowChoice<Choice<'V,'T>,Choice<'V,'U>>) :
                                (static member Left:
                                    ^ArrowChoice<'T,'U>
                                     ->  ^ArrowChoice<Choice<'V,'T>,Choice<'V,'U>>)
        
        static member
          inline Left: f: System.Func<'T,'U> *
                       _output: System.Func<Choice<'V,'T>,Choice<'V,'U>> *
                       _mthd: AcLeft -> System.Func<Choice<'V,'T>,Choice<'V,'U>>
        
        static member
          inline Left: f: ('T -> 'U) * _output: (Choice<'V,'T> -> Choice<'V,'U>) *
                       _mthd: AcLeft -> (Choice<'V,'T> -> Choice<'V,'U>)
    
    [<Class>]
    type AcRight =
        inherit Internals.Default1
        
        static member
          inline Invoke: f:  ^ArrowChoice<'T,'U>
                           ->  ^ArrowChoice<Choice<'T,'V>,Choice<'U,'V>>
                           when (AcRight or  ^ArrowChoice<'T,'U> or
                                  ^ArrowChoice<Choice<'T,'V>,Choice<'U,'V>>) :
                                  (static member Right:
                                      ^ArrowChoice<'T,'U> *
                                      ^ArrowChoice<Choice<'T,'V>,Choice<'U,'V>> *
                                     AcRight
                                       ->  ^ArrowChoice<Choice<'T,'V>,Choice<'U,'V>>)
        
        static member
          inline InvokeOnInstance: f:  ^ArrowChoice<'T,'U>
                                     ->  ^ArrowChoice<Choice<'V,'T>,Choice<'U,'V>>
                                     when ( ^ArrowChoice<'T,'U> or
                                            ^ArrowChoice<Choice<'V,'T>,Choice<'U,'V>>) :
                                            (static member Right:
                                                ^ArrowChoice<'T,'U>
                                                 ->  ^ArrowChoice<Choice<'V,'T>,Choice<'U,'V>>)
        
        static member
          inline Right: 'ArrowChoice<'T,'U> * _output:  ^t *
                        _mthd: Internals.Default1 -> ('a -> 'a)
                          when  ^t: null and  ^t: struct
        
        static member
          inline Right: f:  ^ArrowChoice<'T,'U> *
                        _output:  ^ArrowChoice<Choice<'V,'T>,Choice<'U,'V>> *
                        _mthd: Internals.Default1
                          ->  ^ArrowChoice<Choice<'V,'T>,Choice<'U,'V>>
                          when ( ^ArrowChoice<'T,'U> or
                                 ^ArrowChoice<Choice<'V,'T>,Choice<'U,'V>>) :
                                 (static member Right:
                                     ^ArrowChoice<'T,'U>
                                      ->  ^ArrowChoice<Choice<'V,'T>,Choice<'U,'V>>)
        
        static member
          inline Right: f: System.Func<'T,'U> *
                        _output: System.Func<Choice<'T,'V>,Choice<'U,'V>> *
                        _mthd: AcRight
                          -> System.Func<Choice<'T,'V>,Choice<'U,'V>>
        
        static member
          inline Right: f: ('T -> 'U) *
                        _output: (Choice<'T,'V> -> Choice<'U,'V>) *
                        _mthd: AcRight -> (Choice<'T,'V> -> Choice<'U,'V>)

