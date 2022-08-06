namespace FSharpPlus.Control
    
    [<Class>]
    type Arr =
        inherit Internals.Default1
        
        static member
          inline Arr: ('T -> 'U) * _output:  ^t * _mthd: Internals.Default1
                        -> ('a -> 'a) when  ^t: null and  ^t: struct
        
        static member
          inline Arr: f: ('T -> 'U) * _output:  ^Arrow<'T,'U> *
                      _mthd: Internals.Default1 ->  ^Arrow<'T,'U>
                        when  ^Arrow<'T,'U> :
                               (static member Arr: ('T -> 'U) ->  ^Arrow<'T,'U>)
        
        static member
          Arr: f: ('T -> 'U) * _output: System.Func<'T,'U> * _mthd: Arr
                 -> System.Func<'T,'U>
        
        static member
          Arr: f: ('T -> 'U) * _output: ('T -> 'U) * _mthd: Arr -> ('T -> 'U)
        
        static member
          inline Invoke: f: ('T -> 'U) ->  ^Arrow<'T,'U>
                           when (Arr or  ^Arrow<'T,'U>) :
                                  (static member Arr:
                                     ('T -> 'U) *  ^Arrow<'T,'U> * Arr
                                       ->  ^Arrow<'T,'U>)
        
        static member
          inline InvokeOnInstance: f: ('T -> 'U) ->  ^Arrow<'T,'U>
                                     when  ^Arrow<'T,'U> :
                                            (static member Arr:
                                               ('T -> 'U) ->  ^Arrow<'T,'U>)
    
    [<Class>]
    type ArrFirst =
        inherit Internals.Default1
        
        static member
          inline First:  ^t * _output: 'a * _mthd: Internals.Default1
                          -> ('b -> 'b) when  ^t: null and  ^t: struct
        
        static member
          inline First: f:  ^Arrow<'T,'U> *
                        _output:  ^Arrow<('T * 'V),('U * 'V)> *
                        _mthd: Internals.Default1
                          ->  ^Arrow<('T * 'V),('U * 'V)>
                          when ( ^Arrow<'T,'U> or  ^Arrow<('T * 'V),('U * 'V)>) :
                                 (static member First:
                                     ^Arrow<'T,'U>
                                      ->  ^Arrow<('T * 'V),('U * 'V)>)
        
        static member
          First: f: System.Func<'T,'U> *
                 _output: System.Func<('T * 'V),('U * 'V)> * _mthd: ArrFirst
                   -> System.Func<('T * 'V),('U * 'V)>
        
        static member
          First: f: ('T -> 'U) * _output: ('T * 'V -> 'U * 'V) * _mthd: ArrFirst
                   -> ('T * 'V -> 'U * 'V)
        
        static member
          inline Invoke: f:  ^Arrow<'T,'U> ->  ^Arrow<('T * 'V),('U * 'V)>
                           when (ArrFirst or  ^Arrow<'T,'U> or
                                  ^Arrow<('T * 'V),('U * 'V)>) :
                                  (static member First:
                                      ^Arrow<'T,'U> *
                                      ^Arrow<('T * 'V),('U * 'V)> * ArrFirst
                                       ->  ^Arrow<('T * 'V),('U * 'V)>)
        
        static member
          inline InvokeOnInstance: f:  ^Arrow<'T,'U>
                                     ->  ^Arrow<('T * 'V),('U * 'V)>
                                     when ( ^Arrow<'T,'U> or
                                            ^Arrow<('T * 'V),('U * 'V)>) :
                                            (static member First:
                                                ^Arrow<'T,'U>
                                                 ->  ^Arrow<('T * 'V),('U * 'V)>)
    
    [<Class>]
    type ArrSecond =
        inherit Internals.Default1
        
        static member
          inline Invoke: f:  ^Arrow<'T,'U> ->  ^Arrow<('V * 'T),('V * 'U)>
                           when (ArrSecond or  ^Arrow<'T,'U> or
                                  ^Arrow<('V * 'T),('V * 'U)>) :
                                  (static member Second:
                                      ^Arrow<'T,'U> *
                                      ^Arrow<('V * 'T),('V * 'U)> * ArrSecond
                                       ->  ^Arrow<('V * 'T),('V * 'U)>)
        
        static member
          inline InvokeOnInstance: f:  ^Arrow<'T,'U>
                                     ->  ^Arrow<('V * 'T),('V * 'U)>
                                     when ( ^Arrow<'T,'U> or
                                            ^Arrow<('V * 'T),('V * 'U)>) :
                                            (static member Second:
                                                ^Arrow<'T,'U>
                                                 ->  ^Arrow<('V * 'T),('V * 'U)>)
        
        static member
          inline Second:  ^t * _output: 'a * _mthd: Internals.Default1
                           -> ('b -> 'b) when  ^t: null and  ^t: struct
        
        static member
          inline Second: f:  ^Arrow<'T,'U> *
                         _output:  ^Arrow<('V * 'T),('V * 'U)> *
                         _mthd: Internals.Default1
                           ->  ^Arrow<('V * 'T),('V * 'U)>
                           when ( ^Arrow<'T,'U> or  ^Arrow<('V * 'T),('V * 'U)>) :
                                  (static member Second:
                                      ^Arrow<'T,'U>
                                       ->  ^Arrow<('V * 'T),('V * 'U)>)
        
        static member
          inline Second: f:  ^Arrow<'T,'U> *
                         _output:  ^Arrow<('V * 'T),('V * 'U)> *
                         _mthd: Internals.Default2
                           ->  ^Arrow<('V * 'T),('V * 'U)>
                           when ( ^Arrow<'T,'U> or  ^a) :
                                  (static member First:  ^Arrow<'T,'U> ->  ^a) and
                                 ^Arrow<('V * 'T),('V * 'U)> :
                                  (static member (<<<) :
                                      ^b *  ^e ->  ^Arrow<('V * 'T),('V * 'U)>) and
                                 ^b:
                                  (static member Arr:
                                     ('c * 'd -> 'd * 'c) ->  ^b) and
                                 ^e: (static member (<<<) :  ^a *  ^b ->  ^e)
        
        static member
          Second: f: System.Func<'T,'U> *
                  _output: System.Func<('V * 'T),('V * 'U)> * _mthd: ArrSecond
                    -> System.Func<('V * 'T),('V * 'U)>
        
        static member
          Second: f: ('T -> 'U) * _output: ('V * 'T -> 'V * 'U) *
                  _mthd: ArrSecond -> ('V * 'T -> 'V * 'U)
    
    [<Class>]
    type ArrCombine =
        inherit Internals.Default1
        
        static member
          inline Invoke: f: 'Arrow<'T1,'U1> -> g: 'Arrow<'T2,'U2>
                           ->  ^Arrow<('T1 * 'T2),('U1 * 'U2)>
                           when (ArrCombine or  ^Arrow<('T1 * 'T2),('U1 * 'U2)>) :
                                  (static member ``***`` :
                                     'Arrow<'T1,'U1> * 'Arrow<'T2,'U2> *
                                      ^Arrow<('T1 * 'T2),('U1 * 'U2)> *
                                     ArrCombine
                                       ->  ^Arrow<('T1 * 'T2),('U1 * 'U2)>)
        
        static member
          inline InvokeOnInstance: f: 'Arrow<'T1,'U1> -> g: 'Arrow<'T2,'U2>
                                     ->  ^Arrow<('T1 * 'T2),('U1 * 'U2)>
                                     when  ^Arrow<('T1 * 'T2),('U1 * 'U2)> :
                                            (static member ``***`` :
                                               'Arrow<'T1,'U1> * 'Arrow<'T2,'U2>
                                                 ->  ^Arrow<('T1 * 'T2),('U1 * 'U2)>)
        
        static member
          inline ``***`` : 'Arrow<'T1,'U1> * 'Arrow<'T2,'U2> * _output:  ^t *
                           _mthd: Internals.Default1 -> ('a -> 'a)
                             when  ^t: null and  ^t: struct
        
        static member
          inline ``***`` : f: 'Arrow<'T1,'U1> * g: 'Arrow<'T2,'U2> *
                           _output:  ^Arrow<('T1 * 'T2),('U1 * 'U2)> *
                           _mthd: Internals.Default1
                             ->  ^Arrow<('T1 * 'T2),('U1 * 'U2)>
                             when  ^Arrow<('T1 * 'T2),('U1 * 'U2)> :
                                    (static member ``***`` :
                                       'Arrow<'T1,'U1> * 'Arrow<'T2,'U2>
                                         ->  ^Arrow<('T1 * 'T2),('U1 * 'U2)>)
        
        static member
          inline ``***`` : f:  ^Arrow<'T1,'U1> * g:  ^Arrow<'T2,'U2> *
                           _output:  ^Arrow<('T1 * 'T2),('U1 * 'U2)> *
                           _mthd: Internals.Default2
                             ->  ^Arrow<('T1 * 'T2),('U1 * 'U2)>
                             when ( ^Arrow<'T1,'U1> or  ^a) :
                                    (static member First:
                                        ^Arrow<'T1,'U1> ->  ^a) and
                                  ( ^Arrow<'T2,'U2> or  ^b) :
                                    (static member Second:
                                        ^Arrow<'T2,'U2> ->  ^b) and
                                   ^Arrow<('T1 * 'T2),('U1 * 'U2)> :
                                    (static member (<<<) :
                                        ^b *  ^a
                                         ->  ^Arrow<('T1 * 'T2),('U1 * 'U2)>)
        
        static member
          ``***`` : f: System.Func<'T1,'U1> * g: System.Func<'T2,'U2> *
                    _output: System.Func<('T1 * 'T2),('U1 * 'U2)> *
                    _mthd: ArrCombine -> System.Func<('T1 * 'T2),('U1 * 'U2)>
        
        static member
          ``***`` : f: ('T1 -> 'U1) * g: ('T2 -> 'U2) *
                    _output: ('T1 * 'T2 -> 'U1 * 'U2) * _mthd: ArrCombine
                      -> ('T1 * 'T2 -> 'U1 * 'U2)
    
    [<Class>]
    type Fanout =
        inherit Internals.Default1
        
        static member
          inline Invoke: f: 'Arrow<'T,'U1> -> g: 'Arrow<'T,'U2>
                           ->  ^Arrow<'T,('U1 * 'U2)>
                           when (Fanout or  ^Arrow<'T,('U1 * 'U2)>) :
                                  (static member ``&&&`` :
                                     'Arrow<'T,'U1> * 'Arrow<'T,'U2> *
                                      ^Arrow<'T,('U1 * 'U2)> * Fanout
                                       ->  ^Arrow<'T,('U1 * 'U2)>)
        
        static member
          inline InvokeOnInstance: f: 'Arrow<'T,'U1> -> g: 'Arrow<'T,'U2>
                                     ->  ^Arrow<'T,('U1 * 'U2)>
                                     when  ^Arrow<'T,('U1 * 'U2)> :
                                            (static member (&&&) :
                                               'Arrow<'T,'U1> * 'Arrow<'T,'U2>
                                                 ->  ^Arrow<'T,('U1 * 'U2)>)
        
        static member
          inline ``&&&`` : 'Arrow<'T,'U1> * 'Arrow<'T,'U2> * _output:  ^t *
                           _mthd: Internals.Default1 -> ('a -> 'a)
                             when  ^t: null and  ^t: struct
        
        static member
          inline ``&&&`` : f: 'Arrow<'T,'U1> * g: 'Arrow<'T,'U2> *
                           _output:  ^Arrow<'T,('U1 * 'U2)> *
                           _mthd: Internals.Default1 ->  ^Arrow<'T,('U1 * 'U2)>
                             when  ^Arrow<'T,('U1 * 'U2)> :
                                    (static member (&&&) :
                                       'Arrow<'T,'U1> * 'Arrow<'T,'U2>
                                         ->  ^Arrow<'T,('U1 * 'U2)>)
        
        static member
          inline ``&&&`` : f: 'Arrow<'T,'U1> * g: 'Arrow<'T,'U2> *
                           _output:  ^Arrow<'T,('U1 * 'U2)> *
                           _mthd: Internals.Default2 ->  ^Arrow<'T,('U1 * 'U2)>
                             when  ^Arrow<'T,('U1 * 'U2)> :
                                    (static member (<<<) :
                                        ^a *  ^b ->  ^Arrow<'T,('U1 * 'U2)>) and
                                   ^a:
                                    (static member ``***`` :
                                       'Arrow<'T,'U1> * 'Arrow<'T,'U2> ->  ^a) and
                                   ^b:
                                    (static member Arr: ('c -> 'c * 'c) ->  ^b)
        
        static member
          inline ``&&&`` : f:  ^Arrow<'T,'U1> * g:  ^Arrow<'T,'U2> *
                           _output:  ^Arrow<'T,('U1 * 'U2)> *
                           _mthd: Internals.Default3 ->  ^Arrow<'T,('U1 * 'U2)>
                             when ( ^Arrow<'T,'U1> or  ^a) :
                                    (static member First:  ^Arrow<'T,'U1> ->  ^a) and
                                  ( ^Arrow<'T,'U2> or  ^b) :
                                    (static member Second:
                                        ^Arrow<'T,'U2> ->  ^b) and
                                   ^Arrow<'T,('U1 * 'U2)> :
                                    (static member (<<<) :
                                        ^c *  ^d ->  ^Arrow<'T,('U1 * 'U2)>) and
                                   ^c: (static member (<<<) :  ^b *  ^a ->  ^c) and
                                   ^d:
                                    (static member Arr: ('e -> 'e * 'e) ->  ^d)
        
        static member
          ``&&&`` : f: System.Func<'T,'U1> * g: System.Func<'T,'U2> *
                    _output: System.Func<'T,('U1 * 'U2)> * _mthd: Fanout
                      -> System.Func<'T,('U1 * 'U2)>
        
        static member
          ``&&&`` : f: ('T -> 'U1) * g: ('T -> 'U2) * _output: ('T -> 'U1 * 'U2) *
                    _mthd: Fanout -> ('T -> 'U1 * 'U2)

