namespace FSharpPlus.Control
    
    [<Class>]
    type Invoke =
        inherit Internals.Default1
        
        static member
          inline Invoke: f:  ^Category<'T,'U> * x:  ^T ->  ^U
                           when (Invoke or  ^T) :
                                  (static member Invoke:
                                      ^Category<'T,'U> *  ^T *  ^U * Invoke
                                       ->  ^U)
        
        static member
          Invoke: g: System.Func<'T,'U> * x: 'T * _output: 'U * _mthd: Invoke
                    -> 'U
        
        static member
          Invoke: g: ('T -> 'U) * x: 'T * _output: 'U * _mthd: Invoke -> 'U
        
        static member
          inline Invoke:  ^T * x: 'a * _output:  ^O * _mthd: Internals.Default1
                           -> 'b when  ^T: (static member Invoke: 'a -> 'b)
        
        static member
          inline Invoke:  ^t * 'a * _output:  ^O * _mthd: Internals.Default1
                           -> ('b -> 'b) when  ^t: null and  ^t: struct
        
        static member
          inline InvokeNRTC: f:  ^Category<'T,'U> * x:  ^T -> 'a
                               when ( ^Category<'T,'U> or  ^T) :
                                      (static member Invoke:  ^T -> 'a)
    
    [<Class>]
    type ComposedStaticInvokable< ^F, ^G> =
        
        static member
          inline Invoke: x:  ^a ->  ^c
                           when (Invoke or  ^a) :
                                  (static member Invoke:
                                      ^G *  ^a *  ^b * Invoke ->  ^b) and
                                (Invoke or  ^b) :
                                  (static member Invoke:
                                      ^F *  ^b *  ^c * Invoke ->  ^c)

