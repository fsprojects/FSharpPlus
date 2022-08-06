namespace FSharpPlus.Control
    
    [<Class>]
    type App =
        inherit Internals.Default1
        
        static member
          inline App: _output:  ^t * _mthd: Internals.Default1 -> ('a -> 'a)
                        when  ^t: null and  ^t: struct
        
        static member
          inline App: _output:  ^ArrowApply<('ArrowApply<'T,'U> * 'T)>,'U)> *
                      _mthd: Internals.Default1
                        ->  ^ArrowApply<('ArrowApply<'T,'U> * 'T)>,'U)>
                        when  ^ArrowApply<('ArrowApply<'T,'U> * 'T)>,'U)> :
                               (static member get_App:
                                  ->  ^ArrowApply<('ArrowApply<'T,'U> * 'T)>,'U)>)
        
        static member
          App: _output: System.Func<(System.Func<'T,'U> * 'T),'U> * _mthd: App
                 -> System.Func<(System.Func<'T,'U> * 'T),'U>
        
        static member
          App: _output: (('T -> 'U) * 'T -> 'U) * _mthd: App
                 -> (('T -> 'U) * 'T -> 'U)
        
        static member
          inline Invoke: unit ->  ^ArrowApply<('ArrowApply<'T,'U> * 'T)>,'U)>
                           when (App or
                                  ^ArrowApply<('ArrowApply<'T,'U> * 'T)>,'U)>) :
                                  (static member App:
                                      ^ArrowApply<('ArrowApply<'T,'U> * 'T)>,'U)> *
                                     App
                                       ->  ^ArrowApply<('ArrowApply<'T,'U> * 'T)>,'U)>)
        
        static member
          inline InvokeOnInstance: unit
                                     ->  ^ArrowApply<('ArrowApply<'T,'U> * 'T)>,'U)>
                                     when  ^ArrowApply<('ArrowApply<'T,'U> * 'T)>,'U)> :
                                            (static member get_App:
                                               ->  ^ArrowApply<('ArrowApply<'T,'U> * 'T)>,'U)>)

