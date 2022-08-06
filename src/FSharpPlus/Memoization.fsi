namespace FSharpPlus
    
    module Memoization =
        
        [<Struct>]
        type MemoizationKeyWrapper<'a> = | MemoizationKeyWrapper of 'a
        
        [<Class>]
        type MemoizeN =
            inherit Internals.Default1
            
            static member
              inline MemoizeN: MemoizeN * ('t -> 'a -> 'b)
                                 -> (('a0 ->  ^b1) -> 'a0 ->  ^b1)
                                 when (MemoizeN or  ^b1) :
                                        (static member MemoizeN:
                                           MemoizeN *  ^b1 -> ( ^b1 ->  ^b1))
            
            static member
              MemoizeN: Internals.Default1 * ('a -> 'b)
                          -> (('a0 -> 'b1) -> 'a0 -> 'b1)
            
            static member
              getOrAdd: cd: System.Collections.Concurrent.ConcurrentDictionary<MemoizationKeyWrapper<'a>,
                                                                               'b>
                        -> f: ('a -> 'b) -> k: 'a -> 'b
        
        val inline memoizeN:
          f:  ^(T1 -> T2 -> ... -> Tn) ->  ^(T1 -> T2 -> ... -> Tn)
            when (MemoizeN or  ^(T1 -> T2 -> ... -> Tn)) :
                   (static member MemoizeN:
                      MemoizeN *  ^(T1 -> T2 -> ... -> Tn)
                        -> ( ^(T1 -> T2 -> ... -> Tn)
                              ->  ^(T1 -> T2 -> ... -> Tn)))

