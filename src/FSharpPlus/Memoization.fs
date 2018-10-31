namespace FSharpPlus


[<AutoOpen>]
module Memoization =

    // From https://gist.github.com/gusty/70e5af737f2f6aed2bc0303a2e17c7d7

    open System.Collections.Concurrent
  
    // Key wrapper to allow null values, since dict keys can't be null
    [<Struct>]
    type MemoizationKeyWrapper<'a> = MemoizationKeyWrapper of 'a

    type T = T with static member getOrAdd (cd:ConcurrentDictionary<MemoizationKeyWrapper<'a>,'b>) (f:'a -> 'b) k = 
      cd.GetOrAdd (MemoizationKeyWrapper k, (fun (MemoizationKeyWrapper x) -> x) >> f)

    let inline memoize (f:'``(T1 -> T2 -> ... -> Tn)``): '``(T1 -> T2 -> ... -> Tn)`` = (T $ Unchecked.defaultof<'``(T1 -> T2 -> ... -> Tn)``>) f

    type T with     
        static member        ($) (_:obj,  _: 'a -> 'b) = T.getOrAdd (ConcurrentDictionary ())
        static member inline ($) (T, _:'t -> 'a -> 'b) = T.getOrAdd (ConcurrentDictionary ()) << (<<) memoize
