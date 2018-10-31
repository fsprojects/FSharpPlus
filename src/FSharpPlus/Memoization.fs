namespace FSharpPlus


[<AutoOpen>]
module Memoization =

    // From https://gist.github.com/gusty/70e5af737f2f6aed2bc0303a2e17c7d7

    open System.Collections.Concurrent

    // Key wrapper to allow null values, since dict keys can't be null
    [<Struct>]
    type MemoizationKeyWrapper<'a> = MemoizationKeyWrapper of 'a

    type MemoizeConcurrentHelper = MemoizeConcurrentHelper with
        static member getOrAdd (cd:ConcurrentDictionary<MemoizationKeyWrapper<'a>,'b>) (f:'a -> 'b) k =
            cd.GetOrAdd (MemoizationKeyWrapper k, (fun (MemoizationKeyWrapper x) -> x) >> f)

    let inline memoize (f:'``(T1 -> T2 -> ... -> Tn)``): '``(T1 -> T2 -> ... -> Tn)`` =
        (MemoizeConcurrentHelper $ Unchecked.defaultof<'``(T1 -> T2 -> ... -> Tn)``>) f

    type MemoizeConcurrentHelper with
        static member ($) (_:obj,  _: 'a -> 'b) =
            MemoizeConcurrentHelper.getOrAdd (ConcurrentDictionary ())
        static member inline ($) (MemoizeConcurrentHelper, _:'t -> 'a -> 'b) =
            MemoizeConcurrentHelper.getOrAdd (ConcurrentDictionary ()) << (<<) memoize
