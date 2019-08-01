namespace FSharpPlus

open FSharpPlus.Internals

[<AutoOpen>]
module Memoization =

    // From https://gist.github.com/gusty/70e5af737f2f6aed2bc0303a2e17c7d7

    open System.Collections.Concurrent

    // Key wrapper to allow null values, since dict keys can't be null
    [<Struct>]
    type MemoizationKeyWrapper<'a> = MemoizationKeyWrapper of 'a

    type MemoizeN = MemoizeN with
        static member getOrAdd (cd: ConcurrentDictionary<MemoizationKeyWrapper<'a>,'b>) (f: 'a -> 'b) k =
            cd.GetOrAdd (MemoizationKeyWrapper k, (fun (MemoizationKeyWrapper x) -> x) >> f)

    let inline memoizeN (f:'``(T1 -> T2 -> ... -> Tn)``): '``(T1 -> T2 -> ... -> Tn)`` =
        let inline call_2 (a: ^MemoizeN, b: ^b) = ((^MemoizeN or ^b) : (static member MemoizeN : ^MemoizeN * 'b -> _ ) (a, b))
        call_2 (Unchecked.defaultof<MemoizeN>, Unchecked.defaultof<'``(T1 -> T2 -> ... -> Tn)``>) f

    type MemoizeN with
        static member inline Invoke (f:'``(T1 -> T2 -> ... -> Tn)``): '``(T1 -> T2 -> ... -> Tn)`` =
            let inline call_2 (a: ^MemoizeN, b: ^b) = ((^MemoizeN or ^b) : (static member MemoizeN : ^MemoizeN * 'b -> _ ) (a, b))
            call_2 (Unchecked.defaultof<MemoizeN>, Unchecked.defaultof<'``(T1 -> T2 -> ... -> Tn)``>) f

    type MemoizeN with
        static member        MemoizeN (_: Default1, _:   'a -> 'b) = MemoizeN.getOrAdd (ConcurrentDictionary ())
        static member inline MemoizeN (MemoizeN, _:'t -> 'a -> 'b) = MemoizeN.getOrAdd (ConcurrentDictionary ()) << (<<) MemoizeN.Invoke
