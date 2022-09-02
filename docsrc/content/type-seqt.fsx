(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.

#r @"../../src/FSharpPlus/bin/Release/net45/FSharpPlus.dll"

// For some reason AsyncDownloadString is not found during doc build. The following is a dumb implementation just to make the compiler happy.
// TODO find out why.

type System.Net.WebClient with member wc.AsyncDownloadString (uri: System.Uri) = async { return wc.DownloadString uri }

(**
SeqT&lt;Monad&lt;bool&gt;, 'T&gt;
=====================

This is the the Monad Transformer for `seq<'T>` so it adds sequencing to existing monads by composing then with `seq<'T>`.

Any monad can be composed, but a very typical usage is when combined with `Async` or `Task`, which gives rise to what's called async sequences.

Therefore the [AsyncSeq](https://github.com/fsprojects/FSharp.Control.AsyncSeq) library can be considered a specialization of this monad in Async.

The original post from AsyncSeq can be found [here](http://tomasp.net/blog/async-sequences.aspx) and we can run those examples with `SeqT` by adapting the code.

In order to do so we need to be aware of the design differences of both implementations.

<style>#fsdocs-content table, th, td{border: 1px solid black;border-collapse: collapse;}</style>

| **AsyncSeq**                  | **SeqT**                              | **Notes**	|
|:------------------------------|:--------------------------------------|:----------|
|`AsyncSeq<'T>`                 |`SeqT<Async<bool>, 'T>`                |           |
|`asyncSeq { .. }`              |`monad.plus { .. }`                    | At some point it needs to be inferred as `SeqT<Async<bool>, 'T>`, or it can be specified with type parameters: `monad<SeqT<Async<bool>, 'T>>.plus` |
|`let! x = y`                   |`let! x = SeqT.lift y`                 | No auto lifting. Lifting should be explicit. |
|`do! x`                        |`do! SeqT.lift x`                      | ''        |
|`for x in s`                   |`let! x = s`                           | When `s: AsyncSeq<'T>` otherwise `for` is still ok with regular sequences. |
|`AsyncSeq.[function]`          |`SeqT.[function]`                      | See differences in functions below. |
|`AsyncSeq.[function]Async`     |`SeqT.[function]M`                     | ''        |
|`AsyncSeq.skip`                |`SeqT.drop`                            | `.skip` is available but consistently with F# collections, it throws when the sequence doesn't have enough elements. |
|`AsyncSeq.take`                |`SeqT.truncate`                        | `.take` is available but consistently with F# collections, it throws when the sequence doesn't have enough elements. |
|`AsyncSeq.toBlockingSequence`  |`SeqT.run >> Async.RunSynchronously`   | Not really the same but semantically equivalent. |
|`AsyncSeq.toListAsync`         |`SeqT.runAsList`                       |           |
|`AsyncSeq.toArrayAsync`        |`SeqT.runAsArray`                      |           |
|`AsyncSeq.zipWith`             |`SeqT.map2`                            | Aligned with F# collections. |
|`AsyncSeq.zipWithAsync`        |`SeqT.map2M`                           |  ''       |
|`AsyncSeq.ofObservable`        |`Observable.toAsyncSeq`                |`.toTaskSeq` is also available. |
|`AsyncSeq.toObservable`        |`Observable.ofAsyncSeq`                |`.ofTaskSeq` is also available. |



Examples
--------
*)

#r "nuget: FSharpPlus"

open System
open System.Net
open FSharpPlus
open FSharpPlus.Data

let urls =
  [ "http://bing.com"; "http://yahoo.com";
    "http://google.com"; "http://msn.com"; ]

// Asynchronous sequence that returns URLs and lengths
// of the downloaded HTML. Web pages from a given list
// are downloaded asynchronously in sequence.
let pages: SeqT<_, _> = monad.plus {
    use wc = new WebClient ()
    for url in urls do
        try
            let! html = wc.AsyncDownloadString (Uri url) |> SeqT.lift
            yield url, html.Length
        with _ ->
            yield url, -1 }


// Print URL of pages that are smaller than 100k
let printPages =
    pages
    |> SeqT.filter (fun (_, len) -> len < 100000)
    |> SeqT.map fst
    |> SeqT.iter (printfn "%s")
 
printPages |> Async.Start

(**
To make it work with tasks simply add `|> Async.StartAsTask` between `wc.AsyncDownloadString (Uri url)` and `|> SeqT.lift` then run eveything but the `printPages |> Async.Start`.
*)
