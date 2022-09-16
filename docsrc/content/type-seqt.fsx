(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.

#r @"../../src/FSharpPlus/bin/Release/netstandard2.0/FSharpPlus.dll"

// For some reason AsyncDownloadString is not found during doc build. The following is a dumb implementation just to make the compiler happy.
// TODO find out why.

type System.Net.WebClient with member wc.AsyncDownloadString (uri: System.Uri) = async { return wc.DownloadString uri }

(**
SeqT&lt;Monad&lt;bool&gt;, 'T&gt;
=====================

This is the the Monad Transformer for `seq<'T>` so it adds sequencing to existing monads by composing them with `seq<'T>`.

Any monad can be composed, but a very typical usage is when combined with `Async` or `Task`, which gives rise to what's called async sequences.

Therefore the [AsyncSeq](https://github.com/fsprojects/FSharp.Control.AsyncSeq) library can be considered a specialization of this monad in Async.

The original post from AsyncSeq can be found [here](http://tomasp.net/blog/async-sequences.aspx) and we can run those examples with `SeqT` by adapting the code.

In order to do so we need to be aware of the design differences of both implementations.

<style>
body #fsdocs-content table, th, td { border: 1px solid black;border-collapse: collapse; }
body #fsdocs-content table code { word-break: normal; }
</style>

| **AsyncSeq**                  | **SeqT**                              | **Notes**	|
|:------------------------------|:--------------------------------------|:---------:|
|`AsyncSeq<'T>`                 |`SeqT<Async<bool>, 'T>`                |           |
|`asyncSeq { .. }`              |`monad.plus { .. }`                    | At some point it needs to be inferred as `SeqT<Async<bool>, 'T>`, or it can be specified with type parameters: `monad<SeqT<Async<bool>, 'T>>.plus` |
|`let! x = y`                   |`let! x = SeqT.lift y`                 | No auto lifting. Lifting should be explicit. |
|`do! x`                        |`do! SeqT.lift x`                      | ''        |
|`for x in s`                   |`let! x = s`                           | When `s: SeqT` otherwise `for` is still ok with regular sequences. |
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
These samples above and below come from the [original AsyncSeq post](http://tomasp.net/blog/async-sequences.aspx) and they can be easily switched to task sequeces (taskSeq), simply add `|> Async.StartAsTask` between `wc.AsyncDownloadString (Uri url)` and `|> SeqT.lift` then run eveything but the `printPages |> Async.Start`.
*)

// A simple webcrawler

#r "nuget: FSharpPlus"
#r "nuget: HtmlAgilityPack"

open System
open System.Net
open System.Text.RegularExpressions
open HtmlAgilityPack
open FSharp.Control

open FSharpPlus
open FSharpPlus.Data

// ----------------------------------------------------------------------------
// Helper functions for downloading documents, extracting links etc.

/// Asynchronously download the document and parse the HTML
let downloadDocument url = async {
  try let wc = new WebClient ()
      let! html = wc.AsyncDownloadString (Uri url)
      let doc = new HtmlDocument ()
      doc.LoadHtml html
      return Some doc 
  with _ -> return None }

/// Extract all links from the document that start with "http://"
let extractLinks (doc:HtmlDocument) = 
  try
    [ for a in doc.DocumentNode.SelectNodes ("//a") do
        if a.Attributes.Contains "href" then
          let href = a.Attributes.["href"].Value
          if href.StartsWith "https://" then
            let endl = href.IndexOf '?'
            yield if endl > 0 then href.Substring(0, endl) else href ]
  with _ -> []

/// Extract the <title> of the web page
let getTitle (doc: HtmlDocument) =
  let title = doc.DocumentNode.SelectSingleNode "//title"
  if title <> null then title.InnerText.Trim () else "Untitled"

// ----------------------------------------------------------------------------
// Basic crawling - crawl web pages and follow just one link from every page

/// Crawl the internet starting from the specified page
/// From each page follow the first not-yet-visited page
let rec randomCrawl url = 
  let visited = new System.Collections.Generic.HashSet<_> ()

  // Visits page and then recursively visits all referenced pages
  let rec loop url = monad.plus {
    if visited.Add(url) then
      let! doc = downloadDocument url |> SeqT.lift
      match doc with 
      | Some doc ->
          // Yield url and title as the next element
          yield url, getTitle doc
          // For every link, yield all referenced pages too
          for link in extractLinks doc do
            yield! loop link 
      | _ -> () }
  loop url

// Use SeqT combinators to print the titles of the first 10
// web sites that are from other domains than en.wikipedia.org
randomCrawl "https://en.wikipedia.org/wiki/Main_Page"
|> SeqT.filter (fun (url, title) -> url.Contains "en.wikipedia.org" |> not)
|> SeqT.map snd
|> SeqT.take 10
|> SeqT.iter (printfn "%s")
|> Async.Start
