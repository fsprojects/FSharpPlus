module FSharpPlus.Tests.ListT

open System
open FSharpPlus
open FSharpPlus.Data
open NUnit.Framework
open FsCheck
open Helpers
open System.Collections.Generic

module BasicTests =
    [<Test>]
    let wrap_unwrap () =
        let c = listT (async.Return (['a'..'g']))
        let res = c |> ListT.run |> listT |> ListT.run |> extract
        let exp = c |> ListT.run |> extract
        CollectionAssert.AreEqual (res, exp)
  
    [<Test>]
    let infiniteLists =
        let (infinite: ListT<Lazy<_>>) = ListT.unfold (fun x -> monad { return (Some (x, x + 1) ) }) 0
        let finite = take 12 infinite
        let res = finite <|> infinite
        CollectionAssert.AreEqual (res |> take 13 |> ListT.run |> extract, [0;1;2;3;4;5;6;7;8;9;10;11;0])