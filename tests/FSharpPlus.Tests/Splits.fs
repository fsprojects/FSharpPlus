namespace FSharpPlus.Tests

open System
open System.Collections.ObjectModel
open FSharpPlus
open FSharpPlus.Data
open FSharpPlus.Control
open NUnit.Framework
open Helpers
open CSharpLib


type Sum<'a> = Sum of 'a with
    static member inline get_Zero () = Sum 0G
    static member inline (+) (Sum (x:'n), Sum (y:'n)) = Sum (x + y)

module Splits = 
    [<Test>]
    let splitArraysAndStrings () = 
        let a1 = "this.isABa.tABCest"  |> split [|"AT" ; "ABC" |]
        let a2 = "this.isABa.tABCest"B |> split [|"AT"B; "ABC"B|] |> Seq.map System.Text.Encoding.ASCII.GetString

        let b1 = "this.is.a.t...est"  |> split [|"." ; "..." |]
        let b2 = "this.is.a.t...est"B |> split [|"."B; "..."B|] |> Seq.map System.Text.Encoding.ASCII.GetString

        Assert.IsTrue((toList a1 = toList a2))
        Assert.IsTrue((toList b1 = toList b2))
        Assert.IsInstanceOf<Option<string []>> (Some a1)

    [<Test>]
    let replaceArraysAndStrings () = 
        let a1 = "this.isABa.tABCest"  |> replace "AT"  "ABC"
        let a2 = "this.isABa.tABCest"B |> replace "AT"B "ABC"B |> System.Text.Encoding.ASCII.GetString

        let b1 = "this.is.a.t...est"  |> replace "."  "..."
        let b2 = "this.is.a.t...est"B |> replace "."B "..."B |> System.Text.Encoding.ASCII.GetString

        Assert.IsTrue ((a1 = a2))
        Assert.IsTrue ((b1 = b2))

    [<Test>]
    let intercalateArraysAndStrings () = 
        let a1 = [|"this" ; "is" ; "a" ; "test" |] |> intercalate " "
        let a2 = [|"this"B; "is"B; "a"B; "test"B|] |> intercalate " "B |> System.Text.Encoding.ASCII.GetString

        let b = [WrappedListB [1;2]; WrappedListB [3;4]; WrappedListB [6;7]] |> intercalate (WrappedListB [0;1])

        let _c = [| Sum 1; Sum 2 |] |> intercalate (Sum 10)
        let d  = WrappedListB [Sum 1; Sum 2] |> intercalate (Sum 10)
        let _e = intercalate 10 (seq [1; 2; 3])

        Assert.IsTrue((a1 = a2))
        Assert.IsTrue((b = WrappedListB [1; 2; 0; 1; 3; 4; 0; 1; 6; 7]))
        // Assert.IsTrue((c = Sum 13))
        Assert.IsTrue((d = Sum 13))