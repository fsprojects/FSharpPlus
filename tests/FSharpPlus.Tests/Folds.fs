namespace FSharpPlus.Tests

#nowarn "686"

open System
open System.Collections.ObjectModel
open FSharpPlus
open FSharpPlus.Data
open FSharpPlus.Control
open NUnit.Framework
open Helpers
open FSharpPlus.Math.Applicative
open CSharpLib
open System.Threading.Tasks
#if TEST_TRACE
open FSharpPlus.Internals
#endif

module Folds =

    [<Test>]
    let basicFolds () =
        #if TEST_TRACE
        Traces.reset()
        #endif
        let r1 = set [1..3] |> fold (+) 0
        let r2 = set [1..3] |> toSeq
        let r3 = ValueSome 1 |> toSeq
        let r4 = ValueSome 1 |> fold (+) 0
        Assert.AreEqual (6, r1)
        CollectionAssert.AreEqual ([1; 2; 3], r2)
        CollectionAssert.AreEqual ([1], r3)
        Assert.AreEqual (1, r4)
        #if TEST_TRACE
        CollectionAssert.AreEqual (["ToSeq seq"], Traces.get())
        #endif