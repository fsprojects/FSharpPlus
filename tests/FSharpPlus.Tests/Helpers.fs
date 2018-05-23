module FSharpPlus.Tests.Helpers

open System
open NUnit.Framework

let areEqual (x:'t) (y:'t) = Assert.AreEqual (x, y)