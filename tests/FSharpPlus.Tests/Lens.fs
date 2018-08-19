module FSharpPlus.Tests.Lens

open System
open FSharpPlus
open FSharpPlus.Lens
open NUnit.Framework
open FsCheck
open Helpers


[<Test>]
let simple_lens() =
  areEqual 1 (view _1 (1, '2'))
  areEqual 2 (view _2 ('1', 2))

[<Test>]
let ok_prism() =
  areEqual (None) (preview _Ok (Error 1))
  areEqual (Some 1) (preview _Ok (Ok 1))

[<Test>]
let error_prism() =
  areEqual (Some 1) (preview _Error (Error 1))
  areEqual (None) (preview _Error (Ok 1))

[<Test>]
let some_prism() =
  areEqual (None) (preview _Some None)
  areEqual (Some 1) (preview _Some (Some 1))

[<Test>]
let none_prism() =
  areEqual (Some ()) (preview _None None)
  areEqual (None) (preview _None (Some 1))