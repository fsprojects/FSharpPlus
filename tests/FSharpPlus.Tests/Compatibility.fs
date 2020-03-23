module FSharpPlus.Tests.Compatibility
open FSharpPlus.One.Usage
open NUnit.Framework


[<Test>]
let choice() =
    Choice.run() |> ignore
[<Test>]
let testSeqConversions() =
    TestSeqConversions.run() |> ignore
[<Test>]
let testListConversions() =
    TestListConversions.run() |> ignore
[<Test>]
let applicatives() =
    Applicatives.run() |> ignore
[<Test>]
let applicativeInference() =
    ApplicativeInference.run() |> ignore
