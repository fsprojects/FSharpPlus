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

open FSharpPlus.One.ComputationExpressions
[<Test>]
let testMonadFx() =
    monadFx() |> ignore

[<Test>]
let testMonadPlus() =
    monadPlus() |> ignore

[<Test>]
let testUsingInForLoops() =
    usingInForLoops() |> ignore

[<Test>]
let testUsingInAsyncs() =
    usingInAsyncs() |> ignore

[<Test>]
let testUsingInOptionT() =
    usingInOptionT() |> ignore

[<Test>]
let testUsingInWhileLoops() =
    usingInWhileLoops() |> ignore

module Validations=
    open FSharpPlus.One.Validations
    type FunctorPT()= inherit FunctorP()
    type BifunctorPT()= inherit BifunctorP()
    type ApplicativePT()= inherit ApplicativeP()
    type AlternativePT()= inherit AlternativeP()
    type TraversablePT()= inherit TraversableP()
    type BaseT()= inherit Base()
    type TestsT()= inherit Tests()

module Lens=
    open FSharpPlus.One.Lenses
    type LensT()=inherit Lens()

open FSharpPlus.One.FleeceTests
open Fuchu
[<Test>]
let testFleece() =
    runParallel tests |> ignore

module General=
    open FSharpPlus.One.General

    type MonoidT( )= inherit Monoid()
    type FunctorT() = inherit Functor()
    type FoldableT() = inherit Foldable()
    type IndexableT() = inherit Indexable()
    type MonadT() = inherit Monad()
    type TraversableT() = inherit Traversable()
    type ApplicativeT() = inherit Applicative()
    type IdiomBracketsT() = inherit IdiomBrackets()
    type AlternativeT() = inherit Alternative()
    type SplitsT() = inherit Splits()
    type ParsingT() = inherit Parsing()
    type ConversionsT() = inherit Conversions()
    type BitConverterT() = inherit BitConverter()
    type SequencesT() = inherit Sequences()

