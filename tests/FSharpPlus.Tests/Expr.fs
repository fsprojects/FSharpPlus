namespace FSharpPlus.Tests

open System
open NUnit.Framework
open FSharpPlus
open FSharpPlus.Tests.Helpers

module Expr =

    let ``Simple quotation combination`` evaluator =
        let one = <@ 1 @>
        let add10AndToString x =
            let a = string (x + 10)
            <@ a @>

        let expr = one >>= add10AndToString
        let res = Expr.run evaluator expr

        areEqual "11" res

    [<Test>]
    let ``Simple quotation combination (QuotationEvaluator)`` () =
        ``Simple quotation combination`` FSharp.Quotations.Evaluator.QuotationEvaluator.EvaluateUntyped

    [<Test>]
    let ``Simple quotation combination (Unquote)`` () =
        ``Simple quotation combination`` Swensen.Unquote.Operators.evalRaw
    
    [<Test>]
    let ``Simple quotation combination (PowerPack)`` () =
        ``Simple quotation combination`` Microsoft.FSharp.Linq.QuotationEvaluator.EvaluateUntyped 