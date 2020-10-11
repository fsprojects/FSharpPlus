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

    let unquote   x = Swensen.Unquote.Operators.evalRaw x
    let powerpack x = Microsoft.FSharp.Linq.QuotationEvaluator.EvaluateUntyped x
    
    // [<Test>]
    // let ``Simple quotation combination (QuotationEvaluator)`` () =
    //     ``Simple quotation combination`` FSharp.Quotations.Evaluator.QuotationEvaluator.EvaluateUntyped

    [<Test>]
    let ``Simple quotation combination [Unquote]`` () = ``Simple quotation combination`` unquote
    
    [<Test>]
    let ``Simple quotation combination [PowerPack]`` () = ``Simple quotation combination`` powerpack


    let ``2-layers quotation combination`` evaluator =
        let expr = 
            <@ 4 + 5 @>
            >>= fun x ->
                let a = x + 10
                <@  (a, a*a) @>
            >>= fun (x, y) ->
                <@  ([x + y], x, y, [|x; y|]) @>
        let res = Expr.run evaluator expr

        areEqual ([380], 19, 361, [|19; 361|]) res

    [<Test>]
    let ``2-layers quotation combination [Unquote]`` () = ``2-layers quotation combination`` unquote
    
    [<Test>]
    let ``2-layers quotation combination [PowerPack]`` () = ``2-layers quotation combination`` powerpack