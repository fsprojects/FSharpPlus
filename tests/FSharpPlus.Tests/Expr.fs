namespace FSharpPlus.Tests

open System
open NUnit.Framework
open FSharpPlus
open FSharpPlus.Tests.Helpers

module Expr =

    let quotseval x =
#if NETSTANDARD
        FSharp.Quotations.Evaluator.QuotationEvaluator.EvaluateUntyped x
#else
        Swensen.Unquote.Operators.evalRaw x
#endif
    let unquote   x = Swensen.Unquote.Operators.evalRaw x
    let powerpack x = Microsoft.FSharp.Linq.QuotationEvaluator.EvaluateUntyped x


    
    let ``Simple quotation combination`` evaluator =
        let one = <@ 1 @>
        let add10AndToString x =
            let a = string (x + 10)
            <@ a @>

        let expr = one >>= add10AndToString
        let res = Expr.run evaluator expr

        areEqual "11" res
    
    let [<Test>] ``Simple quotation combination [QuotationEvaluator]`` () = ``Simple quotation combination`` quotseval
    let [<Test>] ``Simple quotation combination [Unquote]``            () = ``Simple quotation combination`` unquote
    let [<Test>] ``Simple quotation combination [PowerPack]``          () = ``Simple quotation combination`` powerpack


    let ``2-layers quotation combination`` evaluator =
        let expr = 
            <@ 4 + 5 @>
            >>= (fun x ->
                    let a = x + 10
                    <@  (a, a*a) @>
                    >>= fun (x, y) ->
                        <@  ([x + y], x, y, [|x; y|]) @>)
        let res = Expr.run evaluator expr

        areEqual ([380], 19, 361, [|19; 361|]) res

    let [<Test>] ``2-layers quotation combination [QuotationEvaluator]`` () = ``2-layers quotation combination`` quotseval
    let [<Test>] ``2-layers quotation combination [Unquote]``            () = ``2-layers quotation combination`` unquote
    let [<Test>] ``2-layers quotation combination [PowerPack]``          () = ``2-layers quotation combination`` powerpack


    let ``2-layers quot comb associative`` evaluator =
        let expr = 
            (<@ 4 + 5 @>
            >>= fun x ->
                let a = x + 10
                <@  (a, a*a) @>)
            >>= fun (x, y) ->
                <@  ([x + y], x, y, [|x; y|]) @>
        let res = Expr.run evaluator expr

        areEqual ([380], 19, 361, [|19; 361|]) res

    let [<Test>] ``2-layers quot comb associative [QuotationEvaluator]`` () = ``2-layers quot comb associative`` quotseval
    let [<Test>] ``2-layers quot comb associative [Unquote]``            () = ``2-layers quot comb associative`` unquote
    let [<Test>] ``2-layers quot comb associative [PowerPack]``          () = ``2-layers quot comb associative`` powerpack


    let ``simple CE same type`` evaluator =
        let expr = monad {
            let! x = <@ 1 @>
            let! y = <@ 2 @>
            return! <@ x + y @>
        }
        let res = Expr.run evaluator expr
        
        areEqual 3 res

    let [<Test>] ``simple CE same type [QuotationEvaluator]`` () = ``simple CE same type`` quotseval
    let [<Test>] ``simple CE same type [Unquote]``            () = ``simple CE same type`` unquote
    let [<Test>] ``simple CE same type [PowerPack]``          () = ``simple CE same type`` powerpack


    let ``simple CE different types`` evaluator =
        let expr = monad {
            let! x = <@ 1 @>
            let! y = <@ "2" @>
            return! <@ string x + y @>
        }
        let res = Expr.run evaluator expr
        
        areEqual "12" res

    let [<Test>] ``simple CE different types [QuotationEvaluator]`` () = ``simple CE different types`` quotseval
    let [<Test>] ``simple CE different types [Unquote]``            () = ``simple CE different types`` unquote
    let [<Test>] ``simple CE different types [PowerPack]``          () = ``simple CE different types`` powerpack