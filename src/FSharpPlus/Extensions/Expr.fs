namespace FSharpPlus

/// Additional operations on Quotations.Expr
[<RequireQualifiedAccess>]
module Expr =
    
    open System
    open Microsoft.FSharp.Quotations
    open Microsoft.FSharp.Quotations.Patterns
    open Microsoft.FSharp.Quotations.ExprShape

    let [<Literal>] private fsNamespace = "Microsoft.FSharp.Core"

    let [<Literal>] private opSliceName = "SpliceExpression"
    let [<Literal>] private opSliceType = "ExtraTopLevelOperators"

    let private fsCoreAs = AppDomain.CurrentDomain.GetAssemblies () |> Seq.find (fun a -> a.GetName().Name = "FSharp.Core")
    let private miSplice = fsCoreAs.GetType(fsNamespace + "." + opSliceType).GetMethod opSliceName
        
    let bind (f: 'T -> Expr<'U>) (x: Expr<'T>) : Expr<'U> =
        Expr.Coerce (Expr.Call (miSplice.MakeGenericMethod typeof<'U>, [Expr.Application (Expr.Value f, x)]), typeof<'U>)
        |> Expr.Cast

    let rec runWithUntyped (eval: Expr -> obj) (exp: Expr) s =
        let m = if isNull s then let x = Reflection.MethodInfo.GetCurrentMethod () in x.DeclaringType.GetMethod x.Name else s
        let rec subsExpr = function
            | Call (None, mi, exprLst) 
                when (mi.Name, mi.DeclaringType.Name, mi.DeclaringType.Namespace) = (opSliceName, opSliceType, fsNamespace)
                -> Expr.Call (m, [Expr.Value eval; subsExpr exprLst.Head; Expr.Value m])
            | ShapeVar var                        -> Expr.Var var
            | ShapeLambda (var, expr)             -> Expr.Lambda (var, subsExpr expr)
            | ShapeCombination (shpComb, exprLst) -> RebuildShapeCombination (shpComb, List.map subsExpr exprLst)
        eval (subsExpr exp)
    
    /// Executes quoted expression, given a quotation evaluator function.
    let run (eval: Expr -> obj) (exp: Expr<'T>) : 'T = runWithUntyped eval exp.Raw null :?> 'T