namespace FSharpPlus.Tests


module ValueTask =

    open System
    open System.Threading.Tasks
    open NUnit.Framework
    open FSharpPlus
    open FSharpPlus.Data
    open FSharpPlus.Tests.Helpers
    
    exception TestException of string
    
    module ValueTaskTests =

        let createValueTask isFailed value =
            if not isFailed then ValueTask.fromResult value
            else
                ValueTask.fromException (TestException (sprintf "Ouch, can't create: %A" value ))

        let (|AggregateException|_|) (x: exn) =
            match x with
            | :? AggregateException as e -> e.InnerExceptions |> Seq.toList |> Some
            | _ -> None
            
        let require x msg = if not x then failwith msg
        
        [<Test>]
        let shortCircuits () =
            let x1 = createValueTask false 1
            let x2 = createValueTask false 2
            let x3 = createValueTask false 3

            let a = ValueTask.map string x1
            require a.IsCompleted "ValueTask.map didn't short-circuit"

            let b = ValueTask.zip x1 x2
            require b.IsCompleted "ValueTask.zip didn't short-circuit"

            let c = ValueTask.map2 (+) x1 x2
            require c.IsCompleted "ValueTask.map2 didn't short-circuit"
            
            let d = ValueTask.map3 (fun x y z -> x + y + z) x1 x2 x3
            require d.IsCompleted "ValueTask.map3 didn't short-circiut"
