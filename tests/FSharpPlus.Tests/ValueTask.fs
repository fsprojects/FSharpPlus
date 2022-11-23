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
            areEqual a.Result "1"

            let b = ValueTask.zip x1 x2
            require b.IsCompleted "ValueTask.zip didn't short-circuit"
            let (b1, b2) = b.Result
            areEqual b1 1
            areEqual b2 2

            let c = ValueTask.map2 (+) x1 x2
            require c.IsCompleted "ValueTask.map2 didn't short-circuit"
            areEqual c.Result 3
            
            let d = ValueTask.map3 (fun x y z -> x + y + z) x1 x2 x3
            require d.IsCompleted "ValueTask.map3 didn't short-circiut"
            areEqual d.Result 6
        
        [<Test>]
        let erroredValueTasks () =
            let e1 () = createValueTask true  1
            let e2 () = createValueTask true  2
            let e3 () = createValueTask true  3
            let x1 () = createValueTask false 1
            let x2 () = createValueTask false 2
            let x3 () = createValueTask false 3
        
        
            let mapping  isFailure x     = if isFailure then raise (TestException "I was told to fail") else x
            let mapping2 isFailure x y   = if isFailure then raise (TestException "I was told to fail") else x + y
            let mapping3 isFailure x y z = if isFailure then raise (TestException "I was told to fail") else x + y + z
            let binding  isFailure x     = if isFailure then raise (TestException "I was told to fail") else ValueTask.FromResult (x + 10)
        
            let r01 = ValueTask.map (mapping false) (e1 ())
            r01.AsTask().Exception.InnerExceptions |> areEquivalent [TestException "Ouch, can't create: 1"]
        
            let r02 = ValueTask.map (mapping true) (x1 ())
            r02.AsTask().Exception.InnerExceptions |> areEquivalent [TestException "I was told to fail"]
        
            let r03 = ValueTask.zip (e1 ()) (x2 ())
            r03.AsTask().Exception.InnerExceptions |> areEquivalent [TestException "Ouch, can't create: 1"]
        
            let r04 = ValueTask.zip (e1 ()) (e2 ())
            r04.AsTask().Exception.InnerExceptions |> areEquivalent [TestException "Ouch, can't create: 1"]
        
            let r05 = ValueTask.map2 (mapping2 false) (e1 ()) (x2 ())
            r05.AsTask().Exception.InnerExceptions |> areEquivalent [TestException "Ouch, can't create: 1"]
        
            let r06 = ValueTask.map3 (mapping3 false) (e1 ()) (e2 ()) (e3 ())
            r06.AsTask().Exception.InnerExceptions |> areEquivalent [TestException "Ouch, can't create: 1"]
            
            let r07 = ValueTask.map3 (mapping3 false) (x1 ()) (e2 ()) (e3 ())
            r07.AsTask().Exception.InnerExceptions |> areEquivalent [TestException "Ouch, can't create: 2"]
        
            let r08 = ValueTask.bind (binding true) (e1 ())
            r08.AsTask().Exception.InnerExceptions |> areEquivalent [TestException "Ouch, can't create: 1"]
        
            let r09 = ValueTask.bind (binding true) (x1 ())
            r09.AsTask().Exception.InnerExceptions |> areEquivalent [TestException "I was told to fail"]
     
