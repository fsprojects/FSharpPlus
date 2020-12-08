namespace FSharpPlus.Tests

module Task =

    open System
    open System.Threading.Tasks
    open NUnit.Framework
    open FSharpPlus
    open FSharpPlus.Data
    open FSharpPlus.Tests.Helpers
    
    exception TestException of string
    
    module TaskTests =

        let createTask isFailed delay value =
            if not isFailed && delay = 0 then Task.FromResult value
            else
                let tcs = TaskCompletionSource<_> ()
                if delay = 0 then tcs.SetException (TestException (sprintf "Ouch, can't create: %A" value ))
                else (Task.Delay delay).ContinueWith (fun _ ->
                    if isFailed then tcs.SetException (TestException (sprintf "Ouch, can't create: %A" value )) else tcs.SetResult value) |> ignore
                tcs.Task

        let (|AggregateException|_|) (x: exn) =
            match x with
            | :? AggregateException as e -> e.InnerExceptions |> Seq.toList |> Some
            | _ -> None

        [<Test>]
        let shortCircuits () =
            let x1 = createTask false 0 1
            let x2 = createTask false 0 2

            let a = Task.map string x1
            require a.IsCompleted "Task.map didn't short-circuit"

            let b = Task.zip x1 x2
            require b.IsCompleted "Task.zip didn't short-circuit"

            let c = Task.map2 (+) x1 x2
            require c.IsCompleted "Task.map2 didn't short-circuit"

        [<Test>]
        let erroredTasks () =
            let e1 () = createTask true  0 1
            let e2 () = createTask true  0 2
            let x1 () = createTask false 0 1
            let x2 () = createTask false 0 2

            let e1d () = createTask true  10 1
            let e2d () = createTask true  10 2
            let x1d () = createTask false 10 1
            let x2d () = createTask false 10 2

            let mapping  isFailure x   = if isFailure then raise (TestException "I was told to fail") else x
            let mapping2 isFailure x y = if isFailure then raise (TestException "I was told to fail") else x + y
            let binding  isFailure x   = if isFailure then raise (TestException "I was told to fail") else Task.FromResult (x + 10)
            let binding' isFailure x   = if isFailure then createTask true 0 (x + 20) else Task.FromResult (x + 10)

            let r01 = Task.map (mapping false) (e1 ())
            r01.Exception.InnerExceptions |> areEquivalent [TestException "Ouch, can't create: 1"]

            let r02 = Task.map (mapping true) (x1 ())
            r02.Exception.InnerExceptions |> areEquivalent [TestException "I was told to fail"]

            let r03 = Task.zip (e1 ()) (x2 ())
            r03.Exception.InnerExceptions |> areEquivalent [TestException "Ouch, can't create: 1"]

            let r04 = Task.zip (e1 ()) (e2 ())
            r04.Exception.InnerExceptions |> areEquivalent [TestException "Ouch, can't create: 1"]

            let r05 = Task.map2 (mapping2 false) (e1 ()) (x2 ())
            r05.Exception.InnerExceptions |> areEquivalent [TestException "Ouch, can't create: 1"]

            let r06 = Task.map2 (mapping2 false) (e1 ()) (e2 ())
            r06.Exception.InnerExceptions |> areEquivalent [TestException "Ouch, can't create: 1"]

            let r07 = Task.bind (binding true) (e1 ())
            r07.Exception.InnerExceptions |> areEquivalent [TestException "Ouch, can't create: 1"]

            let r08 = Task.bind (binding true) (x1 ())
            r08.Exception.InnerExceptions |> areEquivalent [TestException "I was told to fail"]

            let r09 = Task.bind (binding true) (e1d ())
            try r09.Wait ()
            with
               | AggregateException [TestException "Ouch, can't create: 1"] -> ()
               | AggregateException [TestException e] -> failwithf "Another TestException came in: %A" e
               | AggregateException [e]               -> failwithf "Something else came in: %A" e
               | AggregateException e                 -> failwithf "Many errors came in: %A" e

            let r10 = Task.bind (binding true) (x1 ())
            try r10.Wait ()
            with
               | AggregateException [TestException "I was told to fail"] -> ()
               | AggregateException [TestException e] -> failwithf "Another TestException came in: %A" e
               | AggregateException [e]               -> failwithf "Something else came in: %A" e
               | AggregateException e                 -> failwithf "Many errors came in: %A" e


            let r11 = Task.bind (binding' true) (e1d ())
            try r11.Wait ()
            with
               | AggregateException [TestException "Ouch, can't create: 1"] -> ()
               | AggregateException [TestException e] -> failwithf "Another TestException came in: %A" e
               | AggregateException [e]               -> failwithf "Something else came in: %A" e
               | AggregateException e                 -> failwithf "Many errors came in: %A" e

            let r12 = Task.bind (binding' true) (x1 ())
            try r12.Wait ()
            with
               | AggregateException [TestException "Ouch, can't create: 21"] -> ()
               | AggregateException [TestException e] -> failwithf "Another TestException came in: %A" e
               | AggregateException [e]               -> failwithf "Something else came in: %A" e
               | AggregateException e                 -> failwithf "Many errors came in: %A" e


            let r13 = (e1d ()) =>> (fun x -> if true then raise (TestException "I was told to fail") else extract x)
            try r13.Wait ()
            with
               | AggregateException [TestException "Ouch, can't create: 1"] -> ()
               | AggregateException [TestException e] -> failwithf "Another TestException came in: %A" e
               | AggregateException [e]               -> failwithf "Something else came in: %A" e
               | AggregateException e                 -> failwithf "Many errors came in: %A" e

            let r14 = (x1d ()) =>> (fun x -> if true then raise (TestException "I was told to fail") else extract x)
            try r14.Wait ()
            with
               | AggregateException [TestException "I was told to fail"] -> ()
               | AggregateException [TestException e] -> failwithf "Another TestException came in: %A" e
               | AggregateException [e]               -> failwithf "Something else came in: %A" e
               | AggregateException e                 -> failwithf "Many errors came in: %A" e