namespace FSharpPlus.Tests

module Task =

    open System
    open System.Threading.Tasks
    open FsCheck
    open NUnit.Framework
    open FSharpPlus
    open FSharpPlus.Data
    open FSharpPlus.Tests.Helpers
    
    exception TestException of string
    
    module TaskValues =
        let createTask isFailed delay value =
            if not isFailed && delay = 0 then Task.FromResult value
            else
                let tcs = TaskCompletionSource<_> ()
                if delay = 0 then tcs.SetException (TestException (sprintf "Ouch, can't create: %A" value ))
                else (Task.Delay delay).ContinueWith (fun _ -> tcs.SetResult value) |> ignore
                tcs.Task
      
    
    module TaskTests =

        open TaskValues

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
            let e1 = createTask true  0 1
            let e2 = createTask true  0 2
            let x1 = createTask false 0 1
            let x2 = createTask false 0 2

            let mapping isFailure x = if isFailure then raise (TestException "I was told to fail") else x


            let a = Task.map (mapping false) e1
            a.Exception.InnerExceptions |> areEquivalent [TestException "Ouch, can't create: 1"]

            let b = Task.map (mapping true) x1
            b.Exception.InnerExceptions |> areEquivalent [TestException "I was told to fail"]