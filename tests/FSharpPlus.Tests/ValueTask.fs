namespace FSharpPlus.Tests

module ValueTask =

    open System
    open System.Threading
    open System.Threading.Tasks
    open NUnit.Framework
    open FSharpPlus
    open FSharpPlus.Data
    open FSharpPlus.Tests.Helpers
    
    exception TestException of string
    
    type ValueTask<'T> with
        static member WhenAll (source: ValueTask<'T> seq) = source |> Seq.map (fun x -> x.AsTask ()) |> Task.WhenAll |> ValueTask<'T []>
        static member WaitAny (source: ValueTask<'T>) = source.AsTask () |> Task.WaitAny |> ignore

    module ValueTask =

        // Following is not available in F#6

        /// <summary>Creates a <see cref="ValueTask{TResult}"/> that's completed exceptionally with the specified exception.</summary>
        /// <typeparam name="TResult">The type of the result returned by the task.</typeparam>
        /// <param name="exception">The exception with which to complete the task.</param>
        /// <returns>The faulted task.</returns>
        let FromException<'TResult> (``exception``: exn) = ValueTask<'TResult> (Task.FromException<'TResult> ``exception``)

        /// <summary>Creates a <see cref="ValueTask{TResult}"/> that's completed due to cancellation with the specified token.</summary>
        /// <typeparam name="TResult">The type of the result returned by the task.</typeparam>
        /// <param name="cancellationToken">The token with which to complete the task.</param>
        /// <returns>The canceled task.</returns>
        let FromCanceled<'TResult> (cancellationToken: CancellationToken) = ValueTask<'TResult> (Task.FromCanceled<'TResult> cancellationToken)

    module ValueTaskTests =

        let createValueTask isFailed delay (value: 'T) =
            if not isFailed && delay = 0 then ValueTask.result value
            else
                let tcs = TaskCompletionSource<_> ()
                let excn = TestException (sprintf "Ouch, can't create: %A" value)
                excn.Data.Add("key", value)

                if delay = 0 then tcs.SetException (excn)
                else (Task.Delay delay).ContinueWith (fun _ ->
                    if isFailed then tcs.SetException (excn) else tcs.SetResult value) |> ignore
                tcs.Task |> ValueTask<'T>

        let (|AggregateException|_|) (x: exn) =
            match x with
            | :? AggregateException as e -> e.InnerExceptions |> Seq.toList |> Some
            | _ -> None
            
        let require x msg = if not x then failwith msg
        
        [<Test>]
        let shortCircuits () =
            let x1 = createValueTask false 0 1
            let x2 = createValueTask false 0 2
            let x3 = createValueTask false 0 3

            let a = ValueTask.map string x1
            require a.IsCompleted "ValueTask.map didn't short-circuit"
            areEqual a.Result "1"

            let b = ValueTask.zip x1 x2
            require b.IsCompleted "ValueTask.zip didn't short-circuit"
            let b1, b2 = b.Result
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
            let e1 () = createValueTask true  0 1
            let e2 () = createValueTask true  0 2
            let e3 () = createValueTask true  0 3
            let x1 () = createValueTask false 0 1
            let x2 () = createValueTask false 0 2
        
            let mapping  isFailure x     = if isFailure then raise (TestException "I was told to fail") else x
            let mapping2 isFailure x y   = if isFailure then raise (TestException "I was told to fail") else x + y
            let mapping3 isFailure x y z = if isFailure then raise (TestException "I was told to fail") else x + y + z
            let binding  isFailure x     = if isFailure then raise (TestException "I was told to fail") else ValueTask.FromResult (x + 10)
        
            let r01 = ValueTask.map (mapping false) (e1 ())
            r01.AsTask().Exception.InnerExceptions |> areEquivalent [TestException "Ouch, can't create: 1"]
        
            let r02 = ValueTask.map (mapping true) (x1 ())
            r02.AsTask().Exception.InnerExceptions |> areEquivalent [TestException "I was told to fail"]
        
            let r03 = ValueTask.zipSequentially (e1 ()) (x2 ())
            r03.AsTask().Exception.InnerExceptions |> areEquivalent [TestException "Ouch, can't create: 1"]
        
            let r04 = ValueTask.zipSequentially (e1 ()) (e2 ())
            r04.AsTask().Exception.InnerExceptions |> areEquivalent [TestException "Ouch, can't create: 1"]
        
            let r05 = ValueTask.lift2 (mapping2 false) (e1 ()) (x2 ())
            r05.AsTask().Exception.InnerExceptions |> areEquivalent [TestException "Ouch, can't create: 1"]
        
            let r06 = ValueTask.lift3 (mapping3 false) (e1 ()) (e2 ()) (e3 ())
            r06.AsTask().Exception.InnerExceptions |> areEquivalent [TestException "Ouch, can't create: 1"]
            
            let r07 = ValueTask.lift3 (mapping3 false) (x1 ()) (e2 ()) (e3 ())
            r07.AsTask().Exception.InnerExceptions |> areEquivalent [TestException "Ouch, can't create: 2"]
        
            let r08 = ValueTask.bind (binding true) (e1 ())
            r08.AsTask().Exception.InnerExceptions |> areEquivalent [TestException "Ouch, can't create: 1"]
        
            let r09 = ValueTask.bind (binding true) (x1 ())
            r09.AsTask().Exception.InnerExceptions |> areEquivalent [TestException "I was told to fail"]


        [<Test>]
        let testValueTaskZip () =
            let t1 = createValueTask true 0 1
            let t2 = createValueTask true 0 2
            let t3 = createValueTask true 0 3

            let c = new CancellationToken true
            let t4 = ValueTask.FromCanceled<int> c

            let t5 = createValueTask false 0 5
            let t6 = createValueTask false 0 6

            let t12 =    ValueTask.WhenAll [t1; t2]
            let t12t12 = ValueTask.WhenAll [t12; t12]
            let t33    = ValueTask.WhenAll [t3; t3]

            ValueTask.WaitAny t12    |> ignore
            ValueTask.WaitAny t12t12 |> ignore
            ValueTask.WaitAny t33    |> ignore

            let t12123 = ValueTask.zip3 t12t12 t33 t4
            let ac1 =
                try
                    (t12123.AsTask ()).Exception.InnerExceptions |> Seq.map (fun x -> int (Char.GetNumericValue x.Message.[35]))
                with e ->
                    failwithf "Failure in testValueTaskZip. ValueTask status is %A . Exception is %A" (t12123.AsTask ()).Status e

            CollectionAssert.AreEquivalent ([1; 2; 1; 2; 3], ac1, "ValueTask.zip(3) should add only non already existing exceptions.")

            let t13 = ValueTask.zip3 (ValueTask.zip t1 t3) t4 (ValueTask.zip t5 t6)
            Assert.AreEqual (true, t13.IsFaulted, "ValueTask.zip(3) between a value, an exception and a cancellation -> exception wins.")
            let ac2 = (t13.AsTask ()).Exception.InnerExceptions |> Seq.map (fun x -> int (Char.GetNumericValue x.Message.[35]))
            CollectionAssert.AreEquivalent ([1; 3], ac2, "ValueTask.zip between 2 exceptions => both exceptions returned, even after combining with cancellation and values.")

        [<Test>]
        let testValueTaskZipAsync () =
            let t1 = createValueTask true 20 1
            let t2 = createValueTask true 10 2
            let t3 = createValueTask true 30 3

            let c = new CancellationToken true
            let t4 = ValueTask.FromCanceled<int> c

            let t5 = createValueTask false 20 5
            let t6 = createValueTask false 10 6

            let t12 =    ValueTask.WhenAll [t1; t2]
            let t12t12 = ValueTask.WhenAll [t12; t12]
            let t33    = ValueTask.WhenAll [t3; t3]

            ValueTask.WaitAny t12    |> ignore
            ValueTask.WaitAny t12t12 |> ignore
            ValueTask.WaitAny t33    |> ignore

            let t12123 = ValueTask.zip3 t12t12 t33 t4            
            let ac1 =
                try
                    (t12123.AsTask ()).Exception.InnerExceptions |> Seq.map (fun x -> int (Char.GetNumericValue x.Message.[35]))
                with e ->
                    failwithf "Failure in testValueTaskZipAsync. ValueTask status is %A . Exception is %A" (t12123.AsTask ()).Status e

            CollectionAssert.AreEquivalent ([1; 2; 1; 2; 3], ac1, "ValueTask.zip(3)Async should add only non already existing exceptions.")

            let t13 = ValueTask.zip3 (ValueTask.zip t1 t3) t4 (ValueTask.zip t5 t6)
            Assert.AreEqual (true, t13.IsFaulted, "ValueTask.zip(3)Async between a value, an exception and a cancellation -> exception wins.")
            let ac2 = (t13.AsTask ()).Exception.InnerExceptions |> Seq.map (fun x -> int (Char.GetNumericValue x.Message.[35]))
            CollectionAssert.AreEquivalent ([1; 3], ac2, "ValueTask.zipAsync between 2 exceptions => both exceptions returned, even after combining with cancellation and values.")

        [<Test>]
        let testTaskTraversals =
            let t1 = createValueTask true  20 1
            let t2 = createValueTask true  10 2
            let t3 = createValueTask false 30 3

            let t123 = ValueTask.map3 (fun x y z -> [x; y; z]) t1 t2 t3
            let t123' = transpose [t1; t2; t3]
            let t123'' = sequence [t1; t2; t3]
            CollectionAssert.AreEquivalent (t123.AsTask().Exception.InnerExceptions, t123'.AsTask().Exception.InnerExceptions, "ValueTask.map3 (fun x y z -> [x; y; z]) t1 t2 t3 is the same as transpose [t1; t2; t3]")
            CollectionAssert.AreNotEquivalent (t123.AsTask().Exception.InnerExceptions, t123''.AsTask().Exception.InnerExceptions, "ValueTask.map3 (fun x y z -> [x; y; z]) t1 t2 t3 is not the same as sequence [t1; t2; t3]")
