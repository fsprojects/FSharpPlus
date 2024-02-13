namespace FSharpPlus.Tests

#if !NET462 && !NETSTANDARD2_0

module Async =

    open System
    open System.Threading
    open System.Threading.Tasks
    open NUnit.Framework
    open FSharpPlus
    open FSharpPlus.Data
    open FSharpPlus.Extensions
    
    exception TestException of string
    
    type Async with
        static member AsTaskAndWait computation =
            let t = Async.AsTask computation
            Task.WaitAny t |> ignore
            t

        static member WhenAll (source: Async<'T> seq) = source |> Seq.map Async.AsTask |> Task.WhenAll |> Async.Await


    module AsyncTests =

        let createAsync isFailed delay (value: 'T) =
            if not isFailed && delay = 0 then async.Return value
            else
                async {
                    if delay = 0 then do! Async.raise (TestException (sprintf "Ouch, can't create: %A" value ))
                    do! Async.Sleep delay
                    if isFailed then do! Async.raise (TestException (sprintf "Ouch, can't create: %A" value ))
                    return value }


        [<Test>]
        let testAsyncZip () =
            let t1 = createAsync true 0 1
            let t2 = createAsync true 0 2
            let t3 = createAsync true 0 3

            let c = new CancellationToken true
            let t4 = Task.FromCanceled<int> c |> Async.Await

            let t5 = createAsync false 0 5
            let t6 = createAsync false 0 6

            let t12 =    Async.WhenAll [t1; t2]
            let t12t12 = Async.WhenAll [t12; t12]
            let t33    = Async.WhenAll [t3; t3]

            let t12123 = Async.zip3 t12t12 t33 t4
            let ac1 =
                try
                    Async.AsTaskAndWait(t12123).Exception.InnerExceptions |> Seq.map (fun x -> int (Char.GetNumericValue x.Message.[35]))
                with e ->
                    failwithf "Failure in testAsyncZip. Async status is %A . Exception is %A" (Async.AsTaskAndWait t12123).Status e

            CollectionAssert.AreEquivalent ([1; 2; 1; 2; 3], ac1, "Async.zip(3) should add only non already existing exceptions.")

            let t13 = Async.zip3 (Async.zip t1 t3) t4 (Async.zip t5 t6)
            Assert.AreEqual (true, Async.AsTaskAndWait(t13).IsFaulted, "Async.zip(3) between a value, an exception and a cancellation -> exception wins.")
            let ac2 = Async.AsTaskAndWait(t13).Exception.InnerExceptions |> Seq.map (fun x -> int (Char.GetNumericValue x.Message.[35]))
            CollectionAssert.AreEquivalent ([1; 3], ac2, "Async.zip between 2 exceptions => both exceptions returned, even after combining with cancellation and values.")

        [<Test>]
        let testAsyncZipAsync () =
            let t1 = createAsync true 20 1
            let t2 = createAsync true 10 2
            let t3 = createAsync true 30 3

            let c = new CancellationToken true
            let t4 = Task.FromCanceled<int> c |> Async.Await

            let t5 = createAsync false 20 5
            let t6 = createAsync false 10 6

            let t12 =    Async.WhenAll [t1; t2]
            let t12t12 = Async.WhenAll [t12; t12]
            let t33    = Async.WhenAll [t3; t3]

            let t12123 = Async.zip3 t12t12 t33 t4            
            let ac1 =
                try
                    Async.AsTaskAndWait(t12123).Exception.InnerExceptions |> Seq.map (fun x -> int (Char.GetNumericValue x.Message.[35]))
                with e ->
                    failwithf "Failure in testAsyncZipAsync. Async status is %A . Exception is %A" (Async.AsTaskAndWait t12123).Status e

            CollectionAssert.AreEquivalent ([1; 2; 1; 2; 3], ac1, "Async.zip(3)Async should add only non already existing exceptions.")

            let t13 = Async.zip3 (Async.zip t1 t3) t4 (Async.zip t5 t6)
            Assert.AreEqual (true, Async.AsTaskAndWait(t13).IsFaulted, "Async.zip(3)Async between a value, an exception and a cancellation -> exception wins.")
            let ac2 = Async.AsTaskAndWait(t13).Exception.InnerExceptions |> Seq.map (fun x -> int (Char.GetNumericValue x.Message.[35]))
            CollectionAssert.AreEquivalent ([1; 3], ac2, "Async.zipAsync between 2 exceptions => both exceptions returned, even after combining with cancellation and values.")
     
        [<Test>]
        let testAsyncTraversals =
            let t1 = createAsync true  20 1
            let t2 = createAsync true  10 2
            let t3 = createAsync false 30 3

            let t123 = Async.map3 (fun x y z -> [x; y; z]) t1 t2 t3
            let t123' = transpose [t1; t2; t3]
            let t123'' = sequence [t1; t2; t3]
            CollectionAssert.AreEquivalent ((Async.AsTaskAndWait t123).Exception.InnerExceptions, (Async.AsTaskAndWait t123').Exception.InnerExceptions, "Async.map3 (fun x y z -> [x; y; z]) t1 t2 t3 is the same as transpose [t1; t2; t3]")
            CollectionAssert.AreNotEquivalent ((Async.AsTaskAndWait t123).Exception.InnerExceptions, (Async.AsTaskAndWait t123'').Exception.InnerExceptions, "Async.map3 (fun x y z -> [x; y; z]) t1 t2 t3 is not the same as sequence [t1; t2; t3]")

#endif
