namespace FSharpPlus.Tests

open System
open System.Collections.ObjectModel
open FSharpPlus
open FSharpPlus.Data
open NUnit.Framework
open Helpers

module Applicatives =
    
    [<Test>]
    let pureAndZipApply () =        
        let res9n5  = map ((+) 1) [8;4]
        CollectionAssert.AreEqual ([9; 5], res9n5)

        let red20n30  = pur (+) <.> pur 10 <.> NonEmptySeq.ofList [10;20]
        CollectionAssert.AreEqual (NonEmptySeq.ofList [20; 30], red20n30)

    
    [<Test>]
    let zipApply () =        
        let arr1 = app2 {
            let! x1 = async { return [|1; 2; 3|] }
            and! x2 = async { return [|10; 20; 30|] }
            and! x3 = async { return [|100; 200; 300|] }
            and! x4 = async { return [|1000; 2000; 3000|] }
            return x1 + x2 + x3 + x4 }
        CollectionAssert.AreEqual ([|1111; 2222; 3333|], arr1 |> Async.RunSynchronously)

        let arr2 = (+) <!> [|1;2;3|] <.> [|10;20;30|]
        CollectionAssert.AreEqual ([|11; 22; 33|], arr2)
        
        let arr3 = (+) <!> Compose (async { return [|1;2;3|] } ) <.> Compose (async { return [|10;20;30|] })
        CollectionAssert.AreEqual ([|11; 22; 33|], arr3 |> Compose.run |> Async.RunSynchronously)
