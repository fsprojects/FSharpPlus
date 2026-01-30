namespace FSharpPlus.Tests

open System
open System.Collections.ObjectModel
open System.Threading.Tasks
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
        let arr1 = zapp2 {
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


    [<Test>]
    let zip3Test () =

        SideEffects.reset ()
        let _a = zip3 (seq [1;2;3]) (seq [1. .. 3. ]) (seq ['a';'b';'c'])
        Assert.AreEqual (list<string>.Empty, SideEffects.get ())

        let _b = zip3 (dict [1,'1' ; 2,'2' ; 4,'4']) (dict [1,'1' ; 2,'2' ; 3,'3']) (dict [1,'a' ; 2,'b' ; 3,'c'])
        let _c = zip3 [ 1;2;3 ] [ 1. .. 3. ] ['a';'b';'c']
        let _d = zip3 [|1;2;3|] [|1. .. 3.|] [|'a';'b';'c'|]
        let _e = zip3 (async {return 1}) (async {return '2'}) (async {return 3.})
        let _f = zip3 (Task.FromResult 1) (Task.FromResult '2') (Task.FromResult 3.)
        let _g = zip3 (Some 1) (Some '2') (Some 3.)
        let _h = zip3 (Ok 1) (if true then Ok '2' else Error "No") (Ok 3.)

        let _fa a = zip3 a (seq [1. .. 3. ]) (seq ['a';'b';'c'])
        let _fb a = zip3 a [ 1. .. 3. ] ['a';'b';'c']
        let _fc a = zip3 a [|1. .. 3.|] [|'a';'b';'c'|]
        let _fd a = zip3 a (async {return '2'}) (async {return 3.})
        let _fe a = zip3 a (Task.FromResult '2') (Task.FromResult 3.)

        let _ga b = zip3 (seq [1;2;3]) b (seq ['a';'b';'c'])
        let _gb b = zip3 [ 1;2;3 ] b ['a';'b';'c']
        let _gc b = zip3 [|1;2;3|] b [|'a';'b';'c'|]
        let _gd b = zip3 (async {return 1}) b (async {return 3.})
        let _ge b = zip3 (Task.FromResult 1) b (Task.FromResult 3.)

        let _ha c = zip3 (seq [1;2;3]) (seq [1. .. 3. ]) c
        let _hb c = zip3 [ 1;2;3 ] [ 1. .. 3. ] c
        let _hc c = zip3 [|1;2;3|] [|1. .. 3.|] c
        let _hd c = zip3 (async {return 1}) (async {return '2'}) c
        let _he c = zip3 (Task.FromResult 1) (Task.FromResult '2') c

        let _ia : _ -> _ -> _ -> _ seq     = zip3
        let _ib : _ -> _ -> _ -> _ list    = zip3
        let _ic : _ -> _ -> _ -> _ []      = zip3
        let _id : _ -> _ -> _ -> Async<_>  = zip3
        let _ie : _ -> _ -> _ -> Task<_>   = zip3

        ()

    [<Test>]
    let genericZip3Shortest () =
        let a = zip3 [|1; 2; 3|] [|"a"; "b"|] [|10.; 20.; 30.|]
        CollectionAssert.AreEqual ([|1,"a",10.; 2,"b",20.|], a)
        
        let l = zip3 [1; 2] ["a"; "b"; "c"] [10.; 20.; 30.]
        CollectionAssert.AreEqual ([1,"a",10.; 2,"b",20.], l)
        
        let e = zip3 (ResizeArray [1; 2]) (ResizeArray ["a"; "b"; "c"]) (ResizeArray [10.; 20.])
        CollectionAssert.AreEqual (ResizeArray [1,"a",10.; 2,"b",20.], e)
        
        let nel = zip3 (NonEmptyList.ofList [1; 2]) (NonEmptyList.ofList ["a"; "b"; "c"]) (NonEmptyList.ofList [10.; 20.; 30.])
        CollectionAssert.AreEqual (NonEmptyList.ofList [1,"a",10.; 2,"b",20.], nel)