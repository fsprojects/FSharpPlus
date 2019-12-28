module BifoldableTests
open FSharpPlus.Operators.Bifold
open NUnit.Framework

[<Test>]
let ``bifoldMap over Choice`` () =
    let listMapSeqLength = List.map Seq.length
    let listMapTimes2 = List.map ((*) 2)
    
    let c1 : Choice<int list,string list> = Choice1Of2 [1..2]
    let c2 : Choice<int list,string list> = Choice2Of2 ["a";"bbbb"]
    let r1 = bifoldMap listMapTimes2 listMapSeqLength c1
    let r2 = bifoldMap listMapTimes2 listMapSeqLength c2
    let e1 = [2;4]
    let e2 = [1;4]

    Assert.AreEqual(e1, r1)
    Assert.AreEqual(e2, r2)
