module BifoldableTests
open FSharpPlus.Operators
open NUnit.Framework

let listMapSeqLength = List.map Seq.length
let listMapTimes2 = List.map ((*) 2)

[<Test>]
let ``bifoldMap over Choice`` () =
    
    let c1 : Choice<int list,string list> = Choice1Of2 [1..2]
    let c2 : Choice<int list,string list> = Choice2Of2 ["a";"bbbb"]
    let r1 = bifoldMap listMapTimes2 listMapSeqLength c1
    let r2 = bifoldMap listMapTimes2 listMapSeqLength c2
    let e1 = [2;4]
    let e2 = [1;4]

    Assert.AreEqual(e1, r1)
    Assert.AreEqual(e2, r2)

[<Test>]
let ``bifoldMap over Result`` () =
    
    let c1 : Result<int list,string list> = Ok [1..2]
    let c2 : Result<int list,string list> = Error ["a";"bbbb"]
    let r1 = bifoldMap listMapTimes2 listMapSeqLength c1
    let r2 = bifoldMap listMapTimes2 listMapSeqLength c2
    let e1 = [2;4]
    let e2 = [1;4]

    Assert.AreEqual(e1, r1)
    Assert.AreEqual(e2, r2)

type MyEither<'a,'b> = MyLeft of 'a | MyRight of 'b
open System.Runtime.InteropServices
open FSharpPlus.Internals
type MyEither<'a,'b> with
    static member inline BifoldMap (x: MyEither<_,_>, f, g, [<Optional>]_impl: Default1) =
      match x with
      | MyLeft a -> f a
      | MyRight a -> g a

[<Test>]
let ``bifoldMap picks up on external type defining it`` () =

    let l : MyEither<int list, string list> = MyLeft [1..2]
    let r : MyEither<int list, string list> = MyRight ["a";"bbbb"]

    let r1 = bifoldMap listMapTimes2 listMapSeqLength l
    let r2 = bifoldMap listMapTimes2 listMapSeqLength r
    let e1 = [2;4]
    let e2 = [1;4]

    Assert.AreEqual(e1, r1)
    Assert.AreEqual(e2, r2)


[<Test>]
let ``bifoldMap over rank 2 tuples`` () =
    // note: bifoldMap is implemented only for rank 2 tuples as a design choice
    let t1 = 1,2
    let r1 = bifoldMap ((*) 3) ((*) 2) t1
    Assert.AreEqual(7, r1)

// bifoldBack
(*
open FSharpPlus.Control
[<Test>]
let ``bifoldBack over Choice`` () =
    
    let c1 : Choice<int list,string list> = Choice1Of2 [1..2]
    let c2 : Choice<int list,string list> = Choice2Of2 ["a";"bbbb"]
    let r1 = bifoldBack Plus.Invoke Plus.Invoke [0] c1
    //let r2 = bifoldBack listMapTimes2 listMapSeqLength [0] c2
    let e1 = [1;2;0]
    //let e2 = [1;4;0]

    Assert.AreEqual(e1, r1)
    //Assert.AreEqual(e2, r2)*)