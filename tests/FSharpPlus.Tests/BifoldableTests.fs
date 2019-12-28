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

(*

doesn't compile in the lib, yet works in minimal setting https://github.com/smoothdeveloper/minimalfsharppluscompileissue or in FSI...???

type Either<'a,'b> = Left of 'a | Right of 'b
open System.Runtime.InteropServices
open FSharpPlus.Internals
type Either<'a,'b> with
    static member inline BifoldMap (x: Either<_,_>, f, g, [<Optional>]_impl: Default1) =
      match x with
      | Left a -> f a
      | Right a -> g a

[<Test>]
let ``bifoldMap picks up on external type defining it`` () =

    let l : Either<int list, string list> = Left [1..2]
    let r : Either<int list, string list> = Right ["a";"bbbb"]

    let r1 = bifoldMap listMapTimes2 listMapSeqLength l
    let r2 = bifoldMap listMapTimes2 listMapSeqLength r
    let e1 = [2;4]
    let e2 = [1;4]

    Assert.AreEqual(e1, r1)
    Assert.AreEqual(e2, r2)
*)

[<Test>]
let ``bifoldMap over rank 2 tuples`` () =
    // note: bifoldMap is implemented only for rank 2 tuples as a design choice
    
    let t1 = 1,2
    
    
    let r1 = bifoldMap ((*) 3) ((*) 2) t1
    
    Assert.AreEqual(7, r1)
