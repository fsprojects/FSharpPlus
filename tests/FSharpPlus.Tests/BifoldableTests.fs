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


open System.Runtime.InteropServices
open FSharpPlus.Internals

type MyEither<'a,'b> =
    | MyLeft of 'a 
    | MyRight of 'b
    static member BifoldMap (x: MyEither<_,_>, f, g) =
        match x with
        | MyLeft a -> f a
        | MyRight a -> g a

    static member BifoldBack (x: MyEither<_,_>, f, g, z) =
        match x with
        | MyLeft a -> f a z
        | MyRight a -> g a z

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

open FSharpPlus.Control
[<Test>]
let ``bifoldBack over Choice`` () =
    
    let c1 : Choice<int list,string list> = Choice1Of2 [1..2]
    let c2 : Choice<int list,string list> = Choice2Of2 ["a";"bbbb"]

    let r1 = bifoldBack (listMapTimes2 >> (++)) (listMapSeqLength >> (++)) [0] c1
    let r2 = bifoldBack (listMapTimes2 >> (++)) (listMapSeqLength >> (++)) [0] c2
    let e1 = [2;4;0]
    let e2 = [1;4;0]

    Assert.AreEqual(e1, r1)
    Assert.AreEqual(e2, r2)
    
[<Test>]
let ``bifoldBack over Result`` () =
    
    let c1 : Result<int list,string list> = Ok [1..2]
    let c2 : Result<int list,string list> = Error ["a";"bbbb"]

    let r1 = bifoldBack (listMapTimes2 >> (++)) (listMapSeqLength >> (++)) [0] c1
    let r2 = bifoldBack (listMapTimes2 >> (++)) (listMapSeqLength >> (++)) [0] c2
    let e1 = [2;4;0]
    let e2 = [1;4;0]

    Assert.AreEqual(e1, r1)
    Assert.AreEqual(e2, r2)
    
[<Test>]
let ``bifoldBack over rank 2 tuples`` () =
    let t = ("b","c")
    let r = bifoldBack (++) (++) "a" t
    Assert.AreEqual("bca", r)

    

[<Test>]
let ``bifoldBack picks up on external type defining it`` () =

    let l : MyEither<int list, string list> = MyLeft [1..2]
    let r : MyEither<int list, string list> = MyRight ["a";"bbbb"]

    let r1 = bifoldBack (listMapTimes2 >> (++)) (listMapSeqLength >> (++)) [0] l
    let r2 = bifoldBack (listMapTimes2 >> (++)) (listMapSeqLength >> (++)) [0] r
    let e1 = [2;4;0]
    let e2 = [1;4;0]

    Assert.AreEqual(e1, r1)
    Assert.AreEqual(e2, r2)
    
// bisum
type MyEither2<'a,'b> =
    | MyLeft2 of 'a 
    | MyRight2 of 'b
    static member Bisum (x: MyEither2<_,_>) =
        match x with
        | MyLeft2 a -> a
        | MyRight2 a -> a

[<Test>]
let ``bisum checks`` () =
    Assert.AreEqual("a", bisum (Choice1Of2 "a"))
    Assert.AreEqual("b", bisum (Choice1Of2 "b"))
    Assert.AreEqual("a", bisum (Ok "a"))
    Assert.AreEqual("b", bisum (Error "b"))
    Assert.AreEqual("ab", bisum ("a","b"))
    Assert.AreEqual("a", bisum (MyEither.MyLeft "a"))
    Assert.AreEqual("a", bisum (MyEither2.MyLeft2 "a"))

open FSharpPlus.Data

// Bifoldable instance for Const<'T,'U>
[<Test>]
let ``Const instance`` () =
    let c1 : Const<int, string> = Const 1
    let c2 = Const [1..2]
    let c3 = Const 1
    Assert.AreEqual(2, bifoldMap ((*) 2) Seq.length c1)
    Assert.AreEqual([2;4;0], bifoldBack (listMapTimes2 >> (++)) (listMapSeqLength >> (++)) [0] c2)
    Assert.AreEqual(1, bisum c3)

// Bifoldable instance for Validation<'err,'a>
[<Test>]
let ``Validation instance`` () =
    let v1 = Failure 1
    let v2 : Validation<int list,string list> = Success ["22";"4444"]
    let v3 = Failure 1
    Assert.AreEqual(2, bifoldMap ((*) 2) Seq.length v1)
    Assert.AreEqual([2;4;0], bifoldBack (listMapTimes2 >> (++)) (listMapSeqLength >> (++)) [0] v2)
    Assert.AreEqual(1, bisum v3)


