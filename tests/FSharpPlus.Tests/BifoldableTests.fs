namespace FSharpPlus.Tests

module BifoldableTests =

    open FSharpPlus
    open NUnit.Framework

    let listMapSeqLength = List.map Seq.length
    let listMapTimes2 = List.map ((*) 2)

    type Assert with
        static member AreEqual(expected: 'a list, actual: 'b list) =
            Assert.AreEqual(box expected, box actual, sprintf "expected %A but got %A" expected actual)

    [<Test>]
    let ``bifoldMap over Choice`` () =

        let c1 : Choice<int list,string list> = Choice1Of2 [1..2]
        let c2 : Choice<int list,string list> = Choice2Of2 ["a";"bbbb"]
        let r1 = bifoldMap listMapSeqLength listMapTimes2 c1
        let r2 = bifoldMap listMapSeqLength listMapTimes2 c2
        let e1 = [2;4]
        let e2 = [1;4]

        Assert.AreEqual(e1, r1)
        Assert.AreEqual(e2, r2)

    [<Test>]
    let ``bifoldMap over Result`` () =

        let c1 : Result<int list,string list> = Ok [1..2]
        let c2 : Result<int list,string list> = Error ["a";"bbbb"]
        let r1 = bifoldMap listMapSeqLength listMapTimes2 c1
        let r2 = bifoldMap listMapSeqLength listMapTimes2 c2
        let e1 = [2;4]
        let e2 = [1;4]

        Assert.AreEqual(e1, r1)
        Assert.AreEqual(e2, r2)

    type MyEither<'a,'b> =
        | MyLeft of 'a 
        | MyRight of 'b
        static member BifoldMap (x: MyEither<_,_>, f, g) =
            match x with
            | MyLeft a -> f a
            | MyRight a -> g a
        static member Bifold (x: MyEither<_,_>, f,g,z) =
            match x with
            | MyLeft a -> f z a
            | MyRight a -> g z a
        static member inline BifoldBack (x: MyEither<_,_>, f, g, z) =
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
    [<Test>]
    let ``bifoldBack over Choice`` () =

        let c1 : Choice<int list,string list> = Choice1Of2 [1..2]
        let c2 : Choice<int list,string list> = Choice2Of2 ["a";"bbbb"]

        let r1 = bifoldBack (listMapSeqLength >> (++)) (listMapTimes2 >> (++)) c1 [0] 
        let r2 = bifoldBack (listMapSeqLength >> (++)) (listMapTimes2 >> (++)) c2 [0] 
        let e1 = [2;4;0]
        let e2 = [1;4;0]

        Assert.AreEqual(e1, r1)
        Assert.AreEqual(e2, r2)

    [<Test>]
    let ``bifoldBack over Result`` () =

        let c1 : Result<int list,string list> = Ok [1..2]
        let c2 : Result<int list,string list> = Error ["a";"bbbb"]
        let r1 = bifoldBack (listMapSeqLength >> (++)) (listMapTimes2 >> (++)) c1 [0]
        let r2 = bifoldBack (listMapSeqLength >> (++)) (listMapTimes2 >> (++)) c2 [0]
        let e1 = [2;4;0]
        let e2 = [1;4;0]

        Assert.AreEqual(e1, r1)
        Assert.AreEqual(e2, r2)

    [<Test>]
    let ``bifoldBack over rank 2 tuples`` () =
        let t = ("b","c")
        let r = bifoldBack (++) (++) t "a"
        Assert.AreEqual("bca", r)

    [<Test>]
    let ``bifoldBack picks up on external type defining it`` () =

        let l : MyEither<int list, string list> = MyLeft [1..2]
        let r : MyEither<int list, string list> = MyRight ["a";"bbbb"]

        let r1 = bifoldBack (listMapTimes2 >> (++)) (listMapSeqLength >> (++)) l [0]
        let r2 = bifoldBack (listMapTimes2 >> (++)) (listMapSeqLength >> (++)) r [0]
        let e1 = [2;4;0]
        let e2 = [1;4;0]

        Assert.AreEqual(e1, r1)
        Assert.AreEqual(e2, r2)

    // bifold
    [<Test>]
    let ``bifold over Choice`` () =

        let c1 : Choice<int list,string list> = Choice1Of2 [1..2]
        let c2 : Choice<int list,string list> = Choice2Of2 ["a";"bbbb"]

        let r1 = bifold (listMapSeqLength >> (++) |> flip)               (listMapTimes2 >> (++)) [4] c1
        let r2 = bifold (fun prepend v -> prepend ++ listMapSeqLength v) (listMapTimes2 >> (++)) [3] c2
        let e1 = [8;1;2]
        let e2 = [3;1;4]

        Assert.AreEqual(e1, r1)
        Assert.AreEqual(e2, r2)

    [<Test>]
    let ``bifold over Result`` () =

        let c1 : Result<int list,string list> = Ok [1..2]
        let c2 : Result<int list,string list> = Error ["a";"bbbb"]

        let r1 = bifold (listMapSeqLength >> (++) |> flip)               (listMapTimes2 >> (++)) [4] c1
        let r2 = bifold (fun prepend v -> prepend ++ listMapSeqLength v) (listMapTimes2 >> (++)) [3] c2
        let e1 = [8;1;2]
        let e2 = [3;1;4]

        Assert.AreEqual(e1, r1)
        Assert.AreEqual(e2, r2)

    [<Test>]
    let ``bifold over rank 2 tuples`` () =
        let t = ("b","c")
        let r = bifold (++) (++) "a" t
        Assert.AreEqual("abc", r)

    [<Test>]
    let ``bifold picks up on external type defining it`` () =

        let l : MyEither<int list, string list> = MyLeft [1..2]
        let r : MyEither<int list, string list> = MyRight ["a";"bbbb"]

        let r1 = bifold (listMapTimes2 >> (++)) (failwithf "not called %A %A") [3] l
        let r2 = bifold (failwithf "not called %A %A") (fun prepend v -> prepend ++ listMapSeqLength v) [0] r
        let e1 = [6;1;2]
        let e2 = [0;1;4]

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
        Assert.AreEqual(2, bifoldMap ((*) 2) (failwithf "never called1 %A") c1)
        Assert.AreEqual([2;4;0], bifoldBack (listMapTimes2 >> (++)) (failwithf "never called2 %A %A") c2 [0])
        Assert.AreEqual([8;1;2], bifold (listMapTimes2 >> (++)) (failwithf "never called3 %A %A") [4] c2)
        Assert.AreEqual(1, bisum c3)

    // Bifoldable instance for Validation<'err,'a>
    [<Test>]
    let ``Validation instance`` () =
        let v1 = Failure 1
        let v2 : Validation<int list,string list> = Success ["22";"4444"]
        let v3 = Failure 1
        Assert.AreEqual(2, bifoldMap ((*) 2) Seq.length v1)
        Assert.AreEqual([2;4;0], bifoldBack (listMapTimes2 >> (++)) (listMapSeqLength >> (++)) v2 [0])
        Assert.AreEqual([0;2;4], bifold (listMapTimes2 >> (++)) (fun prepend v -> prepend ++ listMapSeqLength v) [0] v2)
        Assert.AreEqual(1, bisum v3)