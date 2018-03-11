namespace FSharpPlus.Tests.Data

open System
open FSharpPlus
open FSharpPlus.Data
open FSharpPlus.Control
open NUnit.Framework
open FsCheck.NUnit
open NUnit.Framework
open FsCheck

module Helpers =
    let areEqual (x:'t) (y:'t) = Assert.AreEqual (x, y)
    let throws (exnT:Type) (x: unit -> unit) = Assert.Throws(exnT, TestDelegate( x)) |> ignore
    let fsCheck s x= Check.Quick( s , x )

module DList=
    open FSharpPlus.Data.DList
    open Helpers
    let inline enDListThruList l q  =
        let rec loop (q' : 'a DList) (l' : 'a list) = 
            match l' with
            | hd :: [] -> DList.snoc hd q'
            | hd :: tl -> loop (DList.snoc hd q') tl
            | [] -> q'
            
        loop q l
    module Gen =
        let listInt n  = Gen.listOfLength n Arb.generate<int>
        let listObj n  = Gen.listOfLength n Arb.generate<obj>
        let listString n  = Gen.listOfLength n Arb.generate<string>
        let length1thru12 = Gen.choose (1, 12)
        let length2thru12 = Gen.choose (2, 12)

    //DList
    (*
    non-IDList generators from random ofList
    *)
    let DListOfListGen =
        gen {   let! n = Gen.length2thru12
                let! x = Gen.listInt n
                return ( (DList.ofSeq x), x) }

    (*
    IDList generators from random ofSeq and/or snoc elements from random list 
    *)
    let DListIntGen =
        gen {   let! n = Gen.length1thru12
                let! n2 = Gen.length2thru12
                let! x =  Gen.listInt n
                let! y =  Gen.listInt n2
                return ( (DList.ofSeq x |> enDListThruList y), (x @ y) ) }

    let DListIntOfSeqGen =
        gen {   let! n = Gen.length1thru12
                let! x = Gen.listInt n
                return ( (DList.ofSeq x), x) }

    let DListIntSnocGen =
        gen {   let! n = Gen.length1thru12
                let! x = Gen.listInt n
                return ( (DList.empty |> enDListThruList x), x) }

    let DListObjGen =
        gen {   let! n = Gen.length2thru12
                let! n2 = Gen.length1thru12
                let! x =  Gen.listObj n
                let! y =  Gen.listObj n2
                return ( (DList.ofSeq x |> enDListThruList y), (x @ y) ) }

    let DListStringGen =
        gen {   let! n = Gen.length1thru12
                let! n2 = Gen.length2thru12
                let! x =  Gen.listString n
                let! y =  Gen.listString n2  
                return ( (DList.ofSeq x |> enDListThruList y), (x @ y) ) }

    // NUnit TestCaseSource does not understand array of tuples at runtime
    let intGens start =
        let v = Array.create 3 (box (DListIntGen, "DList"))
        v.[1] <- box ((DListIntOfSeqGen |> Gen.filter (fun (q, l) -> l.Length >= start)), "DList OfSeq")
        v.[2] <- box ((DListIntSnocGen |> Gen.filter (fun (q, l) -> l.Length >= start)), "DList snocDList") 
        v

    let intGensStart1 =
        intGens 1  //this will accept all

    let intGensStart2 =
        intGens 2 // this will accept 11 out of 12

    [<Test>]
    let ``allow to tail to work``() =
        DList.empty |> snoc 1 |> tail |> isEmpty |> areEqual true

    [<Test>]
    let ``snoc to work``() =
        DList.empty |> snoc 1 |> snoc 2 |> isEmpty |> areEqual false

    [<Test>]
    let ``cons to work``() =
        DList.empty |> cons 1 |> cons 2 |> length |> areEqual 2

    [<Test>]
    let ``allow to cons and snoc to work``() =
        DList.empty |> cons 1 |> cons 2 |> snoc 3 |> length |> areEqual 3

(*  [<Test>]
    let ``cons pattern discriminator - DList``() =
        let q = ofSeq ["f";"e";"d";"c";"b";"a"]
        
        let h1, t1 = 
            match q with
            | Cons(h, t) -> h, t
            | _ ->  "x", q

        ((h1 = "f") && (t1.Length = 5)) |> areEqual true
*)
    [<Test>]
    let ``empty DList should be empty``() =
        DList.empty |> isEmpty |> areEqual true

    [<Test>]
    let ``fail if there is no head in the DList``() =
        (fun () -> DList.empty |> head |> ignore) |> throws typeof<System.ArgumentException>

    [<Test>]
    let ``fail if there is no tail in the DList``() =
        (fun () -> DList.empty |> tail |> ignore) |> throws typeof<System.ArgumentException>

    [<Test>]
    let ``fold matches build list rev``() =
        fsCheck "DList" (Prop.forAll (Arb.fromGen DListIntGen) 
            (fun ((q :DList<int>), (l : int list)) -> q |> fold (fun (l' : int list) (elem : int) -> elem::l') [] = (List.rev l) ))
                  
        fsCheck "DList OfSeq" (Prop.forAll (Arb.fromGen DListIntGen) 
            (fun ((q :DList<int>), (l : int list)) -> q |> fold (fun (l' : int list) (elem : int) -> elem::l') [] = (List.rev l) ))

        fsCheck "DList Snoc" (Prop.forAll (Arb.fromGen DListIntGen) 
             (fun ((q :DList<int>), (l : int list)) -> q |> fold (fun (l' : int list) (elem : int) -> elem::l') [] = (List.rev l) ))

    [<Test>]
    let ``foldBack matches build list``() =

        fsCheck "DList" (Prop.forAll (Arb.fromGen DListIntGen) 
            (fun ((q :DList<int>), (l : int list)) -> foldBack (fun (elem : int) (l' : int list) -> elem::l') q [] = l ))
                  
        fsCheck "DList OfSeq" (Prop.forAll (Arb.fromGen DListIntOfSeqGen) 
            (fun ((q :DList<int>), (l : int list)) -> foldBack (fun (elem : int) (l' : int list) -> elem::l') q [] = l ))

        fsCheck "DList Snoc" (Prop.forAll (Arb.fromGen DListIntSnocGen) 
             (fun ((q :DList<int>), (l : int list)) -> foldBack (fun (elem : int) (l' : int list) -> elem::l') q [] = l ))

    [<Test>]
    let ``foldBack matches build list 2``() =
        let q = ofSeq ["f";"e";"d";"c";"b";"a"]
        let lq = foldBack (fun (elem : string) (l' : string list) -> elem::l') q []
        areEqual lq (DList.toList q)

    [<Test>]
    let ``fold matches build list rev 2``() =
        let q = ofSeq ["f";"e";"d";"c";"b";"a"]
        let lq = fold (fun (l' : string list) (elem : string) -> elem::l') [] q
        areEqual lq (List.rev (DList.toList q))

    [<Test>]
    [<TestCaseSource("intGensStart1")>]
    let ``get head from DList``(x : obj) =
        let genAndName = unbox x 
        fsCheck (snd genAndName) (Prop.forAll (Arb.fromGen (fst genAndName)) (fun (q : DList<int>, l) -> (head q) = (List.item 0 l) ))

    [<Test>]
    [<TestCaseSource("intGensStart2")>]
    let ``get tail from DList``(x : obj) =
        let genAndName = unbox x 
        fsCheck (snd genAndName) (Prop.forAll (Arb.fromGen (fst genAndName)) (fun ((q : DList<int>), l) -> head (tail q) = (List.item 1 l) ))

    [<Test>]
    let ``give None if there is no head in the DList``() =
        DList.empty |> tryHead |> areEqual None

    [<Test>]
    let ``give None if there is no tail in the DList``() =
        DList.empty |> tryTail |> areEqual None

    [<Test>]
    [<TestCaseSource("intGensStart1")>]
    let ``int DList builds and serializes``(x : obj) =
        let genAndName = unbox x 
        fsCheck (snd genAndName) (Prop.forAll (Arb.fromGen (fst genAndName)) (fun (q : DList<int>, l) -> q |> Seq.toList = l ))

    [<Test>]
    let ``obj DList builds and serializes``() =
        fsCheck "obj DList" (Prop.forAll (Arb.fromGen DListObjGen) (fun (q : DList<obj>, l) -> q |> Seq.toList = l ))

    [<Test>]
    let ``string DList builds and serializes``() =
        fsCheck "string DList" (Prop.forAll (Arb.fromGen DListStringGen) (fun (q : DList<string>, l) -> q |> Seq.toList = l ))

    [<Test>]
    let ``test length should return 6``() =
        let q = ofSeq ["f";"e";"d";"c";"b";"a"] 
        length q |> areEqual 6

    [<Test>]
    let ``singleton length 1``() =
        singleton 1 |> length |> areEqual 1

    [<Test>]
    let ``empty length 0``() =
        empty |> length |> areEqual 0

    [<Test>]
    let ``test singleton should return a Unit containing the solo value``() =
        singleton 1 |> head |> areEqual 1

    [<Test>]
    let ``test append should join two DLists together``() =
        let q = ofSeq ["f";"e";"d";"c";"b";"a"]
        let q2 = ofSeq ["1";"2";"3";"4";"5";"6"]
        let q3 =  append q q2
        q3 |> length |> areEqual 12
        q3 |> head |> areEqual "f"

    [<Test>]
    let ``test toSeq``() =
        let q = ofSeq ["f";"e";"d";"c";"b";"a"] 
        List.ofSeq (DList.toSeq q) |> areEqual ["f";"e";"d";"c";"b";"a"]

    [<Test>]
    let ``test toList``() =
        let q = ofSeq ["f";"e";"d";"c";"b";"a"] 
        DList.toList q |> areEqual ["f";"e";"d";"c";"b";"a"]