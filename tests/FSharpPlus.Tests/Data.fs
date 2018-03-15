namespace FSharpPlus.Tests.Data

open System
open FSharpPlus
open FSharpPlus.Data
open FSharpPlus.Control
open NUnit.Framework
open FsCheck

module Helpers =
    let areEqual (x:'t) (y:'t) = Assert.AreEqual (x, y)

module DList=
    open FSharpPlus.Data.DList
    open Helpers
    // tests from FSharpx.Collections DList
    let shouldEqual (x:'t) (y:'t) = Assert.AreEqual (x, y)
    let shouldThrow (exnT:Type) (x: unit -> unit) = Assert.Throws(exnT, TestDelegate( x)) |> ignore
    let fsCheck s x= Check.Quick( s , x )
        
    module Gen = // FSharpx.Collections extension of Gen
        let listInt n  = Gen.listOfLength n Arb.generate<int>
        let listObj n  = Gen.listOfLength n Arb.generate<obj>
        let listString n  = Gen.listOfLength n Arb.generate<string>
        let length1thru12 = Gen.choose (1, 12)
        let length2thru12 = Gen.choose (2, 12)
    let emptyDList = DList.empty

    let enDListThruList l q  =
        let rec loop (q' : 'a DList) (l' : 'a list) = 
            match l' with
            | hd :: [] -> q'.Add hd
            | hd :: tl -> loop (q'.Add hd) tl
            | [] -> q'
            
        loop q l

    //DList
    (*
    non-IDList generators from random ofList
    *)
    let DListOfListGen =
        gen {   let! n = Gen.length2thru12
                let! x = Gen.listInt n
                return ( (DList.ofSeq x), x) }                
    (*
    IDList generators from random ofSeq and/or add elements from random list 
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

    let DListIntAddGen =
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
        v.[1] <- box ((DListIntOfSeqGen |> Gen.suchThat (fun (q, l) -> l.Length >= start)), "DList OfSeq")
        v.[2] <- box ((DListIntAddGen |> Gen.suchThat (fun (q, l) -> l.Length >= start)), "DList addDList") 
        v

    let intGensStart1 =
        intGens 1  //this will accept all

    let intGensStart2 =
        intGens 2 // this will accept 11 out of 12

    [<Test>]
    let ``allow to tail to work``() =
        emptyDList |> add 1 |> tail |> isEmpty |> shouldEqual true

    [<Test>]
    let ``add to work``() =
        emptyDList |> add 1 |> add 2 |> isEmpty |> shouldEqual false

    [<Test>]
    let ``cons to work``() =
        emptyDList |> cons 1 |> cons 2 |> length |> shouldEqual 2

    [<Test>]
    let ``allow to cons and add to work``() =
        emptyDList |> cons 1 |> cons 2 |> add 3 |> length |> shouldEqual 3

    [<Test>]
    let ``cons pattern discriminator - DList``() =
        let q = ofSeq ["f";"e";"d";"c";"b";"a"]
        
        let h1, t1 = 
            match q with
            | Cons(h, t) -> h, t
            | _ ->  "x", q

        ((h1 = "f") && (t1.Length = 5)) |> shouldEqual true

    [<Test>]
    let ``empty DList should be empty``() =
        emptyDList |> isEmpty |> shouldEqual true

    [<Test>]
    let ``fail if there is no head in the DList``() =
        (fun () -> emptyDList |> head |> ignore) |> shouldThrow typeof<System.Exception>

    [<Test>]
    let ``fail if there is no tail in the DList``() =
        (fun () -> emptyDList |> tail |> ignore) |> shouldThrow typeof<System.Exception>

    [<Test>]
    let ``fold matches build list rev``() =

        fsCheck "DList" (Prop.forAll (Arb.fromGen DListIntGen) 
            (fun ((q :DList<int>), (l : int list)) -> q |> fold (fun (l' : int list) (elem : int) -> elem::l') [] = (List.rev l) ))
                  
        fsCheck "DList OfSeq" (Prop.forAll (Arb.fromGen DListIntOfSeqGen) 
            (fun ((q :DList<int>), (l : int list)) -> q |> fold (fun (l' : int list) (elem : int) -> elem::l') [] = (List.rev l) ))

        fsCheck "DList Add" (Prop.forAll (Arb.fromGen DListIntAddGen) 
             (fun ((q :DList<int>), (l : int list)) -> q |> fold (fun (l' : int list) (elem : int) -> elem::l') [] = (List.rev l) ))

    [<Test>]
    let ``foldBack matches build list``() =

        fsCheck "DList" (Prop.forAll (Arb.fromGen DListIntGen) 
            (fun ((q :DList<int>), (l : int list)) -> foldBack (fun (elem : int) (l' : int list) -> elem::l') q [] = l ))
                  
        fsCheck "DList OfSeq" (Prop.forAll (Arb.fromGen DListIntOfSeqGen) 
            (fun ((q :DList<int>), (l : int list)) -> foldBack (fun (elem : int) (l' : int list) -> elem::l') q [] = l ))

        fsCheck "DList Add" (Prop.forAll (Arb.fromGen DListIntAddGen) 
             (fun ((q :DList<int>), (l : int list)) -> foldBack (fun (elem : int) (l' : int list) -> elem::l') q [] = l ))

    [<Test>]
    let ``foldBack matches build list 2``() =
        let q = ofSeq ["f";"e";"d";"c";"b";"a"]
        let lq = foldBack (fun (elem : string) (l' : string list) -> elem::l') q []
        lq |> shouldEqual (DList.toList q)

    [<Test>]
    let ``fold matches build list rev 2``() =
        let q = ofSeq ["f";"e";"d";"c";"b";"a"]
        let lq = fold (fun (l' : string list) (elem : string) -> elem::l') [] q
        lq |> shouldEqual (List.rev (DList.toList q))

    [<Test>]
    [<TestCaseSource("intGensStart1")>]
    let ``get head from DList``(x : obj) =
        let genAndName = unbox x 
        fsCheck (snd genAndName) (Prop.forAll (Arb.fromGen (fst genAndName)) (fun (q : DList<int>, l) -> (head q) = (List.nth l 0) ))

    [<Test>]
    [<TestCaseSource("intGensStart1")>]
    let ``get head from DList safely``(x : obj) =
        let genAndName = unbox x 
        fsCheck (snd genAndName) (Prop.forAll (Arb.fromGen (fst genAndName)) (fun (q : DList<int>, l) -> (tryHead q).Value = (List.nth l 0) ))

    [<Test>]
    [<TestCaseSource("intGensStart2")>]
    let ``get tail from DList``(x : obj) =
        let genAndName = unbox x 
        fsCheck (snd genAndName) (Prop.forAll (Arb.fromGen (fst genAndName)) (fun ((q : DList<int>), l) -> q.Tail.Head = (List.nth l 1) ))

    [<Test>]
    [<TestCaseSource("intGensStart2")>]
    let ``get tail from DList safely``(x : obj) =
        let genAndName = unbox x 
        fsCheck (snd genAndName) (Prop.forAll (Arb.fromGen (fst genAndName)) (fun (q : DList<int>, l) -> q.TryTail.Value.Head = (List.nth l 1) ))

    [<Test>]
    let ``give None if there is no head in the DList``() =
        emptyDList |> tryHead |> shouldEqual None

    [<Test>]
    let ``give None if there is no tail in the DList``() =
        emptyDList |> tryTail |> shouldEqual None

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
    let ``TryUncons wind-down to None``() =
        let q = ofSeq ["f";"e";"d";"c";"b";"a"] 

        let rec loop (q' : DList<string>) = 
            match (q'.TryUncons) with
            | Some(hd, tl) ->  loop tl
            | None -> ()

        loop q

        true |> shouldEqual true

    [<Test>]
    let ``Uncons wind-down to None``() =
        let q = ofSeq ["f";"e";"d";"c";"b";"a"] 

        let rec loop (q' : DList<string>) = 
            match (q'.Uncons) with
            | hd, tl when tl.IsEmpty ->  ()
            | hd, tl ->  loop tl

        loop q

        true |> shouldEqual true

    [<Test>]
    let ``test length should return 6``() =
        let q = ofSeq ["f";"e";"d";"c";"b";"a"] 
        length q |> shouldEqual 6

    [<Test>]
    let ``singleton length 1``() =
        singleton 1 |> length |> shouldEqual 1

    [<Test>]
    let ``empty length 0``() =
        empty |> length |> shouldEqual 0

    [<Test>]
    let ``test ofSeq should create a DList from a list``() =
        let test = [ for i in 0..4 -> i ]
        let x = DList.ofSeq test 
        x :> seq<_> |> shouldEqual (List.toSeq test) 

    [<Test>]
    let ``test ofSeq should create a DList from an array``() =
        let test = [| for i in 0..4 -> i |]
        DList.ofSeq test :> seq<_> |> shouldEqual (Array.toSeq test) 

    [<Test>]
    let ``test singleton should return a Unit containing the solo value``() =
        singleton 1 |> head |> shouldEqual 1

    [<Test>]
    let ``test append should join two DLists together``() =
        let q = ofSeq ["f";"e";"d";"c";"b";"a"]
        let q2 = ofSeq ["1";"2";"3";"4";"5";"6"]
        let q3 =  append q q2
        q3 |> length |> shouldEqual 12
        q3 |> head |> shouldEqual "f"

    [<Test>]
    let ``test toSeq``() =
        let q = ofSeq ["f";"e";"d";"c";"b";"a"] 
        List.ofSeq (DList.toSeq q) |> shouldEqual ["f";"e";"d";"c";"b";"a"]

    [<Test>]
    let ``test toList``() =
        let q = ofSeq ["f";"e";"d";"c";"b";"a"] 
        DList.toList q |> shouldEqual ["f";"e";"d";"c";"b";"a"]

    type DListGen =
        static member DList() =
            let rec dListGen() = 
                gen {
                    let! xs = Arb.generate
                    return DList.ofSeq (Seq.ofList xs)
                }
            Arb.fromGen (dListGen())

    let registerGen = lazy (Arb.register<DListGen>() |> ignore)

    [<Test>]
    let ``structural equality``() =

        let l1 = ofSeq [1..100]
        let l2 = ofSeq [1..100]

        l1 = l2 |> shouldEqual true

        let l3 = ofSeq [1..99] |> add 7

        l1 = l3 |> shouldEqual false