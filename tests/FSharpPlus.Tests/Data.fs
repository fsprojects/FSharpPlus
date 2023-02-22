module FSharpPlus.Tests.Data

open System
open NUnit.Framework
open FsCheck
open FSharpPlus
open FSharpPlus.Data
module Gen = // FSharpx.Collections extension of Gen
    let listInt n  = Gen.listOfLength n Arb.generate<int>
    let listObj n  = Gen.listOfLength n Arb.generate<obj>
    let listString n  = Gen.listOfLength n Arb.generate<string>
    let length1thru12 = Gen.choose (1, 12)
    let length2thru12 = Gen.choose (2, 12)
let fsCheck s x = Check.One({Config.QuickThrowOnFailure with Name = s}, x)


// tests from FSharpx.Collections DList
let shouldEqual (x: 't) (y: 't) = Assert.AreEqual (x, y)
let shoulSeqEqual (x: 't) (y: 't) = CollectionAssert.AreEqual (x, y)
let shouldThrow (exnT: Type) (x: unit -> unit) = Assert.Throws(exnT, TestDelegate (x)) |> ignore

module ZipList =

    module Alternative =
        let _123andZeroesToTheInfinite = ZipList [1;2;3] <|> result 0
        let _ZeroesToTheInfiniteAnd123 = result 0 <|> ZipList [1;2;3]
        ()

module DList =

    module Monoid =        
        let _Empty: DList<int> = empty
        let _Zero: DList<int> = zero
        let _42: DList<int> = ofList [4] + ofList [2]

    let emptyDList = DList.empty

    let enDListThruList l q =
        let rec loop (q': 'a DList) (l': 'a list) = 
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
        v.[1] <- box ((DListIntOfSeqGen |> Gen.where (fun (_, l) -> l.Length >= start)), "DList OfSeq")
        v.[2] <- box ((DListIntAddGen |> Gen.where (fun (_, l) -> l.Length >= start)), "DList addDList") 
        v

    let intGensStart1 =
        intGens 1  //this will accept all

    let intGensStart2 =
        intGens 2 // this will accept 11 out of 12

    [<Test>]
    let ``allow to tail to work`` () =
        emptyDList |> DList.add 1 |> DList.tail |> DList.isEmpty |> shouldEqual true

    [<Test>]
    let ``add to work`` () =
        emptyDList |> DList.add 1 |> DList.add 2 |> DList.isEmpty |> shouldEqual false

    [<Test>]
    let ``cons to work`` () =
        emptyDList |> DList.cons 1 |> DList.cons 2 |> DList.length |> shouldEqual 2

    [<Test>]
    let ``allow to cons and add to work`` () =
        emptyDList |> DList.cons 1 |> DList.cons 2 |> DList.add 3 |> DList.length |> shouldEqual 3

    [<Test>]
    let ``empty DList should be empty`` () =
        emptyDList |> DList.isEmpty |> shouldEqual true

    [<Test>]
    let ``fail if there is no head in the DList`` () =
        (fun () -> emptyDList |> DList.head |> ignore) |> shouldThrow typeof<System.Exception>

    [<Test>]
    let ``fail if there is no tail in the DList`` () =
        (fun () -> emptyDList |> DList.tail |> ignore) |> shouldThrow typeof<System.Exception>

    [<Test>]
    let ``fold matches build list rev`` () =

        fsCheck "DList" (Prop.forAll (Arb.fromGen DListIntGen) 
            (fun ((q :DList<int>), (l : int list)) -> q |> DList.fold (fun (l' : int list) (elem : int) -> elem::l') [] = (List.rev l) ))
                  
        fsCheck "DList OfSeq" (Prop.forAll (Arb.fromGen DListIntOfSeqGen) 
            (fun ((q :DList<int>), (l : int list)) -> q |> DList.fold (fun (l' : int list) (elem : int) -> elem::l') [] = (List.rev l) ))

        fsCheck "DList Add" (Prop.forAll (Arb.fromGen DListIntAddGen) 
             (fun ((q :DList<int>), (l : int list)) -> q |> DList.fold (fun (l' : int list) (elem : int) -> elem::l') [] = (List.rev l) ))

    [<Test>]
    let ``foldBack matches build list`` () =

        fsCheck "DList" (Prop.forAll (Arb.fromGen DListIntGen) 
            (fun ((q :DList<int>), (l : int list)) -> DList.foldBack (fun (elem : int) (l' : int list) -> elem::l') q [] = l ))
                  
        fsCheck "DList OfSeq" (Prop.forAll (Arb.fromGen DListIntOfSeqGen) 
            (fun ((q :DList<int>), (l : int list)) -> DList.foldBack (fun (elem : int) (l' : int list) -> elem::l') q [] = l ))

        fsCheck "DList Add" (Prop.forAll (Arb.fromGen DListIntAddGen) 
             (fun ((q :DList<int>), (l : int list)) -> DList.foldBack (fun (elem : int) (l' : int list) -> elem::l') q [] = l ))

    [<Test>]
    let ``foldBack matches build list 2`` () =
        let q = DList.ofSeq ["f";"e";"d";"c";"b";"a"]
        let lq = DList.foldBack (fun (elem : string) (l' : string list) -> elem::l') q []
        lq |> shouldEqual (DList.toList q)

    [<Test>]
    let ``fold matches build list rev 2`` () =
        let q = DList.ofSeq ["f";"e";"d";"c";"b";"a"]
        let lq = DList.fold (fun (l' : string list) (elem : string) -> elem::l') [] q
        lq |> shouldEqual (List.rev (DList.toList q))

    [<Test>]
    [<TestCaseSource("intGensStart1")>]
    let ``get head from DList`` (x: obj) =
        let genAndName = unbox x 
        fsCheck (snd genAndName) (Prop.forAll (Arb.fromGen (fst genAndName)) (fun (q : DList<int>, l) -> (DList.head q) = (List.item 0 l) ))

    [<Test>]
    [<TestCaseSource("intGensStart1")>]
    let ``get head from DList safely`` (x: obj) =
        let genAndName = unbox x 
        fsCheck (snd genAndName) (Prop.forAll (Arb.fromGen (fst genAndName)) (fun (q : DList<int>, l) -> (DList.tryHead q).Value = (List.item 0 l) ))

    [<Test>]
    [<TestCaseSource("intGensStart2")>]
    let ``get tail from DList`` (x: obj) =
        let genAndName = unbox x 
        fsCheck (snd genAndName) (Prop.forAll (Arb.fromGen (fst genAndName)) (fun ((q : DList<int>), l) -> q.Tail.Head = (List.item 1 l) ))

    [<Test>]
    [<TestCaseSource("intGensStart2")>]
    let ``get tail from DList safely`` (x: obj) =
        let genAndName = unbox x 
        fsCheck (snd genAndName) (Prop.forAll (Arb.fromGen (fst genAndName)) (fun (q : DList<int>, l) -> q.TryTail.Value.Head = (List.item 1 l) ))

    [<Test>]
    let ``give None if there is no head in the DList`` () =
        emptyDList |> DList.tryHead |> shouldEqual None

    [<Test>]
    let ``give None if there is no tail in the DList`` () =
        emptyDList |> DList.tryTail |> shouldEqual None

    [<Test>]
    [<TestCaseSource("intGensStart1")>]
    let ``int DList builds and serializes`` (x: obj) =
        let genAndName = unbox x 
        fsCheck (snd genAndName) (Prop.forAll (Arb.fromGen (fst genAndName)) (fun (q : DList<int>, l) -> q |> Seq.toList = l ))

    [<Test>]
    let ``obj DList builds and serializes`` () =
        fsCheck "obj DList" (Prop.forAll (Arb.fromGen DListObjGen) (fun (q : DList<obj>, l) -> q |> Seq.toList = l ))

    [<Test>]
    let ``string DList builds and serializes`` () =
        fsCheck "string DList" (Prop.forAll (Arb.fromGen DListStringGen) (fun (q : DList<string>, l) -> q |> Seq.toList = l ))

    [<Test>]
    let ``TryUncons wind-down to None`` () =
        let q = DList.ofSeq ["f";"e";"d";"c";"b";"a"] 

        let rec loop (q' : DList<string>) = 
            match (q'.TryUncons) with
            | Some(hd, tl) ->  loop tl
            | None -> ()

        loop q

        true |> shouldEqual true

    [<Test>]
    let ``Uncons wind-down to None`` () =
        let q = DList.ofSeq ["f";"e";"d";"c";"b";"a"] 

        let rec loop (q' : DList<string>) = 
            match (q'.Uncons) with
            | hd, tl when tl.IsEmpty -> ()
            | hd, tl ->  loop tl

        loop q

        true |> shouldEqual true

    [<Test>]
    let ``test length should return 6`` () =
        let q = DList.ofSeq ["f";"e";"d";"c";"b";"a"] 
        DList.length q |> shouldEqual 6

    [<Test>]
    let ``singleton length 1`` () =
        DList.singleton 1 |> DList.length |> shouldEqual 1

    [<Test>]
    let ``empty length 0`` () =
        DList.empty |> DList.length |> shouldEqual 0

    [<Test>]
    let ``test ofSeq should create a DList from a list`` () =
        let test = [ for i in 0..4 -> i ]
        let x = DList.ofSeq test 
        x :> seq<_> |> shouldEqual (List.toSeq test) 

    [<Test>]
    let ``test ofSeq should create a DList from an array`` () =
        let test = [| for i in 0..4 -> i |]
        DList.ofSeq test :> seq<_> |> shouldEqual (Array.toSeq test) 

    [<Test>]
    let ``test singleton should return a Unit containing the solo value`` () =
        DList.singleton 1 |> DList.head |> shouldEqual 1

    [<Test>]
    let ``test append should join two DLists together`` () =
        let q = DList.ofSeq ["f";"e";"d";"c";"b";"a"]
        let q2 = DList.ofSeq ["1";"2";"3";"4";"5";"6"]
        let q3 = DList.append q q2
        q3 |> DList.length |> shouldEqual 12
        q3 |> DList.head |> shouldEqual "f"

    [<Test>]
    let ``test toSeq`` () =
        let q = DList.ofSeq ["f";"e";"d";"c";"b";"a"] 
        List.ofSeq (DList.toSeq q) |> shouldEqual ["f";"e";"d";"c";"b";"a"]

    [<Test>]
    let ``test toList`` () =
        let q = DList.ofSeq ["f";"e";"d";"c";"b";"a"] 
        DList.toList q |> shouldEqual ["f";"e";"d";"c";"b";"a"]

    type DListGen =
        static member DList () =
            let rec dListGen () = 
                gen {
                    let! xs = Arb.generate
                    return DList.ofSeq (Seq.ofList xs)
                }
            Arb.fromGen (dListGen ())

    let registerGen = lazy (Arb.register<DListGen> () |> ignore)

    [<Test>]
    let ``structural equality`` () =

        let l1 = DList.ofSeq [1..100]
        let l2 = DList.ofSeq [1..100]

        l1 = l2 |> shouldEqual true

        let l3 = DList.ofSeq [1..99] |> DList.add 7

        l1 = l3 |> shouldEqual false

    [<Test>]
    let ``get [i]`` () =
        let l1 = [1..100]
                 |> List.map string
                 |>  DList.ofSeq
        for i1 in [0..99] do
            let v= string (i1+1)
            shouldEqual v l1.[i1]

    let assertThrowsIndexOutOfRange fn = Assert.Throws<System.IndexOutOfRangeException> (fun () -> fn () |> ignore) |> ignore
    [<Test>]
    let ``get [i] outside of range`` () =
        let l1 = [1..100]
                 |> List.map string
                 |>  DList.ofSeq
        assertThrowsIndexOutOfRange (fun _ -> l1.[100])
        assertThrowsIndexOutOfRange (fun _ -> l1.[-1])

module NonEmptyList =
    let nonEmptyList = NonEmptyList.create 1 []

    let NonEmptyListIntOfSeqGen =
        gen {   let! n = Gen.length1thru12
                let! x = Gen.listInt n
                return ( (NonEmptyList.ofList x), x) }

    let TwoNonEmptyListIntOfSeqGen =
        gen {   let! n1 = Gen.length1thru12
                let! x1 = Gen.listInt n1
                let! n2 = Gen.length1thru12
                let! x2 = Gen.listInt n2
                return ((NonEmptyList.ofList x1, NonEmptyList.ofList x2), (x1, x2)) }
        
    let NonEmptyListStringGen =
        gen {   let! n = Gen.length1thru12
                let! n2 = Gen.length2thru12
                let! x =  Gen.listString n
                let! y =  Gen.listString n2
                return ( (NonEmptyList.ofList x ++ NonEmptyList.ofList y), (x @ y) ) }

    [<Test>]
    let ``cons works`` () =
        nonEmptyList |> NonEmptyList.cons 2 |> NonEmptyList.toList |> shoulSeqEqual [2;1]

    [<Test>]
    let ``zip `` () =
        nonEmptyList |> NonEmptyList.zip nonEmptyList |> NonEmptyList.toList |> shoulSeqEqual [(1,1)]
    
    [<Test>]
    let zipShortest () =
        let nonEmptyList' = nonEmptyList |> NonEmptyList.cons 2
        nonEmptyList |> NonEmptyList.zipShortest nonEmptyList' |> NonEmptyList.toList |> shoulSeqEqual [(2,1)]

    [<Test>]
    let ``get head from NonEmptyList`` () =
        fsCheck "list of int" (Prop.forAll (Arb.fromGen NonEmptyListIntOfSeqGen) (fun (q : NonEmptyList<int>, l) -> (NonEmptyList.head q) = (List.item 0 l) ))

    [<Test>]
    let ``get tail from NonEmptyList`` () =
        let atLeastOfLengthTwo (_,l) = List.length l >= 2
        fsCheck "list of int" (Prop.forAll (Arb.fromGen NonEmptyListIntOfSeqGen |> Arb.filter atLeastOfLengthTwo) (fun ((q : NonEmptyList<int>), l) -> (NonEmptyList.tail q |> NonEmptyList.toList) = (List.tail l) ))

    [<Test>]
    let ``get length of NonEmptyList`` () =
        fsCheck "list of int" (Prop.forAll (Arb.fromGen NonEmptyListIntOfSeqGen) (fun ((q : NonEmptyList<int>), l) -> q.Length = (List.length l) ))

    [<Test>]
    let ``int NonEmptyList builds and serializes`` () =
        fsCheck "list of int" (Prop.forAll (Arb.fromGen NonEmptyListIntOfSeqGen) (fun (q : NonEmptyList<int>, l) -> q |> Seq.toList = l ))

    [<Test>]
    let ``string NonEmptyList builds and serializes`` () =
        fsCheck "string NonEmptyList" (Prop.forAll (Arb.fromGen NonEmptyListStringGen) (fun (q : NonEmptyList<string>, l) -> q |> Seq.toList = l ))

    [<Test>]
    let ``toArray`` () =
        fsCheck "string NonEmptyList" (Prop.forAll (Arb.fromGen NonEmptyListStringGen) (fun (q : NonEmptyList<string>, l) -> q |> NonEmptyList.toArray = List.toArray l ))

    [<Test>]
    let ``map on non empty list should equal map on list`` () =
        fsCheck "list of int" (Prop.forAll (Arb.fromGen NonEmptyListIntOfSeqGen) (fun (q : NonEmptyList<int>, l) -> (q |> NonEmptyList.map string |> NonEmptyList.toList) = (l |> List.map string)))

    [<Test>]
    let ``map2Shortest on non empty list should equal map2Shortest on list`` () =
        fsCheck "list of int" (Prop.forAll (Arb.fromGen TwoNonEmptyListIntOfSeqGen) (fun ((nel1, nel2), (l1, l2)) ->
            (NonEmptyList.map2Shortest (+) nel1 nel2 |> NonEmptyList.toList) = (List.map2Shortest (+) l1 l2)))
        
    [<Test>]
    let ``mapi on non empty list should equal mapi on list`` () =
        let mapOp a b = sprintf "%d-%d" a b
        fsCheck "list of int" (Prop.forAll (Arb.fromGen NonEmptyListIntOfSeqGen) (fun (q : NonEmptyList<int>, l) -> (q |> NonEmptyList.mapi mapOp |> NonEmptyList.toList) = (l |> List.mapi mapOp)))

module MultiMap =
    [<Test>]
    let ``monoid works`` () =
        MultiMap.ofList [1, 'a'; 3, 'b'] ++ MultiMap.ofList [1, 'c'; 5, 'd'; 6, 'e'] |> shoulSeqEqual (MultiMap.ofList [1, 'a'; 1, 'c'; 3, 'b'; 5, 'd'; 6, 'e'])