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
        
    [<Test>]
    let ``append works`` () =
        let list1 = NonEmptyList.create 1 [2; 3]
        let list2 = NonEmptyList.create 4 [5; 6]
        let result = NonEmptyList.append list1 list2
        Assert.AreEqual(NonEmptyList.create 1 [2; 3; 4; 5; 6], result)
    
    [<Test>]
    let ``choose works`` () =
        let list = NonEmptyList.create 1 [2; 3; 4]
        let result = NonEmptyList.choose (fun x -> if x % 2 = 0 then Some (x * 2) else None) list
        Assert.AreEqual(NonEmptyList.create 4 [8], result)
    
    [<Test>]
    let ``collect works`` () =
        let list = NonEmptyList.create 1 [2; 3]
        let result = NonEmptyList.collect (fun x -> NonEmptyList.create x [x * 2]) list
        Assert.AreEqual(NonEmptyList.create 1 [2; 2; 4; 3; 6], result)
    
    [<Test>]
    let ``concat works`` () =
        let lists = NonEmptyList.create (NonEmptyList.create 1 [2]) [NonEmptyList.create 3 [4]; NonEmptyList.create 5 [6]]
        let result = NonEmptyList.concat lists
        Assert.AreEqual(NonEmptyList.create 1 [2; 3; 4; 5; 6], result)
    
    [<Test>]
    let ``contains works`` () =
        let list = NonEmptyList.create 1 [2; 3; 4]
        let result = NonEmptyList.contains 3 list
        Assert.IsTrue(result)
    
    [<Test>]
    let ``distinct works`` () =
        fsCheck "list of int" (Prop.forAll (Arb.fromGen NonEmptyListIntOfSeqGen) (fun (q, l) -> 
            (q |> NonEmptyList.distinct |> NonEmptyList.toList) = (l |> List.distinct)))
    
    [<Test>]
    let ``exists works`` () =
        let list = NonEmptyList.create 1 [2; 3; 4]
        let result = NonEmptyList.exists (fun x -> x = 3) list
        Assert.IsTrue(result)
    
    [<Test>]
    let ``filter works`` () =
        let list = NonEmptyList.create 1 [2; 3; 4]
        let result = NonEmptyList.filter (fun x -> x % 2 = 0) list
        Assert.AreEqual(NonEmptyList.create 2 [4], result)
    
    [<Test>]
    let ``find works`` () =
        let list = NonEmptyList.create 1 [2; 3; 4]
        let result = NonEmptyList.find (fun x -> x = 3) list
        Assert.AreEqual(3, result)
    
    [<Test>]
    let ``findIndex works`` () =
        let list = NonEmptyList.create 1 [2; 3; 4]
        let result = NonEmptyList.findIndex (fun x -> x = 3) list
        Assert.AreEqual(2, result)
    
    [<Test>]
    let ``fold works`` () =
        let list = NonEmptyList.create 1 [2; 3; 4]
        let result = NonEmptyList.fold (+) 0 list
        Assert.AreEqual(10, result)
    
    [<Test>]
    let ``foldBack works`` () =
        let list = NonEmptyList.create 1 [2; 3; 4]
        let result = NonEmptyList.foldBack (+) list 0
        Assert.AreEqual(10, result)
    
    [<Test>]
    let ``forall works`` () =
        let list = NonEmptyList.create 1 [2; 3; 4]
        let result = NonEmptyList.forall (fun x -> x > 0) list
        Assert.IsTrue(result)
    
    [<Test>]
    let ``groupBy works`` () =
        let list = NonEmptyList.create 1 [2; 3; 4; 5]
        let result = NonEmptyList.groupBy (fun x -> x % 2) list
        let expected = NonEmptyList.create (1, NonEmptyList.create 1 [3; 5]) [(0, NonEmptyList.create 2 [4])]
        Assert.AreEqual(expected, result)

    [<Test>]
    let ``indexed works`` () =
        let list = NonEmptyList.create 1 [2; 3; 4]
        let result = NonEmptyList.indexed list
        Assert.AreEqual(NonEmptyList.create (0, 1) [(1, 2); (2, 3); (3, 4)], result)
    
    [<Test>]
    let ``init works`` () =
        let result = NonEmptyList.init 4 (fun i -> i * 2)
        Assert.AreEqual(NonEmptyList.create 0 [2; 4; 6], result)
    
    [<Test>]
    let ``insertAt works`` () =
        let list = NonEmptyList.create 1 [2; 3; 4]
        let result = NonEmptyList.insertAt 2 99 list
        Assert.AreEqual(NonEmptyList.create 1 [2; 99; 3; 4], result)
    
    [<Test>]
    let ``insertManyAt works`` () =
        let list = NonEmptyList.create 1 [2; 3; 4]
        let result = NonEmptyList.insertManyAt 2 [99; 100] list
        Assert.AreEqual(NonEmptyList.create 1 [2; 99; 100; 3; 4], result)
    
    [<Test>]
    let ``item works`` () =
        let list = NonEmptyList.create 1 [2; 3; 4]
        let result = NonEmptyList.item 2 list
        Assert.AreEqual(3, result)
    
    [<Test>]
    let ``iter works`` () =
        let list = NonEmptyList.create 1 [2; 3; 4]
        let mutable sum = 0
        NonEmptyList.iter (fun x -> sum <- sum + x) list
        Assert.AreEqual(10, sum)
    
    [<Test>]
    let ``iter2 works`` () =
        let list1 = NonEmptyList.create 1 [2; 3]
        let list2 = NonEmptyList.create 4 [5; 6]
        let mutable sum = 0
        NonEmptyList.iter2 (fun x y -> sum <- sum + x + y) list1 list2
        Assert.AreEqual(21, sum)
    
    [<Test>]
    let ``iteri works`` () =
        let list = NonEmptyList.create 1 [2; 3; 4]
        let mutable sum = 0
        NonEmptyList.iteri (fun i x -> sum <- sum + i + x) list
        Assert.AreEqual(16, sum)
    
    [<Test>]
    let ``iteri2 works`` () =
        let list1 = NonEmptyList.create 1 [2; 3]
        let list2 = NonEmptyList.create 4 [5; 6]
        let mutable sum = 0
        NonEmptyList.iteri2 (fun i x y -> sum <- sum + i + x + y) list1 list2
        Assert.AreEqual(24, sum)
    
    [<Test>]
    let ``last works`` () =
        let list = NonEmptyList.create 1 [2; 3; 4]
        let result = NonEmptyList.last list
        Assert.AreEqual(4, result)
    
    [<Test>]
    let ``map2 works`` () =
        let list1 = NonEmptyList.create 1 [2; 3]
        let list2 = NonEmptyList.create 4 [5; 6]
        let result = NonEmptyList.map2 (fun x y -> x + y) list1 list2
        Assert.AreEqual(NonEmptyList.create 5 [7; 9], result)
    
    [<Test>]
    let ``map3 works`` () =
        let list1 = NonEmptyList.create 1 [2; 3]
        let list2 = NonEmptyList.create 4 [5; 6]
        let list3 = NonEmptyList.create 7 [8; 9]
        let result = NonEmptyList.map3 (fun x y z -> x + y + z) list1 list2 list3
        Assert.AreEqual(NonEmptyList.create 12 [15; 18], result)
    
    [<Test>]
    let ``mapFold works`` () =
        let list = NonEmptyList.create 1 [2; 3]
        let result, state = NonEmptyList.mapFold (fun acc x -> (x * 2, acc + x)) 0 list
        Assert.AreEqual(NonEmptyList.create 2 [4; 6], result)
        Assert.AreEqual(6, state)
    
    [<Test>]
    let ``mapFoldBack works`` () =
        let list = NonEmptyList.create 1 [2; 3]
        let result, state = NonEmptyList.mapFoldBack (fun x acc -> (x * 2, acc + x)) list 0
        Assert.AreEqual(NonEmptyList.create 2 [4; 6], result)
        Assert.AreEqual(6, state)
    
    [<Test>]
    let ``mapi works`` () =
        let list = NonEmptyList.create 1 [2; 3]
        let result = NonEmptyList.mapi (fun i x -> i + x) list
        Assert.AreEqual(NonEmptyList.create 1 [3; 5], result)
    
    [<Test>]
    let ``mapi2 works`` () =
        let list1 = NonEmptyList.create 1 [2; 3]
        let list2 = NonEmptyList.create 4 [5; 6]
        let result = NonEmptyList.mapi2 (fun i x y -> i + x + y) list1 list2
        Assert.AreEqual(NonEmptyList.create 5 [8; 11], result)
    
    [<Test>]
    let ``max works`` () =
        let list = NonEmptyList.create 1 [2; 3; 4]
        let result = NonEmptyList.max list
        Assert.AreEqual(4, result)
    
    [<Test>]
    let ``maxBy works`` () =
        let list = NonEmptyList.create 1 [2; 3; 4]
        let result = NonEmptyList.maxBy (fun x -> -x) list
        Assert.AreEqual(1, result)
    
    [<Test>]
    let ``min works`` () =
        let list = NonEmptyList.create 1 [2; 3; 4]
        let result = NonEmptyList.min list
        Assert.AreEqual(1, result)
    
    [<Test>]
    let ``minBy works`` () =
        let list = NonEmptyList.create 1 [2; 3; 4]
        let result = NonEmptyList.minBy (fun x -> -x) list
        Assert.AreEqual(4, result)
    
    [<Test>]
    let ``pairwise works`` () =
        let list = NonEmptyList.create 1 [2; 3; 4]
        let result = NonEmptyList.pairwise list
        Assert.AreEqual(NonEmptyList.create (1, 2) [(2, 3); (3, 4)], result)
    
    [<Test>]
    let ``partition works`` () =
        let list = NonEmptyList.create 1 [2; 3; 4]
        let result1, result2 = NonEmptyList.partition (fun x -> x % 2 = 0) list
        Assert.AreEqual([2; 4], result1)
        Assert.AreEqual([1; 3], result2)
    
    [<Test>]
    let ``permute works`` () =
        let list = NonEmptyList.create 1 [2; 3; 4]
        let result = NonEmptyList.permute (fun i -> (i + 1) % 4) list
        Assert.AreEqual(NonEmptyList.create 4 [1; 2; 3], result)
    
    [<Test>]
    let ``pick works`` () =
        let list = NonEmptyList.create 1 [2; 3; 4]
        let result = NonEmptyList.pick (fun x -> if x % 2 = 0 then Some (x * 2) else None) list
        Assert.AreEqual(4, result)
    
    [<Test>]
    let ``removeAt works`` () =
        let list = NonEmptyList.create 1 [2; 3; 4]
        let result = NonEmptyList.removeAt 2 list
        Assert.AreEqual(NonEmptyList.create 1 [2; 4], result)
    
    [<Test>]
    let ``removeManyAt works`` () =
        let list = NonEmptyList.create 1 [2; 3; 4]
        let result = NonEmptyList.removeManyAt 1 2 list
        Assert.AreEqual(NonEmptyList.create 1 [4], result)
    
    [<Test>]
    let ``replicate works`` () =
        let result = NonEmptyList.replicate 4 99
        Assert.AreEqual(NonEmptyList.create 99 [99; 99; 99], result)
    
    [<Test>]
    let ``rev works`` () =
        let list = NonEmptyList.create 1 [2; 3; 4]
        let result = NonEmptyList.rev list
        Assert.AreEqual(NonEmptyList.create 4 [3; 2; 1], result)
    
    [<Test>]
    let ``scan works`` () =
        let list = NonEmptyList.create 1 [2; 3; 4]
        let result = NonEmptyList.scan (+) 0 list
        Assert.AreEqual(NonEmptyList.create 0 [1; 3; 6; 10], result)
    
    [<Test>]
    let ``scanBack works`` () =
        let list = NonEmptyList.create 1 [2; 3; 4]
        let result = NonEmptyList.scanBack (+) list 0
        Assert.AreEqual(NonEmptyList.create 10 [9; 7; 4; 0], result)
    
    [<Test>]
    let ``singleton works`` () =
        let result = NonEmptyList.singleton 99
        Assert.AreEqual(NonEmptyList.create 99 [], result)
    
    [<Test>]
    let ``skip works`` () =
        let list = NonEmptyList.create 1 [2; 3; 4]
        let result = NonEmptyList.skip 2 list
        Assert.AreEqual(NonEmptyList.create 3 [4], result)
    
    [<Test>]
    let ``skipWhile works`` () =
        let list = NonEmptyList.create 1 [2; 3; 4]
        let result = NonEmptyList.skipWhile (fun x -> x < 3) list
        Assert.AreEqual(NonEmptyList.create 3 [4], result)
    
    [<Test>]
    let ``sort works`` () =
        let list = NonEmptyList.create 3 [1; 4; 2]
        let result = NonEmptyList.sort list
        Assert.AreEqual(NonEmptyList.create 1 [2; 3; 4], result)
    
    [<Test>]
    let ``sortBy works`` () =
        let list = NonEmptyList.create 3 [1; 4; 2]
        let result = NonEmptyList.sortBy (fun x -> -x) list
        Assert.AreEqual(NonEmptyList.create 4 [3; 2; 1], result)
    
    [<Test>]
    let ``sortByDescending works`` () =
        let list = NonEmptyList.create 3 [1; 4; 2]
        let result = NonEmptyList.sortByDescending id list
        Assert.AreEqual(NonEmptyList.create 4 [3; 2; 1], result)
    
    [<Test>]
    let ``sortDescending works`` () =
        let list = NonEmptyList.create 3 [1; 4; 2]
        let result = NonEmptyList.sortDescending list
        Assert.AreEqual(NonEmptyList.create 4 [3; 2; 1], result)
    
    [<Test>]
    let ``sortWith works`` () =
        let list = NonEmptyList.create 3 [1; 4; 2]
        let result = NonEmptyList.sortWith compare list
        Assert.AreEqual(NonEmptyList.create 1 [2; 3; 4], result)
    
    [<Test>]
    let ``splitAt works`` () =
        let list = NonEmptyList.create 1 [2; 3; 4]
        let result1, result2 = NonEmptyList.splitAt 2 list
        Assert.AreEqual(NonEmptyList.create 1 [2], result1)
        Assert.AreEqual(NonEmptyList.create 3 [4], result2)
    
    [<Test>]
    let ``splitInto works`` () =
        let list = NonEmptyList.create 1 [2; 3; 4; 5]
        let result = NonEmptyList.splitInto 2 list
        let expected = NonEmptyList.create (NonEmptyList.create 1 [2; 3]) [NonEmptyList.create 4 [5]]
        shoulSeqEqual expected result
    
    [<Test>]
    let ``sum works`` () =
        let list = NonEmptyList.create 1 [2; 3; 4]
        let result = NonEmptyList.sum list
        Assert.AreEqual(10, result)
    
    [<Test>]
    let ``sumBy works`` () =
        let list = NonEmptyList.create 1 [2; 3; 4]
        let result = NonEmptyList.sumBy (fun x -> x * 2) list
        Assert.AreEqual(20, result)
    
    [<Test>]
    let ``tail works`` () =
        let list = NonEmptyList.create 1 [2; 3; 4]
        let result = NonEmptyList.tail list
        Assert.AreEqual(NonEmptyList.create 2 [3; 4], result)
    
    [<Test>]
    let ``take works`` () =
        let list = NonEmptyList.create 1 [2; 3; 4]
        let result = NonEmptyList.take 2 list
        Assert.AreEqual(NonEmptyList.create 1 [2], result)
    
    [<Test>]
    let ``takeWhile works`` () =
        let list = NonEmptyList.create 1 [2; 3; 4]
        let result = NonEmptyList.takeWhile (fun x -> x < 3) list
        Assert.AreEqual(NonEmptyList.create 1 [2], result)
    
    [<Test>]
    let ``transpose works`` () =
        let list = NonEmptyList.create (NonEmptyList.create 1 [2]) [NonEmptyList.create 3 [4]]
        let result = NonEmptyList.transpose list
        Assert.AreEqual(NonEmptyList.create (NonEmptyList.create 1 [3]) [NonEmptyList.create 2 [4]], result)
    
    [<Test>]
    let ``truncate works`` () =
        let list = NonEmptyList.create 1 [2; 3; 4]
        let result = NonEmptyList.truncate 2 list
        Assert.AreEqual(NonEmptyList.create 1 [2], result)
    
    [<Test>]
    let ``tryExactlyOne works`` () =
        let list = NonEmptyList.create 1 []
        let result = NonEmptyList.tryExactlyOne list
        Assert.AreEqual(Some 1, result)
    
    [<Test>]
    let ``tryFind works`` () =
        let list = NonEmptyList.create 1 [2; 3; 4]
        let result = NonEmptyList.tryFind (fun x -> x % 2 = 0) list
        Assert.AreEqual(Some 2, result)
    
    [<Test>]
    let ``tryFindBack works`` () =
        let list = NonEmptyList.create 1 [2; 3; 4]
        let result = NonEmptyList.tryFindBack (fun x -> x % 2 = 0) list
        Assert.AreEqual(Some 4, result)
    
    [<Test>]
    let ``tryFindIndex works`` () =
        let list = NonEmptyList.create 1 [2; 3; 4]
        let result = NonEmptyList.tryFindIndex (fun x -> x % 2 = 0) list
        Assert.AreEqual(Some 1, result)
    
    [<Test>]
    let ``tryFindIndexBack works`` () =
        let list = NonEmptyList.create 1 [2; 3; 4]
        let result = NonEmptyList.tryFindIndexBack (fun x -> x % 2 = 0) list
        Assert.AreEqual(Some 3, result)
    
    [<Test>]
    let ``tryItem works`` () =
        let list = NonEmptyList.create 1 [2; 3; 4]
        let result = NonEmptyList.tryItem 2 list
        Assert.AreEqual(Some 3, result)
    
    [<Test>]
    let ``tryLast works`` () =
        let list = NonEmptyList.create 1 [2; 3; 4]
        let result = NonEmptyList.tryLast list
        Assert.AreEqual(Some 4, result)
    
    [<Test>]
    let ``tryPick works`` () =
        let list = NonEmptyList.create 1 [2; 3; 4]
        let result = NonEmptyList.tryPick (fun x -> if x % 2 = 0 then Some (x * 2) else None) list
        Assert.AreEqual(Some 4, result)
    
    [<Test>]
    let ``unfold works`` () =
        let result = NonEmptyList.unfold (fun state -> if state > 3 then None else Some (state, state + 1)) 0
        Assert.AreEqual(NonEmptyList.create 0 [1; 2; 3], result)
    
    [<Test>]
    let ``unzip works`` () =
        let list = NonEmptyList.create (1, 'a') [(2, 'b'); (3, 'c')]
        let result1, result2 = NonEmptyList.unzip list
        Assert.AreEqual(NonEmptyList.create 1 [2; 3], result1)
        Assert.AreEqual(NonEmptyList.create 'a' ['b'; 'c'], result2)
    
    [<Test>]
    let ``unzip3 works`` () =
        let list = NonEmptyList.create (1, 'a', true) [(2, 'b', false); (3, 'c', true)]
        let result1, result2, result3 = NonEmptyList.unzip3 list
        Assert.AreEqual(NonEmptyList.create 1 [2; 3], result1)
        Assert.AreEqual(NonEmptyList.create 'a' ['b'; 'c'], result2)
        Assert.AreEqual(NonEmptyList.create true [false; true], result3)
    
    [<Test>]
    let ``updateAt works`` () =
        let list = NonEmptyList.create 1 [2; 3; 4]
        let result = NonEmptyList.updateAt 2 99 list
        Assert.AreEqual(NonEmptyList.create 1 [2; 99; 4], result)
    
    [<Test>]
    let ``where works`` () =
        let list = NonEmptyList.create 1 [2; 3; 4]
        let result = NonEmptyList.where (fun x -> x % 2 = 0) list
        Assert.AreEqual(NonEmptyList.create 2 [4], result)
    
    [<Test>]
    let ``windowed works`` () =
        let list = NonEmptyList.create 1 [2; 3; 4]
        let result = NonEmptyList.windowed 2 list
        Assert.AreEqual(NonEmptyList.create (NonEmptyList.create 1 [2]) [NonEmptyList.create 2 [3]; NonEmptyList.create 3 [4]], result)
    
    [<Test>]
    let ``zip works`` () =
        let list1 = NonEmptyList.create 1 [2; 3]
        let list2 = NonEmptyList.create 'a' ['b'; 'c']
        let result = NonEmptyList.zip list1 list2
        Assert.AreEqual(NonEmptyList.create (1, 'a') [(2, 'b'); (3, 'c')], result)
    
    [<Test>]
    let ``zip3 works`` () =
        let list1 = NonEmptyList.create 1 [2; 3]
        let list2 = NonEmptyList.create 'a' ['b'; 'c']
        let list3 = NonEmptyList.create true [false; true]
        let result = NonEmptyList.zip3 list1 list2 list3
        Assert.AreEqual(NonEmptyList.create (1, 'a', true) [(2, 'b', false); (3, 'c', true)], result)
    
module MultiMap =
    [<Test>]
    let ``monoid works`` () =
        MultiMap.ofList [1, 'a'; 3, 'b'] ++ MultiMap.ofList [1, 'c'; 5, 'd'; 6, 'e'] |> shoulSeqEqual (MultiMap.ofList [1, 'a'; 1, 'c'; 3, 'b'; 5, 'd'; 6, 'e'])