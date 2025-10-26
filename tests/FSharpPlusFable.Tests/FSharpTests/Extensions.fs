module ExtensionsTests

open Testing
open FSharpPlus
open System.Collections.Generic

open FSharpPlus.Data
open System



let ExtensionsTest = 

    let dlistA = DList.ofSeq [1;2;3]
    let dlistB = DList.ofSeq [1;2;3]
    let dlistC = DList.ofSeq [1;2]
    let dlistD = DList.ofSeq [1 :> obj;2:> obj;3:> obj]

    testList "Extension Tests" [

      testCase "Applying Option.Zip and Option.Unzip returns the original value" 
        (fun () -> let (fst, lst) = Option.unzip (Some (1, 2))
                   equal (Some (1, 2)) (Option.zip fst lst))

      testCase "List.Cons of an item and empty list is equivalent to List.Singleton" 
        (fun () -> let m1 = List.cons 1 List.empty
                   let m2 = List.singleton 1
                   equal m1 m2)

      testCase "Map.union gives same map when joined with an empty map (identity)" 
        (fun () -> let m1 = [1, "2"; 2,"4"; 4,"8"] |> Map.ofList
                   let r1 = Map.union m1 Map.empty |> Map.toList
                   equalSeq [1, "2"; 2,"4"; 4,"8"] r1)

      testCase "Map.union returns same results independent of the order (associative)" 
        (fun () -> let m1 = [1, "1"; 2,"2"; 3,"3"] |> Map.ofList
                   let m2 = [3, "3"; 4,"4"; 5,"6"] |> Map.ofList
                   let r1 = m1 |> Map.union m2
                   let r2 = m2 |> Map.union m1
                   equalMap r1 r2)

      testCase "Map.union provides same end result as Map.unionWith picking the first source value for dupes" 
        (fun () -> let m1 = [1, "2"; 2,"4"; 4,"8"] |> Map.ofList
                   let m2 = [1, "4"; 2,"8"; 4,"16"] |> Map.ofList
                   let r1 = m1 |> Map.union m2
                   let r2 = m1 |> Map.unionWith konst m2
                   equalMap r1 r2)

#if !FABLE_COMPILER
      testCase "Bind" 
        (fun () ->  let x = [1;2] >>= fun x -> [string x ; string (x + 1000) ]
                    let y = { Head = 1; Tail = [2] } >>= fun x -> { Head = string x ; Tail = [string (x + 1000)] }
                    let z = ("a", 1) >>= fun x -> (string x, x + 10)
                    equal ["1"; "1001"; "2"; "1002"] x
                    equal { Head = "1"; Tail = ["1001"; "2"; "1002"] } y
                    equal ("a1", 11) z)
#endif
#if !FABLE_COMPILER
      testCase "Comonad" 
        (fun () ->  let x = [1;2;3;4;5]
                    let y = { Head = 1 ; Tail = [2;3;4;5] }
                    equal (List.head x) 1
                    equal (y.Head) 1
                    equal (duplicate x) [[1; 2; 3; 4; 5]; [2; 3; 4; 5]; [3; 4; 5]; [4; 5]; [5]]
                    equal (duplicate y) { Head = { Head = 1; Tail = [2; 3; 4; 5] }; Tail = [{ Head = 2; Tail = [3; 4; 5] }; { Head = 3; Tail = [4; 5] }; { Head = 4; Tail = [5] }; { Head = 5; Tail = [] }] }
                    equal (extend List.head x) x
                    equal (extend (fun x -> x.Head) y) y)
#endif
      testCase "Tuple"
        (fun () ->
                   equal (mapItem2 string (1,2,3)) (1,"2",3)
                   equal (item3 (1,2,3)) 3
                   )

      testCase "eq on DList 1" (fun () -> equal true  (dlistA = dlistB))
      testCase "eq on DList 2" (fun () -> equal false (dlistA = dlistC))
      testCase "eq on DList 3" (fun () -> equal true  ((dlistA :> obj) = (dlistB :> obj)))
      testCase "eq on DList 4" (fun () -> equal false ((dlistA :> obj) = (dlistC :> obj)))
      #if FABLE_COMPILER && !FABLE_COMPILER_FAKE
      testCase "eq on DList 5" (fun () -> equal true  ((dlistA :> obj) = (dlistD :> obj))) // this behavior differs from (non-fable) F# but same way it would be with normal lists.
      #endif
      #if !FABLE_COMPILER
      testCase "semigroups 1"
        (fun () ->
            let lzy1 = plus (lazy [1]) (lazy [2;3])
            let asy1 = plus (async.Return [1]) (async.Return [2;3])
            
            let mapA =
                Map.empty
                |> Map.add 1 (lazy "Hey")
                |> Map.add 2 (lazy "Hello")

            let mapB =
                Map.empty
                |> Map.add 3 (lazy " You")
                |> Map.add 2 (lazy " World")

            let mapAB = plus mapA mapB
            
            equal [1;2;3] lzy1.Value
            // equal [1;2;3] (Async.RunSynchronously asy1)  <-- weird runSynchronously error
            // equal [(1, "Hey"); (2, "Hello World"); (3, " You")] (Map.toList mapAB |> List.map (fun (x, y) -> (x, y.Value))) <-- gets " World" instead of "Hellp World" ???
            (* can be made to compile, but the tests fail:
            let tup3 = ([1;2], [|1;2|], Some 1) ++ ([3;4], [|3;4|], Some 2)
            equal ([1; 2; 3; 4], [|1; 2; 3; 4|], Some 3) tup3
            *)
            )
      #endif


      #if !FABLE_COMPILER
      testCase "Dict.union gives same dictionary when joined with an empty dictionary (identity)" (fun () ->
        let m1 = dict [1, "2"; 2,"4"; 4,"8"]
        let m2 = dict []
        let r1 = m1 |> Dict.union m2 |> Seq.toList

        equal (m1 |> Seq.toList) r1 )

      testCase "Dict.union returns same results independent of the order (commutative)" (fun () ->
        let m1 = dict [1, "1"; 2,"2"; 3,"3"]
        let m2 = dict [3, "3"; 4,"4"; 5,"6"]

        let r1 = m1 |> Dict.union m2 |> Seq.toList |> List.sortBy (fun x -> x.Key)
        let r2 = m2 |> Dict.union m1 |> Seq.toList |> List.sortBy (fun x -> x.Key)

        equal r1 r2)

      testCase "Dict.union returns same results independent of the grouping (associative)" (fun () ->
        let m1 = dict [1, "1"; 2,"2"; 3,"3"]
        let m2 = dict [3, "3"; 4,"4"; 5,"6"]
        let m3 = dict [6, "6"; 6,"6"; 8,"8"]

        let r1 = Dict.union (Dict.union m1 m2) m3 |> Seq.toList  
        let r2 = Dict.union m1 (Dict.union m2 m3) |> Seq.toList  

        equal r1 r2)

      testCase "Dict.union provides same end result as Dict.unionWith picking the first source value for dupes" (fun () ->
        let m1 = dict [1, "2"; 2,"4"; 4,"8"]
        let m2 = dict [1, "4"; 2,"8"; 4,"16"]

        let r1 = m1 |> Dict.union m2 |> Seq.toList
        let r2 = m1 |> Dict.unionWith konst m2 |> Seq.toList

        equal r1 r2)
      #endif

      testCase "Map.union gives same map when joined with an empty map (identity)" (fun () ->
        let m1 = [1, "2"; 2,"4"; 4,"8"] |> Map.ofList
        let r1 = Map.union m1 Map.empty |> Map.toList

        equal [1, "2"; 2,"4"; 4,"8"] r1)

      testCase "Map.union returns same results independent of the order (associative)" (fun () ->
        let m1 = [1, "1"; 2,"2"; 3,"3"] |> Map.ofList
        let m2 = [3, "3"; 4,"4"; 5,"6"] |> Map.ofList

        let r1 = m1 |> Map.union m2
        let r2 = m2 |> Map.union m1

        equal r1 r2)

      testCase "Map.union provides same end result as Map.unionWith picking the first source value for dupes" (fun () ->
        let m1 = [1, "2"; 2,"4"; 4,"8"] |> Map.ofList
        let m2 = [1, "4"; 2,"8"; 4,"16"] |> Map.ofList

        let r1 = m1 |> Map.union m2
        let r2 = m1 |> Map.unionWith konst m2

        equal r1 r2)

      testCase "Map.intersect returns any dictionary when intersected with the empty map (identity)" (fun () ->
        let m1 = [1, "2"; 2,"4"; 4,"8"] |> Map.ofList 
        let m2 = Map.empty

        let r1 = m1 |> Map.intersect m2

        equal Map.empty r1)

      testCase "Map.intersect returns same results independent of the order (commutative)" (fun () ->
        let m1 = [1, "1"; 2,"2"; 3,"3"] |> Map.ofList
        let m2 = [3, "3"; 4,"4"; 5,"5"] |> Map.ofList

        let r1 = m1 |> Map.intersect m2
        let r2 = m2 |> Map.intersect m1

        equal r1 r2)

      testCase "Map.intersect returns same results independent of grouping (associative)" (fun () ->
        let m1 = [1, "1"; 2,"2"; 3,"3"] |> Map.ofList
        let m2 = [3, "3"; 4,"4"; 5,"5"] |> Map.ofList
        let m3 = [5, "5"; 6,"6"; 7,"7"] |> Map.ofList

        let r1 = Map.intersect (Map.intersect m1 m2) m3
        let r2 =  Map.intersect m1 (Map.intersect m2 m3)

        equal r1 r2)

      testCase "Map.intersect provides same end result as Map.intersectWith picking the first source value for dupes" (fun () ->
        let m1 = [1, "2"; 2,"4"; 4,"8"] |> Map.ofList
        let m2 = [1, "4"; 2,"8"; 4,"16"] |> Map.ofList

        let r1 = m1 |> Map.intersect m2
        let r2 = m1 |> Map.intersectWith konst m2

        equal r1 r2)

      #if !FABLE_COMPILER
      testCase "Dict.intersect returns any dictionary when intersected with the empty dictionary (identity)" (fun () ->
        let m1 = dict [1, "2"; 2,"4"; 4,"8"]
        let m2 = Dictionary<int,string>() :> IDictionary<int,string>

        let r1 = m1 |> Dict.intersect m2 |> Seq.toList

        equal [] r1)

      testCase "Dict.intersect returns same results independent of the order (commutative)" (fun () ->
        let m1 = dict [1, "1"; 2,"2"; 3,"3"]
        let m2 = dict [3, "3"; 4,"4"; 5,"5"]

        let r1 = m1 |> Dict.intersect m2 |> Seq.toList
        let r2 = m2 |> Dict.intersect m1 |> Seq.toList

        equal r1 r2)

      testCase "Dict.intersect returns same results independent of the grouping (associative)" (fun () ->
        let m1 = dict [1, "1"; 2,"2"; 3,"3"]
        let m2 = dict [3, "3"; 4,"4"; 5,"5"]
        let m3 = dict [5, "5"; 6,"6"; 7,"7"]

        let r1 = (Dict.intersect (Dict.intersect m1 m2) m3) |> Seq.toList
        let r2 = (Dict.intersect m1 (Dict.intersect m2 m3)) |> Seq.toList

        equal r1 r2)

      testCase "Dict.intersect provides same end result as Dict.intersectWith picking the first source value for dupes" (fun () ->
        let m1 = dict [1, "2"; 2,"4"; 4,"8"]
        let m2 = dict [1, "4"; 2,"8"; 4,"16"]

        let r1 = m1 |> Dict.intersect m2 |> Seq.toList
        let r2 = m1 |> Dict.intersectWith konst m2 |> Seq.toList

        equal r1 r2)

      testCase "IReadOnlyDictionary.intersect returns any dictionary when intersected with the empty dictionary (identity)" (fun () ->
        let m1 = readOnlyDict [1, "2"; 2,"4"; 4,"8"]
        let m2: IReadOnlyDictionary<int, string> = readOnlyDict []

        let r1 = m1 |> IReadOnlyDictionary.intersect m2 |> Seq.toList

        equal [] r1)

      testCase "IReadOnlyDictionary.intersect returns same results independent of the order (commutative)" (fun () ->
        let m1 = readOnlyDict [1, "1"; 2,"2"; 3,"3"]
        let m2 = readOnlyDict [3, "3"; 4,"4"; 5,"5"]

        let r1 = IReadOnlyDictionary.intersect m2 m1
        let r2 = IReadOnlyDictionary.intersect m1 m2

        equalSeq r1 r2)


      testCase "IReadOnlyDictionary.intersect returns same results independent of the grouping (distributive)" (fun () ->
        let m1 = readOnlyDict [1, "1"; 2,"2"; 3,"3"]
        let m2 = readOnlyDict [3, "3"; 4,"4"; 5,"5"]
        let m3 = readOnlyDict [5, "5"; 6,"6"; 7,"7"]

        let r1 = (IReadOnlyDictionary.intersect (IReadOnlyDictionary.intersect m1 m2) m3)
        let r2 = (IReadOnlyDictionary.intersect m1 (IReadOnlyDictionary.intersect m2 m3))

        equalSeq r1 r2)

      testCase "IReadOnlyDictionary.union gives same dictionary when joined with an empty ditionary (identity)" (fun () ->
        let m1 = Dictionary(dict [1, "2"; 2,"4"; 4,"8"]) :> IReadOnlyDictionary<int,string>
        let m2 = Dictionary(dict []) :> IReadOnlyDictionary<int,string>
        let r1 = IReadOnlyDictionary.union m2 m1

        equalSeq (m1 |> Seq.toList) r1)

      testCase "IReadOnlyDictionary.union returns same results independent of the order (commutative)" (fun () ->
        let m1 = readOnlyDict [1, "1"; 2,"2"; 3,"3"]
        let m2 = readOnlyDict [3, "3"; 4,"4"; 5,"6"]

        let toTuples (m:IReadOnlyDictionary<_,_>)= m |> Seq.toList |> List.map ( fun kv-> (kv.Key,kv.Value) ) |> List.sort
        let r1 = m1 |> IReadOnlyDictionary.union m2 |> toTuples
        let r2 = m2 |> IReadOnlyDictionary.union m1 |> toTuples

        equalSeq r1 r2)

      testCase "IReadOnlyDictionary.union returns same results independent of the groupping (associative)" (fun () ->
        let m1 = readOnlyDict [1, "1"; 2,"2"; 3,"3"]
        let m2 = readOnlyDict [3, "3"; 4,"4"; 5,"6"]
        let m3 = readOnlyDict [6, "6"; 6,"6"; 8,"8"]

        let r1 = IReadOnlyDictionary.union (IReadOnlyDictionary.union m1 m2) m3
        let r2 = IReadOnlyDictionary.union m1 (IReadOnlyDictionary.union m2 m3)
      
        equalSeq r1 r2)

      testCase "IReadOnlyDictionary.intersect provides same end result as IReadOnlyDictionary.intersectWith picking the first source value for dupes" (fun () ->
        let m1 = readOnlyDict [1, "2"; 2,"4"; 4,"8"]
        let m2 = readOnlyDict [1, "4"; 2,"8"; 4,"16"]

        let r1 = m1 |> IReadOnlyDictionary.intersect m2
        let r2 = m1 |> IReadOnlyDictionary.intersectWith konst m2

        equalSeq r1 r2)

      testCase "IReadOnlyDictionary.union provides same end result as Dict.unionWith picking the first source value for dupes" (fun () ->
        let m1 = readOnlyDict [1, "2"; 2,"4"; 4,"8"]
        let m2 = readOnlyDict [1, "4"; 2,"8"; 4,"16"]

        let r1 = m1 |> IReadOnlyDictionary.union m2
        let r2 = m1 |> IReadOnlyDictionary.unionWith konst m2

        equalSeq r1 r2)
      #endif

      #if !FABLE_COMPILER
      testCase "String.toCodePoints >> String.ofCodePoints should preserve the original string" (fun () ->
        // some naughty strings adopted from https://github.com/minimaxir/big-list-of-naughty-strings
        // The MIT License (MIT), Copyright (c) 2015 Max Woolf
        let testStrings = [
          "田中さんにあげて下さい"
          "パーティーへ行かないか"
          "和製漢語"
          "部落格"
          "사회과학원 어학연구소"
          "찦차를 타고 온 펲시맨과 쑛다리 똠방각하"
          "社會科學院語學研究所"
          "울란바토르"
          "𠜎𠜱𠝹𠱓𠱸𠲖𠳏"
          "ヽ༼ຈل͜ຈ༽ﾉ ヽ༼ຈل͜ຈ༽ﾉ"
          "(｡◕ ∀ ◕｡)"
          "｀ｨ(´∀｀∩"
          "__ﾛ(_*)"
          "・(￣∀￣)・:*:"
          "ﾟ･✿ヾ╲(｡◕‿◕｡)╱✿･ﾟ"
          "表ポあA鷗ŒéＢ逍Üßªąñ丂㐀𠀀"
          "0️⃣ 1️⃣ 2️⃣ 3️⃣ 4️⃣ 5️⃣ 6️⃣ 7️⃣ 8️⃣ 9️⃣ 🔟"
          "🇺🇸🇷🇺🇸 🇦🇫🇦🇲🇸"
          "🇺🇸🇷🇺🇸🇦🇫🇦🇲"
          "🇺🇸🇷🇺🇸🇦"
          "If you're reading this, you've been in a coma for almost 20 years now. We're trying a new technique. We don't know where this message will end up in your dream, but we hope it works. Please wake up, we miss you."
        ]

        for s in testStrings do
          equal s (s |> String.toCodePoints |> String.ofCodePoints))
      #endif

      testCase "Array.lift2 should combine all arrays " (fun () ->
        equal [|11; 21; 31; 12; 22; 32|] (Array.lift2 (+) [|1;2|] [|10;20;30|]))

      #if !FABLE_COMPILER
      testCase "Nullable.bind should return the expected result" (fun () ->
        let add1 x = x + 1 |> Nullable
        let firstOfYear x = DateTime(x, 1, 1) |> Nullable
        Nullable.bind add1 (Nullable 2) |> equal (Nullable 3)
        Nullable.bind firstOfYear (Nullable 2020) |> equal (Nullable (DateTime(2020, 1, 1)))
        // generic bind should work the same way
        bind add1 (Nullable 2) |> equal (Nullable 3)
        bind firstOfYear (Nullable 2020) |> equal (Nullable (DateTime(2020, 1, 1))))

      testCase "Nullable.map should return the expected result" (fun () ->
        let firstOfYear x = DateTime(x, 1, 1)
        Nullable.map ((+) 1) (Nullable 1) |> equal (Nullable 2)
        Nullable.map firstOfYear (Nullable 2020) |> equal (Nullable (DateTime(2020, 1, 1)))
        // generic map should work the same way
        map ((+) 1) (Nullable 1) |> equal (Nullable 2)
        map firstOfYear (Nullable 2020) |> equal (Nullable (DateTime(2020, 1, 1))))

      testCase "Nullable.iter should only invoke the function when a value is present" (fun () ->
        let mutable v = 0
        Nullable.iter (fun x -> v <- x) (Nullable 1)
        v |> equal 1

        Nullable.iter (fun x -> failwith "this should not be called") (Nullable ()))

      testCase "Nullable.defaultWith should only invoke thunk if needed" (fun () ->
        Nullable.defaultWith (fun () -> failwith "this should not be called") (Nullable 2) |> equal 2
        Nullable.defaultWith (fun () -> 1) (Nullable()) |> equal 1)

      testCase "Nullable.defaultValue uses default when Nullable has no value" (fun () ->
        Nullable.defaultValue 1 (Nullable 2) |> equal 2
        Nullable.defaultValue 1 (Nullable()) |> equal 1)

      testCase "Nullable.exists returns whether there is a matching value" (fun () ->
        let pred x = x > 1
        Nullable.exists pred (Nullable 2) |> equal true
        Nullable.exists pred (Nullable 1) |> equal false
        Nullable.exists pred (Nullable()) |> equal false)

      testCase "Nullable.filter returns empty Nullable when there is no matching value" (fun () ->
        let pred x = x > 1
        Nullable.filter pred (Nullable 2) |> equal (Nullable 2)
        Nullable.filter pred (Nullable 1) |> equal (Nullable())
        Nullable.filter pred (Nullable()) |> equal (Nullable()))

      testCase "Nullable.fold and foldBack return the expected value" (fun () ->
        let d = DateTime(2020, 1, 1)
        let update (s: DateTime) (x: int) = s.AddDays(float x)
        Nullable.fold update d (Nullable 2) |> equal (DateTime(2020, 1, 3))
        Nullable.fold update d (Nullable()) |> equal d
        Nullable.foldBack (flip update) d (Nullable 2) |> equal (DateTime(2020, 1, 3))
        Nullable.foldBack (flip update) d (Nullable()) |> equal d)

      testCase "Nullable.forall returns whether there is a matching value or no value" (fun () ->
        let pred x = x > 1
        Nullable.forall pred (Nullable 2) |> equal true
        Nullable.forall pred (Nullable 1) |> equal false
        Nullable.forall pred (Nullable()) |> equal true)

      testCase "Nullable.toList returns a list with the value if there is one" (fun () ->
        Nullable.toList (Nullable 1) |> equal [1]
        Nullable.toList (Nullable()) |> equal [])
      #endif

      testCase "map2Shortest" (fun () ->
        List.map2Shortest (+) [1;2;3] [2;3] |> equalSeq [3;5]
        Array.map2Shortest (+) [|1;2|] [|2;3;4|] |> equalSeq [|3;5|]
        ResizeArray.map2Shortest (+) (ResizeArray [1;2;3]) (ResizeArray [2;3]) |> equalSeq (ResizeArray [3;5]))

]
