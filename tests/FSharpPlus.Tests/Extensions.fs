namespace FSharpPlus.Tests

module Extensions =

  open System
  open System.Collections.Generic
  open NUnit.Framework
  open FSharpPlus
  open FSharpPlus.Tests.Helpers

  [<Test>]
  let ``Dict.union gives same dictionary when joined with an empty dictionary (identity)`` () =
    let m1 = dict [1, "2"; 2,"4"; 4,"8"]
    let m2 = dict []
    let r1 = m1 |> Dict.union m2 |> Seq.toList

    areStEqual (m1 |> Seq.toList) r1

  [<Test>]
  let ``Dict.union returns same results independent of the order (commutative)`` () = 
    let m1 = dict [1, "1"; 2,"2"; 3,"3"]
    let m2 = dict [3, "3"; 4,"4"; 5,"6"]

    let r1 = m1 |> Dict.union m2 |> Seq.toList |> List.sortBy (fun x -> x.Key)
    let r2 = m2 |> Dict.union m1 |> Seq.toList |> List.sortBy (fun x -> x.Key)

    areStEqual r1 r2

  [<Test>]
  let ``Dict.union returns same results independent of the grouping (associative)`` () = 
    let m1 = dict [1, "1"; 2,"2"; 3,"3"]
    let m2 = dict [3, "3"; 4,"4"; 5,"6"]
    let m3 = dict [6, "6"; 6,"6"; 8,"8"]

    let r1 = Dict.union (Dict.union m1 m2) m3 |> Seq.toList  
    let r2 = Dict.union m1 (Dict.union m2 m3) |> Seq.toList  

    areStEqual r1 r2

  [<Test>]
  let ``Dict.union provides same end result as Dict.unionWith picking the first source value for dupes`` () =
    let m1 = dict [1, "2"; 2,"4"; 4,"8"]
    let m2 = dict [1, "4"; 2,"8"; 4,"16"]

    let r1 = m1 |> Dict.union m2 |> Seq.toList
    let r2 = m1 |> Dict.unionWith konst m2 |> Seq.toList

    areStEqual r1 r2
    
  [<Test>]
  let ``Map.union gives same map when joined with an empty map (identity)`` () =
    let m1 = [1, "2"; 2,"4"; 4,"8"] |> Map.ofList
    let r1 = Map.union m1 Map.empty |> Map.toList

    areStEqual [1, "2"; 2,"4"; 4,"8"] r1

  [<Test>]
  let ``Map.union returns same results independent of the order (associative)`` () = 
    let m1 = [1, "1"; 2,"2"; 3,"3"] |> Map.ofList
    let m2 = [3, "3"; 4,"4"; 5,"6"] |> Map.ofList

    let r1 = m1 |> Map.union m2
    let r2 = m2 |> Map.union m1

    areStEqual r1 r2

  [<Test>]
  let ``Map.union provides same end result as Map.unionWith picking the first source value for dupes`` () =
    let m1 = [1, "2"; 2,"4"; 4,"8"] |> Map.ofList
    let m2 = [1, "4"; 2,"8"; 4,"16"] |> Map.ofList

    let r1 = m1 |> Map.union m2
    let r2 = m1 |> Map.unionWith konst m2

    areStEqual r1 r2 

  [<Test>]
  let ``Map.intersect returns any dictionary when intersected with the empty map (identity)`` () = 
    let m1 = [1, "2"; 2,"4"; 4,"8"] |> Map.ofList 
    let m2 = Map.empty

    let r1 = m1 |> Map.intersect m2

    areStEqual Map.empty r1

  [<Test>]
  let ``Map.intersect returns same results independent of the order (commutative)`` () = 
    let m1 = [1, "1"; 2,"2"; 3,"3"] |> Map.ofList
    let m2 = [3, "3"; 4,"4"; 5,"5"] |> Map.ofList

    let r1 = m1 |> Map.intersect m2
    let r2 = m2 |> Map.intersect m1

    areStEqual r1 r2

  [<Test>]
  let ``Map.intersect returns same results independent of grouping (associative)`` () = 
    let m1 = [1, "1"; 2,"2"; 3,"3"] |> Map.ofList
    let m2 = [3, "3"; 4,"4"; 5,"5"] |> Map.ofList
    let m3 = [5, "5"; 6,"6"; 7,"7"] |> Map.ofList

    let r1 = Map.intersect (Map.intersect m1 m2) m3
    let r2 =  Map.intersect m1 (Map.intersect m2 m3)

    areStEqual r1 r2

  [<Test>]
  let ``Map.intersect provides same end result as Map.intersectWith picking the first source value for dupes`` () =
    let m1 = [1, "2"; 2,"4"; 4,"8"] |> Map.ofList
    let m2 = [1, "4"; 2,"8"; 4,"16"] |> Map.ofList

    let r1 = m1 |> Map.intersect m2
    let r2 = m1 |> Map.intersectWith konst m2

    areStEqual r1 r2

  [<Test>]
  let ``Dict.intersect returns any dictionary when intersected with the empty dictionary (identity)`` () = 
    let m1 = dict [1, "2"; 2,"4"; 4,"8"]
    let m2 = Dictionary<int,string>() :> IDictionary<int,string>

    let r1 = m1 |> Dict.intersect m2 |> Seq.toList

    areStEqual [] r1

  [<Test>]
  let ``Dict.intersect returns same results independent of the order (commutative)`` () = 
    let m1 = dict [1, "1"; 2,"2"; 3,"3"]
    let m2 = dict [3, "3"; 4,"4"; 5,"5"]

    let r1 = m1 |> Dict.intersect m2 |> Seq.toList
    let r2 = m2 |> Dict.intersect m1 |> Seq.toList

    areStEqual r1 r2

  [<Test>]
  let ``Dict.intersect returns same results independent of the grouping (associative)`` () = 
    let m1 = dict [1, "1"; 2,"2"; 3,"3"]
    let m2 = dict [3, "3"; 4,"4"; 5,"5"]
    let m3 = dict [5, "5"; 6,"6"; 7,"7"]

    let r1 = (Dict.intersect (Dict.intersect m1 m2) m3) |> Seq.toList
    let r2 = (Dict.intersect m1 (Dict.intersect m2 m3)) |> Seq.toList

    areStEqual r1 r2

  [<Test>]
  let ``Dict.intersect provides same end result as Dict.intersectWith picking the first source value for dupes`` () =
    let m1 = dict [1, "2"; 2,"4"; 4,"8"]
    let m2 = dict [1, "4"; 2,"8"; 4,"16"]

    let r1 = m1 |> Dict.intersect m2 |> Seq.toList
    let r2 = m1 |> Dict.intersectWith konst m2 |> Seq.toList

    areStEqual r1 r2


  [<Test>]
  let ``IReadOnlyDictionary.intersect returns any dictionary when intersected with the empty dictionary (identity)`` () = 
    let m1 = readOnlyDict [1, "2"; 2,"4"; 4,"8"]
    let m2: IReadOnlyDictionary<int, string> = readOnlyDict []

    let r1 = m1 |> IReadOnlyDictionary.intersect m2 |> Seq.toList

    areStEqual [] r1

  [<Test>]
  let ``IReadOnlyDictionary.intersect returns same results independent of the order (commutative)`` () = 
    let m1 = readOnlyDict [1, "1"; 2,"2"; 3,"3"]
    let m2 = readOnlyDict [3, "3"; 4,"4"; 5,"5"]

    let r1 = IReadOnlyDictionary.intersect m2 m1
    let r2 = IReadOnlyDictionary.intersect m1 m2

    areEquivalent r1 r2


  [<Test>]
  let ``IReadOnlyDictionary.intersect returns same results independent of the grouping (distributive)`` () = 
    let m1 = readOnlyDict [1, "1"; 2,"2"; 3,"3"]
    let m2 = readOnlyDict [3, "3"; 4,"4"; 5,"5"]
    let m3 = readOnlyDict [5, "5"; 6,"6"; 7,"7"]

    let r1 = (IReadOnlyDictionary.intersect (IReadOnlyDictionary.intersect m1 m2) m3)
    let r2 = (IReadOnlyDictionary.intersect m1 (IReadOnlyDictionary.intersect m2 m3))

    areEquivalent r1 r2

  [<Test>]
  let ``IReadOnlyDictionary.union gives same dictionary when joined with an empty ditionary (identity)`` () =
    let m1 = Dictionary(dict [1, "2"; 2,"4"; 4,"8"]) :> IReadOnlyDictionary<int,string>
    let m2 = Dictionary(dict []) :> IReadOnlyDictionary<int,string>
    let r1 = IReadOnlyDictionary.union m2 m1

    areEquivalent (m1 |> Seq.toList) r1

  [<Test>]
  let ``IReadOnlyDictionary.union returns same results independent of the order (commutative)`` () = 
    let m1 = readOnlyDict [1, "1"; 2,"2"; 3,"3"]
    let m2 = readOnlyDict [3, "3"; 4,"4"; 5,"6"]

    let r1 = m1 |> IReadOnlyDictionary.union m2
    let r2 = m2 |> IReadOnlyDictionary.union m1

    areEquivalent r1 r2

  [<Test>]
  let ``IReadOnlyDictionary.union returns same results independent of the groupping (associative)`` () = 
    let m1 = readOnlyDict [1, "1"; 2,"2"; 3,"3"]
    let m2 = readOnlyDict [3, "3"; 4,"4"; 5,"6"]
    let m3 = readOnlyDict [6, "6"; 6,"6"; 8,"8"]

    let r1 = IReadOnlyDictionary.union (IReadOnlyDictionary.union m1 m2) m3
    let r2 = IReadOnlyDictionary.union m1 (IReadOnlyDictionary.union m2 m3)
  
    areEquivalent r1 r2

  [<Test>]
  let ``IReadOnlyDictionary.intersect provides same end result as IReadOnlyDictionary.intersectWith picking the first source value for dupes`` () =
    let m1 = readOnlyDict [1, "2"; 2,"4"; 4,"8"]
    let m2 = readOnlyDict [1, "4"; 2,"8"; 4,"16"]

    let r1 = m1 |> IReadOnlyDictionary.intersect m2
    let r2 = m1 |> IReadOnlyDictionary.intersectWith konst m2

    areEquivalent r1 r2

  [<Test>]
  let ``IReadOnlyDictionary.union provides same end result as Dict.unionWith picking the first source value for dupes`` () =
    let m1 = readOnlyDict [1, "2"; 2,"4"; 4,"8"]
    let m2 = readOnlyDict [1, "4"; 2,"8"; 4,"16"]

    let r1 = m1 |> IReadOnlyDictionary.union m2
    let r2 = m1 |> IReadOnlyDictionary.unionWith konst m2

    areEquivalent r1 r2

  [<Test>]
  let ``String.toCodePoints >> String.ofCodePoints should preserve the original string`` () =
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
      areStEqual s (s |> String.toCodePoints |> String.ofCodePoints)

  [<Test>]
  let ``Array.lift2 should combine all arrays `` () =
    areStEqual [|11; 21; 31; 12; 22; 32|] (Array.lift2 (+) [|1;2|] [|10;20;30|])

  [<Test>]
  let ``Nullable.bind should return the expected result`` () =
    let add1 x = x + 1 |> Nullable
    let firstOfYear x = DateTime(x, 1, 1) |> Nullable
    Nullable.bind add1 (Nullable 2) |> areEqual (Nullable 3)
    Nullable.bind firstOfYear (Nullable 2020) |> areEqual (Nullable (DateTime(2020, 1, 1)))
    // generic bind should work the same way
    bind add1 (Nullable 2) |> areEqual (Nullable 3)
    bind firstOfYear (Nullable 2020) |> areEqual (Nullable (DateTime(2020, 1, 1)))

  [<Test>]
  let ``Nullable.map should return the expected result`` () =
    let firstOfYear x = DateTime(x, 1, 1)
    Nullable.map ((+) 1) (Nullable 1) |> areEqual (Nullable 2)
    Nullable.map firstOfYear (Nullable 2020) |> areEqual (Nullable (DateTime(2020, 1, 1)))
    // generic map should work the same way
    map ((+) 1) (Nullable 1) |> areEqual (Nullable 2)
    map firstOfYear (Nullable 2020) |> areEqual (Nullable (DateTime(2020, 1, 1)))

  [<Test>]
  let ``Nullable.iter should only invoke the function when a value is present`` () =
    let mutable v = 0
    Nullable.iter (fun x -> v <- x) (Nullable 1)
    v |> areEqual 1

    Nullable.iter (fun x -> failwith "this should not be called") (Nullable ())

  [<Test>]
  let ``Nullable.defaultWith should only invoke thunk if needed`` () =
    Nullable.defaultWith (fun () -> failwith "this should not be called") (Nullable 2) |> areEqual 2
    Nullable.defaultWith (fun () -> 1) (Nullable()) |> areEqual 1

  [<Test>]
  let ``Nullable.defaultValue uses default when Nullable has no value`` () =
    Nullable.defaultValue 1 (Nullable 2) |> areEqual 2
    Nullable.defaultValue 1 (Nullable()) |> areEqual 1

  [<Test>]
  let ``Nullable.exists returns whether there is a matching value`` () =
    let pred x = x > 1
    Nullable.exists pred (Nullable 2) |> areEqual true
    Nullable.exists pred (Nullable 1) |> areEqual false
    Nullable.exists pred (Nullable()) |> areEqual false

  [<Test>]
  let ``Nullable.filter returns empty Nullable when there is no matching value`` () =
    let pred x = x > 1
    Nullable.filter pred (Nullable 2) |> areEqual (Nullable 2)
    Nullable.filter pred (Nullable 1) |> areEqual (Nullable())
    Nullable.filter pred (Nullable()) |> areEqual (Nullable())

  [<Test>]
  let ``Nullable.fold and foldBack return the expected value`` () =
    let d = DateTime(2020, 1, 1)
    let update (s: DateTime) (x: int) = s.AddDays(float x)
    Nullable.fold update d (Nullable 2) |> areEqual (DateTime(2020, 1, 3))
    Nullable.fold update d (Nullable()) |> areEqual d
    Nullable.foldBack (flip update) d (Nullable 2) |> areEqual (DateTime(2020, 1, 3))
    Nullable.foldBack (flip update) d (Nullable()) |> areEqual d

  [<Test>]
  let ``Nullable.forall returns whether there is a matching value or no value`` () =
    let pred x = x > 1
    Nullable.forall pred (Nullable 2) |> areEqual true
    Nullable.forall pred (Nullable 1) |> areEqual false
    Nullable.forall pred (Nullable()) |> areEqual true

  [<Test>]
  let ``Nullable.toList returns a list with the value if there is one`` () =
    Nullable.toList (Nullable 1) |> areEqual [1]
    Nullable.toList (Nullable()) |> areEqual []
 
  [<Test>]
  let ``Option.ofPair returns Some when operation succeeds`` () =
    Int32.TryParse("123") |> Option.ofPair |> areEqual (Some 123)
    (dict [("abc",234)]).TryGetValue("abc") |> Option.ofPair |> areEqual (Some 234)

  [<Test>]
  let ``Option.ofPair returns None when operation fails`` () =
    Int32.TryParse("abc") |> Option.ofPair |> areEqual None
    (dict []).TryGetValue("abc") |> Option.ofPair |> areEqual None

  [<Test>]
  let map2Shortest () =
    List.map2Shortest (+) [1;2;3] [2;3] |> areEqual [3;5]
    Array.map2Shortest (+) [|1;2|] [|2;3;4|] |> areEqual [|3;5|]
    ResizeArray.map2Shortest (+) (ResizeArray [1;2;3]) (ResizeArray [2;3]) |> areEqual (ResizeArray [3;5])
  
  [<Test>]
  let ``choosei does not throw stack overflow exception`` () =
    List.choosei (fun _ x -> Some x) [1..30000] |> ignore
    Array.choosei (fun _ x -> Some x) [|1..30000|] |> ignore
    Seq.choosei (fun _ x -> Some x) (seq [1..30000]) |> ignore
    Map.choosei (fun _ x -> Some x) ([1..30000] |> List.map (fun x -> (x, "x")) |> Map) |> ignore

  [<Test>]
  let ``choosei returns elements in correct order`` () =
    Array.choosei (fun _ x -> Some x) [|1..10|] |> areEqual [|1..10|]
    List.choosei (fun _ x -> Some x) [1..10] |> areEqual [1..10]
    
  [<Test>]
  let ``map3 should work`` () =
    Result.map3 (fun x y z -> x + y + z) (Ok 1:Result<int, int>) (Ok 3) (Ok 5) |> areEqual (Ok 9: Result<int, int>)
    Result.map3 (fun x y z -> x + y + z) (Error 1:Result<int, int>) (Error 3) (Ok 5) |> areEqual (Error 1: Result<int, int>)
    Result.map3 (fun x y z -> x + y + z) (Ok 1:Result<int, int>) (Error 3) (Error 5) |> areEqual (Error 3: Result<int, int>)
    Result.map3 (fun x y z -> x + y + z) (Ok 1:Result<int, int>) (Ok 3) (Error 5) |> areEqual (Error 5: Result<int, int>)