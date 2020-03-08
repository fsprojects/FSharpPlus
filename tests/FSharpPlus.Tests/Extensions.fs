namespace FSharpPlus.Tests

module Extensions =

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