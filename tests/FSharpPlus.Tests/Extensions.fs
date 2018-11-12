module Extensions

open FSharpPlus
open NUnit.Framework
open Validations

[<Test>]
let ``Dict.union gives empty dictionary when two empty dictionaries are joined`` () =
  let m1 = dict []
  let m2 = dict []
  let r1 = m1 |> Dict.union m2 |> Seq.toList

  areEqual [] r1

[<Test>]
let ``Dict.union provides same end result as Dict.unionWith picking the first source value for dupes`` () =
  let m1 = dict [1, "2"; 2,"4"; 4,"8"]
  let m2 = dict [1, "4"; 2,"8"; 4,"16"]

  let r1 = m1 |> Dict.union m2 |> Seq.toList
  let r2 = m1 |> Dict.unionWith konst m2 |> Seq.toList

  areEqual r1 r2

