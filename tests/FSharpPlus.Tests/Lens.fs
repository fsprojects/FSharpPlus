module FSharpPlus.Tests.Lens

open System
open FSharpPlus
open FSharpPlus.Lens
open NUnit.Framework
open FsCheck
open Helpers
open System.Collections.Generic


[<Test>]
let simple_lens() =
  areEqual 1 (view _1 (1, '2'))
  areEqual 2 (view _2 ('1', 2))

[<Test>]
let ok_prism() =
  areEqual (None) (preview _Ok (Error 1))
  areEqual (Some 1) (preview _Ok (Ok 1))

[<Test>]
let error_prism() =
  areEqual (Some 1) (preview _Error (Error 1))
  areEqual (None) (preview _Error (Ok 1))

[<Test>]
let some_prism() =
  areEqual (None) (preview _Some None)
  areEqual (Some 1) (preview _Some (Some 1))

[<Test>]
let none_prism() =
  areEqual (Some ()) (preview _None None)
  areEqual (None) (preview _None (Some 1))


type NotAMonoid = N0 | N1 | N2 | N3 | N4

[<Test>]
let maximumOf () =
    areEqual (Some 3) (maximumOf traverse [1;3;2;1])
    areEqual None (maximumOf traverse [])

[<Test>]
let all () =
    areEqual [|"Something"; "Nothing"; "Something Else"|] ([|"Something"; "x"; "Something Else"|] |> setl (_all "x") "Nothing")
    areEqual [| N1; N4; N2; N3; N4 |] ([| N1; N0; N2; N3; N0 |] |> setl (_all N0) N4)

[<Test>]
let filtered () =
    areEqual [12; 5; 20] (['a',-10; 'b',12; 'c',5; 'd',-3; 'e',20]^..(items << _2 << filtered (fun x -> x > 0)))
    areEqual [12; 5; 20] ([N0,-10; N1,12; N2,5; N3,-3; N4,20]^..(items << _2 << filtered (fun x -> x > 0)))
    areEqual [N2; N2]    ([N0,N2; N1,N1; N2,N2; N3,N3; N4,N4]^..(items << _2 << filtered (fun x -> x = N2)))

[<Test>]
let choosing () =
    let f x = if x then Result<_,string*int>.Ok (1,'2',3) else Error ("Not success", -1)
    areEqual (Ok (1, '2', "x")          ) (setl (choosing _2 _3) "x" (f true) )
    areEqual (Error ("Not success", "x")) (setl (choosing _2 _3) "x" (f false))

[<Test>]
let iso () =
    let toOption (isSome, v) = if isSome then Some v else None
    let fromOption = function Some (x:'t) -> (true, x) | None -> (false, Unchecked.defaultof<'t>)
    let inline isoTupleOption x = x |> iso toOption fromOption
    areEqual (true, 42) (view (from' isoTupleOption) (Some 42))
    areEqual (Some 42) (view (_1 << isoTupleOption) (System.Int32.TryParse "42", ()))

[<Test>]
let lens_map_item () =
    let m = Map.ofList [("hello","there")]
    areEqual (Some "there") (m ^. Map._item "hello")
    areEqual (Map.ofList [("hello","world")]) (m |> setl (Map._item "hello") "world")

[<Test>]
let lens_readonlydictionary_item () =
    let r = Map.ofList [("hello","there")] :> IReadOnlyDictionary<_,_>
    areEqual (Some "there") (r ^. IReadOnlyDictionary._item "hello")
    areEqual (Map.ofList [("hello","world")] :> IReadOnlyDictionary<_,_>) (r |> setl (IReadOnlyDictionary._item "hello") "world")

[<Test>]
let lens_set_contains () =
    let s = set [1;2]
    areEqual (true) (s ^. Set._contains 1)
    areEqual (set [1;2;3]) (s |> Set._contains 3 .-> true)
