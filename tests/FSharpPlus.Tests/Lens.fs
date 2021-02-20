namespace FSharpPlus.Tests

module Lens =

    open NUnit.Framework
    open FSharpPlus
    open FSharpPlus.Data
    open FSharpPlus.Lens
    open Helpers


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
    let choosed () =
        areEqual [2;4] ([1;2;3;4] ^.. (items << choosed (fun x -> if x % 2 = 0 then Some x else None)))
        areEqual [1;3;3;5;3;5;5;7] ([[0;1;2;3];[1;2;3;4];[2;3;4;5];[3;4;5;6]] ^.. (traverse << List.traverse << choosed (fun x -> if x % 2 = 0 then Some (x + 1) else None)))
        areEqual [6] ([[0;1;2;3];[1;2;3;4];[2;3;4;5];[3;4;5;6]] ^.. (traverse << List.traverse << choosed (fun x -> if x = 6 then Some x else None)))

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
        let m = Map.ofList [("Hello", 100); ("Hi", 200)]
        areEqual (Some 100) (m ^. Map._item "Hello")
        areEqual None (m ^. Map._item "Hey")
        areEqual (Map.ofList [("Hello", 150); ("Hi", 200)]) (m |> Map._item "Hello" .-> Some 150)
        areEqual (Map.ofList [("Hello", 150); ("Hi", 200)]) (m |> Map._item "Hello" %-> (function | None -> failwith "Unexpected None" | Some(x) -> Some(x+50)))
        areEqual (Map.ofList [("Hi", 200)]) (m |> Map._item "Hello" .-> None)
        areEqual (Map.ofList [("Hello", 100);("Hi", 200);("Hey", 300)]) (m |> Map._item "Hey" .-> Some 300)

        areEqual (Map.ofList [("Hello", 100);("Hi", 200);("Hey", 50)]) (m |> (Map._item "Hey" << non 0) %-> (fun x -> x + 50))
        areEqual (Map.ofList [("Hi", 200)]) (m |> (Map._item "Hello" << non 0) %-> (fun x -> x - 100))

    [<Test>]
    let lens_readonlydictionary_item () =
        let m = readOnlyDict [("Hello", 100); ("Hi", 200)]
        areEqual (Some 100) (m ^. IReadOnlyDictionary._item "Hello")
        areEqual None       (m ^. IReadOnlyDictionary._item "Hey")
        areEquivalent (readOnlyDict [("Hello", 150); ("Hi", 200)]) (m |> IReadOnlyDictionary._item "Hello" .-> Some 150)
        areEquivalent (readOnlyDict [("Hello", 150); ("Hi", 200)]) (m |> IReadOnlyDictionary._item "Hello" %-> (function | None -> failwith "Unexpected None" | Some(x) -> Some(x+50)))
        areEquivalent (readOnlyDict [("Hi", 200)])                             (m |>  IReadOnlyDictionary._item "Hello" .-> None)
        areEquivalent (readOnlyDict [("Hello", 100);("Hi", 200);("Hey", 300)]) (m |>  IReadOnlyDictionary._item "Hey"   .-> Some 300)
        areEquivalent (readOnlyDict [("Hello", 100);("Hi", 200);("Hey", 50)])  (m |> (IReadOnlyDictionary._item "Hey"   << non 0) %-> (fun x -> x + 50))
        areEquivalent (readOnlyDict [("Hi", 200)])                             (m |> (IReadOnlyDictionary._item "Hello" << non 0) %-> (fun x -> x - 100))

    [<Test>]
    let lens_set_contains () =
        let s = set [1;2]
        areEqual true (s ^. Set._contains 1)
        areEqual (set [1;2;3]) (s |> Set._contains 3 .-> true)

    [<Test>]
    let lens_list_item () =
        let l = [1;2;3]
        areEqual (Some 2) (l ^. List._item 1 )
        areEqual None (l ^. List._item 3)

    [<Test>]
    let lens_array_item () =
        let a = [|1;2;3|]
        areEqual (Some 2) (a ^. Array._item 1 )
        areEqual None (a ^. Array._item 3)