module ExtensionsTests

open Testing
open FSharpPlus
open System.Collections.Generic

open FSharpPlus.Data


let ExtensionsTest = 
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


      testCase "Bind" 
        (fun () ->  let x = [1;2] >>= fun x -> [string x ; string (x + 1000) ]
                    let y = { Head = 1; Tail = [2] } >>= fun x -> { Head = string x ; Tail = [string (x + 1000)] }
                    let z = ("a", 1) >>= fun x -> (string x, x + 10)
                    equal ["1"; "1001"; "2"; "1002"] x
                    equal { Head = "1"; Tail = ["1001"; "2"; "1002"] } y
                    equal ("a1", 11) z)
]