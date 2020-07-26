module ExtensionsTests

open System
open Testing
open FSharpPlus
open System.Collections.Generic

open FSharpPlus.Data
open FSharpPlus.Lens

type StringCodec<'t> = StringCodec of ( (string -> Result<'t,string>) * ('t -> string) ) with
    static member Invmap (StringCodec (d, e), f: 'T -> 'U, g: 'U -> 'T) = StringCodec (d >> Result.map f, e << g) : StringCodec<'U>

module StringCodec =
    let decode (StringCodec (d,_)) x = d x
    let encode (StringCodec (_,e)) x = e x

type Person = { Name: string; DateOfBirth: DateTime }
module Person =
    let inline _name f { Name = a; DateOfBirth = b } = f a <&> fun a' -> { Name = a'; DateOfBirth = b }
type Book = { Title: string; Author: Person }
module Book =
    let inline _author f { Author = a; Title = b } = f a <&> fun a' -> { Author = a'; Title = b }
    let inline _authorName b = _author << Person._name <| b


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


      testCase "Bind" 
        (fun () ->  let x = [1;2] >>= fun x -> [string x ; string (x + 1000) ]
                    let y = { Head = 1; Tail = [2] } >>= fun x -> { Head = string x ; Tail = [string (x + 1000)] }
                    let z = ("a", 1) >>= fun x -> (string x, x + 10)
                    equal ["1"; "1001"; "2"; "1002"] x
                    equal { Head = "1"; Tail = ["1001"; "2"; "1002"] } y
                    equal ("a1", 11) z)

      testCase "Comonad" 
        (fun () ->  let x = [1;2;3;4;5]
                    let y = { Head = 1 ; Tail = [2;3;4;5] }
                    equal (List.head x) 1
                    equal (y.Head) 1
                    equal (duplicate x) [[1; 2; 3; 4; 5]; [2; 3; 4; 5]; [3; 4; 5]; [4; 5]; [5]]
                    equal (duplicate y) { Head = { Head = 1; Tail = [2; 3; 4; 5] }; Tail = [{ Head = 2; Tail = [3; 4; 5] }; { Head = 3; Tail = [4; 5] }; { Head = 4; Tail = [5] }; { Head = 5; Tail = [] }] }
                    equal (extend List.head x) x
                    equal (extend (fun x -> x.Head) y) y)

      testCase "Invariant"
        (fun () ->  let tryParse x =
                        match System.Double.TryParse (x: string) with
                        | (true, x) -> Some x
                        | (false, _) -> None
        
                    let floatCodec = StringCodec ( (tryParse >> Option.toResultWith "Parse error"), string<float>)
                    let floatParsed  = StringCodec.decode floatCodec "1.8"
                    let floatEncoded = StringCodec.encode floatCodec 1.5
                    equal floatParsed (Result<float, string>.Ok 1.8)
                    equal floatEncoded "1.5" 
        
                    let intCodec = invmap int<float> float<int> floatCodec
                    let oneParsed  = StringCodec.decode intCodec "1"
                    let tenEncoded = StringCodec.encode intCodec 10
                    equal oneParsed (Result<int, string>.Ok 1)
                    equal tenEncoded "10" )

      testCase "Tuple"
        (fun () ->
                   equal (mapItem2 string (1,2,3)) (1,"2",3)
                   // equal (item3 (1,2,3)) 3
                   )

      testCase "Const"
        (fun () ->
                   equal (Const.Map (Const 1, fun (x:bool) -> string x)) (Const 1: Const<int,string>)
                   )

      testCase "Lens"
        (fun () ->
                   //equal (view _1 (1, '2')) 1
                   // equal (view _2 ('1', 2)) 2
                   let rayuela =
                        { Book.Title = "Rayuela"
                          Author = { Person.Name = "Julio Cortázar"
                                     DateOfBirth = DateTime (1914, 8, 26) } }
                   equal (view Book._authorName rayuela) "Julio Cortázar"
                   
                   equal 1 (view _1 (1, '2'))
                   equal 2 (view _2 ('1', 2))
                   equal None (preview _Ok (Error 1))
                   equal (Some 1) (preview _Ok (Ok 1))
                   equal (Some 1) (preview _Error (Error 1))
                   equal None (preview _Error (Ok 1))equal (None) (preview _Some None)
                   equal (Some 1) (preview _Some (Some 1))
                   equal (Some ()) (preview _None None)
                   equal None (preview _None (Some 1))

                   )
      testCase "eq on DList 1" (fun () -> equal true  (dlistA = dlistB))
      testCase "eq on DList 2" (fun () -> equal false (dlistA = dlistC))
      testCase "eq on DList 3" (fun () -> equal true  ((dlistA :> obj) = (dlistB :> obj)))
      testCase "eq on DList 4" (fun () -> equal false ((dlistA :> obj) = (dlistC :> obj)))
      testCase "eq on DList 5" (fun () -> equal true  ((dlistA :> obj) = (dlistD :> obj))) // this behavior differs from (non-fable) F# but same way it would be with normal lists.

]
