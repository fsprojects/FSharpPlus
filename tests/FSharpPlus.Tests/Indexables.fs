namespace FSharpPlus.Tests

#nowarn "686"

open System
open System.Collections.Generic
open FSharpPlus
open FSharpPlus.Data
open FSharpPlus.Control
open NUnit.Framework
open Helpers


module Indexables =

    [<Test>]
    let testCompileAndExecuteItem () =

        let a = Map.ofSeq [1, "one"; 2, "two"]
        let _ = item 1 a

        let b = dict [1, "one"; 2, "two"]
        let _ = item 1 b

        let c = "two"
        let _ = item 1 c

        let d = System.Text.StringBuilder "one"
        let _ = item 1 d

        let e = array2D [[1;2];[3;4];[5;6]]
        let _ = item (1, 1) e

        let f = [1, "one"; 2, "two"]
        let _ = item 1 f

        let g = [|1, "one"; 2, "two"|]
        let _ = item 1 g

        let h = ResizeArray [1, "one"; 2, "two"]
        let _ = item 1 h

        let i = Array3D.create 3 2 2 0
        let _ = item (1, 1, 1) i

        let j = Array4D.create 3 2 2 3 0
        let _ = item (1, 1, 1, 1) j

        let k = NonEmptyMap.Create (("a", 1), ("b", 2))
        let _ = item "b" k

        // This doesn't intentionally compile: seq is not Indexable. Not all foldables are Indexable, for example a Set is foldable but not Indexable. For seq use nth instead.
        // let f = seq [1, "one"; 2, "two"]
        // let _ = item 1 f

        ()

    [<Test>]
    let foldIndexed () =
        Assert.AreEqual (123, foldi (fun a b c -> a + b + c) 0 (seq [20; 40; 60]))
        Assert.AreEqual (123, foldi (fun a b c -> a + b + c) 0 [20; 40; 60])
        Assert.AreEqual (123, foldi (fun a b c -> a + b + c) 0 (Map.ofSeq [0, 20; 1, 40; 2, 60]))
        Assert.AreEqual (123, foldi (fun a b c -> a + b + c) 0 (readOnlyDict [0, 20; 1, 40; 2, 60]))
        
        
    [<Test>]
    let traverseIndexed () =
        let m1 = Map.ofList [(1, [1;1;1]); (2, [2;2;2])]
        let r1 = m1 |> traversei (fun _ _ -> None)
        let r2 = m1 |> traversei (fun i v -> if List.forall ((=) i) v then Some (i :: v) else None)
        Assert.AreEqual (None, r1)
        CollectionAssert.AreEqual (Map.ofList [(1, [1; 1; 1; 1]); (2, [2; 2; 2; 2])], r2.Value)
        Assert.IsInstanceOf<Option<Map<int,int list>>> (Some r2.Value)

        let r3 = seq [ ( [0; 0; 0]); ( [1; 1; 1]); ( [2; 2; 2])] |> traversei (fun i v -> if List.forall ((=) i) v then Some (i :: v) else None)
        CollectionAssert.AreEqual (seq [[0; 0; 0; 0]; [1; 1; 1; 1]; [2; 2; 2; 2]], r3.Value)
        Assert.IsInstanceOf<Option<seq<int list>>> (Some r3.Value)

        let r4 = [ ( [0; 0; 0]); ( [1; 1; 1]); ( [2; 2; 2])] |> traversei (fun i v -> if List.forall ((=) i) v then Some (i :: v) else None)
        CollectionAssert.AreEqual ([[0; 0; 0; 0]; [1; 1; 1; 1]; [2; 2; 2; 2]], r4.Value)
        Assert.IsInstanceOf<Option<int list list>> (Some r4.Value)
        
        let r5 = readOnlyDict [ (0, [0; 0; 0]); (1, [1; 1; 1]); (2, [2; 2; 2])] |> traversei (fun i v -> if List.forall ((=) i) v then Some (i :: v) else None)
        CollectionAssert.AreEqual (readOnlyDict [(0, [0; 0; 0; 0]); (1, [1; 1; 1; 1]); (2, [2; 2; 2; 2])], r5.Value)
        Assert.IsInstanceOf<Option<IReadOnlyDictionary<int,int list>>> (Some r5.Value)