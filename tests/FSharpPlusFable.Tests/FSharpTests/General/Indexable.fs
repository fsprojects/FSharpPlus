module General.Indexable
open Testing
open General.Util
open FSharpPlus
open System.Collections.Generic

open FSharpPlus.Data
#nowarn "686"

open System.Collections.ObjectModel
open FSharpPlus.Control

let indexable = testList "Indexable" [
    #if !FABLE_COMPILER || FABLE_COMPILER_3
    testList "testCompileAndExecuteItem" [
        testCase "map" (fun () ->
            let a = Map.ofSeq [1, "one"; 2, "two"]
            let _ = item 1 a
            ())
        testCase "dict" (fun () ->
            let b = dict [1, "one"; 2, "two"]
            let _ = item 1 b
            ())
        testCase "string" (fun () ->
            let c = "two"
            let _ = item 1 c
            ())
        testCase "StringBuilder" (fun () ->
            let d = System.Text.StringBuilder "one"
            let _ = item 1 d
            ())
        #if !FABLE_COMPILER
        testCase "array2D" (fun () ->
            let e = array2D [[1;2];[3;4];[5;6]]
            let _ = item (1, 1) e
            ())
        #endif
        testCase "list" (fun () ->
            let f = [1, "one"; 2, "two"]
            let _ = item 1 f
            ())
        testCase "array" (fun () ->
            let g = [|1, "one"; 2, "two"|]
            let _ = item 1 g
            ())
        #if !FABLE_COMPILER
        testCase "ResizeArray" (fun () ->
            let h = ResizeArray [1, "one"; 2, "two"]
            let _ = item 1 h
            ())
        testCase "Array3D" (fun () ->
            let i = Array3D.create 3 2 2 0
            let _ = item (1, 1, 1) i
            ())
        testCase "Array4D" (fun () ->
            let j = Array4D.create 3 2 2 3 0
            let _ = item (1, 1, 1, 1) j
            ())
        #endif
        testCase "nemap" (fun () ->
            let k = NonEmptyMap.Create (("a", 1), ("b", 2))
            let _ = item "b" k
            ())

        // This doesn't intentionally compile: seq is not Indexable. Not all foldables are Indexable, for example a Set is foldable but not Indexable. For seq use nth instead.
        // let f = seq [1, "one"; 2, "two"]
        // let _ = item 1 f
    ]
    #endif

    #if !FABLE_COMPILER || FABLE_COMPILER_3
    testList "testCompileAndExecuteTryItem" [
        testCase "map" (fun () ->
            let a = Map.ofSeq [1, "one"; 2, "two"]
            let _ = tryItem 1 a
            ())
        testCase "dict" (fun () ->
            let b = dict [1, "one"; 2, "two"]
            let _ = tryItem 1 b
            ())
        testCase "string" (fun () ->
            let c = "two"
            let _ = tryItem 1 c
            ())
        testCase "StringBuilder" (fun () ->
            let d = System.Text.StringBuilder "one"
            let _ = tryItem 1 d
            ())
        #if !FABLE_COMPILER
        testCase "array2D" (fun () ->
            let e = array2D [[1;2];[3;4];[5;6]]
            let _ = tryItem (1, 1) e
            ())
        testCase "Array3D" (fun () ->
            let i = Array3D.create 3 2 2 0
            let _ = tryItem (1, 1, 1) i
            ())
        testCase "Array4D" (fun () ->
            let j = Array4D.create 3 2 2 3 0
            let _ = tryItem (1, 1, 1, 1) j
            ())
        #endif
        testCase "list" (fun () ->
            let f = [1, "one"; 2, "two"]
            let _ = tryItem 1 f
            ())
        testCase "array" (fun () ->
            let g = [|1, "one"; 2, "two"|]
            let _ = tryItem 1 g
            ())
        testCase "ResizeArray" (fun () ->
            let h = ResizeArray [1, "one"; 2, "two"]
            let _ = tryItem 1 h
            ())
        testCase "nemap" (fun () ->
            let k = NonEmptyMap.Create (("a", 1), ("b", 2))
            let _ = tryItem "b" k
            ())
        #if !FABLE_COMPILER // worked in previous iterations of Fable
        testCase "WrappedListA" (fun () ->
            let w = WrappedListA [1, "one"; 2, "two"]
            let _ = tryItem 1 w
            ())
        #endif
        ]
    #endif

    #if !FABLE_COMPILER
    testCase "testCompileAndExecuteTraverseIndexed" (fun () ->
        let nem = NonEmptyMap.Create (("a", Some 1), ("b", Some 2), ("c", Some 3))
        let rs1 = traversei (fun _ v -> v) nem
        Assert.IsInstanceOf<option<NonEmptyMap<string, int>>> rs1)
    #endif

    #if !FABLE_COMPILER
    testCase "tryItemReadonly" (fun () ->
        let d = ReadOnlyDictionary (dict [1, "one"; 2, "two"])
        let iReadOnlyDict = d :> IReadOnlyDictionary<_,_>
        let l = ReadOnlyCollection [|1..10|]
        let iReadOnlyList = l :> IReadOnlyList<_>
        let rarr = ResizeArray [|1..10|]
        equal (Some "one") (tryItem 1 d)
        equal (Some "one") (tryItem 1 iReadOnlyDict)
        equal ("one") (item 1 d)
        equal ("one") (item 1 iReadOnlyDict)
        equal 2 (item 1 l)
        equal 2 (item 1 rarr)
        equal 2 (item 1 iReadOnlyList)
        equal (Some 2) (tryItem 1 l)
        equal (Some 2) (tryItem 1 iReadOnlyList)
        equal (Some 2) (tryItem 1 rarr))
    #endif

    #if !FABLE_COMPILER
    testCase "mapiUsage" (fun () ->
        let m = Map.ofList [1, "one"; 2, "two"]
        let l = ReadOnlyCollection [|1..2|]
        let iReadOnlyList = l :> IReadOnlyList<_>
        let rarr = ResizeArray [|1..2|]
        let mapDS = sprintf "%d-%s"
        equalSeq [KeyValuePair(1,"1-one"); KeyValuePair(2,"2-two")] (mapi mapDS m)
        let mapDD = sprintf "%d-%d"
        equalSeq ["0-1";"1-2"] (mapi mapDD l)
        equalSeq ["0-1";"1-2"] (mapi mapDD iReadOnlyList)
        equalSeq ["0-1";"1-2"] (mapi mapDD rarr)

        // correct overload:
        SideEffects.reset ()
        equalSeq ["0-1";"1-2"] (mapi mapDD (WrappedListD [1..2]))
        equal ["Using WrappedListD's MapIndexed"] (SideEffects.get ())
        SideEffects.reset ()
        equalSeq ["0-1";"1-2"] (MapIndexed.InvokeOnInstance mapDD (WrappedListD [1..2]))
        equal ["Using WrappedListD's MapIndexed"] (SideEffects.get ()))
    #endif

    #if !FABLE_COMPILER
    testCase "iteriUsage" (fun () ->
        let m = Map.ofList [1, "one"; 2, "two"]
        SideEffects.reset ()
        iteri (fun i v -> SideEffects.add <| sprintf "Got %d-%s" i v) m
        equalSeq ["Got 1-one";"Got 2-two"] (SideEffects.get ())

        SideEffects.reset ()
        let onIteration i v= ()
        iteri onIteration (WrappedListD [1..2])
        equal ["Using WrappedListD's IterateIndexed"] (SideEffects.get ())
        SideEffects.reset ()
        IterateIndexed.InvokeOnInstance onIteration (WrappedListD [1..2])
        equal ["Using WrappedListD's IterateIndexed"] (SideEffects.get ()))
    #endif

    #if !FABLE_COMPILER
    testCase "foldiUsage" (fun () ->
        SideEffects.reset ()
        let folder (s:int) (i:int) (t:int) = t * s - i
        let wlist = WrappedListD [1..2]
        let res = foldi folder 10 wlist
        equalSeq ["Using WrappedListD's FoldIndexed"] (SideEffects.get ())
        equal 19 res
        SideEffects.reset ()
        let res1 = FoldIndexed.InvokeOnInstance folder 10 wlist
        equalSeq ["Using WrappedListD's FoldIndexed"] (SideEffects.get ())
        equal 19 res1)
    #endif

    #if !FABLE_COMPILER
    testCase "traverseiUsage" (fun () ->
        let m1 = WrappedMapA.ofList [(1, [1;1;1]); (2, [2;2;2])]

        SideEffects.reset ()
        let r1 = m1 |> TraverseIndexed.InvokeOnInstance (fun _ _ -> None)
        equal None r1
        equalSeq ["Using WrappedMapA's TraverseIndexed"] (SideEffects.get ())

        SideEffects.reset ()
        let r1 = m1 |> traversei (fun _ _ -> None)
        equal None r1
        equalSeq ["Using WrappedMapA's TraverseIndexed"] (SideEffects.get ())

        SideEffects.reset ()
        let r2 = m1 |> TraverseIndexed.InvokeOnInstance (fun i v -> if List.forall ((=) i) v then Some (i :: v) else None)
        equal (WrappedMapA.ofList [(1, [1;1;1;1]); (2, [2;2;2;2])]) r2.Value
        equalSeq ["Using WrappedMapA's TraverseIndexed"] (SideEffects.get ())

        SideEffects.reset ()
        let r3 = m1 |> traversei (fun i v -> if List.forall ((=) i) v then Some (i :: v) else None)
        equal (WrappedMapA.ofList [(1, [1;1;1;1]); (2, [2;2;2;2])]) r3.Value
        equalSeq ["Using WrappedMapA's TraverseIndexed"] (SideEffects.get ()))
    #endif

    #if !FABLE_COMPILER || FABLE_COMPILER_3
    testCase "findIndexUsage" (fun () ->
        let m1 = WrappedListD [0..4]
        SideEffects.reset ()
        let i1 = findIndex ((=) 2) m1
        SideEffects.are ["Using WrappedListD's FindIndex"]
        equal i1 2)
    #endif

    #if !FABLE_COMPILER || FABLE_COMPILER_3
    testCase "findSliceIndexUsage" (fun () ->
        let m1 = WrappedListD [0..4]
        let m2 = WrappedListD [1..3]
        SideEffects.reset ()
        let i1 = findSliceIndex m2 m1
        SideEffects.are ["Using WrappedListD's FindSliceIndex"]
        equal i1 1)
    #endif
]