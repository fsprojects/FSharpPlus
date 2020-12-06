module General.Foldable
open Testing
open General.Util
open FSharpPlus
open System.Collections.Generic

open FSharpPlus.Data
#nowarn "686"
open System
open System.Collections.ObjectModel

#if !FABLE_COMPILER
let foldables =
    let _10  = foldBack (+) (seq [1;2;3;4]) 0
    let _323 = toList (seq [3;2;3])
    let _03  = filter ((=) 3) (seq [1;2;3])
    ()
#endif

let foldable = testList "Foldable" [

#if !FABLE_COMPILER
    testCase "foldMapDefaultCustom" (fun () ->
        SideEffects.reset ()
        let x = foldMap ((+) 10) (WrappedListD [1..4]) //= 50 w side effect
        equal 50 x
        equal ["Using optimized foldMap"] (SideEffects.get ())

        SideEffects.reset ()
        let _ = foldMap ((+) 10) {1..4}  //= 50 w/o side effect
        equal 50 x
        equal [] (SideEffects.get ()))
#endif

#if !FABLE_COMPILER
    testCase "filterDefaultCustom" (fun () -> 
        let wlA1 = WrappedListA [1..10]
        let testVal = filter ((=)2) wlA1
        equal (WrappedListA [2]) testVal
        Assert.IsInstanceOf<Option<WrappedListA<int>>> (Some testVal)

        let _twos   = filter ((=) (box 2)) (([1;2;3;4;3;2;1;2;3] |> ofSeq) : Collections.ArrayList)
        let _five   = filter ((=) 5) (WrappedListB' [1;2;3;4;5;6])   // <- Uses the default method for filter.
        let _optionFilter = filter ((=) 3) (Some 4)

        ())
#endif

#if !FABLE_COMPILER
    testCase "foldAternatives" (fun () -> 
        let x = choice [None; Some 3; Some 4; None]
        let y = choice [| []; [3]; [4]; [] |]
        equal (Some 3) x
        equal [3; 4] y)
#endif

#if !FABLE_COMPILER
    testCase "fromToSeq" (fun () ->
        let s = seq [Collections.Generic.KeyValuePair(1, "One"); Collections.Generic.KeyValuePair(2, "Two")]
        let t = {'a'..'d'}

        let dc2:Collections.Generic.Dictionary<_,_> = ofSeq s
        let s' = toSeq dc2

        let arr:_ [] = ofSeq s
        let s'' = toSeq arr

        let str:string = ofSeq t
        let t' = toSeq str 

        equal (toList s) (toList s')
        equal (toList s) (toList s'')
        equal (toList t) (toList t')

        Assert.IsInstanceOf ((Some s' ).GetType (), Some s)
        Assert.IsInstanceOf ((Some s'').GetType (), Some s)
        Assert.IsInstanceOf ((Some t' ).GetType (), Some t))
#endif

#if !FABLE_COMPILER
    testCase "sortBy" (fun () ->
        let l  = [10;4;6;89]
        let l' = sortBy id l
        let s  = WrappedListB [10;4;6;89]
        //let s' = sortBy id s
        equal [4;6;10;89] l'
        //equal (WrappedListB [4;6;10;89]) s'

        let sortedList = sortBy string     [ 11;2;3;9;5;6;7;8;9;10 ]
        let sortedSeq  = sortBy string (seq [11;2;3;9;5;6;7;8;9;10])
        Assert.IsInstanceOf<Option<list<int>>> (Some sortedList)
        Assert.IsInstanceOf<Option<seq<int>>> (Some sortedSeq))
#endif

#if !FABLE_COMPILER
    testCase "intersperse" (fun () ->
        equal "a,b,c,d,e" (intersperse ',' "abcde")
        equal ["a";",";"b";",";"c";",";"d";",";"e"] (intersperse "," ["a";"b";"c";"d";"e"]))
#endif

#if !FABLE_COMPILER
    testCase "readOnlyIntercalate" (fun () ->
        equal "Lorem, ipsum, dolor" (intercalate ", " ["Lorem"; "ipsum"; "dolor"])
        equal "Lorem, ipsum, dolor" (intercalate ", " (ReadOnlyCollection( [|"Lorem"; "ipsum"; "dolor"|] ))))
#endif

#if !FABLE_COMPILER
    testCase "readOnlyTryPick" (fun () ->
        let readOnlyCollection = ReadOnlyCollection( [|1..10|] )
        let iReadOnlyList = readOnlyCollection :> IReadOnlyList<_>        
        let picker i = if i % 3 = 0 then Some i else None
        equal (Some 3) (tryPick picker [1..10])
        equal (Some 3) (tryPick picker readOnlyCollection)
        equal (Some 3) (tryPick picker iReadOnlyList))
#endif

#if !FABLE_COMPILER
    testCase "readOnlyTryFind" (fun () ->
        let predicate i = i % 3 = 0
        let readOnlyCollection = ReadOnlyCollection( [|1..10|] )
        let iReadOnlyList = readOnlyCollection :> IReadOnlyList<_>
        equal (Some 3) (tryFind predicate [1..10])
        equal (Some 3) (tryFind predicate readOnlyCollection)
        equal (Some 3) (tryFind predicate iReadOnlyList))
#endif

#if !FABLE_COMPILER
    testCase "readOnlyfoldMap" (fun () ->
        let readOnlyCollection = ReadOnlyCollection( [|1..4|] )
        let iReadOnlyList = readOnlyCollection :> IReadOnlyList<_>
        equal 50 (foldMap ((+) 10) readOnlyCollection)
        equal 50 (foldMap ((+) 10) iReadOnlyList))
#endif

#if !FABLE_COMPILER
    testCase "exists" (fun () ->
        SideEffects.reset ()
        let _ = exists ((=) 2) [1..3]
        let _ = exists ((=) '2') (System.Text.StringBuilder "abc")
        let _ = exists ((=) 2) (NonEmptySet.Create(1,2,3))
        let _ = exists ((=) 2) (WrappedListA [1..3])
        let _ = exists ((=) 2) (WrappedListD [1..3])
        equal ["Using WrappedListA's Exists"; "Using WrappedListD's Exists"] (SideEffects.get ())
        ())
#endif

#if !FABLE_COMPILER
    testCase "pick" (fun () ->
        SideEffects.reset ()
        let _ = pick Some [1..3]
        let _ = pick Some (System.Text.StringBuilder "abc")
        let _ = pick Some (NonEmptySet.Create(1,2,3))
        let _ = pick Some (WrappedListA [1..3])
        let _ = pick Some (WrappedListD [1..3])
        equal ["Using WrappedListA's Pick"; "Using WrappedListD's Pick"] (SideEffects.get ())
        ())
#endif

#if !FABLE_COMPILER
    testCase "minimum" (fun () ->
        SideEffects.reset ()
        let _ = minimum [1..3]
        let _ = minimum (System.Text.StringBuilder "abc")
        let _ = minimum (NonEmptySet.Create(1,2,3))
        let _ = minimum (WrappedListA [1..3])
        let _ = minimum (WrappedListD [1..3])
        equal ["Using WrappedListA's Min"; "Using WrappedListD's Min"] (SideEffects.get ())
        ())
#endif

#if !FABLE_COMPILER
    testCase "maxBy" (fun () ->
        SideEffects.reset ()
        let _ = maxBy id [1..3]
        let _ = maxBy id (System.Text.StringBuilder "abc")
        let _ = maxBy id (NonEmptySet.Create(1,2,3))
        let _ = maxBy id (WrappedListA [1..3])
        let _ = maxBy id (WrappedListD [1..3])
        equal ["Using WrappedListA's MaxBy"; "Using WrappedListD's MaxBy"] (SideEffects.get ())
        ())
#endif

#if !FABLE_COMPILER
    testCase "length" (fun () ->
        SideEffects.reset ()
        let _ = length [1..3]
        let _ = length (System.Text.StringBuilder "abc")
        let _ = length (NonEmptySet.Create(1,2,3))
        let _ = length (WrappedListA [1..3])
        let _ = length (WrappedListD [1..3])
        equal ["Using WrappedListA's Length"; "Using WrappedListD's Length"] (SideEffects.get ())
        ())
#endif

#if !FABLE_COMPILER
    testCase "tryHead" (fun () ->
        let s                = tryHead <| seq [1;2]
        let s': int option   = tryHead <| seq []
        equal (Some 1) s
        equal None s'

        let l                = tryHead [1;2;3]
        let l': int option   = tryHead []
        equal (Some 1) l
        equal None l'

        let a                = tryHead [|1|]
        let a': int option   = tryHead [||]
        equal (Some 1) a
        equal None a'

        let nes              = tryHead <| NonEmptySeq.ofList [1;2]
        equal (Some 1) nes

        let str                = tryHead "string"
        let str': char option  = tryHead ""
        equal (Some 's') str
        equal None str'

        let sb               = tryHead (System.Text.StringBuilder("string"))
        let sb'              = tryHead (System.Text.StringBuilder())
        equal (Some 's') sb
        equal None sb'
        ())
#endif

#if !FABLE_COMPILER
    testCase "tryLast" (fun () ->
        let s                = tryLast <| seq [1;2]
        let s': int option   = tryLast <| seq []
        equal (Some 2) s
        equal None s'

        let l                = tryLast [1;2;3]
        let l': int option   = tryLast []
        equal (Some 3) l
        equal None l'

        let a                = tryLast [|1|]
        let a': int option   = tryLast [||]
        equal (Some 1) a
        equal None a'

        let nes              = tryLast <| NonEmptySeq.ofList [1;2]
        equal (Some 2) nes

        let str                = tryLast "string"
        let str': char option  = tryLast ""
        equal (Some 'g') str
        equal None str'

        let sb               = tryLast (System.Text.StringBuilder("string"))
        let sb'              = tryLast (System.Text.StringBuilder())
        equal (Some 'g') sb
        equal None sb'
        ())
#endif
]

