module General.Alternative

open Testing
open General.Util
open FSharpPlus
open FSharpPlus.Data
#nowarn "686"



#if !FABLE_COMPILER || (FABLE_COMPILER_3 || FABLE_COMPILER_4)
let testEmpty () =
    let _: WrappedListE<int> = empty
    let _: list<int>         = empty
    let _: WrappedListG<int> = empty
    let _: seq<int>          = empty
    
    // shoud not compile. 
    // Although WrappedListD implements IEnumerable, it should explicitely implement Empty. Not all IEnumerables have empty.
    // let (z: WrappedListD<int>) = empty
    ()
#endif

#if !FABLE_COMPILER || (FABLE_COMPILER_3 || FABLE_COMPILER_4)
let testAppend () =
    let _ = WrappedListE [1;2] <|> WrappedListE [3;4]
    let _ = [1;2] <|> [3;4]
    let _ = WrappedListG [1;2] <|> WrappedListG [3;4]
    let _ = seq [1;2] <|> seq [3;4]
    let _ = NonEmptySeq.create 1 [2] <|> NonEmptySeq.create 3 [4]

    // shoud not compile. 
    // Although WrappedListD implements IEnumerable, it should explicitely implement (<|>). Not all IEnumerables have (<|>).
    // let z = WrappedListD [1;2] ++ WrappedListD [3;4]
    ()
#endif
let alternative = testList "Alternative" [
#if !FABLE_COMPILER || (FABLE_COMPILER_3 || FABLE_COMPILER_4)
    testCase "testEmptyAndAppendForCustomType" (fun () ->
        let u = WrappedListE [1;2]
        let v = WrappedListG [1;2]
        let w = u <|> empty
        let x = empty <|> u
        let y = v <|> empty
        let z = empty <|> v
        Assert.AreEqual (u, w)
        Assert.AreEqual (u, x)
        Assert.AreEqual (v, y)
        Assert.AreEqual (v, z))
#endif

#if !FABLE_COMPILER
    testCase "testOptionTAppliesFunctionOnce" (fun () ->
        SideEffects.reset ()
        let x = OptionT <| async { SideEffects.add "hello"; return Some 1 }
        let y = OptionT <| async { SideEffects.add "good bye"; return Some 2 }

        let z = (x <|> y) |> OptionT.run |> Async.RunSynchronously

        Assert.AreEqual (["hello"], SideEffects.get ())
        Assert.AreEqual (Some 1, z))
#endif

#if !FABLE_COMPILER || (FABLE_COMPILER_3 || FABLE_COMPILER_4)
    testCase "testChoice" (fun () ->
        let s = seq { 
            yield (SideEffects.add "a"; None)
            yield (SideEffects.add "b"; None)
            yield (SideEffects.add "c"; Some 'c')
            yield (SideEffects.add "d"; Some 'd')
            yield (SideEffects.add "e"; None)
            yield (SideEffects.add "f"; Some 'f')
            }

        let t = seq { 
            yield (SideEffects.add "a"; Error "a")
            yield (SideEffects.add "b"; Error "b")
            yield (SideEffects.add "c"; Ok 'c')
            yield (SideEffects.add "d"; Ok 'd')
            yield (SideEffects.add "e"; Error "e")
            yield (SideEffects.add "f"; Ok 'f')
            }


        let v = seq { 
            yield (SideEffects.add "a"; Failure ["a"])
            yield (SideEffects.add "b"; Failure ["b"])
            yield (SideEffects.add "c"; Success 'c')
            yield (SideEffects.add "d"; Success 'd')
            yield (SideEffects.add "e"; Failure ["e"])
            yield (SideEffects.add "f"; Success 'f')
            }

        let shortList, fullList = ["a"; "b"; "c"; "d"], ["a"; "b"; "c"; "d"; "e"; "f"]

        SideEffects.reset ()
        let _ = choice s                               // uses specific overload for seqs
        Assert.AreEqual (shortList, SideEffects.get ()) // short-circuits

        SideEffects.reset ()
        let _ = choice (toList s)                     // uses specific overload for lists
        Assert.AreEqual (fullList, SideEffects.get ()) // short-circuits but the conversion to list forces all side-effects

        SideEffects.reset ()
        let _ = choice (toArray s)                    // uses specific overload for arrays
        Assert.AreEqual (fullList, SideEffects.get ()) // short-circuits but the conversion to array forces all side-effects

        #if !FABLE_COMPILER
        SideEffects.reset ()
        let _ = choice (NonEmptyList.ofList (toList s)) // uses Default1 (Choice defined on NonEmptyList)
        Assert.AreEqual (fullList, SideEffects.get ()) // short-circuits but the conversion to list forces all side-effects

        SideEffects.reset ()
        let _ = choice (ofSeq s: Set<_>)              // use Default3: choice of an alternative
        Assert.AreEqual (fullList, SideEffects.get ()) // short-circuits but the conversion to set forces all side-effects

        SideEffects.reset ()
        let _ = choice (NonEmptyList.ofList (toList t)) // uses Default1 (Choice defined on NonEmptyList)
        Assert.AreEqual (fullList, SideEffects.get ()) // short-circuits but the conversion to set forces all side-effects

        SideEffects.reset ()
        let _ = choice (WrappedSeqE t)                // uses Default2
        Assert.AreEqual ("Using WrappedSeqE's ToSeq"::shortList, SideEffects.get ()) // short-circuits

        SideEffects.reset ()
        let _ = choice (toList v)                    // uses specific overload for lists
        Assert.AreEqual (fullList, SideEffects.get ()) // short-circuits but the conversion to set forces all side-effects
        #endif
    )
#endif
]