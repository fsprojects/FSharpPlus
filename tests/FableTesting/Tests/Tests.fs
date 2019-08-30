module Tests


open ExtensionTests
open Testing
// Learn more about F# at http://fsharp.org



let AllTests =  testList "AllTests" [ExtensionTest]

let private firstDiff s1 s2 =
    let s1 = Seq.append (Seq.map Some s1) (Seq.initInfinite (fun _ -> None))
    let s2 = Seq.append (Seq.map Some s2) (Seq.initInfinite (fun _ -> None))
    Seq.mapi2 (fun i s p -> i,s,p) s1 s2
    |> Seq.find (function |_,Some s,Some p when s=p -> false |_-> true)

let private sequenceEqual actual expected =
    match firstDiff actual expected with
    | _,None,None -> ()
    | i,Some a, Some e -> failwithf "Sequence does not match at position %i. Expected item: %A, but got %A. Actual %A : Expected %A" i e a actual expected
    | i,None, Some e -> failwithf "Sequence actual shorter than expected, at pos %i for expected item %A. Actual %A : Expected %A" i e actual expected
    | i,Some a, None -> failwithf "Sequence actual longer than expected, at pos %i found item %A. Actual %A : Expected %A" i a actual expected




#if FABLE_COMPILER
open Fable.Core
open Fable.Core.JsInterop


printfn "FABLE COMPILER"
flattenTest AllTests
#else

open Expecto
open Expecto.TestResults




[<EntryPoint>]
    let main args =
        printfn "FSHARPCOMPILER"
        printfn "Result: %i" (runTestsWithArgs defaultConfig args AllTests)
        0


#endif