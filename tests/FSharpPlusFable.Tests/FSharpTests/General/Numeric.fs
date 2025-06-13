module General.Numeric
open Testing
open General.Util
open FSharpPlus
open System.Collections.Generic

open FSharpPlus.Data

let numeric = testList "Numeric" [
    #if !FABLE_COMPILER
    testCase "Zero First" (fun () -> let z : First<int> = getZero() in equal None (First.run z))
    #endif
    ]