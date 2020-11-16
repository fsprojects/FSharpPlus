module Tests

open ExtensionsTests
open Testing


let AllTests =  testList "AllTests" [ExtensionsTest]

open Fuchu
#if FABLE_COMPILER
let exitIfNonZero v =
    if v <> 0 then
        failwithf "expected a nonzero exitcode, but got %i" v
    v
#endif

[<EntryPoint>]
let main args =
    defaultMain AllTests args
    #if FABLE_COMPILER
    |> exitIfNonZero
    #endif