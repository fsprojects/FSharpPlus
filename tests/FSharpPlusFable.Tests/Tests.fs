module Tests

open ExtensionTests
open Testing


let AllTests =  testList "AllTests" [ExtensionTest]

#if FABLE_COMPILER

open Fable.Core
open Fable.Core.JsInterop

flattenTest AllTests

#else

open Expecto
open Expecto.TestResults

[<EntryPoint>]
    let main args =
        printfn "Result: %i" (runTestsWithArgs defaultConfig args AllTests)
        0


#endif