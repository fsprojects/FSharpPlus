module GeneralTests

open Testing
open FSharpPlus
open FSharpPlus.Control

let GeneralTest = 
    testList "General Tests" [

      testCase "possible to map simple values" 
        (fun () -> equal (Some (4)) (Map.Invoke ((+) 2) (Some 2)))
    ]