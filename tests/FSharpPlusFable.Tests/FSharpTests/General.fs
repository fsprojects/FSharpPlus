module GeneralTests

open Testing
open FSharpPlus

let GeneralTest = 
    testList "General Tests" [

      testCase "possible to map simple values" 
        (fun () -> equal (Some (4)) (map ((+) 2) (Some 2)))
    ]