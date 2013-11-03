#r @"..\..\packages\FsControl.1.0.4\lib\net40\FsControl.Core.dll"
#load @"..\Operators.fs"
#load @"..\Builders.fs"

open FSharpPlus

let lst11n21n12n22 = 
    monad {                
        let! x1 = [1;   2]
        let! x2 = [10; 20]
        return ((+) x1 x2)}

let some14 = 
    monad {                
        let! x1 = Some  4
        let! x2 = Some 10
        return ((+) x1 x2)}