#nowarn "3186"
#r @"..\bin\Release\FSharpPlus.dll"

open FSharpPlus

let lst11n21n12n22 = 
    monad {                
        let! x1 = [1;   2]
        let! x2 = [10; 20]
        return ((+) x1 x2)}

let neLst11n21n12n22 = 
    monad {                
        let! x1 = { NonEmptyList.Head =  1; Tail =  [2] }
        let! x2 = { NonEmptyList.Head = 10; Tail = [20] }
        return ((+) x1 x2)}


let some14 = 
    monad {                
        let! x1 = Some  4
        let! x2 = Some 10
        return ((+) x1 x2)}