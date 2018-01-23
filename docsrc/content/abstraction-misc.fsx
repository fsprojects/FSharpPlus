(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../../bin"

(**
Other abstractions
==================

Here are some other abstractions, not present in the diagram.

(see the examples)

 Examples
--------
*)



#r @"../../src/FSharpPlus/bin/Release/net45/FSharpPlus.dll"

open System
open FSharpPlus
open FSharpPlus.Data


// Indexables

let namesWithNdx = mapi (fun k v -> "(" + string k + ")" + v ) (Map.ofSeq ['f',"Fred";'p',"Paul"])
let namesAction = iteri (printfn "(%A)%s") (Map.ofSeq ['f',"Fred";'p',"Paul"])
let res119 = foldi (fun s i t -> t * s - i) 10 [3;4]
let res113 = foldi (fun s i t -> t * s - i) 2 [|3;4;5|]
let resSomeId20 = traversei (fun k t -> Some (10 + t)) (Tuple 10)