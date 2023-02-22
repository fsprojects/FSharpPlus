(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.

(**
Other abstractions
==================

Here are some other abstractions, not present in the diagram.

(see the examples)

 Examples
--------
*)



#r @"../../src/FSharpPlus/bin/Release/netstandard2.0/FSharpPlus.dll"

open System
open FSharpPlus
open FSharpPlus.Data


// Indexable

let namesWithNdx = mapi (fun k v -> "(" + string k + ")" + v ) (Map.ofSeq ['f',"Fred";'p',"Paul"])
let namesAction = iteri (printfn "(%A)%s") (Map.ofSeq ['f',"Fred";'p',"Paul"])
let res119 = foldi (fun s i t -> t * s - i) 10 [3;4]
let res113 = foldi (fun s i t -> t * s - i) 2 [|3;4;5|]
let resSomeId20 = traversei (fun k t -> Some (10 + t)) (Tuple 10)


// ZipFunctor

let (x, y) = zip (async { return 1 }) (async { return '2' }) |> Async.RunSynchronously


// Collection

let a = skip 3 [1..10]
let b = chunkBy fst [1, "a"; 1, "b"; 2, "c"; 1, "d"]


// Reducibles

let c = nelist {1; 2; 3}
let d = reduce (+) c

let resultList = nelist {Error "1"; Error "2"; Ok 3; Ok 4; Error "5"}
let firstOk = choice resultList


// Invariant Functor
type StringConverter<'t> = StringConverter of (string -> 't) * ('t -> string) with
    static member Invmap (StringConverter (f, g), f',g') = StringConverter (f' << f, g << g')

let ofString (StringConverter (f, _)) = f
let toString (StringConverter (_, f)) = f

let floatConv = StringConverter (float<string>, string<float>)

let floatParsed  = ofString floatConv "1.8"
let floatEncoded = toString floatConv 1.5

let intConv = invmap int<float> float<int> floatConv

let oneParsed  = ofString intConv "1"
let tenEncoded = toString intConv 10