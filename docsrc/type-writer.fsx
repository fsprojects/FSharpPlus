(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../bin"

(**
Writer<'Monoid,'T>
==================

The Writer monad is good way to introduce a log of a computation. 
It gives you a different way of logging that can be useful when you want to be able to inspect the logged results.

Examples
--------
*)


#r @"../src/FSharpPlus/bin/Release/net45/FSharpPlus.dll"

open FSharpPlus
open FSharpPlus.Data
type LogEntry={msg:string}
with
    static member create x = {msg = x}

let output x =  Writer.tell [LogEntry.create x]

let calc = monad {
  do! output "I'm going to start a heavy computation" // start logging
  let y = sum [1..100_000]
  do! output (string y)
  do! output "The computation finished"
  return y // return the result of the computation
}

let logs = Writer.exec calc
let (y,logs') = Writer.run calc

(**
There are some performance implications around using a regular list, that's why you should use DList in these scenarios
*)

let output' x =  Writer.tell <| DList.ofSeq [LogEntry.create x]

let calc' = monad {
  do! output' "I'm going to start a heavy computation" // start logging
  let y = sum [1..100_000]
  do! output' (string y)
  do! output' "The computation finished"
  return y // return the result of the computation
}

let logs2 = Writer.exec calc'
let (y',logs2') = Writer.run calc'
