(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../../bin"

(**
Writer
=========================

Examples
--------
*)


#r @"../../src/FSharpPlus/bin/Release/net45/FSharpPlus.dll"

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
