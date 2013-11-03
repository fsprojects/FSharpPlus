#r @"..\..\packages\FsControl.1.0.4\lib\net40\FsControl.Core.dll"
#load @"..\Operators.fs"
#load @"..\Builders.fs"
#load @"..\Extensions.fs"
#load @"..\NonEmptyList.fs"

open FSharpPlus
open FSharpPlus.Extensions

let lst   = {Head = 1; Tail = [2;3;4;5]}
let elem1 = extract   lst
let tails = duplicate lst
let lst'  = extend extract lst