#r @"..\..\packages\FsControl.1.0.4\lib\net40\FsControl.Core.dll"
#load @"..\Operators.fs"
open FSharpPlus

let arr = [|1;2;3;4|]
let skip2 = skip 2 arr