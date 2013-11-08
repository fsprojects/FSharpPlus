#r @"..\..\packages\FsControl.1.0.5\lib\net40\FsControl.Core.dll"
#load @"..\Operators.fs"
open FSharpPlus

let r23asBytes = parse "12" + 11 |> toBytes |> toList