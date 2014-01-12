#r @"..\..\FSharpPlus\bin\Release\FsControl.Core.dll"
#load @"..\Operators.fs"
open FSharpPlus

let r23asBytes = parse "12" + 11 |> toBytes |> toList
let r10:float = convert 10

open GenericMath
let r11 = toBigInteger (5G + 6G)