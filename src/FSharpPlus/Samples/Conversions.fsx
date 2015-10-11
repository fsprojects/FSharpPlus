#nowarn "3186"
#r @"..\..\build\FsControl.Core.dll"
#r @"..\..\build\FSharpPlus.dll"
open FSharpPlus

let r23asBytes = parse "12" + 11 |> toBytes |> toList
let r10:float = explicit 10

open GenericMath
let r11 = toBigInt (5G + 6G)