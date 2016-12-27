#nowarn "3186"
#r @"..\..\build\FSharpPlus.dll"

open FSharpPlus

let r23asBytes = parse "12" + 11 |> toBytes |> toList
let ipAddress:System.Net.IPAddress = [192;168;0;1] |>> string |> intersperse "." |> concat |> parse

let ipAndPort : (System.Net.IPAddress * int) option = 
    match "192.168.0.1:8001".Split ':' with 
    | [|Parse ip; Parse port|] -> Some (ip, port)
    | _ -> None

let r10:float  = explicit 10
let r11:System.Nullable<_> = implicit 11

open GenericMath
let r12:float = fromBigInt 10I + 2G
let r13       = toBigInt (5G + 8G)
let r14:float = parse "11" + 3G