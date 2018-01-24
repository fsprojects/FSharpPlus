#if INTERACTIVE
#r @"../bin/Release/net45/FSharpPlus.dll"
#else
module Samples.Conversions
#endif

open FSharpPlus

let r23asBytes = parse "12" + 11 |> toBytes |> toList
let ipAddress:System.Net.IPAddress = [192;168;0;1] |>> string |> intersperse "." |> Seq.sum |> parse

let ipAndPort : (System.Net.IPAddress * int) option = 
    match "192.168.0.1:8001".Split ':' with 
    | [|Parse ip; Parse port|] -> Some (ip, port)
    | _ -> None

let r10:float  = explicit 10
let r11:System.Nullable<_> = implicit 11

open FSharpPlus.Math.Generic
let r12:float = fromBigInt 10I + 2G
let r13       = toBigInt (5G + 8G : int64)  // plus doens't have the default type (int) therefore a type annotation is required
let r14       = toBigInt (20G - 6G)         // (-) still uses the default type (int)
let r15:float = parse "11" + 4G