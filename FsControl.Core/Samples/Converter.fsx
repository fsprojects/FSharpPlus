#r @"..\bin\Debug\FsControl.Core.dll"

open System
open FsControl.Core.Abstractions.Converter

let inline internal fromBytesWithOffset (startIndex:int) (value:byte[]) = Inline.instance FromBytes (value, startIndex)
let inline internal fromBytes                            (value:byte[]) = Inline.instance FromBytes (value, 0         )
let inline internal toBytes value :byte[] = Inline.instance (ToBytes, value) ()
let inline internal parse  (value:string) = Inline.instance Parse value
let inline internal toString value : string = Inline.instance (ToString, value) ()

let r120 = parse "10" + fromBytes [|10uy;0uy;0uy;0uy;|]                + 100
let r110 = parse "10" + fromBytes [|10uy;0uy;0uy;0uy;0uy;0uy;0uy;0uy|] + 100.

let r130 = toString [1;2;3]
let r140 = toString (1,2)
let r150 = toString (Some 22)
let r160 = toString ([1;2;3] :> _ seq)