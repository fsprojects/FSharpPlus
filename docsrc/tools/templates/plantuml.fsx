#if INTERACTIVE
#else
module Plantuml
#endif
// PlantUml processing
open System
open System.IO.Compression
open System.Text


let toUrl input =
    let encodeByte b =
        if   b < 10uy then 48uy + b
        elif b < 36uy then 55uy + b
        elif b < 62uy then 61uy + b
        elif b = 62uy then byte '-'
        elif b = 63uy then byte '_'
        else               byte '?'

    let encode3Bytes b1 b2 b3 =
        let c1 =  b1 >>> 2
        let c2 = (b2 >>> 4) ||| (b1 &&& 0x3uy <<< 4)
        let c3 = (b3 >>> 6) ||| (b2 &&& 0xFuy <<< 2)
        let c4 =                 b3 &&& 0x3Fuy
        [|
            encodeByte (c1 &&& 0x3Fuy)
            encodeByte (c2 &&& 0x3Fuy)
            encodeByte (c3 &&& 0x3Fuy)
            encodeByte (c4 &&& 0x3Fuy)
        |] |> Encoding.ASCII.GetChars

    let encode bytes =
        let c = Array.length bytes
        let s = StringBuilder ()
        for i in 0..3..c-1 do
            let b1 =                   bytes.[i]
            let b2 = if c > i + 1 then bytes.[i + 1] else 0uy
            let b3 = if c > i + 2 then bytes.[i + 2] else 0uy
            encode3Bytes b1 b2 b3 |> s.Append |> ignore
        string s

    use output = new IO.MemoryStream ()
    let writeFrom (input:string) output =
        use writer = new IO.StreamWriter(new DeflateStream(output, CompressionLevel.Optimal), Encoding.UTF8)
        writer.Write input
    output |> writeFrom input |> ignore
    output.ToArray () |> encode