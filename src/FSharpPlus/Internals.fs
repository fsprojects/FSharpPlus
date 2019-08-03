namespace FSharpPlus.Internals

type Default6 = class end
type Default5 = class inherit Default6 end
type Default4 = class inherit Default5 end
type Default3 = class inherit Default4 end
type Default2 = class inherit Default3 end
type Default1 = class inherit Default2 end

#nowarn "0042" // retype

module internal Prelude =
    let inline flip f x y = f y x
    let inline const' k _ = k
    let inline tupleToOption x = match x with true, value -> Some value | _ -> None
    let inline retype (x: 'T) : 'U = (# "" x: 'U #)



[<RequireQualifiedAccess>]
module internal Implicit = let inline Invoke (x: ^t) = ((^R or ^t) : (static member op_Implicit : ^t -> ^R) x) : ^R

module Errors =
    let exnDivByZero      = new System.DivideByZeroException () :> exn
    let exnNoDivision     = new System.Exception "These numbers are not divisible in this domain."
    let exnSqrtOfNegative = new System.Exception "Cannot calculate square root of a negative number"
    let exnNoSqrt         = new System.Exception "No square root defined for this value in this domain."
    let exnNoSubtraction  = new System.Exception "No subtraction defined for these values in this domain."

module Decimal =
    let inline trySqrt x =
        match sign x with
        | -1 -> Error Errors.exnSqrtOfNegative
        |  0 -> Ok    0.M
        | _  ->
            let rec loop previous =
                let current = (previous + x / previous) / 2.0M
                if previous - current = 0.0M then current else loop current
            x |> float |> sqrt |> decimal |> loop |> Ok

module Rational =
    let inline numerator   x = (^F : (member Numerator   : 'R) x)
    let inline denominator x = (^F : (member Denominator : 'R) x)

module BigInteger =
    open System.Numerics
    let trySqrtRem x =
        if sign x = -1 then Error Errors.exnSqrtOfNegative
        else
            let rec loop previous =
                let current = (previous + x / previous) >>> 1
                if abs (previous - current) < 2I then current else loop current
            let guess = 10I ** (((int (BigInteger.Log10 (x + 1I))) + 1) >>> 1)
            let r = loop guess
            let r2 = r * r
            match compare r2 x with
            | 0 -> Ok (r, 0I)
            | 1 -> let root = r - 1I in Ok (root, x - root * root)
            | _ -> Ok (r, x - r2)


// Dummy types

type Id<'t> (v: 't) =
   let value = v
   member __.getValue = value

[<RequireQualifiedAccess>]
module Id =
    let run   (x: Id<_>) = x.getValue
    let map f (x: Id<_>) = Id (f x.getValue)
    let create x = Id x

type Id0 (v: string) =
   let value = v
   member __.getValue = value

type Either<'t,'u> =
    | Left of 't
    | Right of 'u

type DmStruct = struct end


// BitConverter

#nowarn "9"
#nowarn "51"
open System
open Microsoft.FSharp.NativeInterop

type BitConverter =
    /// Converts a byte into an array of bytes with length one.
    static member GetBytes (value: bool) = Array.singleton (if value then 1uy else 0uy)

    /// Converts a char into an array of bytes with length two.
    static member GetBytes (value: char, isLittleEndian: bool) = BitConverter.GetBytes (int16 value, isLittleEndian)

    /// Converts a short into an array of bytes with length
    /// two.
    static member GetBytes (value: int16, isLittleEndian: bool) =
        if not isLittleEndian then [|byte (value >>> 8); byte value|]
        else
            let bytes : byte [] = Array.zeroCreate 2
            use x = fixed bytes
            x |> NativePtr.toNativeInt |> NativePtr.ofNativeInt |> NativePtr.write <| value
            bytes

    /// Converts an int into an array of bytes with length
    /// four.
    static member GetBytes (value: int, isLittleEndian) =
        if not isLittleEndian then [|byte (value >>> 24); byte (value >>> 16); byte (value >>> 8); byte value|]
        else
            let bytes : byte [] = Array.zeroCreate 4
            use x = fixed bytes
            x |> NativePtr.toNativeInt |> NativePtr.ofNativeInt |> NativePtr.write <| value
            bytes

    /// Converts a long into an array of bytes with length
    /// eight.
    static member GetBytes (value: int64, isLittleEndian) =
        if not isLittleEndian then [|byte (value >>> 56); byte (value >>> 48); byte (value >>> 40); byte (value >>> 32); byte (value >>> 24); byte (value >>> 16); byte (value >>> 8); byte value|]
        else
            let bytes : byte [] = Array.zeroCreate 8
            use x = fixed bytes
            x |> NativePtr.toNativeInt |> NativePtr.ofNativeInt |> NativePtr.write <| value
            bytes


    /// Converts an ushort into an array of bytes with
    /// length two.
    static member GetBytes (value: uint16, isLittleEndian) = BitConverter.GetBytes (int16 value, isLittleEndian)

    /// Converts an uint into an array of bytes with
    /// length four.
    static member GetBytes (value: uint32, isLittleEndian) = BitConverter.GetBytes (int value, isLittleEndian)

    /// Converts an unsigned long into an array of bytes with
    /// length eight.
    static member GetBytes (value: uint64, isLittleEndian) = BitConverter.GetBytes (int64 value, isLittleEndian)

    /// Converts a float into an array of bytes with length
    /// four.
    static member GetBytes (value: float32, isLittleEndian) =
        let mutable value = value
        BitConverter.GetBytes ((&&value |> NativePtr.toNativeInt |> NativePtr.ofNativeInt |> NativePtr.read : int), isLittleEndian)

    /// Converts a double into an array of bytes with length
    /// eight.
    static member GetBytes (value: float, isLittleEndian) =
        let mutable value = value
        BitConverter.GetBytes ((&&value |> NativePtr.toNativeInt |> NativePtr.ofNativeInt |> NativePtr.read : int64), isLittleEndian)

    /// Converts an array of bytes into a char.
    static member ToChar (value: byte [], startIndex: int, isLittleEndian: bool) =
        char <| BitConverter.ToInt16 (value, startIndex, isLittleEndian)

    /// Converts an array of bytes into a short.
    static member ToInt16 (value: byte[], startIndex: int, isLittleEndian: bool) =
        if isNull value then nullArg "value"
        if startIndex >= value.Length     then raise <| new ArgumentOutOfRangeException ("startIndex", "ArgumentOutOfRange_Index")
        if startIndex >  value.Length - 2 then raise <| new ArgumentException "Arg_ArrayPlusOffTooSmall"
        use pbyte = fixed &value.[startIndex]
        if isLittleEndian then
            if startIndex % 2 = 0 then // data is aligned
                pbyte |> NativePtr.toNativeInt |> NativePtr.ofNativeInt |> NativePtr.read
            else (int16 (NativePtr.get pbyte 0)) ||| (int16 (NativePtr.get pbyte 1) <<< 8)
        else (int16 (NativePtr.get pbyte 0) <<< 8) ||| (int16 (NativePtr.get pbyte 1))

    /// Converts an array of bytes into an int.
    static member ToInt32 (value: byte[], startIndex: int, isLittleEndian: bool) : int =
        if isNull value then nullArg "value"
        if startIndex >= value.Length     then raise <| new ArgumentOutOfRangeException ("startIndex", "ArgumentOutOfRange_Index")
        if startIndex >  value.Length - 4 then raise <| new ArgumentException "Arg_ArrayPlusOffTooSmall"
        use pbyte = fixed &value.[startIndex]
        if isLittleEndian then
            if startIndex % 4 = 0 then // data is aligned
                pbyte |> NativePtr.toNativeInt |> NativePtr.ofNativeInt |> NativePtr.read
            else (int (NativePtr.get pbyte 0)) ||| (int (NativePtr.get pbyte 1) <<< 8) ||| (int (NativePtr.get pbyte 2) <<< 16) ||| (int (NativePtr.get pbyte 3) <<< 24)
        else (int (NativePtr.get pbyte 0) <<< 24) ||| (int (NativePtr.get pbyte 1) <<< 16) |||  (int (NativePtr.get pbyte 2) <<< 8) ||| (int (NativePtr.get pbyte 3))

    /// Converts an array of bytes into a long.
    static member ToInt64 (value: byte[], startIndex: int, isLittleEndian: bool) =
        if isNull value then nullArg "value"
        if startIndex >= value.Length     then raise <| new ArgumentOutOfRangeException ("startIndex", "ArgumentOutOfRange_Index")
        if startIndex >  value.Length - 8 then raise <| new ArgumentException "Arg_ArrayPlusOffTooSmall"
        use pbyte = fixed &value.[startIndex]
        if isLittleEndian then
            if startIndex % 8 = 0 then // data is aligned
                pbyte |> NativePtr.toNativeInt |> NativePtr.ofNativeInt |> NativePtr.read
            else 
                let i1 = (int64 (NativePtr.get pbyte 0)) ||| (int64 (NativePtr.get pbyte 1) <<< 8) ||| (int64 (NativePtr.get pbyte 2) <<< 16) ||| (int64 (NativePtr.get pbyte 3) <<< 24)
                let i2 = (int64 (NativePtr.get pbyte 4)) ||| (int64 (NativePtr.get pbyte 5) <<< 8) ||| (int64 (NativePtr.get pbyte 6) <<< 16) ||| (int64 (NativePtr.get pbyte 7) <<< 24)
                int64 i1 ||| ((int64 i2) <<< 32)
        else
            let i1 = (int64 (NativePtr.get pbyte 0) <<< 24) |||  (int64 (NativePtr.get pbyte 1) <<< 16) ||| (int64 (NativePtr.get pbyte 2) <<< 8) ||| (int64 (NativePtr.get pbyte 3))
            let i2 = (int64 (NativePtr.get pbyte 4) <<< 24) |||  (int64 (NativePtr.get pbyte 5) <<< 16) ||| (int64 (NativePtr.get pbyte 6) <<< 8) ||| (int64 (NativePtr.get pbyte 7))
            i2 ||| (i1 <<< 32)


    /// Converts an array of bytes into an ushort.
    ///
    static member ToUInt16 (value: byte [], startIndex, isLittleEndian) = uint16 <| BitConverter.ToInt16 (value, startIndex, isLittleEndian)

    /// Converts an array of bytes into an uint.
    ///
    static member ToUInt32 (value: byte [], startIndex, isLittleEndian) = uint32 <| BitConverter.ToInt32 (value, startIndex, isLittleEndian)

    /// Converts an array of bytes into an unsigned long.
    ///
    static member ToUInt64 (value: byte [], startIndex, isLittleEndian) = uint64 <| BitConverter.ToInt64 (value, startIndex, isLittleEndian)

    /// Converts an array of bytes into a float.
    static member ToSingle (value: byte [], startIndex, isLittleEndian) : float32 =
        let mutable value = BitConverter.ToInt32 (value, startIndex, isLittleEndian)
        &&value |> NativePtr.toNativeInt |> NativePtr.ofNativeInt |> NativePtr.read

    /// Converts an array of bytes into a double.
    static member ToDouble (value: byte [], startIndex, isLittleEndian) : float =
        let mutable value = BitConverter.ToInt64 (value, startIndex, isLittleEndian)
        &&value |> NativePtr.toNativeInt |> NativePtr.ofNativeInt |> NativePtr.read

    static member private GetHexValue (i: int) =
        Diagnostics.Debug.Assert (i >= 0 && i < 16, "i is out of range.")
        if i < 10 then char i + '0'
        else char (i - 10) + 'A'

    /// Converts an array of bytes into a String.
    static member ToString (value: byte [], startIndex, length) =
        if isNull value then nullArg "value"
        let arrayLen = value.Length
        if startIndex >= value.Length then raise <| new ArgumentOutOfRangeException ("startIndex", "ArgumentOutOfRange_StartIndex")        
        let realLength = length
        if realLength < 0                     then raise <| new ArgumentOutOfRangeException ("length", "ArgumentOutOfRange_GenericPositive")
        if startIndex > arrayLen - realLength then raise <| new ArgumentException "Arg_ArrayPlusOffTooSmall"
        if realLength = 0 then String.Empty
        else
            let chArray = Array.zeroCreate (realLength * 3)
            let mutable index = startIndex
            for i in 0 .. 3 .. (3 * realLength) - 1 do
                let b = int value.[index]
                index <- index + 1
                chArray.[i] <- BitConverter.GetHexValue (b / 16)
                chArray.[i + 1] <- BitConverter.GetHexValue (b % 16)
                chArray.[i + 2] <- '-'

            // We don't need the last '-' character
            String (chArray, 0, chArray.Length - 1)

    /// Converts an array of bytes into a String.
    static member ToString (value: byte []) =
        if isNull value then nullArg "value"
        BitConverter.ToString (value, 0, value.Length)

    /// Converts an array of bytes into a String.
    static member ToString (value: byte [], startIndex) =
        if isNull value then nullArg "value"
        BitConverter.ToString (value, startIndex, value.Length - startIndex)

// findSliceIndex
module FindSliceIndex =
    open System.Linq
    open System.Collections.Generic

    let seqImpl (slice: seq<_>) (source: seq<_>) =
        let cache = Queue<_>()
        // we assume the slice is finite (otherwise it cannot be searched)
        let slice = slice |> Seq.toArray
        use sourceEnumerator = source.GetEnumerator()
        // we also assume either the source is finite or it actually contains the slice.
        let rec go index =
            if sourceEnumerator.MoveNext() then
                cache.Enqueue sourceEnumerator.Current
                if cache.Count = slice.Length then
                    if cache.SequenceEqual slice then index - slice.Length + 1
                    else
                        cache.Dequeue() |> ignore
                        go (index + 1)
                else go (index + 1)
            else -1
        go 0

    let listImpl (slice: _ list) (source: _ list) =
        let cache = Queue<_>()
        // List.length is O(n)
        let sliceLength = slice.Length
        let rec go index source =
            match source with
            | h :: t ->
                cache.Enqueue h
                if cache.Count = sliceLength then
                    if cache.SequenceEqual slice then index - sliceLength + 1
                    else
                        cache.Dequeue() |> ignore
                        go (index + 1) t
                else go (index + 1) t
            | [] -> -1
        go 0 source

    let arrayImpl (slice: _ []) (source: _ []) =
        let cache = Queue<_>()
        let rec go index =
            if index < source.Length then
                let h = source.[index]
                cache.Enqueue h
                if cache.Count = slice.Length then
                    if cache.SequenceEqual slice then index - slice.Length + 1
                    else
                        cache.Dequeue() |> ignore
                        go (index + 1)
                else go (index + 1)
            else -1
        go 0


