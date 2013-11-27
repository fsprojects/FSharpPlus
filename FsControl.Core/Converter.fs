namespace FsControl.Core.TypeMethods

open System
open System.Collections.Generic
open System.Text
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Core.Printf
open FsControl.BaseLib
open FsControl.Core
open FsControl.Core.Prelude

module Converter =

    type FromBytes = FromBytes with
        static member instance (FromBytes, _:bool   ) = fun (x, i, _) -> BitConverter.ToBoolean(x, i)
        static member instance (FromBytes, _:char   ) = fun (x, i, e) -> BitConverter.ToChar   (x, i, e)
        static member instance (FromBytes, _:float  ) = fun (x, i, e) -> BitConverter.ToDouble (x, i, e)
        static member instance (FromBytes, _: int16 ) = fun (x, i, e) -> BitConverter.ToInt16  (x, i, e)
        static member instance (FromBytes, _: int   ) = fun (x, i, e) -> BitConverter.ToInt32  (x, i, e)
        static member instance (FromBytes, _:int64  ) = fun (x, i, e) -> BitConverter.ToInt64  (x, i, e)
        static member instance (FromBytes, _:float32) = fun (x, i, e) -> BitConverter.ToSingle (x, i, e)
        static member instance (FromBytes, _:string ) = fun (x, i, _) -> BitConverter.ToString (x, i)
        static member instance (FromBytes, _:uint16 ) = fun (x, i, e) -> BitConverter.ToUInt16 (x, i, e)
        static member instance (FromBytes, _:uint32 ) = fun (x, i, e) -> BitConverter.ToUInt32 (x, i, e)
        static member instance (FromBytes, _:uint64 ) = fun (x, i, e) -> BitConverter.ToUInt64 (x, i, e)
        
    let inline internal fromBytesWithOffset (isLittleEndian:bool) (startIndex:int) (value:byte[]) = Inline.instance FromBytes (value, startIndex, isLittleEndian)
    let inline internal fromBytes           (isLittleEndian:bool)                  (value:byte[]) = Inline.instance FromBytes (value, 0         , isLittleEndian)


    type ToBytes = ToBytes with
        static member instance (ToBytes, x:bool   , _) = fun _ -> BitConverter.GetBytes(x)
        static member instance (ToBytes, x:char   , _) = fun e -> if e = BitConverter.IsLittleEndian then BitConverter.GetBytes(x) else Array.rev (BitConverter.GetBytes(x))
        static member instance (ToBytes, x:float  , _) = fun e -> if e = BitConverter.IsLittleEndian then BitConverter.GetBytes(x) else Array.rev (BitConverter.GetBytes(x))
        static member instance (ToBytes, x: int16 , _) = fun e -> if e = BitConverter.IsLittleEndian then BitConverter.GetBytes(x) else Array.rev (BitConverter.GetBytes(x))
        static member instance (ToBytes, x: int   , _) = fun e -> if e = BitConverter.IsLittleEndian then BitConverter.GetBytes(x) else Array.rev (BitConverter.GetBytes(x))
        static member instance (ToBytes, x:int64  , _) = fun e -> if e = BitConverter.IsLittleEndian then BitConverter.GetBytes(x) else Array.rev (BitConverter.GetBytes(x))
        static member instance (ToBytes, x:float32, _) = fun e -> if e = BitConverter.IsLittleEndian then BitConverter.GetBytes(x) else Array.rev (BitConverter.GetBytes(x))
        static member instance (ToBytes, x:string , _) = fun e -> Array.map byte (x.ToCharArray())
        static member instance (ToBytes, x:uint16 , _) = fun e -> if e = BitConverter.IsLittleEndian then BitConverter.GetBytes(x) else Array.rev (BitConverter.GetBytes(x))
        static member instance (ToBytes, x:uint32 , _) = fun e -> if e = BitConverter.IsLittleEndian then BitConverter.GetBytes(x) else Array.rev (BitConverter.GetBytes(x))
        static member instance (ToBytes, x:uint64 , _) = fun e -> if e = BitConverter.IsLittleEndian then BitConverter.GetBytes(x) else Array.rev (BitConverter.GetBytes(x))

    let inline internal toBytes (isLittleEndian:bool) value :byte[] = Inline.instance (ToBytes, value) isLittleEndian

    let internal inv = Globalization.CultureInfo.InvariantCulture

    type TryParse = TryParse with
        static member inline instance (TryParse, _:'t     option) = fun x -> 
            let mutable r = Unchecked.defaultof< ^R>
            if (^R: (static member TryParse: _ * _ -> _) (x, &r)) then Some r else None

        static member instance (TryParse, _:string        option) = fun x -> Some x                             :option<string>
        static member instance (TryParse, _:StringBuilder option) = fun x -> Some (new StringBuilder(x:string)) :option<StringBuilder>
        
    let inline tryParse (value:string) = Inline.instance TryParse value


    type Parse = Parse with
        static member inline instance (Parse, _:^R) = fun (x:string) -> (^R: (static member Parse: _ * _ -> ^R) (x, Globalization.CultureInfo.InvariantCulture))
        static member        instance (Parse, _:'T when 'T : enum<_>) = fun x ->
            (match Enum.TryParse(x) with
             | (true, v) -> v
             | _         -> invalidArg "value" ("Requested value '" + x + "' was not found.")
            ):'enum

        static member instance (Parse, _:bool         ) = fun x -> Boolean.Parse(x)
        static member instance (Parse, _:char         ) = fun x -> Char   .Parse(x)
        static member instance (Parse, _:string       ) = id :string->_
        static member instance (Parse, _:StringBuilder) = fun x -> new StringBuilder(x:string)

    let inline internal parse (value:string) = Inline.instance Parse value


    type ToString = ToString with
        static member instance (ToString, x:bool,          _) = fun () -> x.ToString inv
        static member instance (ToString, x:char,          _) = fun () -> x.ToString inv
        static member instance (ToString, x:byte,          _) = fun () -> x.ToString inv
        static member instance (ToString, x:sbyte,         _) = fun () -> x.ToString inv
        static member instance (ToString, x:float,         _) = fun () -> x.ToString inv
        static member instance (ToString, x:int16,         _) = fun () -> x.ToString inv
        static member instance (ToString, x:int,           _) = fun () -> x.ToString inv
        static member instance (ToString, x:int64,         _) = fun () -> x.ToString inv
        static member instance (ToString, x:float32,       _) = fun () -> x.ToString inv
        static member instance (ToString, x:string,        _) = fun () -> if x = null then "null" else x
        static member instance (ToString, x:uint16,        _) = fun () -> x.ToString inv
        static member instance (ToString, x:uint32,        _) = fun () -> x.ToString inv
        static member instance (ToString, x:uint64,        _) = fun () -> x.ToString inv
        static member instance (ToString, x:decimal,       _) = fun () -> x.ToString inv
        static member instance (ToString, x:DateTime,      _) = fun () -> x.ToString inv
        static member instance (ToString, x:DateTimeOffset,_) = fun () -> x.ToString inv
        static member instance (ToString, x:StringBuilder, _) = fun () -> if x = null then "null" else x.ToString()

    let inline toString value : string = Inline.instance (ToString, value) ()

    type ToString with static member inline instance (ToString, KeyValue(a,b), _) = fun () ->
                        "(" + toString a + ", " + toString b + ")"
    type ToString with static member inline instance (ToString, (a,b),         _) = fun () ->
                        "(" + toString a + ", " + toString b + ")"
    type ToString with static member inline instance (ToString, (a,b,c),       _) = fun () ->
                        "(" + toString a + ", " + toString b + ", " + toString c + ")"
    type ToString with static member inline instance (ToString, (a,b,c,d),     _) = fun () ->
                        "(" + toString a + ", " + toString b + ", " + toString c + ", " + toString d + ")"
    type ToString with static member inline instance (ToString, (a,b,c,d,e),   _) = fun () ->
                        "(" + toString a + ", " + toString b + ", " + toString c + ", " + toString d + ", " + toString e + ")"


    let inline internal seqToString sepOpen sepClose x (b: StringBuilder) =
        let inline append (s:string) = b.Append s |> ignore
        append sepOpen
        let withSemiColons = Seq.intersperse "; " (Seq.map toString x)
        Seq.iter append withSemiColons
        append sepClose
        toString b

    type ToString with static member inline instance (ToString, x:_ list, _) = fun () ->
                        let b = StringBuilder()
                        seqToString "[" "]" x b

    type ToString with static member inline instance (ToString, x:_ array, _) = fun () ->
                        let b = StringBuilder()
                        seqToString "[|" "|]" x b

    type ToString with 
        static member inline instance (ToString, x:_ ResizeArray, _) = fun () ->
                        let b = StringBuilder()
                        b.Append "ResizeArray " |> ignore
                        seqToString "[" "]" x b
        static member instance (ToString, x:Expr<_>,       _) = fun () -> x.ToString()

    type ToString with static member inline instance (ToString, x:_ seq, _) = fun () ->
                        let b = StringBuilder()
                        b.Append "seq " |> ignore
                        seqToString "[" "]" x b

    type ToString with static member inline instance (ToString, x:_ ICollection, _) = fun () ->
                        toString (x :> _ seq)

    type ToString with static member inline instance (ToString, x:_ IList, _) = fun () ->
                        toString (x :> _ seq)

    type ToString with static member inline instance (ToString, x:Map<_,_>, _) = fun () ->
                        let b = StringBuilder()
                        b.Append "map " |> ignore
                        seqToString "[" "]" x b

    type ToString with 
        static member inline instance (ToString, x:Dictionary<_,_>, _) = fun () ->
                        toString (x :> seq<KeyValuePair<_,_>>)

        static member inline instance (ToString, x:_ Set, _) = fun () ->
                        let b = StringBuilder()
                        b.Append "set " |> ignore
                        seqToString "[" "]" x b

    type ToString with static member inline instance (ToString, x:IDictionary<_,_>, _) = fun () ->
                        toString (x :> seq<KeyValuePair<_,_>>)

    type ToString with static member inline instance (ToString, x:_ option, _) = fun () ->
                        match x with
                        | Some a -> "Some " + toString a
                        | None   -> "None"