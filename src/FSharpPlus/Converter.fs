#nowarn "77" 
// Warn FS0077 -> Member constraints with the name 'op_Explicit' are given special status by the F# compiler as certain .NET types are implicitly augmented with this member. This may result in runtime failures if you attempt to invoke the member constraint from your own code.
// But all simulated types are being handled so here Explicit is SAFE from runtime errors.

namespace FSharpPlus.Control

open System
open System.Runtime.CompilerServices
open System.Collections.Generic
open System.Text
open Microsoft.FSharp.Quotations
open FSharpPlus.Internals
open FSharpPlus.Internals.Prelude
open FSharpPlus

type Explicit =
    inherit Default1
    static member inline Explicit (_: 'R        , _: Default1) = fun (x : ^t) -> ((^R or ^t) : (static member op_Explicit : ^t -> ^R) x)
    static member inline Explicit (_: ^t when ^t: null and ^t: struct, _: Default1) = ()
    static member inline Explicit (_: byte      , _: Explicit) = fun x -> byte            x
    static member inline Explicit (_: sbyte     , _: Explicit) = fun x -> sbyte           x
    static member inline Explicit (_: int16     , _: Explicit) = fun x -> int16           x
    static member inline Explicit (_: uint16    , _: Explicit) = fun x -> uint16          x
    static member inline Explicit (_: int32     , _: Explicit) = fun x -> int             x
    static member inline Explicit (_: uint32    , _: Explicit) = fun x -> uint32          x
    static member inline Explicit (_: int64     , _: Explicit) = fun x -> int64           x
    static member inline Explicit (_: uint64    , _: Explicit) = fun x -> uint64          x
    static member inline Explicit (_: nativeint , _: Explicit) = fun x -> nativeint  (int x)
    static member inline Explicit (_: unativeint, _: Explicit) = fun x -> unativeint (uint32 x)
    static member inline Explicit (_: float     , _: Explicit) = fun x -> float           x
    static member inline Explicit (_: float32   , _: Explicit) = fun x -> float32         x    
    static member inline Explicit (_: decimal   , _: Explicit) = fun x -> decimal         x
    static member inline Explicit (_: char      , _: Explicit) = fun x -> char            x

    static member inline Invoke value : 'T      =
        let inline call_2 (a: ^a, b: ^r) = ((^a or ^r or ^t) : (static member Explicit : _*_ -> ('t  -> ^r)) b, a)
        let inline call (a: 'a) = fun (x: 'x) -> call_2 (a, Unchecked.defaultof<'r>) x : 'r
        call Unchecked.defaultof<Explicit> value

type OfBytes =
    static member OfBytes (_: bool   , _: OfBytes) = fun (x, i, _) -> BitConverter.ToBoolean(x, i)
    
    #if !FABLE_COMPILER
    static member OfBytes (_: char   , _: OfBytes) = fun (x, i, e) -> BitConverter.ToChar   (x, i, e)
    static member OfBytes (_: float  , _: OfBytes) = fun (x, i, e) -> BitConverter.ToDouble (x, i, e)
    static member OfBytes (_: int16  , _: OfBytes) = fun (x, i, e) -> BitConverter.ToInt16  (x, i, e)
    static member OfBytes (_: int    , _: OfBytes) = fun (x, i, e) -> BitConverter.ToInt32  (x, i, e)
    static member OfBytes (_: int64  , _: OfBytes) = fun (x, i, e) -> BitConverter.ToInt64  (x, i, e)
    static member OfBytes (_: float32, _: OfBytes) = fun (x, i, e) -> BitConverter.ToSingle (x, i, e)
    #endif

    static member OfBytes (_: string , _: OfBytes) = fun (x, i, _) -> BitConverter.ToString (x, i)
    
    #if !FABLE_COMPILER
    static member OfBytes (_: uint16 , _: OfBytes) = fun (x, i, e) -> BitConverter.ToUInt16 (x, i, e)
    static member OfBytes (_: uint32 , _: OfBytes) = fun (x, i, e) -> BitConverter.ToUInt32 (x, i, e)
    static member OfBytes (_: uint64 , _: OfBytes) = fun (x, i, e) -> BitConverter.ToUInt64 (x, i, e)
    #endif

    static member inline Invoke (isLtEndian: bool) (startIndex: int) (value: byte[]) =
        let inline call_2 (a: ^a, b: ^b) = ((^a or ^b) : (static member OfBytes : _*_ -> _) b, a)
        let inline call (a: 'a) = fun (x: 'x) -> call_2 (a, Unchecked.defaultof<'r>) x : 'r
        call Unchecked.defaultof<OfBytes> (value, startIndex, isLtEndian)


type ToBytes =
    static member ToBytes (x: bool   , _, _: ToBytes) = BitConverter.GetBytes (x)
    
    #if !FABLE_COMPILER
    static member ToBytes (x: char   , e, _: ToBytes) = BitConverter.GetBytes (x, BitConverter.IsLittleEndian = e)
    static member ToBytes (x: float  , e, _: ToBytes) = BitConverter.GetBytes (x, BitConverter.IsLittleEndian = e)
    static member ToBytes (x: int16  , e, _: ToBytes) = BitConverter.GetBytes (x, BitConverter.IsLittleEndian = e)
    static member ToBytes (x: int    , e, _: ToBytes) = BitConverter.GetBytes (x, BitConverter.IsLittleEndian = e)
    static member ToBytes (x: int64  , e, _: ToBytes) = BitConverter.GetBytes (x, BitConverter.IsLittleEndian = e)
    static member ToBytes (x: float32, e, _: ToBytes) = BitConverter.GetBytes (x, BitConverter.IsLittleEndian = e)
    #endif

    static member ToBytes (x: string , _, _: ToBytes) = Array.map byte (x.ToCharArray ())
    
    #if !FABLE_COMPILER
    static member ToBytes (x: uint16 , e, _: ToBytes) = BitConverter.GetBytes (x, BitConverter.IsLittleEndian = e)
    static member ToBytes (x: uint32 , e, _: ToBytes) = BitConverter.GetBytes (x, BitConverter.IsLittleEndian = e)
    static member ToBytes (x: uint64 , e, _: ToBytes) = BitConverter.GetBytes (x, BitConverter.IsLittleEndian = e)
    #endif

    static member inline Invoke (isLittleEndian: bool) value : byte[] =
        let inline call_2 (a: ^a, b: ^b, e) = ((^a or ^b) : (static member ToBytes : _*_*_ -> _) b, e, a)
        let inline call (a: 'a, b: 'b, e) = call_2 (a, b, e)
        call (Unchecked.defaultof<ToBytes>, value, isLittleEndian)


open System.Globalization

type TryParse =
    inherit Default1

    static member TryParse (_: decimal       , _: TryParse) = fun x -> Decimal.TryParse (x, NumberStyles.Any, CultureInfo.InvariantCulture) |> tupleToOption : option<decimal>
    static member TryParse (_: float32       , _: TryParse) = fun x -> Single.TryParse  (x, NumberStyles.Any, CultureInfo.InvariantCulture) |> tupleToOption : option<float32>
    static member TryParse (_: float         , _: TryParse) = fun x -> Double.TryParse  (x, NumberStyles.Any, CultureInfo.InvariantCulture) |> tupleToOption : option<float>
    static member TryParse (_: uint16        , _: TryParse) = fun x -> UInt16.TryParse  (x, NumberStyles.Any, CultureInfo.InvariantCulture) |> tupleToOption : option<uint16>
    static member TryParse (_: uint32        , _: TryParse) = fun x -> UInt32.TryParse  (x, NumberStyles.Any, CultureInfo.InvariantCulture) |> tupleToOption : option<uint32>
    static member TryParse (_: uint64        , _: TryParse) = fun x -> UInt64.TryParse  (x, NumberStyles.Any, CultureInfo.InvariantCulture) |> tupleToOption : option<uint64>
    static member TryParse (_: int16         , _: TryParse) = fun x -> Int16.TryParse   (x, NumberStyles.Any, CultureInfo.InvariantCulture) |> tupleToOption : option<int16>
    static member TryParse (_: int           , _: TryParse) = fun x -> Int32.TryParse   (x, NumberStyles.Any, CultureInfo.InvariantCulture) |> tupleToOption : option<int>
    static member TryParse (_: int64         , _: TryParse) = fun x -> Int64.TryParse   (x, NumberStyles.Any, CultureInfo.InvariantCulture) |> tupleToOption : option<int64>

    static member TryParse (_: string        , _: TryParse) = fun x -> Some x                               : option<string>
    static member TryParse (_: StringBuilder , _: TryParse) = fun x -> Some (new StringBuilder (x: string)) : option<StringBuilder>
    
    static member TryParse (_: DateTime      , _: TryParse) = fun x -> DateTime.TryParseExact       (x, [|"yyyy-MM-ddTHH:mm:ss.fffZ"; "yyyy-MM-ddTHH:mm:ssZ"|], null, DateTimeStyles.RoundtripKind) |> tupleToOption : option<DateTime>
    static member TryParse (_: DateTimeOffset, _: TryParse) = fun x -> DateTimeOffset.TryParseExact (x, [|"yyyy-MM-ddTHH:mm:ss.fffK"; "yyyy-MM-ddTHH:mm:ssK"|], null, DateTimeStyles.RoundtripKind) |> tupleToOption : option<DateTimeOffset>

    static member inline Invoke (value: string) =
        let inline call_2 (a: ^a, b: ^b) = ((^a or ^b) : (static member TryParse : _*_ -> _) b, a)
        let inline call (a: 'a) = fun (x: 'x) -> call_2 (a, Unchecked.defaultof<'r>) x : 'r option
        call Unchecked.defaultof<TryParse> value

type TryParse with
    static member inline TryParse (_: 'R, _: Default2) = fun x ->
        let mutable r = Unchecked.defaultof< ^R>
        if (^R: (static member TryParse : _ * _ -> _) (x, &r)) then Some r else None

    static member inline TryParse (_: ^t when ^t: null and ^t: struct, _: Default1) = id
    static member inline TryParse (_: 'R, _: Default1) = fun x -> (^R: (static member TryParse : string -> 'R option) x)

type Parse =
    inherit Default1
    static member inline Parse (_: ^R                  , _: Default1) = fun (x:string) -> (^R: (static member Parse : _ -> ^R) x)
    static member inline Parse (_: ^R                  , _: Parse   ) = fun (x:string) -> (^R: (static member Parse : _ * _ -> ^R) (x, CultureInfo.InvariantCulture))
#if NET35
#else
    static member        Parse (_: 'T when 'T : enum<_>, _: Parse   ) = fun x ->
        (match Enum.TryParse (x) with
            | (true, v) -> v
            | _         -> invalidArg "value" ("Requested value '" + x + "' was not found.")
        ) : 'enum
#endif
    
    static member Parse (_: bool         , _: Parse) = fun x -> Boolean.Parse (x)
    static member Parse (_: char         , _: Parse) = fun x -> Char   .Parse (x)
    static member Parse (_: string       , _: Parse) = id : string->_
    static member Parse (_: StringBuilder, _: Parse) = fun x -> new StringBuilder (x: string)

    static member inline Invoke (value: string) =
        let inline call_2 (a: ^a, b: ^b) = ((^a or ^b) : (static member Parse : _*_ -> _) b, a)
        let inline call (a: 'a) = fun (x: 'x) -> call_2 (a, Unchecked.defaultof<'r>) x : 'r
        call Unchecked.defaultof<Parse> value