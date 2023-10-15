namespace FSharpPlus.Control

#nowarn "77" 
// Warn FS0077 -> Member constraints with the name 'op_Explicit' are given special status by the F# compiler as certain .NET types are implicitly augmented with this member. This may result in runtime failures if you attempt to invoke the member constraint from your own code.
// But all simulated types are being handled so here Explicit is SAFE from runtime errors.


open System
open System.Text
open FSharpPlus.Internals
open FSharpPlus.Internals.Prelude

#if (!FABLE_COMPILER || FABLE_COMPILER_3) && !FABLE_COMPILER_4

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
#if !FABLE_COMPILER_3
    static member inline Explicit (_: nativeint , _: Explicit) = fun x -> nativeint  (int x)
    static member inline Explicit (_: unativeint, _: Explicit) = fun x -> unativeint (uint32 x)
#endif    
    static member inline Explicit (_: float     , _: Explicit) = fun x -> float           x
    static member inline Explicit (_: float32   , _: Explicit) = fun x -> float32         x    
    static member inline Explicit (_: decimal   , _: Explicit) = fun x -> decimal         x
    static member inline Explicit (_: char      , _: Explicit) = fun x -> char            x
    static member inline Invoke value : 'T      =
        let inline call_2 (a: ^a, b: ^r) = ((^a or ^r or ^t) : (static member Explicit : _*_ -> ('t  -> ^r)) b, a)
        let inline call (a: 'a) = fun (x: 'x) -> call_2 (a, Unchecked.defaultof<'r>) x : 'r
        call Unchecked.defaultof<Explicit> value
#if !FABLE_COMPILER_3
type OfBytes =
    static member OfBytes (_: bool   , _: OfBytes) = fun (x, i, _) -> BitConverter.ToBoolean(x, i)

    static member OfBytes (_: char   , _: OfBytes) = fun (x, i, e) -> BitConverter.ToChar   (x, i, e)
    static member OfBytes (_: float  , _: OfBytes) = fun (x, i, e) -> BitConverter.ToDouble (x, i, e)
    static member OfBytes (_: int16  , _: OfBytes) = fun (x, i, e) -> BitConverter.ToInt16  (x, i, e)
    static member OfBytes (_: int    , _: OfBytes) = fun (x, i, e) -> BitConverter.ToInt32  (x, i, e)
    static member OfBytes (_: int64  , _: OfBytes) = fun (x, i, e) -> BitConverter.ToInt64  (x, i, e)
    static member OfBytes (_: float32, _: OfBytes) = fun (x, i, e) -> BitConverter.ToSingle (x, i, e)

    static member OfBytes (_: string , _: OfBytes) = fun (x, i, _) -> BitConverter.ToString (x, i)    
    static member OfBytes (_: Guid   , _: OfBytes) = fun (x, i, e) -> BitConverter.ToGuid (x, i, e)

    static member OfBytes (_: uint16 , _: OfBytes) = fun (x, i, e) -> BitConverter.ToUInt16 (x, i, e)
    static member OfBytes (_: uint32 , _: OfBytes) = fun (x, i, e) -> BitConverter.ToUInt32 (x, i, e)
    static member OfBytes (_: uint64 , _: OfBytes) = fun (x, i, e) -> BitConverter.ToUInt64 (x, i, e)

    static member inline Invoke (isLtEndian: bool) (startIndex: int) (value: byte[]) =
        let inline call_2 (a: ^a, b: ^b) = ((^a or ^b) : (static member OfBytes : _*_ -> _) b, a)
        let inline call (a: 'a) = fun (x: 'x) -> call_2 (a, Unchecked.defaultof<'r>) x : 'r
        call Unchecked.defaultof<OfBytes> (value, startIndex, isLtEndian)


type ToBytes =
    static member ToBytes (x: bool   , _, _: ToBytes) = BitConverter.GetBytes (x)
    static member ToBytes (x: char   , e, _: ToBytes) = BitConverter.GetBytes (x, BitConverter.IsLittleEndian = e)
    static member ToBytes (x: float  , e, _: ToBytes) = BitConverter.GetBytes (x, BitConverter.IsLittleEndian = e)
    static member ToBytes (x: int16  , e, _: ToBytes) = BitConverter.GetBytes (x, BitConverter.IsLittleEndian = e)
    static member ToBytes (x: int    , e, _: ToBytes) = BitConverter.GetBytes (x, BitConverter.IsLittleEndian = e)
    static member ToBytes (x: int64  , e, _: ToBytes) = BitConverter.GetBytes (x, BitConverter.IsLittleEndian = e)
    static member ToBytes (x: float32, e, _: ToBytes) = BitConverter.GetBytes (x, BitConverter.IsLittleEndian = e)
    static member ToBytes (x: string , _, _: ToBytes) = Array.map byte (x.ToCharArray ())
    static member ToBytes (x: Guid   , e, _: ToBytes) = BitConverter.GetBytes (x, BitConverter.IsLittleEndian = e)
    static member ToBytes (x: uint16 , e, _: ToBytes) = BitConverter.GetBytes (x, BitConverter.IsLittleEndian = e)
    static member ToBytes (x: uint32 , e, _: ToBytes) = BitConverter.GetBytes (x, BitConverter.IsLittleEndian = e)
    static member ToBytes (x: uint64 , e, _: ToBytes) = BitConverter.GetBytes (x, BitConverter.IsLittleEndian = e)

    static member inline Invoke (isLittleEndian: bool) value : byte[] =
        let inline call_2 (a: ^a, b: ^b, e) = ((^a or ^b) : (static member ToBytes : _*_*_ -> _) b, e, a)
        let inline call (a: 'a, b: 'b, e) = call_2 (a, b, e)
        call (Unchecked.defaultof<ToBytes>, value, isLittleEndian)

#endif
open System.Globalization

type TryParse =
    inherit Default1
#if !FABLE_COMPILER_3
    static member TryParse (_: decimal       , _: TryParse) = fun (x:string) -> Decimal.TryParse (x, NumberStyles.Any, CultureInfo.InvariantCulture) |> tupleToOption : option<decimal>
    static member TryParse (_: float32       , _: TryParse) = fun (x:string) -> Single.TryParse  (x, NumberStyles.Any, CultureInfo.InvariantCulture) |> tupleToOption : option<float32>
    static member TryParse (_: float         , _: TryParse) = fun (x:string) -> Double.TryParse  (x, NumberStyles.Any, CultureInfo.InvariantCulture) |> tupleToOption : option<float>
    static member TryParse (_: uint16        , _: TryParse) = fun (x:string) -> UInt16.TryParse  (x, NumberStyles.Any, CultureInfo.InvariantCulture) |> tupleToOption : option<uint16>
    static member TryParse (_: uint32        , _: TryParse) = fun (x:string) -> UInt32.TryParse  (x, NumberStyles.Any, CultureInfo.InvariantCulture) |> tupleToOption : option<uint32>
    static member TryParse (_: uint64        , _: TryParse) = fun (x:string) -> UInt64.TryParse  (x, NumberStyles.Any, CultureInfo.InvariantCulture) |> tupleToOption : option<uint64>
    static member TryParse (_: int16         , _: TryParse) = fun (x:string) -> Int16.TryParse   (x, NumberStyles.Any, CultureInfo.InvariantCulture) |> tupleToOption : option<int16>
    static member TryParse (_: int           , _: TryParse) = fun (x:string) -> Int32.TryParse   (x, NumberStyles.Any, CultureInfo.InvariantCulture) |> tupleToOption : option<int>
    static member TryParse (_: int64         , _: TryParse) = fun (x:string) -> Int64.TryParse   (x, NumberStyles.Any, CultureInfo.InvariantCulture) |> tupleToOption : option<int64>
#else
    static member TryParse (_: decimal       , _: TryParse) = fun (x:string) -> Decimal.TryParse (x) |> tupleToOption : option<decimal>
    static member TryParse (_: float32       , _: TryParse) = fun (x:string) -> Single.TryParse  (x) |> tupleToOption : option<float32>
    static member TryParse (_: float         , _: TryParse) = fun (x:string) -> Double.TryParse  (x) |> tupleToOption : option<float>
    static member TryParse (_: uint16        , _: TryParse) = fun (x:string) -> UInt16.TryParse  (x) |> tupleToOption : option<uint16>
    static member TryParse (_: uint32        , _: TryParse) = fun (x:string) -> UInt32.TryParse  (x) |> tupleToOption : option<uint32>
    static member TryParse (_: uint64        , _: TryParse) = fun (x:string) -> UInt64.TryParse  (x) |> tupleToOption : option<uint64>
    static member TryParse (_: int16         , _: TryParse) = fun (x:string) -> Int16.TryParse   (x) |> tupleToOption : option<int16>
    static member TryParse (_: int           , _: TryParse) = fun (x:string) -> Int32.TryParse   (x) |> tupleToOption : option<int>
    static member TryParse (_: int64         , _: TryParse) = fun (x:string) -> Int64.TryParse   (x) |> tupleToOption : option<int64>
#endif

    static member TryParse (_: string        , _: TryParse) = fun x -> Some x                               : option<string>
    static member TryParse (_: StringBuilder , _: TryParse) = fun x -> Some (new StringBuilder (x: string)) : option<StringBuilder>
    #if !FABLE_COMPILER
    
    static member TryParse (_: DateTime      , _: TryParse) = fun (x:string) ->
        match DateTime.TryParseExact (x, [|"yyyy-MM-ddTHH:mm:ss.fffZ"; "yyyy-MM-ddTHH:mm:ssZ"|], null, DateTimeStyles.RoundtripKind) with
        | true, x -> Some x
        | _ ->
            match DateTime.TryParse (x, CultureInfo.InvariantCulture, DateTimeStyles.None) with
            | true, x -> Some x
            | _ -> None
    
    static member TryParse (_: DateTimeOffset, _: TryParse) = fun (x:string) ->
        match DateTimeOffset.TryParseExact (x, [|"yyyy-MM-ddTHH:mm:ss.fffK"; "yyyy-MM-ddTHH:mm:ssK"|], null, DateTimeStyles.AssumeUniversal) with
        | true, x -> Some x
        | _ ->
            match DateTimeOffset.TryParse (x, CultureInfo.InvariantCulture, DateTimeStyles.None) with
            | true, x -> Some x
            | _ -> None
    #endif

    static member inline Invoke (value: string) =
        let inline call_2 (a: ^a, b: ^b) = ((^a or ^b) : (static member TryParse : _*_ -> _) b, a)
        let inline call (a: 'a) = fun (x: 'x) -> call_2 (a, Unchecked.defaultof<'r>) x : 'r option
        call Unchecked.defaultof<TryParse> value

    /// The F# signature
    static member inline InvokeOnInstance (value: string) = (^R: (static member TryParse : string -> 'R option) value)

    /// The .Net signature
    static member inline InvokeOnConvention (value: string) =
        let mutable r = Unchecked.defaultof< ^R>
        if (^R: (static member TryParse : _ * _ -> _) (value, &r)) then Some r else None

    #if NET7_0
    /// IParsable<'T>
    static member InvokeOnInterface<'T when 'T :> IParsable<'T>> (value: string) =
        let mutable r = Unchecked.defaultof<'T>
        if ('T.TryParse(value, CultureInfo.InvariantCulture, &r)) then Some r else None
    #endif


type Parse =
    inherit Default1
    static member inline Parse (_: ^R                  , _: Parse   ) = fun (x:string) -> (^R: (static member Parse : _ * _ -> ^R) (x, CultureInfo.InvariantCulture))

    static member inline Parse (_: 'T when 'T : enum<_>, _: Parse   ) = fun (x:string) ->
        (match Enum.TryParse (x) with
            | (true, v) -> v
            | _         -> invalidArg "value" ("Requested value '" + x + "' was not found.")
        ) : 'enum

    #if !FABLE_COMPILER
    static member Parse (_: DateTime      , _: Parse) = fun (x:string) ->
        match DateTime.TryParseExact (x, [|"yyyy-MM-ddTHH:mm:ss.fffZ"; "yyyy-MM-ddTHH:mm:ssZ"|], null, DateTimeStyles.RoundtripKind) with
        | true, x -> x
        | _ -> DateTime.Parse (x, CultureInfo.InvariantCulture, DateTimeStyles.AdjustToUniversal)

    static member Parse (_: DateTimeOffset, _: Parse) = fun (x:string) ->
        try DateTimeOffset.ParseExact (x, [|"yyyy-MM-ddTHH:mm:ss.fffK"; "yyyy-MM-ddTHH:mm:ssK"|], null, DateTimeStyles.AssumeUniversal)
        with _ -> DateTimeOffset.Parse (x, CultureInfo.InvariantCulture)
    #endif

    static member Parse (_: bool         , _: Parse) = fun (x:string) -> Boolean.Parse (x)

    static member Parse (_: char         , _: Parse) = fun x -> Char   .Parse (x)
    static member Parse (_: string       , _: Parse) = id : string->_
    static member Parse (_: StringBuilder, _: Parse) = fun x -> new StringBuilder (x: string)

    static member inline Invoke (value: string) =
        let inline call_2 (a: ^a, b: ^b) = ((^a or ^b) : (static member Parse : _*_ -> _) b, a)
        let inline call (a: 'a) = fun (x: 'x) -> call_2 (a, Unchecked.defaultof<'r>) x : 'r
        call Unchecked.defaultof<Parse> value

    static member inline InvokeOnInstance (value: string) = (^R: (static member Parse : _ -> ^R) value)


type Parse with

    static member inline Parse (_: ^R                  , _: Default4) = fun (value: string) ->
        match TryParse.InvokeOnConvention value with
        | Some x -> x : ^R
        | None -> invalidArg "value" ("Error parsing value '" + value + "'.")

    static member inline Parse (_: ^R                  , _: Default3) = fun (value: string) ->
        match TryParse.InvokeOnInstance value with
        | Some x -> x : ^R
        | None -> invalidArg "value" ("Error parsing value '" + value + "'.")
    
    static member inline Parse (_: ^R                  , _: Default2) : string -> ^R  = Parse.InvokeOnInstance

    #if NET7_0
    static member Parse<'T when 'T :> IParsable<'T>> (_: 'T, _: Default1) = fun (x: string) -> 'T.Parse (x, CultureInfo.InvariantCulture)
    static member inline Parse (_: ^t when ^t: null and ^t: struct, _: Default1) = id
    #else
    static member inline Parse (_: ^t when ^t: null and ^t: struct, _: Default2) = id
    #endif

type TryParse with

    static member inline TryParse (_: 'R, _: Default4) : string -> 'R option = fun (value: string) ->
        try Some (Parse.InvokeOnInstance value) with
        | :? ArgumentNullException | :? FormatException -> None 
        | _ -> reraise ()

    static member inline TryParse (_: 'R, _: Default3) : string -> 'R option = TryParse.InvokeOnConvention

    static member inline TryParse (_: 'R, _: Default2) : string -> 'R option = TryParse.InvokeOnInstance

    #if NET7_0
    static member inline TryParse (_: 'R, _: Default1) : string -> 'R option = TryParse.InvokeOnInterface
    static member inline TryParse (_: ^t when ^t: null and ^t: struct, _: Default1) = id
    #else
    static member inline TryParse (_: ^t when ^t: null and ^t: struct, _: Default2) = id
    #endif

#endif
