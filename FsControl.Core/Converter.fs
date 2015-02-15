namespace FsControl.Core.TypeMethods

open System
open System.Collections.Generic
open System.Text
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Core.Printf
open FsControl.BaseLib
open FsControl.Core
open FsControl.Core.Prelude
open System.Numerics

module Converter =

    type Convert() =
        static member inline Convert (_:Convert, _:sbyte     ) = fun x -> sbyte           x
        static member inline Convert (_:Convert, _:int16     ) = fun x -> int16           x
        static member inline Convert (_:Convert, _:int32     ) = fun x -> int             x
        static member inline Convert (_:Convert, _:int64     ) = fun x -> int64           x
        static member inline Convert (_:Convert, _:nativeint ) = fun x -> nativeint  (int x)
        static member inline Convert (_:Convert, _:byte      ) = fun x -> byte            x
        static member inline Convert (_:Convert, _:uint16    ) = fun x -> uint16          x
        static member inline Convert (_:Convert, _:uint32    ) = fun x -> uint32          x
        static member inline Convert (_:Convert, _:uint64    ) = fun x -> uint64          x
        static member inline Convert (_:Convert, _:unativeint) = fun x -> unativeint (int x)
        static member inline Convert (_:Convert, _:float     ) = fun x -> float           x
        static member inline Convert (_:Convert, _:float32   ) = fun x -> float32         x    
        static member inline Convert (_:Convert, _:decimal   ) = fun x -> decimal         x
#if NOTNET35
        static member inline Convert (_:Convert, _:Complex   ) = fun x -> Complex (float  x, 0.0)
#endif
        static member inline Convert (_:Convert, _:char      ) = fun x -> char x
        static member inline Convert (_:Convert, _:string    ) = fun x -> string x  // better use our ToString

    let Convert = Convert()


    type FromBytes() =
        static member FromBytes (_:FromBytes, _:bool   ) = fun (x, i, _) -> BitConverter.ToBoolean(x, i)
        static member FromBytes (_:FromBytes, _:char   ) = fun (x, i, e) -> BitConverter.ToChar   (x, i, e)
        static member FromBytes (_:FromBytes, _:float  ) = fun (x, i, e) -> BitConverter.ToDouble (x, i, e)
        static member FromBytes (_:FromBytes, _: int16 ) = fun (x, i, e) -> BitConverter.ToInt16  (x, i, e)
        static member FromBytes (_:FromBytes, _: int   ) = fun (x, i, e) -> BitConverter.ToInt32  (x, i, e)
        static member FromBytes (_:FromBytes, _:int64  ) = fun (x, i, e) -> BitConverter.ToInt64  (x, i, e)
        static member FromBytes (_:FromBytes, _:float32) = fun (x, i, e) -> BitConverter.ToSingle (x, i, e)
        static member FromBytes (_:FromBytes, _:string ) = fun (x, i, _) -> BitConverter.ToString (x, i)
        static member FromBytes (_:FromBytes, _:uint16 ) = fun (x, i, e) -> BitConverter.ToUInt16 (x, i, e)
        static member FromBytes (_:FromBytes, _:uint32 ) = fun (x, i, e) -> BitConverter.ToUInt32 (x, i, e)
        static member FromBytes (_:FromBytes, _:uint64 ) = fun (x, i, e) -> BitConverter.ToUInt64 (x, i, e)
        
    let FromBytes = FromBytes()


    type ToBytes() =
        static member ToBytes (_:ToBytes, x:bool   , _) = fun _ -> BitConverter.GetBytes(x)
        static member ToBytes (_:ToBytes, x:char   , _) = fun e -> BitConverter.GetBytes(x, BitConverter.IsLittleEndian = e)
        static member ToBytes (_:ToBytes, x:float  , _) = fun e -> BitConverter.GetBytes(x, BitConverter.IsLittleEndian = e)
        static member ToBytes (_:ToBytes, x: int16 , _) = fun e -> BitConverter.GetBytes(x, BitConverter.IsLittleEndian = e)
        static member ToBytes (_:ToBytes, x: int   , _) = fun e -> BitConverter.GetBytes(x, BitConverter.IsLittleEndian = e)
        static member ToBytes (_:ToBytes, x:int64  , _) = fun e -> BitConverter.GetBytes(x, BitConverter.IsLittleEndian = e)
        static member ToBytes (_:ToBytes, x:float32, _) = fun e -> BitConverter.GetBytes(x, BitConverter.IsLittleEndian = e)
        static member ToBytes (_:ToBytes, x:string , _) = fun e -> Array.map byte (x.ToCharArray())
        static member ToBytes (_:ToBytes, x:uint16 , _) = fun e -> BitConverter.GetBytes(x, BitConverter.IsLittleEndian = e)
        static member ToBytes (_:ToBytes, x:uint32 , _) = fun e -> BitConverter.GetBytes(x, BitConverter.IsLittleEndian = e)
        static member ToBytes (_:ToBytes, x:uint64 , _) = fun e -> BitConverter.GetBytes(x, BitConverter.IsLittleEndian = e)

    let ToBytes = ToBytes()


    type TryParse() =
        static member inline TryParse (_:TryParse, _:'t     option) = fun x -> 
            let mutable r = Unchecked.defaultof< ^R>
            if (^R: (static member TryParse: _ * _ -> _) (x, &r)) then Some r else None

        static member TryParse (_:TryParse, _:string        option) = fun x -> Some x                             :option<string>
        static member TryParse (_:TryParse, _:StringBuilder option) = fun x -> Some (new StringBuilder(x:string)) :option<StringBuilder>
 
    let TryParse = TryParse()


    type Parse() =
        inherit Typ1()
        static member inline Parse (_:Typ1 , _:^R) = fun (x:string) -> (^R: (static member Parse: _ -> ^R) x)
        static member inline Parse (_:Parse, _:^R) = fun (x:string) -> (^R: (static member Parse: _ * _ -> ^R) (x, Globalization.CultureInfo.InvariantCulture))
#if NOTNET35
        static member        Parse (_:Parse, _:'T when 'T : enum<_>) = fun x ->
            (match Enum.TryParse(x) with
             | (true, v) -> v
             | _         -> invalidArg "value" ("Requested value '" + x + "' was not found.")
            ):'enum
#endif
        static member Parse (_:Parse, _:bool         ) = fun x -> Boolean.Parse(x)
        static member Parse (_:Parse, _:char         ) = fun x -> Char   .Parse(x)
        static member Parse (_:Parse, _:string       ) = id :string->_
        static member Parse (_:Parse, _:StringBuilder) = fun x -> new StringBuilder(x:string)

    let Parse = Parse()


    type ToString() =
        static member ToString (_:ToString, x:bool,          _) = fun (k:Globalization.CultureInfo) -> x.ToString k
        static member ToString (_:ToString, x:char,          _) = fun (k:Globalization.CultureInfo) -> x.ToString k
        static member ToString (_:ToString, x:byte,          _) = fun (k:Globalization.CultureInfo) -> x.ToString k
        static member ToString (_:ToString, x:sbyte,         _) = fun (k:Globalization.CultureInfo) -> x.ToString k
        static member ToString (_:ToString, x:float,         _) = fun (k:Globalization.CultureInfo) -> x.ToString k
        static member ToString (_:ToString, x:int16,         _) = fun (k:Globalization.CultureInfo) -> x.ToString k
        static member ToString (_:ToString, x:int,           _) = fun (k:Globalization.CultureInfo) -> x.ToString k
        static member ToString (_:ToString, x:int64,         _) = fun (k:Globalization.CultureInfo) -> x.ToString k
        static member ToString (_:ToString, x:float32,       _) = fun (k:Globalization.CultureInfo) -> x.ToString k
        static member ToString (_:ToString, x:string,        _) = fun (_:Globalization.CultureInfo) -> if x = null then "null" else x
        static member ToString (_:ToString, x:uint16,        _) = fun (k:Globalization.CultureInfo) -> x.ToString k
        static member ToString (_:ToString, x:uint32,        _) = fun (k:Globalization.CultureInfo) -> x.ToString k
        static member ToString (_:ToString, x:uint64,        _) = fun (k:Globalization.CultureInfo) -> x.ToString k
        static member ToString (_:ToString, x:decimal,       _) = fun (k:Globalization.CultureInfo) -> x.ToString k
        static member ToString (_:ToString, x:DateTime,      _) = fun (k:Globalization.CultureInfo) -> x.ToString k
        static member ToString (_:ToString, x:DateTimeOffset,_) = fun (k:Globalization.CultureInfo) -> x.ToString k
        static member ToString (_:ToString, x:StringBuilder, _) = fun (_:Globalization.CultureInfo) -> if x = null then "null" else x.ToString()

    let ToString = ToString()
    let inline internal toString (culture:Globalization.CultureInfo) value : string =
        let inline instance_3 (a:^a, b:^b, c:^c) = ((^a or ^b or ^c) : (static member ToString: _*_*_ -> _) a, b, c)
        let inline instance (a:'a, b:'b) = fun (x:'x) -> instance_3 (a, b, Unchecked.defaultof<'r>) x :'r
        instance (ToString, value) culture

    type ToString with static member inline ToString (_:ToString, KeyValue(a,b), _) = fun (k:Globalization.CultureInfo) ->
                        "(" + toString k a + ", " + toString k b + ")"
    type ToString with static member inline ToString (_:ToString, (a,b),         _) = fun (k:Globalization.CultureInfo) ->
                        "(" + toString k a + ", " + toString k b + ")"
    type ToString with static member inline ToString (_:ToString, (a,b,c),       _) = fun (k:Globalization.CultureInfo) ->
                        "(" + toString k a + ", " + toString k b + ", " + toString k c + ")"
    type ToString with static member inline ToString (_:ToString, (a,b,c,d),     _) = fun (k:Globalization.CultureInfo) ->
                        "(" + toString k a + ", " + toString k b + ", " + toString k c + ", " + toString k d + ")"
    type ToString with static member inline ToString (_:ToString, (a,b,c,d,e),   _) = fun (k:Globalization.CultureInfo) ->
                        "(" + toString k a + ", " + toString k b + ", " + toString k c + ", " + toString k d + ", " + toString k e + ")"


    let inline internal seqToString (k:Globalization.CultureInfo) sepOpen sepClose x (b: StringBuilder) =
        let inline append (s:string) = b.Append s |> ignore
        append sepOpen
        let withSemiColons = Collection.intersperse "; " (Seq.map (toString k) x)
        Seq.iter append withSemiColons
        append sepClose
        toString k b

    type ToString with static member inline ToString (_:ToString, x:_ list, _) = fun (k:Globalization.CultureInfo) ->
                        let b = StringBuilder()
                        seqToString k "[" "]" x b

    type ToString with static member inline ToString (_:ToString, x:_ array, _) = fun (k:Globalization.CultureInfo) ->
                        let b = StringBuilder()
                        seqToString k "[|" "|]" x b

    type ToString with 
        static member inline ToString (_:ToString, x:_ ResizeArray, _) = fun (k:Globalization.CultureInfo) ->
                        let b = StringBuilder()
                        b.Append "ResizeArray " |> ignore
                        seqToString k "[" "]" x b
        static member ToString (_:ToString, x:Expr<_>,       _) = fun (k:Globalization.CultureInfo) -> x.ToString()

    type ToString with static member inline ToString (_:ToString, x:_ seq, _) = fun (k:Globalization.CultureInfo) ->
                        let b = StringBuilder()
                        b.Append "seq " |> ignore
                        seqToString k "[" "]" x b

    type ToString with static member inline ToString (_:ToString, x:_ ICollection, _) = fun (k:Globalization.CultureInfo) ->
                        toString k (x :> _ seq)

    type ToString with static member inline ToString (_:ToString, x:_ IList, _) = fun (k:Globalization.CultureInfo) ->
                        toString k (x :> _ seq)

    type ToString with static member inline ToString (_:ToString, x:Map<_,_>, _) = fun (k:Globalization.CultureInfo) ->
                        let b = StringBuilder()
                        b.Append "map " |> ignore
                        seqToString k "[" "]" x b

    type ToString with 
        static member inline ToString (_:ToString, x:Dictionary<_,_>, _) = fun (k:Globalization.CultureInfo) ->
                        toString k (x :> seq<KeyValuePair<_,_>>)

        static member inline ToString (_:ToString, x:_ Set, _) = fun (k:Globalization.CultureInfo) ->
                        let b = StringBuilder()
                        b.Append "set " |> ignore
                        seqToString k "[" "]" x b

    type ToString with static member inline ToString (_:ToString, x:IDictionary<_,_>, _) = fun (k:Globalization.CultureInfo) ->
                        toString k (x :> seq<KeyValuePair<_,_>>)

    type ToString with static member inline ToString (_:ToString, x:_ option, _) = fun (k:Globalization.CultureInfo) ->
                        match x with
                        | Some a -> "Some " + toString k a
                        | None   -> "None"

    type ToString with static member inline ToString (_:ToString, x:Choice<_,_>, _) = fun (k:Globalization.CultureInfo) ->
                        match x with
                        | Choice1Of2 a -> "Choice1Of2 " + toString k a
                        | Choice2Of2 b -> "Choice2Of2 " + toString k b

    type ToString with static member inline ToString (_:ToString, x:Choice<_,_,_>, _) = fun (k:Globalization.CultureInfo) ->
                        match x with
                        | Choice1Of3 a -> "Choice1Of3 " + toString k a
                        | Choice2Of3 b -> "Choice2Of3 " + toString k b
                        | Choice3Of3 c -> "Choice3Of3 " + toString k c

    type ToString with static member inline ToString (_:ToString, x:Choice<_,_,_,_>, _) = fun (k:Globalization.CultureInfo) ->
                        match x with
                        | Choice1Of4 a -> "Choice1Of4 " + toString k a
                        | Choice2Of4 b -> "Choice2Of4 " + toString k b
                        | Choice3Of4 c -> "Choice3Of4 " + toString k c
                        | Choice4Of4 d -> "Choice4Of4 " + toString k d