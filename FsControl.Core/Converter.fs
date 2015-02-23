namespace FsControl.Core.TypeMethods

open System
open System.Runtime.CompilerServices
open System.Runtime.InteropServices
open System.Collections.Generic
open System.Text
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Core.Printf
open FsControl.BaseLib
open FsControl.Core
open FsControl.Core.Prelude
open System.Numerics


type Convert() =
    static member val Instance = Convert()
    static member inline Convert (_:sbyte     , _:Convert) = fun x -> sbyte           x
    static member inline Convert (_:int16     , _:Convert) = fun x -> int16           x
    static member inline Convert (_:int32     , _:Convert) = fun x -> int             x
    static member inline Convert (_:int64     , _:Convert) = fun x -> int64           x
    static member inline Convert (_:nativeint , _:Convert) = fun x -> nativeint  (int x)
    static member inline Convert (_:byte      , _:Convert) = fun x -> byte            x
    static member inline Convert (_:uint16    , _:Convert) = fun x -> uint16          x
    static member inline Convert (_:uint32    , _:Convert) = fun x -> uint32          x
    static member inline Convert (_:uint64    , _:Convert) = fun x -> uint64          x
    static member inline Convert (_:unativeint, _:Convert) = fun x -> unativeint (int x)
    static member inline Convert (_:float     , _:Convert) = fun x -> float           x
    static member inline Convert (_:float32   , _:Convert) = fun x -> float32         x    
    static member inline Convert (_:decimal   , _:Convert) = fun x -> decimal         x
#if NOTNET35
    static member inline Convert (_:Complex   , _:Convert) = fun x -> Complex (float  x, 0.0)
#endif
    static member inline Convert (_:char      , _:Convert) = fun x -> char x
    static member inline Convert (_:string    , _:Convert) = fun x -> string x  // better use our ToString

    static member inline Invoke   value:'T      =
        let inline call_2 (a:^a, b:^b) = ((^a or ^b) : (static member Convert: _*_ -> _) b, a)
        let inline call (a:'a) = fun (x:'x) -> call_2 (a, Unchecked.defaultof<'r>) x :'r
        call Convert.Instance value

type FromBytes() =
    static member val Instance = FromBytes()
    static member FromBytes (_:bool   , _:FromBytes) = fun (x, i, _) -> BitConverter.ToBoolean(x, i)
    static member FromBytes (_:char   , _:FromBytes) = fun (x, i, e) -> BitConverter.ToChar   (x, i, e)
    static member FromBytes (_:float  , _:FromBytes) = fun (x, i, e) -> BitConverter.ToDouble (x, i, e)
    static member FromBytes (_: int16 , _:FromBytes) = fun (x, i, e) -> BitConverter.ToInt16  (x, i, e)
    static member FromBytes (_: int   , _:FromBytes) = fun (x, i, e) -> BitConverter.ToInt32  (x, i, e)
    static member FromBytes (_:int64  , _:FromBytes) = fun (x, i, e) -> BitConverter.ToInt64  (x, i, e)
    static member FromBytes (_:float32, _:FromBytes) = fun (x, i, e) -> BitConverter.ToSingle (x, i, e)
    static member FromBytes (_:string , _:FromBytes) = fun (x, i, _) -> BitConverter.ToString (x, i)
    static member FromBytes (_:uint16 , _:FromBytes) = fun (x, i, e) -> BitConverter.ToUInt16 (x, i, e)
    static member FromBytes (_:uint32 , _:FromBytes) = fun (x, i, e) -> BitConverter.ToUInt32 (x, i, e)
    static member FromBytes (_:uint64 , _:FromBytes) = fun (x, i, e) -> BitConverter.ToUInt64 (x, i, e)

    static member inline Invoke (isLtEndian:bool) (startIndex:int) (value:byte[]) =
        let inline call_2 (a:^a, b:^b) = ((^a or ^b) : (static member FromBytes: _*_ -> _) b, a)
        let inline call (a:'a) = fun (x:'x) -> call_2 (a, Unchecked.defaultof<'r>) x :'r
        call FromBytes.Instance (value, startIndex, isLtEndian)


[<Extension;Sealed>]
type ToBytes() =
    static member val Instance = ToBytes()
    [<Extension>]static member ToBytes (x:bool   , e, _:ToBytes) = BitConverter.GetBytes(x)
    [<Extension>]static member ToBytes (x:char   , e, _:ToBytes) = BitConverter.GetBytes(x, BitConverter.IsLittleEndian = e)
    [<Extension>]static member ToBytes (x:float  , e, _:ToBytes) = BitConverter.GetBytes(x, BitConverter.IsLittleEndian = e)
    [<Extension>]static member ToBytes (x: int16 , e, _:ToBytes) = BitConverter.GetBytes(x, BitConverter.IsLittleEndian = e)
    [<Extension>]static member ToBytes (x: int   , e, _:ToBytes) = BitConverter.GetBytes(x, BitConverter.IsLittleEndian = e)
    [<Extension>]static member ToBytes (x:int64  , e, _:ToBytes) = BitConverter.GetBytes(x, BitConverter.IsLittleEndian = e)
    [<Extension>]static member ToBytes (x:float32, e, _:ToBytes) = BitConverter.GetBytes(x, BitConverter.IsLittleEndian = e)
    [<Extension>]static member ToBytes (x:string , e, _:ToBytes) = Array.map byte (x.ToCharArray())
    [<Extension>]static member ToBytes (x:uint16 , e, _:ToBytes) = BitConverter.GetBytes(x, BitConverter.IsLittleEndian = e)
    [<Extension>]static member ToBytes (x:uint32 , e, _:ToBytes) = BitConverter.GetBytes(x, BitConverter.IsLittleEndian = e)
    [<Extension>]static member ToBytes (x:uint64 , e, _:ToBytes) = BitConverter.GetBytes(x, BitConverter.IsLittleEndian = e)

    static member inline Invoke (isLittleEndian:bool) value :byte[] =
        let inline call_2 (a:^a, b:^b, e) = ((^a or ^b) : (static member ToBytes: _*_*_ -> _) b, e, a)
        let inline call (a:'a, b:'b, e) = call_2 (a, b, e)
        call (ToBytes.Instance, value, isLittleEndian)


open System.Globalization

type TryParse() =
    static member val Instance = TryParse()
    static member inline TryParse (_:'t     option, _:TryParse) = fun x -> 
        let mutable r = Unchecked.defaultof< ^R>
        if (^R: (static member TryParse: _ * _ -> _) (x, &r)) then Some r else None

    static member TryParse (_:string        option, _:TryParse) = fun x -> Some x                             :option<string>
    static member TryParse (_:StringBuilder option, _:TryParse) = fun x -> Some (new StringBuilder(x:string)) :option<StringBuilder>

    static member inline Invoke (value:string) =
        let inline call_2 (a:^a, b:^b) = ((^a or ^b) : (static member TryParse: _*_ -> _) b, a)
        let inline call (a:'a) = fun (x:'x) -> call_2 (a, Unchecked.defaultof<'r>) x :'r
        call TryParse.Instance value


type Parse() =
    inherit Default1()
    static member val Instance = Parse()
    static member inline Parse (_:^R                  , _:Default1) = fun (x:string) -> (^R: (static member Parse: _ -> ^R) x)
    static member inline Parse (_:^R                  , _:Parse   ) = fun (x:string) -> (^R: (static member Parse: _ * _ -> ^R) (x, CultureInfo.InvariantCulture))
#if NOTNET35
    static member        Parse (_:'T when 'T : enum<_>, _:Parse   ) = fun x ->
        (match Enum.TryParse(x) with
            | (true, v) -> v
            | _         -> invalidArg "value" ("Requested value '" + x + "' was not found.")
        ):'enum
#endif
    static member Parse (_:bool         , _:Parse) = fun x -> Boolean.Parse(x)
    static member Parse (_:char         , _:Parse) = fun x -> Char   .Parse(x)
    static member Parse (_:string       , _:Parse) = id :string->_
    static member Parse (_:StringBuilder, _:Parse) = fun x -> new StringBuilder(x:string)

    static member inline Invoke    (value:string) =
        let inline call_2 (a:^a, b:^b) = ((^a or ^b) : (static member Parse: _*_ -> _) b, a)
        let inline call (a:'a) = fun (x:'x) -> call_2 (a, Unchecked.defaultof<'r>) x :'r
        call Parse.Instance value


type ToString() =
    static member val Instance = ToString()
    static member ToString (x:bool          , _:ToString) = fun (k:CultureInfo) -> x.ToString k
    static member ToString (x:char          , _:ToString) = fun (k:CultureInfo) -> x.ToString k
    static member ToString (x:byte          , _:ToString) = fun (k:CultureInfo) -> x.ToString k
    static member ToString (x:sbyte         , _:ToString) = fun (k:CultureInfo) -> x.ToString k
    static member ToString (x:float         , _:ToString) = fun (k:CultureInfo) -> x.ToString k
    static member ToString (x:int16         , _:ToString) = fun (k:CultureInfo) -> x.ToString k
    static member ToString (x:int           , _:ToString) = fun (k:CultureInfo) -> x.ToString k
    static member ToString (x:int64         , _:ToString) = fun (k:CultureInfo) -> x.ToString k
    static member ToString (x:float32       , _:ToString) = fun (k:CultureInfo) -> x.ToString k
    static member ToString (x:string        , _:ToString) = fun (_:CultureInfo) -> if x = null then "null" else x
    static member ToString (x:uint16        , _:ToString) = fun (k:CultureInfo) -> x.ToString k
    static member ToString (x:uint32        , _:ToString) = fun (k:CultureInfo) -> x.ToString k
    static member ToString (x:uint64        , _:ToString) = fun (k:CultureInfo) -> x.ToString k
    static member ToString (x:decimal       , _:ToString) = fun (k:CultureInfo) -> x.ToString k
    static member ToString (x:DateTime      , _:ToString) = fun (k:CultureInfo) -> x.ToString k
    static member ToString (x:DateTimeOffset, _:ToString) = fun (k:CultureInfo) -> x.ToString k
    static member ToString (x:StringBuilder , _:ToString) = fun (_:CultureInfo) -> if x = null then "null" else x.ToString()

    static member inline Invoke (culture:CultureInfo) value : string =
        let inline call_2 (a:^a, b:^b) = ((^a or ^b) : (static member ToString: _*_ -> _) b, a)
        let inline call (a:'a, b:'b) = fun (x:'x) -> call_2 (a, b) x
        call (ToString.Instance, value) culture


type ToString with 
    static member inline ToString (KeyValue(a,b), _:ToString) = fun (k:CultureInfo) -> "(" + ToString.Invoke k a + ", " + ToString.Invoke k b + ")"
    static member inline ToString ((a,b)        , _:ToString) = fun (k:CultureInfo) -> "(" + ToString.Invoke k a + ", " + ToString.Invoke k b + ")"
    static member inline ToString ((a,b,c)      , _:ToString) = fun (k:CultureInfo) -> "(" + ToString.Invoke k a + ", " + ToString.Invoke k b + ", " + ToString.Invoke k c + ")"
    static member inline ToString ((a,b,c,d)    , _:ToString) = fun (k:CultureInfo) -> "(" + ToString.Invoke k a + ", " + ToString.Invoke k b + ", " + ToString.Invoke k c + ", " + ToString.Invoke k d + ")"
    static member inline ToString ((a,b,c,d,e)  , _:ToString) = fun (k:CultureInfo) -> "(" + ToString.Invoke k a + ", " + ToString.Invoke k b + ", " + ToString.Invoke k c + ", " + ToString.Invoke k d + ", " + ToString.Invoke k e + ")"


type ToString with
    static member inline internal seqToString (k:CultureInfo) sepOpen sepClose x (b: StringBuilder) =
        let inline append (s:string) = b.Append s |> ignore
        append sepOpen
        let withSemiColons = Intersperse.intersperse "; " (Seq.map (ToString.Invoke k) x)
        Seq.iter append withSemiColons
        append sepClose
        ToString.Invoke k b

type ToString with static member inline ToString (x:_ list, _:ToString) = fun (k:CultureInfo) ->
                    let b = StringBuilder()
                    ToString.seqToString k "[" "]" x b

type ToString with static member inline ToString (x:_ array, _:ToString) = fun (k:CultureInfo) ->
                    let b = StringBuilder()
                    ToString.seqToString k "[|" "|]" x b

type ToString with 
    static member inline ToString (x:_ ResizeArray, _:ToString) = fun (k:CultureInfo) ->
                    let b = StringBuilder()
                    b.Append "ResizeArray " |> ignore
                    ToString.seqToString k "[" "]" x b
    static member ToString (x:Expr<_>        , _:ToString) = fun (k:CultureInfo) -> x.ToString()

type ToString with static member inline ToString (x:_ seq, _:ToString) = fun (k:CultureInfo) ->
                    let b = StringBuilder()
                    b.Append "seq " |> ignore
                    ToString.seqToString k "[" "]" x b

type ToString with static member inline ToString (x:_ ICollection, _:ToString) = fun (k:CultureInfo) ->
                    ToString.Invoke k (x :> _ seq)

type ToString with static member inline ToString (x:_ IList      , _:ToString) = fun (k:CultureInfo) ->
                    ToString.Invoke k (x :> _ seq)

type ToString with static member inline ToString (x:Map<_,_>     , _:ToString) = fun (k:CultureInfo) ->
                    let b = StringBuilder()
                    b.Append "map " |> ignore
                    ToString.seqToString k "[" "]" x b

type ToString with 
    static member inline ToString (x:Dictionary<_,_>, _:ToString) = fun (k:CultureInfo) ->
                    ToString.Invoke k (x :> seq<KeyValuePair<_,_>>)

    static member inline ToString (x:_ Set, _, _:ToString) = fun (k:CultureInfo) ->
                    let b = StringBuilder()
                    b.Append "set " |> ignore
                    ToString.seqToString k "[" "]" x b

type ToString with static member inline ToString (x:IDictionary<_,_>, _:ToString) = fun (k:CultureInfo) ->
                    ToString.Invoke k (x :> seq<KeyValuePair<_,_>>)

type ToString with static member inline ToString (x:_ option, _:ToString) = fun (k:CultureInfo) ->
                    match x with
                    | Some a -> "Some " + ToString.Invoke k a
                    | None   -> "None"

type ToString with static member inline ToString (x:Choice<_,_>, _:ToString) = fun (k:CultureInfo) ->
                    match x with
                    | Choice1Of2 a -> "Choice1Of2 " + ToString.Invoke k a
                    | Choice2Of2 b -> "Choice2Of2 " + ToString.Invoke k b

type ToString with static member inline ToString (x:Choice<_,_,_>, _:ToString) = fun (k:CultureInfo) ->
                    match x with
                    | Choice1Of3 a -> "Choice1Of3 " + ToString.Invoke k a
                    | Choice2Of3 b -> "Choice2Of3 " + ToString.Invoke k b
                    | Choice3Of3 c -> "Choice3Of3 " + ToString.Invoke k c

type ToString with static member inline ToString (x:Choice<_,_,_,_>, _:ToString) = fun (k:CultureInfo) ->
                    match x with
                    | Choice1Of4 a -> "Choice1Of4 " + ToString.Invoke k a
                    | Choice2Of4 b -> "Choice2Of4 " + ToString.Invoke k b
                    | Choice3Of4 c -> "Choice3Of4 " + ToString.Invoke k c
                    | Choice4Of4 d -> "Choice4Of4 " + ToString.Invoke k d


type ToString with static member inline ToString (x:Choice<_,_,_,_,_>, _:ToString) = fun (k:CultureInfo) ->
                    match x with
                    | Choice1Of5 a -> "Choice1Of5 " + ToString.Invoke k a
                    | Choice2Of5 b -> "Choice2Of5 " + ToString.Invoke k b
                    | Choice3Of5 c -> "Choice3Of5 " + ToString.Invoke k c
                    | Choice4Of5 d -> "Choice4Of5 " + ToString.Invoke k d
                    | Choice5Of5 e -> "Choice5Of5 " + ToString.Invoke k e

type ToString with static member inline ToString (x:Choice<_,_,_,_,_,_>, _:ToString) = fun (k:CultureInfo) ->
                    match x with
                    | Choice1Of6 a -> "Choice1Of6 " + ToString.Invoke k a
                    | Choice2Of6 b -> "Choice2Of6 " + ToString.Invoke k b
                    | Choice3Of6 c -> "Choice3Of6 " + ToString.Invoke k c
                    | Choice4Of6 d -> "Choice4Of6 " + ToString.Invoke k d
                    | Choice5Of6 e -> "Choice5Of6 " + ToString.Invoke k e
                    | Choice6Of6 f -> "Choice6Of6 " + ToString.Invoke k f

type ToString with static member inline ToString (x:Choice<_,_,_,_,_,_,_>, _:ToString) = fun (k:CultureInfo) ->
                    match x with
                    | Choice1Of7 a -> "Choice1Of7 " + ToString.Invoke k a
                    | Choice2Of7 b -> "Choice2Of7 " + ToString.Invoke k b
                    | Choice3Of7 c -> "Choice3Of7 " + ToString.Invoke k c
                    | Choice4Of7 d -> "Choice4Of7 " + ToString.Invoke k d
                    | Choice5Of7 e -> "Choice5Of7 " + ToString.Invoke k e
                    | Choice6Of7 f -> "Choice6Of7 " + ToString.Invoke k f
                    | Choice7Of7 g -> "Choice7Of7 " + ToString.Invoke k g