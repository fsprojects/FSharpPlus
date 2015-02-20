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


type Convert() =
    static member val Instance = Convert()
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

    static member inline Invoke   value:'T      =
        let inline call_2 (a:^a, b:^b) = ((^a or ^b) : (static member Convert: _*_ -> _) a, b)
        let inline call (a:'a) = fun (x:'x) -> call_2 (a, Unchecked.defaultof<'r>) x :'r
        call Convert.Instance value

type FromBytes() =
    static member val Instance = FromBytes()
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

    static member inline Invoke (isLtEndian:bool) (startIndex:int) (value:byte[]) =
        let inline call_2 (a:^a, b:^b) = ((^a or ^b) : (static member FromBytes: _*_ -> _) a, b)
        let inline call (a:'a) = fun (x:'x) -> call_2 (a, Unchecked.defaultof<'r>) x :'r
        call FromBytes.Instance (value, startIndex, isLtEndian)

type ToBytes() =
    static member val Instance = ToBytes()
    static member ToBytes (_:ToBytes, x:bool   ) = fun _ -> BitConverter.GetBytes(x)
    static member ToBytes (_:ToBytes, x:char   ) = fun e -> BitConverter.GetBytes(x, BitConverter.IsLittleEndian = e)
    static member ToBytes (_:ToBytes, x:float  ) = fun e -> BitConverter.GetBytes(x, BitConverter.IsLittleEndian = e)
    static member ToBytes (_:ToBytes, x: int16 ) = fun e -> BitConverter.GetBytes(x, BitConverter.IsLittleEndian = e)
    static member ToBytes (_:ToBytes, x: int   ) = fun e -> BitConverter.GetBytes(x, BitConverter.IsLittleEndian = e)
    static member ToBytes (_:ToBytes, x:int64  ) = fun e -> BitConverter.GetBytes(x, BitConverter.IsLittleEndian = e)
    static member ToBytes (_:ToBytes, x:float32) = fun e -> BitConverter.GetBytes(x, BitConverter.IsLittleEndian = e)
    static member ToBytes (_:ToBytes, x:string ) = fun e -> Array.map byte (x.ToCharArray())
    static member ToBytes (_:ToBytes, x:uint16 ) = fun e -> BitConverter.GetBytes(x, BitConverter.IsLittleEndian = e)
    static member ToBytes (_:ToBytes, x:uint32 ) = fun e -> BitConverter.GetBytes(x, BitConverter.IsLittleEndian = e)
    static member ToBytes (_:ToBytes, x:uint64 ) = fun e -> BitConverter.GetBytes(x, BitConverter.IsLittleEndian = e)

    static member inline Invoke (isLittleEndian:bool) value :byte[] =
        let inline call_2 (a:^a, b:^b) = ((^a or ^b) : (static member ToBytes: _*_ -> _) a, b)
        let inline call (a:'a, b:'b) = fun (x:'x) -> call_2 (a, b) x
        call (ToBytes.Instance, value) isLittleEndian


open System.Globalization

type TryParse() =
    static member val Instance = TryParse()
    static member inline TryParse (_:TryParse, _:'t     option) = fun x -> 
        let mutable r = Unchecked.defaultof< ^R>
        if (^R: (static member TryParse: _ * _ -> _) (x, &r)) then Some r else None

    static member TryParse (_:TryParse, _:string        option) = fun x -> Some x                             :option<string>
    static member TryParse (_:TryParse, _:StringBuilder option) = fun x -> Some (new StringBuilder(x:string)) :option<StringBuilder>

    static member inline Invoke (value:string) =
        let inline call_2 (a:^a, b:^b) = ((^a or ^b) : (static member TryParse: _*_ -> _) a, b)
        let inline call (a:'a) = fun (x:'x) -> call_2 (a, Unchecked.defaultof<'r>) x :'r
        call TryParse.Instance value


type Parse() =
    inherit Default1()
    static member val Instance = Parse()
    static member inline Parse (_:Default1, _:^R) = fun (x:string) -> (^R: (static member Parse: _ -> ^R) x)
    static member inline Parse (_:Parse   , _:^R) = fun (x:string) -> (^R: (static member Parse: _ * _ -> ^R) (x, CultureInfo.InvariantCulture))
#if NOTNET35
    static member        Parse (_:Parse   , _:'T when 'T : enum<_>) = fun x ->
        (match Enum.TryParse(x) with
            | (true, v) -> v
            | _         -> invalidArg "value" ("Requested value '" + x + "' was not found.")
        ):'enum
#endif
    static member Parse (_:Parse, _:bool         ) = fun x -> Boolean.Parse(x)
    static member Parse (_:Parse, _:char         ) = fun x -> Char   .Parse(x)
    static member Parse (_:Parse, _:string       ) = id :string->_
    static member Parse (_:Parse, _:StringBuilder) = fun x -> new StringBuilder(x:string)

    static member inline Invoke    (value:string) =
        let inline call_2 (a:^a, b:^b) = ((^a or ^b) : (static member Parse: _*_ -> _) a, b)
        let inline call (a:'a) = fun (x:'x) -> call_2 (a, Unchecked.defaultof<'r>) x :'r
        call Parse.Instance value

type ToString() =
    static member val Instance = ToString()
    static member ToString (_:ToString, x:bool          ) = fun (k:CultureInfo) -> x.ToString k
    static member ToString (_:ToString, x:char          ) = fun (k:CultureInfo) -> x.ToString k
    static member ToString (_:ToString, x:byte          ) = fun (k:CultureInfo) -> x.ToString k
    static member ToString (_:ToString, x:sbyte         ) = fun (k:CultureInfo) -> x.ToString k
    static member ToString (_:ToString, x:float         ) = fun (k:CultureInfo) -> x.ToString k
    static member ToString (_:ToString, x:int16         ) = fun (k:CultureInfo) -> x.ToString k
    static member ToString (_:ToString, x:int           ) = fun (k:CultureInfo) -> x.ToString k
    static member ToString (_:ToString, x:int64         ) = fun (k:CultureInfo) -> x.ToString k
    static member ToString (_:ToString, x:float32       ) = fun (k:CultureInfo) -> x.ToString k
    static member ToString (_:ToString, x:string        ) = fun (_:CultureInfo) -> if x = null then "null" else x
    static member ToString (_:ToString, x:uint16        ) = fun (k:CultureInfo) -> x.ToString k
    static member ToString (_:ToString, x:uint32        ) = fun (k:CultureInfo) -> x.ToString k
    static member ToString (_:ToString, x:uint64        ) = fun (k:CultureInfo) -> x.ToString k
    static member ToString (_:ToString, x:decimal       ) = fun (k:CultureInfo) -> x.ToString k
    static member ToString (_:ToString, x:DateTime      ) = fun (k:CultureInfo) -> x.ToString k
    static member ToString (_:ToString, x:DateTimeOffset) = fun (k:CultureInfo) -> x.ToString k
    static member ToString (_:ToString, x:StringBuilder ) = fun (_:CultureInfo) -> if x = null then "null" else x.ToString()

    static member inline Invoke (culture:CultureInfo) value : string =
        let inline call_2 (a:^a, b:^b) = ((^a or ^b) : (static member ToString: _*_ -> _) a, b)
        let inline call (a:'a, b:'b) = fun (x:'x) -> call_2 (a, b) x
        call (ToString.Instance, value) culture


type ToString with 
    static member inline ToString (_:ToString, KeyValue(a,b)) = fun (k:CultureInfo) -> "(" + ToString.Invoke k a + ", " + ToString.Invoke k b + ")"
    static member inline ToString (_:ToString, (a,b)        ) = fun (k:CultureInfo) -> "(" + ToString.Invoke k a + ", " + ToString.Invoke k b + ")"
    static member inline ToString (_:ToString, (a,b,c)      ) = fun (k:CultureInfo) -> "(" + ToString.Invoke k a + ", " + ToString.Invoke k b + ", " + ToString.Invoke k c + ")"
    static member inline ToString (_:ToString, (a,b,c,d)    ) = fun (k:CultureInfo) -> "(" + ToString.Invoke k a + ", " + ToString.Invoke k b + ", " + ToString.Invoke k c + ", " + ToString.Invoke k d + ")"
    static member inline ToString (_:ToString, (a,b,c,d,e)  ) = fun (k:CultureInfo) -> "(" + ToString.Invoke k a + ", " + ToString.Invoke k b + ", " + ToString.Invoke k c + ", " + ToString.Invoke k d + ", " + ToString.Invoke k e + ")"


type ToString with
    static member inline internal seqToString (k:CultureInfo) sepOpen sepClose x (b: StringBuilder) =
        let inline append (s:string) = b.Append s |> ignore
        append sepOpen
        let withSemiColons = Intersperse.intersperse "; " (Seq.map (ToString.Invoke k) x)
        Seq.iter append withSemiColons
        append sepClose
        ToString.Invoke k b

type ToString with static member inline ToString (_:ToString, x:_ list) = fun (k:CultureInfo) ->
                    let b = StringBuilder()
                    ToString.seqToString k "[" "]" x b

type ToString with static member inline ToString (_:ToString, x:_ array) = fun (k:CultureInfo) ->
                    let b = StringBuilder()
                    ToString.seqToString k "[|" "|]" x b

type ToString with 
    static member inline ToString (_:ToString, x:_ ResizeArray) = fun (k:CultureInfo) ->
                    let b = StringBuilder()
                    b.Append "ResizeArray " |> ignore
                    ToString.seqToString k "[" "]" x b
    static member ToString (_:ToString, x:Expr<_>        ) = fun (k:CultureInfo) -> x.ToString()

type ToString with static member inline ToString (_:ToString, x:_ seq) = fun (k:CultureInfo) ->
                    let b = StringBuilder()
                    b.Append "seq " |> ignore
                    ToString.seqToString k "[" "]" x b

type ToString with static member inline ToString (_:ToString, x:_ ICollection) = fun (k:CultureInfo) ->
                    ToString.Invoke k (x :> _ seq)

type ToString with static member inline ToString (_:ToString, x:_ IList      ) = fun (k:CultureInfo) ->
                    ToString.Invoke k (x :> _ seq)

type ToString with static member inline ToString (_:ToString, x:Map<_,_>     ) = fun (k:CultureInfo) ->
                    let b = StringBuilder()
                    b.Append "map " |> ignore
                    ToString.seqToString k "[" "]" x b

type ToString with 
    static member inline ToString (_:ToString, x:Dictionary<_,_>) = fun (k:CultureInfo) ->
                    ToString.Invoke k (x :> seq<KeyValuePair<_,_>>)

    static member inline ToString (_:ToString, x:_ Set, _) = fun (k:CultureInfo) ->
                    let b = StringBuilder()
                    b.Append "set " |> ignore
                    ToString.seqToString k "[" "]" x b

type ToString with static member inline ToString (_:ToString, x:IDictionary<_,_>) = fun (k:CultureInfo) ->
                    ToString.Invoke k (x :> seq<KeyValuePair<_,_>>)

type ToString with static member inline ToString (_:ToString, x:_ option) = fun (k:CultureInfo) ->
                    match x with
                    | Some a -> "Some " + ToString.Invoke k a
                    | None   -> "None"

type ToString with static member inline ToString (_:ToString, x:Choice<_,_>) = fun (k:CultureInfo) ->
                    match x with
                    | Choice1Of2 a -> "Choice1Of2 " + ToString.Invoke k a
                    | Choice2Of2 b -> "Choice2Of2 " + ToString.Invoke k b

type ToString with static member inline ToString (_:ToString, x:Choice<_,_,_>) = fun (k:CultureInfo) ->
                    match x with
                    | Choice1Of3 a -> "Choice1Of3 " + ToString.Invoke k a
                    | Choice2Of3 b -> "Choice2Of3 " + ToString.Invoke k b
                    | Choice3Of3 c -> "Choice3Of3 " + ToString.Invoke k c

type ToString with static member inline ToString (_:ToString, x:Choice<_,_,_,_>) = fun (k:CultureInfo) ->
                    match x with
                    | Choice1Of4 a -> "Choice1Of4 " + ToString.Invoke k a
                    | Choice2Of4 b -> "Choice2Of4 " + ToString.Invoke k b
                    | Choice3Of4 c -> "Choice3Of4 " + ToString.Invoke k c
                    | Choice4Of4 d -> "Choice4Of4 " + ToString.Invoke k d


type ToString with static member inline ToString (_:ToString, x:Choice<_,_,_,_,_>) = fun (k:CultureInfo) ->
                    match x with
                    | Choice1Of5 a -> "Choice1Of5 " + ToString.Invoke k a
                    | Choice2Of5 b -> "Choice2Of5 " + ToString.Invoke k b
                    | Choice3Of5 c -> "Choice3Of5 " + ToString.Invoke k c
                    | Choice4Of5 d -> "Choice4Of5 " + ToString.Invoke k d
                    | Choice5Of5 e -> "Choice5Of5 " + ToString.Invoke k e

type ToString with static member inline ToString (_:ToString, x:Choice<_,_,_,_,_,_>) = fun (k:CultureInfo) ->
                    match x with
                    | Choice1Of6 a -> "Choice1Of6 " + ToString.Invoke k a
                    | Choice2Of6 b -> "Choice2Of6 " + ToString.Invoke k b
                    | Choice3Of6 c -> "Choice3Of6 " + ToString.Invoke k c
                    | Choice4Of6 d -> "Choice4Of6 " + ToString.Invoke k d
                    | Choice5Of6 e -> "Choice5Of6 " + ToString.Invoke k e
                    | Choice6Of6 f -> "Choice6Of6 " + ToString.Invoke k f

type ToString with static member inline ToString (_:ToString, x:Choice<_,_,_,_,_,_,_>) = fun (k:CultureInfo) ->
                    match x with
                    | Choice1Of7 a -> "Choice1Of7 " + ToString.Invoke k a
                    | Choice2Of7 b -> "Choice2Of7 " + ToString.Invoke k b
                    | Choice3Of7 c -> "Choice3Of7 " + ToString.Invoke k c
                    | Choice4Of7 d -> "Choice4Of7 " + ToString.Invoke k d
                    | Choice5Of7 e -> "Choice5Of7 " + ToString.Invoke k e
                    | Choice6Of7 f -> "Choice6Of7 " + ToString.Invoke k f
                    | Choice7Of7 g -> "Choice7Of7 " + ToString.Invoke k g