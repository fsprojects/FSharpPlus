namespace FSharpPlus

#if !FABLE_COMPILER

[<AutoOpen>]
module Parsing =

    open System
    open System.Text.RegularExpressions
    open FSharpPlus
    open FSharpPlus.Internals
    open FSharpPlus.Internals.Prelude

    let inline private getGroups (pf: PrintfFormat<_,_,_,_,_>) str =
        let format = pf.Value
        let regex = System.Text.StringBuilder "^"
        let mutable groups = FSharp.Core.CompilerServices.ArrayCollector()
        let mutable i = 0
        while i < String.length format do
            match format[i] with
            | '%' ->
                let mutable j = i + 1
                while
                    match format[j] with
                    | ' ' | '+' | '-' | '*' | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' -> true
                    | _ -> false
                    do j <- j + 1
                if format[j] <> '%' then groups.Add format[i..j] // %% does not capture a group
                i <- j
                match format[j] with
                | 'A' | 'O' -> "(.*?)"
                | 'b' -> "([Tt][Rr][Uu][Ee]|[Ff][Aa][Ll][Ss][Ee])"
                | 'B' -> "([01]+)"
                | 'c' -> "(.)"
                | 'd' | 'i' -> "([+-]?[0-9]+)"
                | 'e' | 'E' | 'f' | 'F' | 'g' | 'G' | 'M' -> "([+-]?[0-9]+\.?[0-9]*(?:[eE][+-]?[0-9]+)?)"
                | 'o' -> "([0-7]+)"
                | 'u' -> "([0-9]+)"
                | 's' -> "(.*?)"
                | 'x' | 'X' -> "([0-9a-fA-F]+)"
                | '%' -> "%"
                | x -> failwith $"Unknown specifier: {x}"
                |> regex.Append
            | '\\' | '*' | '+' | '?' | '|' | '{' | '[' | '(' | ')' | '^' | '$' | '.' | '#' | ' ' as escape ->
                regex.Append('\\').Append escape
            | c -> regex.Append c
            |> ignore
            i <- i + 1
        regex.Append '$'
        |> string
        |> Regex
        |> _.Match(str)
        |> _.Groups
        |> Seq.cast<Group>
        |> Seq.skip 1
        |> Seq.map _.Value
        |> Seq.toArray 
        |> Array.zip <| groups.Close()      
       
    let inline private conv (destType: System.Type) (b: int) (s: string) =
        match destType with    
        | t when t = typeof<byte>   -> Convert.ToByte   (s, b) |> box
        | t when t = typeof<uint16> -> Convert.ToUInt16 (s, b) |> box
        | t when t = typeof<uint32> -> Convert.ToUInt32 (s, b) |> box
        | t when t = typeof<uint64> -> Convert.ToUInt64 (s, b) |> box
        | t when t = typeof<sbyte>  -> Convert.ToSByte  (s, b) |> box
        | t when t = typeof<int16>  -> Convert.ToInt16  (s, b) |> box
        | t when t = typeof<int>    -> Convert.ToInt32  (s, b) |> box
        | t when t = typeof<int64>  -> Convert.ToInt64  (s, b) |> box
        | _ -> invalidOp (sprintf "Type conversion from string to type %A with base %i is not supported" destType b)        
        
    let inline private parse (s: string, f: string) : 'r =
        match f with
        | "%B"        -> conv typeof<'r>  2 s |> string |> parse
        | "%o"        -> conv typeof<'r>  8 s |> string |> parse
        | "%x" | "%X" -> conv typeof<'r> 16 s |> string |> parse
        | _ -> parse s

    let inline private tryParse (s: string, f: string) : 'r option =
        match f with
        | "%B"        -> Option.protect (conv typeof<'r>  2) s |> Option.map string |> Option.bind tryParse
        | "%o"        -> Option.protect (conv typeof<'r>  8) s |> Option.map string |> Option.bind tryParse
        | "%x" | "%X" -> Option.protect (conv typeof<'r> 16) s |> Option.map string |> Option.bind tryParse
        | _ -> tryParse s
        
    type ParseArray =
        static member inline ParseArray (_: 't  , _: obj) = fun (g: (string * string) []) -> (parse (g.[0])) : 't

        static member inline Invoke (g: (string * string) []) =
            let inline call_2 (a: ^a, b: ^b) = ((^a or ^b) : (static member ParseArray: _*_ -> _) b, a) g
            let inline call (a: 'a) = call_2 (a, Unchecked.defaultof<'r>) : 'r
            call Unchecked.defaultof<ParseArray>

        static member inline ParseArray (t: 't, _: ParseArray) = fun (g: (string * string) [])  ->
            let _f _ = Constraints.whenNestedTuple t : ('t1*'t2*'t3*'t4*'t5*'t6*'t7*'tr)
            let (t1: 't1) = parse (g.[0])
            let (t2: 't2) = parse (g.[1])
            let (t3: 't3) = parse (g.[2])
            let (t4: 't4) = parse (g.[3])
            let (t5: 't5) = parse (g.[4])
            let (t6: 't6) = parse (g.[5])
            let (t7: 't7) = parse (g.[6])
            let (tr: 'tr) = ParseArray.Invoke (g.[7..])
            Tuple<_,_,_,_,_,_,_,_> (t1, t2, t3, t4, t5, t6, t7, tr) |> retype : 't

        static member inline ParseArray (_: unit                        , _: ParseArray) = fun (_: (string * string) []) -> ()
        static member inline ParseArray (_: Tuple<'t1>                  , _: ParseArray) = fun (g: (string * string) []) -> Tuple<_> (parse g.[0]) : Tuple<'t1>
        static member inline ParseArray (_: Id<'t1>                     , _: ParseArray) = fun (g: (string * string) []) -> Id<_>    (parse g.[0])
        static member inline ParseArray (_: 't1*'t2                     , _: ParseArray) = fun (g: (string * string) []) -> parse g.[0], parse g.[1]
        static member inline ParseArray (_: 't1*'t2'*'t3                , _: ParseArray) = fun (g: (string * string) []) -> parse g.[0], parse g.[1], parse g.[2]
        static member inline ParseArray (_: 't1*'t2'*'t3*'t4            , _: ParseArray) = fun (g: (string * string) []) -> parse g.[0], parse g.[1], parse g.[2], parse g.[3]
        static member inline ParseArray (_: 't1*'t2'*'t3*'t4*'t5        , _: ParseArray) = fun (g: (string * string) []) -> parse g.[0], parse g.[1], parse g.[2], parse g.[3], parse g.[4]
        static member inline ParseArray (_: 't1*'t2'*'t3*'t4*'t5*'t6    , _: ParseArray) = fun (g: (string * string) []) -> parse g.[0], parse g.[1], parse g.[2], parse g.[3], parse g.[4], parse g.[5]
        static member inline ParseArray (_: 't1*'t2'*'t3*'t4*'t5*'t6*'t7, _: ParseArray) = fun (g: (string * string) []) -> parse g.[0], parse g.[1], parse g.[2], parse g.[3], parse g.[4], parse g.[5], parse g.[6]

    let inline private tryParseElemAt i (g: (string * string) []) =
        if i < Array.length g then tryParse (g.[i])
        else None

    type TryParseArray =
        static member inline TryParseArray (_:'t, _:obj) = fun (g: (string * string) []) -> tryParseElemAt 0 g : 't option

        static member inline Invoke (g: (string * string) []) =
            let inline call_2 (a: ^a, b: ^b) = ((^a or ^b) : (static member TryParseArray: _*_ -> _) b, a) g
            let inline call (a: 'a) = call_2 (a, Unchecked.defaultof<'r>) : 'r option
            call Unchecked.defaultof<TryParseArray>

        static member inline TryParseArray (t: 't, _: TryParseArray) = fun (g: (string * string) [])  ->
            let _f _ = Constraints.whenNestedTuple t : ('t1*'t2*'t3*'t4*'t5*'t6*'t7*'tr)
            let (t1: 't1 option) = tryParseElemAt 0 g
            let (t2: 't2 option) = tryParseElemAt 1 g
            let (t3: 't3 option) = tryParseElemAt 2 g
            let (t4: 't4 option) = tryParseElemAt 3 g
            let (t5: 't5 option) = tryParseElemAt 4 g
            let (t6: 't6 option) = tryParseElemAt 5 g
            let (t7: 't7 option) = tryParseElemAt 6 g
            let (tr: 'tr option) = if g.Length > 7 then TryParseArray.Invoke (g.[7..]) else None
            match t1, t2, t3, t4, t5, t6, t7, tr with
            |  Some t1, Some t2, Some t3, Some t4, Some t5, Some t6, Some t7, Some tr -> Some (Tuple<_,_,_,_,_,_,_,_> (t1, t2, t3, t4, t5, t6, t7, tr) |> retype : 't)
            | _ -> None

        static member inline TryParseArray (_: unit                        , _: TryParseArray) = fun (_: (string * string) []) -> ()
        static member inline TryParseArray (_: Tuple<'t1>                  , _: TryParseArray) = fun (g: (string * string) []) -> Tuple<_> <!> tryParseElemAt 0 g : Tuple<'t1> option
        static member inline TryParseArray (_: Id<'t1>                     , _: TryParseArray) = fun (g: (string * string) []) -> Id<_>    <!> tryParseElemAt 0 g
        static member inline TryParseArray (_: 't1*'t2                     , _: TryParseArray) = fun (g: (string * string) []) -> tuple2 <!> tryParseElemAt 0 g <*> tryParseElemAt 1 g
        static member inline TryParseArray (_: 't1*'t2'*'t3                , _: TryParseArray) = fun (g: (string * string) []) -> tuple3 <!> tryParseElemAt 0 g <*> tryParseElemAt 1 g <*> tryParseElemAt 2 g
        static member inline TryParseArray (_: 't1*'t2'*'t3*'t4            , _: TryParseArray) = fun (g: (string * string) []) -> tuple4 <!> tryParseElemAt 0 g <*> tryParseElemAt 1 g <*> tryParseElemAt 2 g <*> tryParseElemAt 3 g
        static member inline TryParseArray (_: 't1*'t2'*'t3*'t4*'t5        , _: TryParseArray) = fun (g: (string * string) []) -> tuple5 <!> tryParseElemAt 0 g <*> tryParseElemAt 1 g <*> tryParseElemAt 2 g <*> tryParseElemAt 3 g <*> tryParseElemAt 4 g
        static member inline TryParseArray (_: 't1*'t2'*'t3*'t4*'t5*'t6    , _: TryParseArray) = fun (g: (string * string) []) -> tuple6 <!> tryParseElemAt 0 g <*> tryParseElemAt 1 g <*> tryParseElemAt 2 g <*> tryParseElemAt 3 g <*> tryParseElemAt 4 g <*> tryParseElemAt 5 g
        static member inline TryParseArray (_: 't1*'t2'*'t3*'t4*'t5*'t6*'t7, _: TryParseArray) = fun (g: (string * string) []) -> tuple7 <!> tryParseElemAt 0 g <*> tryParseElemAt 1 g <*> tryParseElemAt 2 g <*> tryParseElemAt 3 g <*> tryParseElemAt 4 g <*> tryParseElemAt 5 g <*> tryParseElemAt 6 g


    /// Gets a tuple with the result of parsing each element of a string array.
    let inline parseArray (source: string []) : '``(T1 * T2 * ... * Tn)`` = ParseArray.Invoke (Array.map (fun x -> (x, "")) source)

    /// Gets a tuple with the result of parsing each element of a formatted text.
    let inline sscanf (pf: PrintfFormat<_,_,_,_,'``(T1 * T2 * ... * Tn)``>) s : '``(T1 * T2 * ... * Tn)`` = getGroups pf s |> ParseArray.Invoke

    /// Gets a tuple with the result of parsing each element of a formatted text from the Console.
    let inline scanfn pf : '``(T1 * T2 * ... * Tn)`` = sscanf pf (Console.ReadLine ())

    /// Gets a tuple with the result of parsing each element of a string array. Returns None in case of failure.
    let inline tryParseArray (source: string []) : '``(T1 * T2 * ... * Tn)`` option = TryParseArray.Invoke (Array.map (fun x -> (x, "")) source)

    /// Gets a tuple with the result of parsing each element of a formatted text. Returns None in case of failure.
    let inline trySscanf (pf: PrintfFormat<_,_,_,_,'``(T1 * T2 * ... * Tn)``>) s : '``(T1 * T2 * ... * Tn)`` option = getGroups pf s |> TryParseArray.Invoke

    /// Gets a tuple with the result of parsing each element of a formatted text from the Console. Returns None in case of failure.
    let inline tryScanfn pf : '``(T1 * T2 * ... * Tn)`` option = trySscanf pf (Console.ReadLine ())

#endif
