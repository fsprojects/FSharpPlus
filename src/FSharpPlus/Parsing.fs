namespace FSharpPlus

open FSharpPlus.Internals

#if !FABLE_COMPILER

[<AutoOpen>]
module Parsing =

    // From https://gist.github.com/gusty/2810b0cc7f8bf89f614facaf121fcaa4

    /// [omit]
    module Internals =

        open System
        open System.Text.RegularExpressions
        open FSharpPlus
        open FSharpPlus.Internals.Prelude

        let formatters = [|"%b"; "%d"; "%i"; "%s"; "%u"; "%x"; "%X"; "%o"; "%e"; "%E"; "%f"; "%F"; "%g"; "%G"; "%M"; "%c"; "%A"|]

        let getGroups (pf: PrintfFormat<_,_,_,_,_>) s =
          let formatStr = replace "%%" "%" pf.Value
          let constants = split formatters formatStr
          let regex = Regex ("^" + String.Join ("(.*?)", constants |> Array.map Regex.Escape) + "$")
          let groups = 
            regex.Match(s).Groups 
            |> Seq.cast<Group> 
            |> Seq.skip 1
          groups
            |> Seq.map (fun g  -> g.Value)
            |> Seq.toArray
        
        type ParseArray =
            static member inline ParseArray (_: 't  , _: obj) = fun (g: string []) -> (parse (g.[0])) : 't

            static member inline Invoke (g: string []) =
                let inline call_2 (a: ^a, b: ^b) = ((^a or ^b) : (static member ParseArray: _*_ -> _) b, a) g
                let inline call (a: 'a) = call_2 (a, Unchecked.defaultof<'r>) : 'r
                call Unchecked.defaultof<ParseArray>

            static member inline ParseArray (t: 't, _: ParseArray) = fun (g: string [])  ->
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

            static member inline ParseArray (_: unit                        , _: ParseArray) = fun (_: string []) -> ()
            static member inline ParseArray (_: Tuple<'t1>                  , _: ParseArray) = fun (g: string []) -> Tuple<_> (parse g.[0]) : Tuple<'t1>
            static member inline ParseArray (_: Id<'t1>                     , _: ParseArray) = fun (g: string []) -> Id<_> (parse g.[0])
            static member inline ParseArray (_: 't1*'t2                     , _: ParseArray) = fun (g: string []) -> parse g.[0], parse g.[1]
            static member inline ParseArray (_: 't1*'t2'*'t3                , _: ParseArray) = fun (g: string []) -> parse g.[0], parse g.[1], parse g.[2]
            static member inline ParseArray (_: 't1*'t2'*'t3*'t4            , _: ParseArray) = fun (g: string []) -> parse g.[0], parse g.[1], parse g.[2], parse g.[3]
            static member inline ParseArray (_: 't1*'t2'*'t3*'t4*'t5        , _: ParseArray) = fun (g: string []) -> parse g.[0], parse g.[1], parse g.[2], parse g.[3], parse g.[4]
            static member inline ParseArray (_: 't1*'t2'*'t3*'t4*'t5*'t6    , _: ParseArray) = fun (g: string []) -> parse g.[0], parse g.[1], parse g.[2], parse g.[3], parse g.[4], parse g.[5]
            static member inline ParseArray (_: 't1*'t2'*'t3*'t4*'t5*'t6*'t7, _: ParseArray) = fun (g: string []) -> parse g.[0], parse g.[1], parse g.[2], parse g.[3], parse g.[4], parse g.[5], parse g.[6]

        let inline tryParseElemAt i (g: string []) =
            if i < Array.length g then tryParse (g.[i])
            else None

        type TryParseArray =
            static member inline TryParseArray (_:'t, _:obj) = fun (g: string []) -> tryParseElemAt 0 g : 't option

            static member inline Invoke (g: string []) =
                let inline call_2 (a: ^a, b: ^b) = ((^a or ^b) : (static member TryParseArray: _*_ -> _) b, a) g
                let inline call (a: 'a) = call_2 (a, Unchecked.defaultof<'r>) : 'r option
                call Unchecked.defaultof<TryParseArray>

            static member inline TryParseArray (t: 't, _: TryParseArray) = fun (g: string [])  ->
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

            static member inline TryParseArray (_: unit                        , _: TryParseArray) = fun (_: string []) -> ()

            static member inline TryParseArray (_: Tuple<'t1>                  , _: TryParseArray) = fun (g: string []) -> Tuple<_> <!> tryParseElemAt 0 g : Tuple<'t1> option
            static member inline TryParseArray (_: Id<'t1>                     , _: TryParseArray) = fun (g: string []) -> Id<_>    <!> tryParseElemAt 0 g
            static member inline TryParseArray (_: 't1*'t2                     , _: TryParseArray) = fun (g: string []) -> tuple2 <!> tryParseElemAt 0 g <*> tryParseElemAt 1 g
            static member inline TryParseArray (_: 't1*'t2'*'t3                , _: TryParseArray) = fun (g: string []) -> tuple3 <!> tryParseElemAt 0 g <*> tryParseElemAt 1 g <*> tryParseElemAt 2 g
            static member inline TryParseArray (_: 't1*'t2'*'t3*'t4            , _: TryParseArray) = fun (g: string []) -> tuple4 <!> tryParseElemAt 0 g <*> tryParseElemAt 1 g <*> tryParseElemAt 2 g <*> tryParseElemAt 3 g
            static member inline TryParseArray (_: 't1*'t2'*'t3*'t4*'t5        , _: TryParseArray) = fun (g: string []) -> tuple5 <!> tryParseElemAt 0 g <*> tryParseElemAt 1 g <*> tryParseElemAt 2 g <*> tryParseElemAt 3 g <*> tryParseElemAt 4 g
            static member inline TryParseArray (_: 't1*'t2'*'t3*'t4*'t5*'t6    , _: TryParseArray) = fun (g: string []) -> tuple6 <!> tryParseElemAt 0 g <*> tryParseElemAt 1 g <*> tryParseElemAt 2 g <*> tryParseElemAt 3 g <*> tryParseElemAt 4 g <*> tryParseElemAt 5 g
            static member inline TryParseArray (_: 't1*'t2'*'t3*'t4*'t5*'t6*'t7, _: TryParseArray) = fun (g: string []) -> tuple7 <!> tryParseElemAt 0 g <*> tryParseElemAt 1 g <*> tryParseElemAt 2 g <*> tryParseElemAt 3 g <*> tryParseElemAt 4 g <*> tryParseElemAt 5 g <*> tryParseElemAt 6 g


    open Internals
    open System

    /// Gets a tuple with the result of parsing each element of a string array.
    let inline parseArray (source: string[]) : '``(T1 * T2 * ... * Tn)`` = ParseArray.Invoke source

    /// Gets a tuple with the result of parsing each element of a formatted text.
    let inline sscanf (pf: PrintfFormat<_,_,_,_,'``(T1 * T2 * ... * Tn)``>) s : '``(T1 * T2 * ... * Tn)`` = getGroups pf s |> parseArray

    /// Gets a tuple with the result of parsing each element of a formatted text from the Console.
    let inline scanfn pf : '``(T1 * T2 * ... * Tn)`` = sscanf pf (Console.ReadLine ())

    /// Gets a tuple with the result of parsing each element of a string array. Returns None in case of failure.
    let inline tryParseArray g : '``(T1 * T2 * ... * Tn)`` option = TryParseArray.Invoke g

    /// Gets a tuple with the result of parsing each element of a formatted text. Returns None in case of failure.
    let inline trySscanf (pf: PrintfFormat<_,_,_,_,'``(T1 * T2 * ... * Tn)``>) s : '``(T1 * T2 * ... * Tn)`` option = getGroups pf s |> tryParseArray

    /// Gets a tuple with the result of parsing each element of a formatted text from the Console. Returns None in case of failure.
    let inline tryScanfn pf : '``(T1 * T2 * ... * Tn)`` option = trySscanf pf (Console.ReadLine ())

#endif