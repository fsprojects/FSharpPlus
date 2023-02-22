﻿namespace FSharpPlus.Data

open System.ComponentModel
open System.Collections.Generic
open FSharpPlus

/// A Map where keys are not unique.
type MultiMap<'Key, 'Value when 'Key : comparison> = private MMap of Map<'Key, 'Value list> with

    interface IEnumerable<KeyValuePair<'Key, 'Value>> with
        member x.GetEnumerator () =
            let (MMap x) = x
            (x |> Map.toSeq |> Seq.collect (fun (k, v) -> v |> List.map (fun v -> KeyValuePair (k, v)))).GetEnumerator ()

    interface System.Collections.IEnumerable with
        member x.GetEnumerator () =
            let (MMap x) = x
            (x |> Map.toSeq |> Seq.collect (fun (k, v) -> v |> List.map (fun v -> KeyValuePair (k, v)))).GetEnumerator () :> System.Collections.IEnumerator

    member x.Item
        with get (key: 'Key) : 'Value list =
            let (MMap x) = x
            match Map.tryFind key x with
            | Some values -> values
            | None        -> []

    static member get_Zero () = MMap (Map.ofList [])
    static member (+) (MMap x, MMap y) = MMap (Map.unionWith (@) x y)


module MultiMap =

    /// Converts a seq of tuples to a multiMap.
    let ofSeq (source: seq<'Key * 'Value>) = source |> Seq.groupBy fst |> Seq.map (fun (k, v) -> k, v |> Seq.map snd |> Seq.toList) |> Map.ofSeq |> MMap

    /// Converts a list of tuples to a multiMap.
    let ofList (source: list<'Key * 'Value>) = source |> ofSeq

    /// Converts an array of tuples to a multiMap.
    let ofArray (source: ('Key * 'Value) []) = source |> ofSeq

    /// Converts a multiMap to a seq of tuples.
    let toSeq (MMap x) = x :> seq<_> |> Seq.collect (fun x -> x.Value |> List.map (fun v -> (x.Key, v)))

    /// Converts a multiMap to a list of tuples.
    let toList source = source |> toSeq |> Seq.toList

    /// Converts a multiMap to an array of tuples.
    let toArray source = source |> toSeq |> Seq.toArray

    /// Returns a new multiMap with the new binding added to the given multiMap.
    let add (key: 'Key) (value: 'Value) source =
        let (MMap x) = source
        let v =
            match Map.tryFind key x with
            | Some values -> value::values
            | None        -> [value]
        MMap (Map.add key v x)

    /// Maps values of the original multiMap.
    let mapValues mapping source = let (MMap x) = source in MMap (Map.mapValues (List.map mapping) x)


type MultiMap<'Key, 'Value when 'Key : comparison> with

    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member ToSeq x = MultiMap.toSeq x

    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member ToList x = MultiMap.toList x

    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member ToArray x = MultiMap.toArray x

    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member OfSeq x = MultiMap.ofSeq x

    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member OfList x = MultiMap.ofList x

    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member OfArray x = MultiMap.ofArray x

    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member Map (x, f) = MultiMap.mapValues f x