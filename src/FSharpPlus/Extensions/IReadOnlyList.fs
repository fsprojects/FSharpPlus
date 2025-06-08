namespace FSharpPlus

/// Additional operations on ReadOnlyList<'T>
[<RequireQualifiedAccess>]
module IReadOnlyList =
    open System.Collections.Generic

    #if !FABLE_COMPILER
    
    let ofArray (source: 'T array) = IList.toIReadOnlyList source
    let ofList (source: 'T list) = source |> Array.ofList |> IList.toIReadOnlyList
    let ofSeq  (source: seq<'T>) = source |> Array.ofSeq  |> IList.toIReadOnlyList

    #endif

    let toArray (source: IReadOnlyList<'T>) = Array.ofSeq source

    #if !FABLE_COMPILER
    
    /// Returns a new IReadOnlyList from a given IReadOnlyList, with replaced binding for index.
    let trySetItem i value (source: IReadOnlyList<'T>) =
        let setNth i v (source: _ array) = source.[i] <- v; source
        if 0 <= i && i < source.Count then
            source |> Array.ofSeq |> setNth i value |> ofArray |> Some
        else None
    
    let map mapping (source: IReadOnlyList<'T>) : IReadOnlyList<'U> = Seq.map mapping source |> Seq.toArray |> IList.toIReadOnlyList
    
    #endif

    let tryItem i (source: IReadOnlyList<_>) =
        if 0 <= i && i < source.Count then Some source.[i]
        else None

    /// <summary>Applies the given function to each element of the collection.</summary>
    /// <param name="action">The function to apply to elements from the input list.</param>
    /// <param name="source">The input list.</param>
    let iter action (source: IReadOnlyList<'T>) = Seq.iter action source