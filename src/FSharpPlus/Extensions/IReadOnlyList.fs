namespace FSharpPlus

/// Additional operations on ReadOnlyList<'T>
[<RequireQualifiedAccess>]
module IReadOnlyList =
    open System.Collections.Generic

    #if !FABLE_COMPILER
    
    let ofArray (source: 'T array) = IList.toIReadOnlyList source
    #endif
    let toArray (source: IReadOnlyList<'T>) = Array.ofSeq source

    #if !FABLE_COMPILER
    
    /// Returns a new IReadOnlyList from a given IReadOnlyList, with replaced binding for index.
    let trySetItem i value (source: IReadOnlyList<'T>) =
        let setNth i v (source: _ array) = source.[i] <- v; source
        if 0 <= i && i < source.Count then
            source |> Array.ofSeq |> setNth i value |> ofArray |> Some
        else None
    #endif

    let tryItem i (source: IReadOnlyList<_>) =
        if 0 <= i && i < source.Count then Some source.[i]
        else None