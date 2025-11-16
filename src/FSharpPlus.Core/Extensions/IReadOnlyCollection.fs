namespace FSharpPlus

/// Additional operations on IReadOnlyCollection<'T>
[<RequireQualifiedAccess>]
module IReadOnlyCollection =
    open System.Collections.Generic

    [<GeneralizableValue>]
    let empty<'T> = [||]                                  :> IReadOnlyCollection<'T>
    let ofArray (source: 'T[]   ) = source                :> IReadOnlyCollection<'T>
    let ofList  (source: 'T list) = source                :> IReadOnlyCollection<'T>
    let ofSeq   (source: seq<'T>) = source |> Array.ofSeq :> IReadOnlyCollection<'T>
    let map  mapping (source: IReadOnlyCollection<'T>) = Seq.map  mapping source |> Seq.toArray :> IReadOnlyCollection<'U>
    
    /// <summary>Applies the given function to each element of the collection.</summary>
    /// <param name="action">The function to apply to elements from the input collection.</param>
    /// <param name="source">The input collection.</param>
    let iter action (source: IReadOnlyCollection<'T>) = Seq.iter action source

    let isEmpty (source: IReadOnlyCollection<'T>) = source.Count = 0
