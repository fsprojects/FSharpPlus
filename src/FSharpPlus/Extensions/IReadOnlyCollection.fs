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
    let iter mapping (source: IReadOnlyCollection<'T>) = Seq.iter mapping source
    let isEmpty (source: IReadOnlyCollection<'T>) = source.Count = 0
