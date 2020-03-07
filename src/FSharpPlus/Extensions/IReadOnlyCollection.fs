namespace FSharpPlus

/// Additional operations on IReadOnlyCollection<'T>
[<RequireQualifiedAccess>]
module IReadOnlyCollection =
    open System.Collections.Generic

    let ofArray (source: 'T[]   ) = source                :> IReadOnlyCollection<'T>
    let ofList  (source: 'T list) = source                :> IReadOnlyCollection<'T>
    let ofSeq   (source: seq<'T>) = source |> Array.ofSeq :> IReadOnlyCollection<'T>
    let map mapping (source: IReadOnlyCollection<'T>) = Seq.map mapping source |> Seq.toArray :> IReadOnlyCollection<'U>