namespace FSharpPlus


/// Additional operations IList<'T>
[<RequireQualifiedAccess>]
module IList =

    open System.Collections.ObjectModel
    open System.Collections.Generic

#if !FABLE_COMPILER
    /// <summary>Converts an IList to an IReadOnlyList (from System.Collections.Generic).</summary>
    /// <param name="source">The System.Collections.Generic.IList</param>
    /// <returns>The list converted to a System.Collections.Generic.IReadOnlyList</returns>
    let toIReadOnlyList (source: IList<_>) = ReadOnlyCollection source :> IReadOnlyList<_>

#endif

    let ofArray (source: 'T[]   ) = source                 :> IList<'T>
    let ofList  (source: 'T list) = source |> Array.ofList :> IList<'T>
    let ofSeq   (source: seq<'T>) = source |> Array.ofSeq  :> IList<'T>
    let map  mapping (source: IList<'T>) = Seq.map  mapping source |> Seq.toArray :> IList<'U>
    let iter mapping (source: IList<'T>) = Seq.iter mapping source

