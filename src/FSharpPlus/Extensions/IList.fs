namespace FSharpPlus

#if !FABLE_COMPILER

/// Additional operations IList<'T>
[<RequireQualifiedAccess>]
module IList =

    open System.Collections.ObjectModel
    open System.Collections.Generic

    let toIReadOnlyList (source: IList<_>) = ReadOnlyCollection source :> IReadOnlyList<_>

#endif