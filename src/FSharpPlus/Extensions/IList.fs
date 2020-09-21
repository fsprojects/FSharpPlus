namespace FSharpPlus

#if !FABLE_COMPILER

/// Additional operations IList<'T>
[<RequireQualifiedAccess>]
module IList =

    open System.Collections.ObjectModel
    open System.Collections.Generic

    /// <summary>Converts an IList to an IReadOnlyList from System.Collections.Generic.</summary>
    /// <param name="source">The System.Collections.Generic.IList</param>
    /// <returns>The list converted to a System.Collections.Generic.IReadOnlyList</returns>
    let toIReadOnlyList (source: IList<_>) = ReadOnlyCollection source :> IReadOnlyList<_>

#endif
