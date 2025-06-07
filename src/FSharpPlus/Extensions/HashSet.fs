namespace FSharpPlus

/// Additional operations on HashSet<'T>
[<RequireQualifiedAccess>]
module HashSet =
    open System.Collections.Generic
    open System.Collections.ObjectModel


    /// <summary>The empty set for the type 'T.</summary>
    [<GeneralizableValue>]
    [<CompiledName("Empty")>]
    let empty<'T> : HashSet<'T> = HashSet<'T> ()

    /// <summary>The set containing the given element.</summary>
    /// <param name="value">The value for the set to contain.</param>
    /// <returns>The set containing <c>value</c>.</returns>
    [<CompiledName("Singleton")>]
    let singleton (value: 'T) : HashSet<'T> =
        let set =
            #if FABLE_COMPILER
            HashSet<'T> ()
            #else
            HashSet<'T> 1
            #endif
        set.Add value |> ignore
        set

    /// <summary>Computes the union of the two sets.</summary>
    /// <param name="set1">The first input set.</param>
    /// <param name="set2">The second input set.</param>
    /// <returns>The union of <c>set1</c> and <c>set2</c>.</returns>
    [<CompiledName("Union")>]
    let union (x: HashSet<'T>) (y: HashSet<'T>) : HashSet<'T> =
        let union =
            #if FABLE_COMPILER
            HashSet<'T> ()
            #else
            HashSet<'T> (max x.Count y.Count)
            #endif
        for item in x do union.Add item |> ignore
        for item in y do union.Add item |> ignore
        union