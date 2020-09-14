namespace FSharpPlus

/// Additional operations on Seq
[<RequireQualifiedAccess>]
module Seq =
    open System

    /// <summary>Applies the given function to each element of the sequence and concatenates all the
    /// results.</summary>
    ///
    /// <remarks>Remember sequence is lazy, effects are delayed until it is enumerated.</remarks>
    /// <remarks>This is the same as Seq.collect but the type of the mapping function is not flexible.</remarks>
    ///
    /// <param name="mapping">A function to transform elements of the input sequence into the sequences
    /// that will then be concatenated.</param>
    /// <param name="source">The input sequence.</param>
    ///
    /// <returns>The result sequence.</returns>
    ///
    /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
    let bind (mapping: 'T->seq<'U>) source = Seq.collect mapping source

    /// <summary>Applies a sequence of functions to a sequence of values and concatenates them</summary>
    /// <param name="f">The seq of functions.</param>
    /// <param name="x">The seq of values.</param>
    /// <returns>A seq concatenating the results from applying each function to each value</returns>
    /// 
    /// <example>
    /// <code>
    /// > Seq.apply [double; triple] [1; 2; 3];;  
    /// val it : seq&lt;int&gt; = seq [2; 4; 6; 3; ...]
    /// </code>
    /// </example>
    let apply f x = bind (fun f -> Seq.map ((<|) f) x) f

    let lift2 f x1 x2 = Seq.allPairs x1 x2 |> Seq.map (fun (x, y) -> f x y)

    let foldBack f x z = Array.foldBack f (Seq.toArray x) z

    /// <summary>Applies a key-generating function to each element of a sequence and yields a sequence of 
    /// keys tupled with values. Each key contains an array of all adjacent elements that match 
    /// to this key, therefore keys are not unique but they can't be adjacent
    /// as each time the key changes, a new group is yield.</summary>
    /// 
    /// <remarks>The ordering of the original sequence is respected.</remarks>
    ///
    /// <param name="projection">A function that transforms an element of the sequence into a comparable key.</param>
    /// <param name="source">The input collection.</param>
    ///
    /// <returns>The result sequence.</returns>
    let chunkBy (projection: 'T -> 'Key) (source: _ seq) = seq {
        use e = source.GetEnumerator ()
        if e.MoveNext () then
            let mutable g = projection e.Current
            let mutable members = ResizeArray ()
            members.Add e.Current
            while e.MoveNext () do
                let key = projection e.Current
                if g = key then members.Add e.Current
                else
                    yield g, members
                    g <- key
                    members <- ResizeArray ()
                    members.Add e.Current
            yield g, members }

    /// Inserts a separator element between each element in the source seq.
    // http://codebetter.com/matthewpodwysocki/2009/05/06/functionally-implementing-intersperse/
    let intersperse sep list = seq {
        let mutable notFirst = false
        for element in list do
            if notFirst then yield sep
            yield element
            notFirst <- true }

    /// Inserts a separator between each element in the source sequence.
    let intercalate separator source = seq {
        let mutable notFirst = false
        for element in source do
            if notFirst then yield! separator
            yield! element
            notFirst <- true }

    /// Creates a sequence of sequences by splitting the source sequence on any of the given separators.
    let split separators source =
        let split options = seq {
            match separators |> Seq.map Seq.toList |> Seq.toList with
            | []         -> yield source
            | separators ->
                let buffer = ResizeArray ()
                let candidate = separators |> List.map List.length |> List.max |> ResizeArray
                let mutable i = 0
                for item in source do
                    candidate.Add item
                    match separators |> List.filter (fun sep -> sep.Length > i && item = sep.[i]) with
                    | [] ->
                        i <- 0
                        buffer.AddRange candidate
                        candidate.Clear ()
                    | seps ->
                        if seps |> List.exists (fun sep -> sep.Length = i + 1) then
                            i <- 0
                            if options = StringSplitOptions.None || buffer.Count > 0 then yield buffer.ToArray () :> seq<_>
                            buffer.Clear ()
                            candidate.Clear ()
                        else i <- i + 1
                if candidate.Count > 0 then buffer.AddRange candidate
                if options = StringSplitOptions.None || buffer.Count > 0 then yield buffer :> seq<_> }
        split StringSplitOptions.None

    let replace (oldValue: seq<'T>) (newValue: seq<'T>) (source: seq<'T>) : seq<'T> = seq {
        let old = oldValue |> Seq.toList
        if old.Length = 0 then
            yield! source
        else
            let candidate = ResizeArray old.Length
            let mutable sindex = 0
            for item in source do
                candidate.Add item
                if item = old.[sindex] then
                    sindex <- sindex + 1
                    if sindex >= old.Length then
                        sindex <- 0
                        yield! newValue
                        candidate.Clear ()
                else
                    sindex <- 0
                    yield! candidate
                    candidate.Clear ()
            yield! candidate }

    /// <summary>Returns a sequence that drops N elements of the original sequence and then yields the
    /// remaining elements of the sequence.</summary>
    /// <remarks>When count exceeds the number of elements in the sequence it
    /// returns an empty sequence instead of throwing an exception.</remarks>
    /// <param name="count">The number of items to drop.</param>
    /// <param name="source">The input sequence.</param>
    ///
    /// <returns>The result sequence.</returns>
    let drop i (source: seq<_>) =
        let mutable count = i
        use e = source.GetEnumerator ()
        while (count > 0 && e.MoveNext ()) do count <- count-1
        seq { while e.MoveNext () do yield e.Current }

    #if !FABLE_COMPILER
    
    let replicate count initial = Linq.Enumerable.Repeat (initial, count)
    #endif

    open System.Collections.ObjectModel
    open System.Collections.Generic

    #if !FABLE_COMPILER
    
    let toIReadOnlyList (x: seq<_>) = x |> ResizeArray |> ReadOnlyCollection :> IReadOnlyList<_>

    /// <summary>
    /// Gets the index of the first occurrence of the specified slice in the source.
    /// </summary>
    /// <remarks>
    /// It is assumed that 1) the slice is finite and 2) either the source is finite or actually contains the slice, otherwise it will not return forever.
    /// The slice will always be iterated to the end.
    /// The source will be iterated until the slice is found or it reaches the end.
    /// </remarks>
    /// <exception cref="System.ArgumentException">
    /// Thrown when the slice was not found in the sequence.
    /// </exception>
    /// <returns>
    /// The index of the slice.
    /// </returns>
    let findSliceIndex (slice: seq<_>) (source: seq<_>) =
        let index = Internals.FindSliceIndex.seqImpl slice source
        if index = -1 then
            ArgumentException("The specified slice was not found in the sequence.") |> raise
        else
            index

    /// <summary>
    /// Gets the index of the first occurrence of the specified slice in the source.
    /// Returns <c>None</c> if not found.
    /// </summary>
    /// <remarks>
    /// It is assumed that 1) the slice is finite and 2) either the source is finite or actually contains the slice, otherwise it will not return forever.
    /// The slice will always be iterated to the end.
    /// The source will be iterated until the slice is found or it reaches the end.
    /// </remarks>
    /// <returns>
    /// The index of the slice or <c>None</c>.
    /// </returns>
    let tryFindSliceIndex (slice: seq<_>) (source: seq<_>) =
        let index = Internals.FindSliceIndex.seqImpl slice source
        if index = -1 then None else Some index
    #endif