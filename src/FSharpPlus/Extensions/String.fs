namespace FSharpPlus

/// Additional operations on String
[<RequireQualifiedAccess>]
module String =
    open System
    open System.Text
    open System.Globalization

    /// Concatenates all elements, using the specified separator between each element.
    let intercalate (separator: string) (source: seq<string>) = String.Join (separator, source)

    /// Inserts a separator between each char in the source string.
    let intersperse (element: char) (source: string) = String.Join ("", Array.ofSeq (source |> Seq.intersperse element))

    /// Creates a sequence of strings by splitting the srouce string on any of the given separators.
    let split (separators: seq<string>) (source: string) = source.Split (Seq.toArray separators, StringSplitOptions.None) :> seq<_>

    /// Replace a substring with the given replacement string.
    let replace (oldValue: string) newValue (source: string) = if oldValue.Length = 0 then source else source.Replace (oldValue, newValue)

    let isSubString (subString: string) (source: string) = source.Contains subString

    #if !FABLE_COMPILER
    
    let startsWith (subString: string) (source: string) = source.StartsWith (subString, false, CultureInfo.InvariantCulture)
    #endif
    let endsWith subString (source: string) = source.EndsWith (subString, false, CultureInfo.InvariantCulture)
    let contains char      (source: string) = Seq.contains char source
    let toUpper (source: string) = if isNull source then source else source.ToUpperInvariant ()
    let toLower (source: string) = if isNull source then source else source.ToLowerInvariant ()
    let trimWhiteSpaces (source: string) = source.Trim ()

    #if !FABLE_COMPILER
    
    let normalize normalizationForm (source: string) = if isNull source then source else source.Normalize normalizationForm
    let removeDiacritics (source: string) =
        if isNull source then source
        else
            source
            |> normalize NormalizationForm.FormD
            |> String.filter (fun ch -> CharUnicodeInfo.GetUnicodeCategory ch <> UnicodeCategory.NonSpacingMark)
            |> normalize NormalizationForm.FormC
    #endif

    /// Pads the beginning of the given string with spaces so that it has a specified total length.
    let padLeft totalLength (source: string) = source.PadLeft totalLength
    /// Pads the beginning of the given string with a specified character so that it has a specified total length.
    let padLeftWith totalLength paddingChar (source: string) = source.PadLeft (totalLength, paddingChar)
    /// Pads the end of the given string with spaces so that it has a specified total length.
    let padRight totalLength (source: string) = source.PadRight totalLength
    /// Pads the end of the given string with a specified character so that it has a specified total length.
    let padRightWith totalLength paddingChar (source: string) = source.PadRight (totalLength, paddingChar)

    /// Removes all leading and trailing occurrences of specified characters from the given string.
    let trim      (trimChars: char seq) (source: string) = source.Trim (Seq.toArray trimChars)
    /// Removes all leading occurrences of specified characters from the given string.
    let trimStart (trimChars: char seq) (source: string) = source.TrimStart (Seq.toArray trimChars)
    /// Removes all trailing occurrences of specified characters from the given string.
    let trimEnd   (trimChars: char seq) (source: string) = source.TrimEnd (Seq.toArray trimChars)

    let toArray (source: string)    = source.ToCharArray ()
    let ofArray (source: char [])   = new String (source)
    let toList  (source: string)    = toArray source |> List.ofArray
    let ofList  (source: char list) = new String (source |> Array.ofList)
    let toSeq   (source: string)    = source :> seq<char>
    let ofSeq   (source: seq<char>) = String.Join (String.Empty, source)

    let item    (index: int) (source: string) = source.[index]
    let tryItem (index: int) (source: string) = if index >= 0 && index < source.Length then Some source.[index] else None

    let rev (source: string) = new String (source.ToCharArray () |> Array.rev)

    let take count (source: string) = source.[..count-1]
    let skip count (source: string) = source.[count..]
    let takeWhile (predicate: char -> bool) (source: string) =
        if String.IsNullOrEmpty source then
            String.Empty
        else
            let mutable i = 0
            let length = String.length source
            while i < length && predicate source.[i] do
                i <- i + 1
            if i = 0 then ""
            else source |> take i
    let skipWhile (predicate: char -> bool) (source: string) =
        if String.IsNullOrEmpty source then
            String.Empty
        else
            let mutable i = 0
            let length = String.length source
            while i < length && predicate source.[i] do
                i <- i + 1
            if i = 0 then ""
            else source |> skip i
    /// Returns a string that have at most N characters from the beginning of the original string.
    /// It returns the original string if it is shorter than count.
    let truncate count (source: string) =
        if count < 1 then String.Empty
        else if String.length source <= count then source
        else take count source
    /// Returns a string that drops first N characters of the original string.
    /// When count exceeds the length of the string it returns an empty string.
    let drop count (source: string) =
        if count < 1 then source
        else if String.length source >= count then String.Empty
        else skip count source

    let findIndex (predicate: char -> bool) (source: string) =
        let rec go index =
            if index >= source.Length then
                ArgumentException("An index satisfying the predicate was not found in the string.") |> raise
            else if predicate source.[index] then index
            else go (index + 1)
        go 0
    let tryFindIndex (predicate: char -> bool) (source: string) =
        let rec go index =
            if index >= source.Length then None
            else if predicate source.[index] then Some index
            else go (index + 1)
        go 0

    /// <summary>
    /// Returns the index of the first occurrence of the specified slice in the source.
    /// </summary>
    /// <exception cref="System.ArgumentException">
    /// Thrown when the slice was not found in the sequence.
    /// </exception>
    /// <returns>
    /// The index of the slice.
    /// </returns>
    let findSliceIndex (slice: string) (source: string) =
        let index = source.IndexOf slice
        if index = -1 then
            ArgumentException("The specified substring was not found in the string.") |> raise
        else
            index
    /// <summary>
    /// Returns the index of the first occurrence of the specified slice in the source.
    /// Returns <c>None</c> if not found.
    /// </summary>
    /// <returns>
    /// The index of the slice or <c>None</c>.
    /// </returns>
    let tryFindSliceIndex (slice: string) (source: string) =
        let index = source.IndexOf slice
        if index = -1 then None else Some index

    #if !FABLE_COMPILER

    /// Converts the string to an array of Int32 code-points (the actual Unicode Code Point number).
    let toCodePoints (source : string) : seq<int> =
        let mapper i c =
            // Ignore the low-surrogate because it's already been converted
            if c |> Char.IsLowSurrogate then None
            else Char.ConvertToUtf32 (source, i) |> Some
        source |> Seq.mapi mapper |> Seq.choose id
    /// Converts the array of Int32 code-points (the actual Unicode Code Point number) to a string.
    let ofCodePoints (source: seq<int>) : string =
        source |> Seq.map Char.ConvertFromUtf32 |> String.concat String.Empty
    #endif
    
    /// Converts a string to a byte-array using the specified encoding.
    let getBytes (encoding: System.Text.Encoding) (source: string) : byte [] = encoding.GetBytes source