namespace FSharpPlus

/// Additional operations on String
[<RequireQualifiedAccess>]
module String =
    open System
    open System.Text
    open System.Globalization
    open FSharpPlus.Internals.Errors

    /// Concatenates all elements, using the specified separator between each element.
    let intercalate (separator: string) (source: seq<string>) =
        raiseIfNull (nameof separator) separator
        raiseIfNull (nameof source) source
        
        String.Join (separator, source)

    /// Inserts a separator char between each char in the source string.
    let intersperse (element: char) (source: string) =
        raiseIfNull (nameof source) source
        
        String.Join ("", Array.ofSeq (source |> Seq.intersperse element))

    /// Creates a sequence of strings by splitting the source string on any of the given separators.
    let split (separators: seq<string>) (source: string) =
        raiseIfNull (nameof separators) separators
        raiseIfNull (nameof source) source
        
        source.Split (Seq.toArray separators, StringSplitOptions.None) :> seq<_>

    /// Replaces a substring with the given replacement string.
    let replace (oldValue: string) newValue (source: string) =
        raiseIfNull (nameof oldValue) oldValue
        raiseIfNull (nameof source) source
        
        if oldValue.Length = 0 then source else source.Replace (oldValue, newValue)

    /// Does the source string contain the given subString? -- function wrapper for String.Contains method.
    let isSubString (subString: string) (source: string) =
        raiseIfNull (nameof subString) subString
        raiseIfNull (nameof source) source
        
        source.Contains subString

    #if !FABLE_COMPILER
    
    /// Does the source string start with the given subString? -- function wrapper for String.StartsWith method using InvariantCulture.
    let startsWith (subString: string) (source: string) =
        raiseIfNull (nameof subString) subString
        raiseIfNull (nameof source) source
        
        source.StartsWith (subString, false, CultureInfo.InvariantCulture)
    #endif

    /// Does the source string end with the given subString? -- function wrapper for String.EndsWith method using InvariantCulture.
    let endsWith subString (source: string) =
        raiseIfNull (nameof subString) subString
        raiseIfNull (nameof source) source
        
        source.EndsWith (subString, false, CultureInfo.InvariantCulture)

    /// Does the source string contain the given character?
    /// Use `String.isSubstring` to check for strings.
    let contains char      (source: string) =
        raiseIfNull (nameof source) source
        
        Seq.contains char source

    /// Converts to uppercase -- nullsafe function wrapper for String.ToUpperInvariant method.
    let toUpper (source: string) = 
        raiseIfNull (nameof source) source
        
        if isNull source then source else source.ToUpperInvariant ()

    /// Converts to lowercase -- nullsafe function wrapper for String.ToLowerInvariant method.
    let toLower (source: string) =
        raiseIfNull (nameof source) source
        
        if isNull source then source else source.ToLowerInvariant ()

    /// Trims leading and trailing white spaces -- function wrapper for String.Trim method.
    /// 
    /// Note this is distinct from trim which trims the given characters,
    /// not white spaces.
    let trimWhiteSpaces (source: string) =
        raiseIfNull (nameof source) source
        
        source.Trim ()
    
    /// Trims leading white spaces -- function wrapper for String.TrimStart method.
    /// 
    /// Note this is distinct from trim which trims the given characters,
    /// not white spaces.
    let trimStartWhiteSpaces (source: string) =
        raiseIfNull (nameof source) source
        
        source.TrimStart ()
    
    /// Trims trailing white spaces -- function wrapper for String.TrimEnd method.
    /// 
    /// Note this is distinct from trim which trims the given characters,
    /// not white spaces.
    let trimEndWhiteSpaces (source: string) =
        raiseIfNull (nameof source) source
        
        source.TrimEnd ()

    #if !FABLE_COMPILER
       
    /// Returns a new string whose textual value is the same as this string, but whose binary representation is in the specified Unicode normalization form.
    /// 
    /// This is a null safe function wrapper of the String.Normalize method.
    let normalize normalizationForm (source: string) = if isNull source then source else source.Normalize normalizationForm

    /// Removes diacritics (accents) from the given source string.
    /// 
    /// The approach uses `normalize` to split the input string into constituent glyphs
    /// (basically separating the "base" characters from the diacritics) and then scans
    /// the result and retains only the base characters. 
    let removeDiacritics (source: string) =
        if isNull source then source
        else
            source
            |> normalize NormalizationForm.FormD
            |> String.filter (fun ch -> CharUnicodeInfo.GetUnicodeCategory ch <> UnicodeCategory.NonSpacingMark)
            |> normalize NormalizationForm.FormC
    #endif

    /// Pads the beginning of the given string with spaces so that it has a specified total length.
    let padLeft totalLength (source: string) =
        raiseIfNull (nameof source) source
        
        source.PadLeft totalLength

    /// Pads the beginning of the given string with a specified character so that it has a specified total length.
    let padLeftWith totalLength paddingChar (source: string) =
        raiseIfNull (nameof source) source
        
        source.PadLeft (totalLength, paddingChar)

    /// Pads the end of the given string with spaces so that it has a specified total length.
    let padRight totalLength (source: string) =
        raiseIfNull (nameof source) source
        
        source.PadRight totalLength
    
    /// Pads the end of the given string with a specified character so that it has a specified total length.
    let padRightWith totalLength paddingChar (source: string) =
        raiseIfNull (nameof source) source
        
        source.PadRight (totalLength, paddingChar)

    /// Removes all leading and trailing occurrences of specified characters from the given string.
    let trim      (trimChars: char seq) (source: string) =
        raiseIfNull (nameof source) source
        
        source.Trim (Seq.toArray trimChars)
    
    /// Removes all leading occurrences of specified characters from the given string.
    let trimStart (trimChars: char seq) (source: string) =
        raiseIfNull (nameof source) source
        
        source.TrimStart (Seq.toArray trimChars)

    /// Removes all trailing occurrences of specified characters from the given string.
    let trimEnd   (trimChars: char seq) (source: string) =
        raiseIfNull (nameof source) source
        
        source.TrimEnd (Seq.toArray trimChars)

    /// Converts the given string to an array of chars.
    let toArray (source: string)    =
        raiseIfNull (nameof source) source
        
        source.ToCharArray ()

    /// Converts an array of chars to a String.
    let ofArray (source: char [])   =
        raiseIfNull (nameof source) source
        
        String (source)

    /// Converts the given string to a list of chars.
    let toList  (source: string)    =
        raiseIfNull (nameof source) source
        
        toArray source |> List.ofArray

    /// Converts a list of chars to a String.
    let ofList  (source: char list) = String (source |> Array.ofList)

    /// Converts the given string to a seq of chars.
    let toSeq   (source: string)    =
        raiseIfNull (nameof source) source
        
        source :> seq<char>

    /// Converts a seq of chars to a String.
    let ofSeq   (source: seq<char>) =
        raiseIfNull (nameof source) source
        
        String.Join (String.Empty, source)

    /// (Unsafely) Returns the char at the given index in the source string.
    /// 
    /// This is a function wrapper for `source.[index]` method.
    /// 
    /// Note: this is not exception safe, and will throw System.IndexOutOfRangeException when
    /// the given index is out of bounds.
    let item    (index: int) (source: string) =
        raiseIfNull (nameof source) source
        
        source.[index]

    /// Returns the char (as an Option) at the given index in the source string,
    /// returning `None` if out of bounds.
    let tryItem (index: int) (source: string) =
        raiseIfNull (nameof source) source
        
        if index >= 0 && index < source.Length then Some source.[index] else None

    /// Reverses the given string.
    let rev (source: string) =
        raiseIfNull (nameof source) source
        
        String (source.ToCharArray () |> Array.rev)

    /// (Unsafely) Takes the first count chars in the string.
    /// Use `String.truncate` for a safe version.
    /// 
    /// Note: will throw System.ArgumentOutOfRangeException if you try to take more than the
    /// number of chars in the string.
    let take count (source: string) =
        raiseIfNull (nameof source) source
        
        source[..count-1]

    /// (Unsafely) Skips over the first count chars in the string.
    /// Use `String.drop` for a safe version.
    /// 
    /// Note: will throw System.ArgumentOutOfRangeException if you try to skip more than the
    /// number of chars in the string.
    let skip count (source: string) =
        raiseIfNull (nameof source) source
        
        source[count..]

    /// Takes chars from the source string while the given predicate is true.
    let takeWhile (predicate: char -> bool) (source: string) =
        raiseIfNull (nameof source) source
        
        if String.IsNullOrEmpty source then
            String.Empty
        else
            let mutable i = 0
            let length = String.length source
            while i < length && predicate source.[i] do
                i <- i + 1
            if i = 0 then ""
            else source |> take i

    /// Skips over chars from the source string while the given predicate is true.
    let skipWhile (predicate: char -> bool) (source: string) =
        raiseIfNull (nameof source) source
        
        if String.IsNullOrEmpty source then
            String.Empty
        else
            let mutable i = 0
            let length = String.length source
            while i < length && predicate source.[i] do
                i <- i + 1
            if i = 0 then ""
            else source |> skip i

    /// <summary>Gets the first char of the string, or
    /// <c>None</c> if the string is empty.</summary>
    let tryHead (source: string) =
        raiseIfNull (nameof source) source
        
        if String.length source = 0 then None else Some source.[0]

    /// <summary>Gets the last char of the string, or
    /// <c>None</c> if the string is empty.</summary>
    let tryLast (source: string) =
        raiseIfNull (nameof source) source
        
        let length = String.length source
        if length = 0 then None else Some source.[length-1]

    /// Returns a string that has at most N characters from the beginning of the original string.
    /// It returns the original string if it is shorter than count.
    let truncate count (source: string) =
        raiseIfNull (nameof source) source
        
        if count < 1 then String.Empty
        else if String.length source <= count then source
        else take count source

    /// Returns a string that drops first N characters of the original string.
    /// When count exceeds the length of the string it returns an empty string.
    let drop count (source: string) =
        raiseIfNull (nameof source) source
        
        if count < 1 then source
        else if String.length source <= count then String.Empty
        else skip count source

    /// Finds the first index of the char in the substring which satisfies the given predicate.
    /// 
    /// Note: throws an ArgumentException when not found.
    let findIndex (predicate: char -> bool) (source: string) =
        raiseIfNull (nameof source) source
        
        let rec go index =
            if index >= source.Length then
                ArgumentException("An index satisfying the predicate was not found in the string.") |> raise
            else if predicate source.[index] then index
            else go (index + 1)
        go 0

    /// Tries to find the first index of the char in the substring which satisfies the given predicate.
    let tryFindIndex (predicate: char -> bool) (source: string) =
        raiseIfNull (nameof source) source
        
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
        raiseIfNull (nameof slice) slice
        raiseIfNull (nameof source) source
        
        let index = source.IndexOf slice
        if index = -1 then
            ArgumentException("The specified substring was not found in the string.") |> raise
        else
            index

    /// <summary>
    /// Returns the index of the last occurrence of the specified slice in the source.
    /// </summary>
    /// <exception cref="System.ArgumentException">
    /// Thrown when the slice was not found in the sequence.
    /// </exception>
    /// <returns>
    /// The index of the slice.
    /// </returns>
    let findLastSliceIndex (slice: string) (source: string) =
        let index = source.LastIndexOf slice
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
        raiseIfNull (nameof slice) slice
        raiseIfNull (nameof source) source
        
        let index = source.IndexOf slice
        if index = -1 then None else Some index

    /// <summary>
    /// Returns the index of the last occurrence of the specified slice in the source.
    /// Returns <c>None</c> if not found.
    /// </summary>
    /// <returns>
    /// The index of the slice or <c>None</c>.
    /// </returns>
    let tryFindLastSliceIndex (slice: string) (source: string) =
        let index = source.LastIndexOf slice
        if index = -1 then None else Some index

    #if !FABLE_COMPILER

    /// Converts the given string to an array of Int32 code-points (the actual Unicode Code Point number).
    let toCodePoints (source : string) : seq<int> =
        raiseIfNull (nameof source) source
        
        let mapper i c =
            // Ignore the low-surrogate because it's already been converted
            if c |> Char.IsLowSurrogate then None
            else Char.ConvertToUtf32 (source, i) |> Some
        source |> Seq.mapi mapper |> Seq.choose id

    /// Converts the array of Int32 code-points (the actual Unicode Code Point number) to a string.
    let ofCodePoints (source: seq<int>) : string =
        raiseIfNull (nameof source) source
        
        source |> Seq.map Char.ConvertFromUtf32 |> String.concat String.Empty
    #endif
    
    /// Converts a string to a byte-array using the specified encoding.
    let getBytes (encoding: Encoding) (source: string) : byte [] =
        raiseIfNull (nameof encoding) encoding
        raiseIfNull (nameof source) source
        
        encoding.GetBytes source
