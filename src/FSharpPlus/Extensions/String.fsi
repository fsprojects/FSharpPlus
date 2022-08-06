namespace FSharpPlus
    
    /// Additional operations on String
    module String =
        
        /// Concatenates all elements, using the specified separator between each element.
        val intercalate: separator: string -> source: seq<string> -> string
        
        /// Inserts a separator char between each char in the source string.
        val intersperse: element: char -> source: string -> string
        
        /// Creates a sequence of strings by splitting the source string on any of the given separators.
        val split: separators: seq<string> -> source: string -> seq<string>
        
        /// Replaces a substring with the given replacement string.
        val replace:
          oldValue: string -> newValue: string -> source: string -> string
        
        /// Does the source string contain the given subString? -- function wrapper for String.Contains method.
        val isSubString: subString: string -> source: string -> bool
        
        /// Does the source string start with the given subString? -- function wrapper for String.StartsWith method using InvariantCulture.
        val startsWith: subString: string -> source: string -> bool
        
        /// Does the source string end with the given subString? -- function wrapper for String.EndsWith method using InvariantCulture.
        val endsWith: subString: string -> source: string -> bool
        
        /// Does the source string contain the given character?
        /// Use `String.isSubstring` to check for strings.
        val contains: char: char -> source: string -> bool
        
        /// Converts to uppercase -- nullsafe function wrapper for String.ToUpperInvariant method.
        val toUpper: source: string -> string
        
        /// Converts to lowercase -- nullsafe function wrapper for String.ToLowerInvariant method.
        val toLower: source: string -> string
        
        /// Trims leading and trailing white spaces -- function wrapper for String.Trim method.
        /// 
        /// Note this is distinct from trim which trims the given characters,
        /// not white spaces.
        val trimWhiteSpaces: source: string -> string
        
        /// Trims leading white spaces -- function wrapper for String.TrimStart method.
        /// 
        /// Note this is distinct from trim which trims the given characters,
        /// not white spaces.
        val trimStartWhiteSpaces: source: string -> string
        
        /// Trims trailing white spaces -- function wrapper for String.TrimEnd method.
        /// 
        /// Note this is distinct from trim which trims the given characters,
        /// not white spaces.
        val trimEndWhiteSpaces: source: string -> string
        
        /// Returns a new string whose textual value is the same as this string, but whose binary representation is in the specified Unicode normalization form.
        /// 
        /// This is a null safe function wrapper of the String.Normalize method.
        val normalize:
          normalizationForm: System.Text.NormalizationForm -> source: string
            -> string
        
        /// Removes diacritics (accents) from the given source string.
        /// 
        /// The approach uses `normalize` to split the input string into constituent glyphs
        /// (basically separating the "base" characters from the diacritics) and then scans
        /// the result and retains only the base characters. 
        val removeDiacritics: source: string -> string
        
        /// Pads the beginning of the given string with spaces so that it has a specified total length.
        val padLeft: totalLength: int -> source: string -> string
        
        /// Pads the beginning of the given string with a specified character so that it has a specified total length.
        val padLeftWith:
          totalLength: int -> paddingChar: char -> source: string -> string
        
        /// Pads the end of the given string with spaces so that it has a specified total length.
        val padRight: totalLength: int -> source: string -> string
        
        /// Pads the end of the given string with a specified character so that it has a specified total length.
        val padRightWith:
          totalLength: int -> paddingChar: char -> source: string -> string
        
        /// Removes all leading and trailing occurrences of specified characters from the given string.
        val trim: trimChars: seq<char> -> source: string -> string
        
        /// Removes all leading occurrences of specified characters from the given string.
        val trimStart: trimChars: seq<char> -> source: string -> string
        
        /// Removes all trailing occurrences of specified characters from the given string.
        val trimEnd: trimChars: seq<char> -> source: string -> string
        
        /// Converts the given string to an array of chars.
        val toArray: source: string -> char[]
        
        /// Converts an array of chars to a String.
        val ofArray: source: char[] -> System.String
        
        /// Converts the given string to a list of chars.
        val toList: source: string -> char list
        
        /// Converts a list of chars to a String.
        val ofList: source: char list -> System.String
        
        /// Converts the given string to a seq of chars.
        val toSeq: source: string -> seq<char>
        
        /// Converts a seq of chars to a String.
        val ofSeq: source: seq<char> -> string
        
        /// (Unsafely) Returns the char at the given index in the source string.
        /// 
        /// This is a function wrapper for `source.[index]` method.
        /// 
        /// Note: this is not exception safe, and will throw System.IndexOutOfRangeException when
        /// the given index is out of bounds.
        val item: index: int -> source: string -> char
        
        /// Returns the char (as an Option) at the given index in the source string,
        /// returning `None` if out of bounds.
        val tryItem: index: int -> source: string -> char option
        
        /// Reverses the given string.
        val rev: source: string -> System.String
        
        /// (Unsafely) Takes the first count chars in the string.
        /// Use `String.truncate` for a safe version.
        /// 
        /// Note: will throw System.ArgumentOutOfRangeException if you try to take more than the
        /// number of chars in the string.
        val take: count: int -> source: string -> string
        
        /// (Unsafely) Skips over the first count chars in the string.
        /// Use `String.drop` for a safe version.
        /// 
        /// Note: will throw System.ArgumentOutOfRangeException if you try to skip more than the
        /// number of chars in the string.
        val skip: count: int -> source: string -> string
        
        /// Takes chars from the source string while the given predicate is true.
        val takeWhile: predicate: (char -> bool) -> source: string -> string
        
        /// Skips over chars from the source string while the given predicate is true.
        val skipWhile: predicate: (char -> bool) -> source: string -> string
        
        /// <summary>Gets the first char of the string, or
        /// <c>None</c> if the string is empty.</summary>
        val tryHead: source: string -> char option
        
        /// <summary>Gets the last char of the string, or
        /// <c>None</c> if the string is empty.</summary>
        val tryLast: source: string -> char option
        
        /// Returns a string that has at most N characters from the beginning of the original string.
        /// It returns the original string if it is shorter than count.
        val truncate: count: int -> source: string -> string
        
        /// Returns a string that drops first N characters of the original string.
        /// When count exceeds the length of the string it returns an empty string.
        val drop: count: int -> source: string -> string
        
        /// Finds the first index of the char in the substring which satisfies the given predicate.
        /// 
        /// Note: throws an ArgumentException when not found.
        val findIndex: predicate: (char -> bool) -> source: string -> int
        
        /// Tries to find the first index of the char in the substring which satisfies the given predicate.
        val tryFindIndex:
          predicate: (char -> bool) -> source: string -> int option
        
        /// <summary>
        /// Returns the index of the first occurrence of the specified slice in the source.
        /// </summary>
        /// <exception cref="System.ArgumentException">
        /// Thrown when the slice was not found in the sequence.
        /// </exception>
        /// <returns>
        /// The index of the slice.
        /// </returns>
        val findSliceIndex: slice: string -> source: string -> int
        
        /// <summary>
        /// Returns the index of the first occurrence of the specified slice in the source.
        /// Returns <c>None</c> if not found.
        /// </summary>
        /// <returns>
        /// The index of the slice or <c>None</c>.
        /// </returns>
        val tryFindSliceIndex: slice: string -> source: string -> int option
        
        /// Converts the given string to an array of Int32 code-points (the actual Unicode Code Point number).
        val toCodePoints: source: string -> seq<int>
        
        /// Converts the array of Int32 code-points (the actual Unicode Code Point number) to a string.
        val ofCodePoints: source: seq<int> -> string
        
        /// Converts a string to a byte-array using the specified encoding.
        val getBytes: encoding: System.Text.Encoding -> source: string -> byte[]

