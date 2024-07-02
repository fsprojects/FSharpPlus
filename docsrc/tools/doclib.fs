module DocLib
// Based on Fake project
// Copyright 2008 "FAKE - F# Make" Project
// Copyright 2010 Steffen Forkmann
// Apache license


open System.IO

module String =

    open System
    open System.Collections.Generic

    /// Converts a sequence of strings to a string with delimiters
    let inline separated delimiter (items : string seq) = String.Join((delimiter: string), Array.ofSeq items)

    /// Splits the given string at the given string delimiter
    let inline splitStr (delimiterStr : string) (text : string) = text.Split([| delimiterStr |], StringSplitOptions.None) |> Array.toList

    /// Converts a sequence of strings into a string separated with line ends
    let inline toLines text = separated Environment.NewLine text

    /// Checks whether the given text starts with the given prefix
    let startsWith (prefix: string) (text : string) = text.StartsWith prefix

    let private regexes = new Dictionary<_, _> ()

    let getRegEx pattern = 
        match regexes.TryGetValue pattern with
        | true, regex -> regex
        | _ -> (new System.Text.RegularExpressions.Regex(pattern))

    /// Trims the given string
    let inline trim (x : string) = if String.IsNullOrEmpty x then x else x.Trim()

    /// Trims the given string
    let inline trimChars (chars : char []) (x : string) = if String.IsNullOrEmpty x then x else x.Trim chars

    /// Trims the start of the given string
    let inline trimStartChars (chars : char []) (x : string) = if String.IsNullOrEmpty x then x else x.TrimStart chars

    /// Trims the end of the given string
    let inline trimEndChars (chars : char []) (x : string) = if String.IsNullOrEmpty x then x else x.TrimEnd chars

    /// Removes all trailing .0 from a version string
    let rec NormalizeVersion(version : string) =
        if version = null then "" else
        let elements = version.Split [| '.' |]
        let mutable version = ""
        for i in 0..3 do
            if i < elements.Length then 
                if version = "" then version <- elements.[i]
                else version <- version + "." + elements.[i]
        if version.EndsWith ".0" then version.Remove(version.Length - 2, 2) |> NormalizeVersion
        else version


module Path =

    /// Combines two path strings using Path.Combine
    let inline combineTrimEnd path1 (path2 : string) = Path.Combine(path1, path2.TrimStart [| '\\'; '/' |])
    
    /// Normalizes a filename.
    let normalizeFileName (fileName : string) =
        let dirsepChar = Path.DirectorySeparatorChar
        let dirsep = dirsepChar.ToString()
        fileName
            .Replace("\\", dirsep)
            .Replace("/", dirsep)
            .TrimEnd(dirsepChar)
            .ToLower()

    /// Detects whether the given path does not contains invalid characters.
    let isValidPath (path:string) =
        Path.GetInvalidPathChars()
        |> Array.exists (fun char -> path.Contains(char.ToString()))
        |> not


let inline (@@) path1 path2 = Path.combineTrimEnd path1 path2


module SemVer =

    open System
    open System.Globalization
    open System.Text.RegularExpressions

    [<CustomEquality; CustomComparison>]
    type PreReleaseSegment = 
        | AlphaNumeric of string
        | Numeric of bigint

        member x.CompareTo(y) =
            match x, y with
            | AlphaNumeric a, AlphaNumeric b -> compare a b
            | Numeric a, Numeric b -> compare a b
            | AlphaNumeric _, Numeric _ -> 1
            | Numeric _, AlphaNumeric _ -> -1

        interface System.IComparable with
            member x.CompareTo yobj =
                match yobj with
                | :? PreReleaseSegment as y -> x.CompareTo(y)                
                | _ -> invalidArg "yobj" "can't compare to other types of objects."
            
        override x.GetHashCode() = hash x
    
        member x.Equals(y) =
            match x, y with
            | AlphaNumeric a, AlphaNumeric b -> a = b
            | Numeric a, Numeric b -> a = b
            | AlphaNumeric _, Numeric _ -> false
            | Numeric _, AlphaNumeric _ -> false

        override x.Equals yobj = 
            match yobj with 
            | :? PreReleaseSegment as y -> x.Equals(y)
            | _ -> false

    /// Information about PreRelease packages.
    [<CustomEquality; CustomComparison>]
    type PreRelease = 
        { Origin : string
          Name : string
          Values : PreReleaseSegment list }
      
        static member TryParse (str : string) = 
            if String.IsNullOrEmpty str then None
            else
                let getName fromList =
                    match fromList with
                    | AlphaNumeric(a)::_ -> a
                    | _::AlphaNumeric(a)::_ -> a // fallback to 2nd
                    | _ -> ""
                
                let parse (segment: string) =
                    match bigint.TryParse segment with
                    | true, number when number >= 0I -> Numeric number
                    | _ -> AlphaNumeric segment
                
                let notEmpty = StringSplitOptions.RemoveEmptyEntries
                let name, values = 
                    match str.Split([|'.'|],notEmpty) with
                    | [|one|] -> 
                        let list = one.Split([|'-'|],notEmpty) |> Array.map parse |> List.ofArray
                    
                        // semver1-like embedded / inlined prerelease numbers
                        let name =
                            match Regex("^(?<name>[a-zA-Z]+)").Match(one) with 
                            | ex when ex.Success -> ex.Value
                            | _ -> getName list
                        
                        name, list
                                    
                    | multiple -> //semver2: dashes are ok, inline numbers not
                
                        let list = multiple |> Array.map parse |> List.ofArray
                        getName list, list
                    
                Some { Origin = str; Name = name; Values = values }

        member x.Equals(y) = x.Origin = y.Origin

        override x.Equals(yobj) = 
            match yobj with
            | :? PreRelease as y -> x.Equals(y)
            | _ -> false
        
        override x.ToString() = x.Origin
    
        override x.GetHashCode() = hash x.Origin
    
        member x.CompareTo(yobj) = 
            let rec cmp item count xlist ylist = 
                if item < count then
                    let res = compare (List.head xlist) (List.head ylist)
                    if res = 0 then 
                        cmp (item + 1) count (List.tail xlist) (List.tail ylist)
                    else
                        res // result given by first difference
                else
                    sign xlist.Length - ylist.Length // https://semver.org/#spec-item-11
            let len = min x.Values.Length yobj.Values.Length // compare up to common len
            cmp 0 len x.Values yobj.Values
        
        interface System.IComparable with
            member x.CompareTo yobj =
                match yobj with
                | :? PreRelease as y -> x.CompareTo(y)
                | _ -> invalidArg "yobj" "PreRelease: cannot compare to values of different types"


    /// Contains the version information. For parsing use [SemVer.parse](fake-core-semver.html)
    /// 
    /// > Note: If you use `{ version with Patch = myPath; Original = None }` to overwrite some parts of this string make sure to overwrite `Original` to `None` in order to recalculate the version string.
    /// 
    /// > Note: For overwriting the `PreRelease` part use: `{ Version with Original = None; PreRelease = PreRelease.TryParse "alpha.1" }`
    [<CustomEquality; CustomComparison; StructuredFormatDisplay("{AsString}")>]
    type SemVerInfo = 
        { /// MAJOR version when you make incompatible API changes.
          Major : uint32
          /// MINOR version when you add functionality in a backwards-compatible manner.
          Minor : uint32
          /// PATCH version when you make backwards-compatible bug fixes.
          Patch : uint32
          /// The optional PreRelease version
          PreRelease : PreRelease option
          /// The optional build no.
          Build : bigint
          BuildMetaData : string
          // The original version text
          Original : string option }
    
        member x.Normalize() = 
            let build = if x.Build > 0I then ("." + x.Build.ToString("D")) else ""                        
            let pre = 
                match x.PreRelease with
                | Some preRelease -> ("-" + preRelease.Origin)
                | None -> ""
            sprintf "%d.%d.%d%s%s" x.Major x.Minor x.Patch build pre

        override x.ToString() = 
            match x.Original with
            | Some version -> version.Trim()
            | None -> x.Normalize()
        
        member x.Equals(y) = x.Major = y.Major && x.Minor = y.Minor && x.Patch = y.Patch && x.Build = y.Build && x.PreRelease = y.PreRelease

        override x.Equals(yobj) = 
            match yobj with
            | :? SemVerInfo as y -> x.Equals(y)
            | _ -> false
    
        override x.GetHashCode() = hash (x.Major, x.Minor, x.Patch, x.Build, x.PreRelease)
    
        member x.CompareTo(y) =
            let comparison =  
                match compare x.Major y.Major with 
                | 0 ->
                    match compare x.Minor y.Minor with
                    | 0 ->
                        match compare x.Patch y.Patch with
                        | 0 ->  
                            match compare x.Build y.Build with 
                            | 0 -> 
                                match x.PreRelease, y.PreRelease with
                                | None, None -> 0
                                | Some _, None -> -1
                                | None, Some p -> 1
                                | Some p, Some p2 when p.Origin = "prerelease" && p2.Origin = "prerelease" -> 0
                                | Some p, _ when p.Origin = "prerelease" -> -1
                                | _, Some p when p.Origin = "prerelease" -> 1
                                | Some left, Some right -> compare left right
                            | c -> c
                        | c -> c
                    | c -> c
                | c -> c
            comparison
    
        interface System.IComparable with
            member x.CompareTo yobj = 
                match yobj with
                | :? SemVerInfo as y -> x.CompareTo(y)
                | _ -> invalidArg "yobj" "SemVerInfo: cannot compare to values of different types"

    ///  Parser which allows to deal with [Semantic Versioning](http://semver.org/) (SemVer).
    ///  Make sure to read the documentation in the [SemVerInfo](fake-core-semverinfo.html) record as well if you manually create versions.
    [<RequireQualifiedAccess>]
    module SemVer =
        open System.Numerics

        /// Matches if str is convertible to Int and not less than zero, and returns the value as UInt.
        let inline private (|Int|_|) (str : string) =
            match Int32.TryParse (str, NumberStyles.Integer, null) with
            | true, num when num > -1 -> Some num
            | _ -> None
        
        /// Matches if str is convertible to big int and not less than zero, and returns the bigint value.
        let inline private (|Big|_|) (str: string) =
            match BigInteger.TryParse (str, NumberStyles.Integer, null) with
            | true, big when big > -1I -> Some big
            | _ -> None

        /// Splits the given version string by possible delimiters but keeps them as parts of resulting list.
        let private expand delimiter (text : string) =
            let sb = Text.StringBuilder()
            seq {
                for ch in text do
                    match List.contains ch delimiter with
                    | true -> 
                        yield sb.ToString()
                        sb.Clear() |> ignore
                        yield ch.ToString()
                    | false ->
                        sb.Append(ch) |> ignore
                if sb.Length > 0 then
                    yield sb.ToString()
                    sb.Clear() |> ignore
                } |> Seq.toList
        
        let private validContent = Regex(@"(?in)^[a-z0-9-]+(\.[a-z0-9-]+)*")

        /// Parses the given version string into a SemVerInfo which can be printed using ToString() or compared
        /// according to the rules described in the [SemVer docs](http://semver.org/).
        /// ## Sample
        ///
        ///     parse "1.0.0-rc.1"     < parse "1.0.0"          // true
        ///     parse "1.2.3-alpha"    > parse "1.2.2"          // true
        ///     parse "1.2.3-alpha2"   > parse "1.2.3-alpha"    // true
        ///     parse "1.2.3-alpha002" > parse "1.2.3-alpha1"   // false
        ///     parse "1.5.0-beta.2"   > parse "1.5.0-rc.1"     // false
        let parse (version : string) = 
            try
                /// sanity check to make sure that all of the integers in the string are positive.
                /// because we use raw substrings with dashes this is very complex :(
                for s in version.Split([|'.'|]) do
                    match Int32.TryParse s with 
                    | true, s when s < 0 -> failwith "no negatives!" 
                    | _ -> ignore ()  // non-numeric parts are valid

                if version.Contains("!") then 
                    failwithf "Invalid character found in %s" version
                if version.Contains("..") then 
                    failwithf "Empty version part found in %s" version

                let plusIndex = version.IndexOf("+")

                let versionStr = 
                    match plusIndex with
                    | n when n < 0 -> version
                    | n -> version.Substring(0, n)

                /// there can only be one piece of build metadata, and it is signified by + sign
                /// and then any number of dot-separated alpha-numeric groups.
                let buildmeta =
                    match plusIndex with
                    | -1 -> ""
                    | n when n = version.Length - 1 -> ""
                    | n -> 
                        let content = validContent.Match(version.Substring(n + 1))
                        if content.Success then content.Value else ""

                let fragments = expand [ '.'; '-' ] versionStr
                /// matches over list of the version fragments *and* delimiters
                let major, minor, patch, revision, suffix =
                    match fragments with
                    | (Int M)::"."::(Int m)::"."::(Int p)::"."::(Big b)::tail -> M, m, p, b, tail
                    | (Int M)::"."::(Int m)::"."::(Int p)::tail -> M, m, p, 0I, tail
                    | (Int M)::"."::(Int m)::tail -> M, m, 0, 0I, tail
                    | (Int M)::tail -> M, 0, 0, 0I, tail
                    | _ -> raise(ArgumentException("SemVer.Parse", "version"))
                    //this is expected to fail, for now :/
                    //| [text] -> 0, 0, 0, 0I, [text] 
                    //| [] | _ -> 0, 0, 0, 0I, []
            
                /// recreate the remaining string to parse as prerelease segments
                let prerelease =
                    if suffix.IsEmpty || suffix.Tail.IsEmpty then ""
                    else String.Concat(suffix.Tail).TrimEnd([|'.'; '-'|])

                { Major = uint32 major
                  Minor = uint32 minor
                  Patch = uint32 patch
                  Build = revision
                  PreRelease = PreRelease.TryParse prerelease
                  BuildMetaData = buildmeta
                  Original = Some version }

            with e -> raise <| exn(sprintf "Can't parse \"%s\"." version, e)


module ReleaseNotes =

    open System
    open SemVer

    /// Contains the parsed information of the release notes text file.
    type ReleaseNotes =
        { /// The parsed version.
            AssemblyVersion: string
            /// The nuget package version.
            NugetVersion: string
            /// Semantic version
            SemVer: SemVerInfo
            /// Release date
            Date : DateTime option
            // The parsed release notes.
            Notes: string list }
        override x.ToString() = sprintf "%A" x

        static member New(assemblyVersion,nugetVersion,date,notes) = { 
            AssemblyVersion = assemblyVersion
            NugetVersion = nugetVersion
            SemVer = SemVer.parse nugetVersion
            Date = date
            Notes = notes }

    let private parseVersions =
        let nugetRegex = String.getRegEx @"([0-9]+.)+[0-9]+(-[a-zA-Z]+\d*)?(.[0-9]+)?"
        let assemblyVersionRegex = String.getRegEx @"([0-9]+.)+[0-9]+"
        fun line ->
            let assemblyVersion = assemblyVersionRegex.Match line
            if not assemblyVersion.Success
            then failwithf "Unable to parse valid Assembly version from release notes (%s)." line

            let nugetVersion = nugetRegex.Match line
            if not nugetVersion.Success
            then failwithf "Unable to parse valid NuGet version from release notes (%s)." line
            assemblyVersion, nugetVersion

    let private parseDate =
        let dateRegex = String.getRegEx @"(19|20)\d\d([- /.])(0[1-9]|1[012]|[1-9])\2(0[1-9]|[12][0-9]|3[01]|[1-9])"
        fun line ->
            let possibleDate = dateRegex.Match line
            if not possibleDate.Success then None
            else
                match DateTime.TryParse possibleDate.Value with
                | false, _ -> None
                | true , x -> Some x

    /// Parse simple release notes sequence
    let private parseSimple line =
        let assemblyVersion, nugetVersion = parseVersions line
        let trimDot (s:string) = s.TrimEnd('.')

        let notes = 
            line.Substring (nugetVersion.Index + nugetVersion.Length)
            |> String.trimChars [|' '; '-'|]
            |> String.splitStr ". "
            |> List.map (trimDot >> String.trim)
            |> List.filter (not << String.IsNullOrEmpty)
            |> List.map (fun x -> x + ".")
        ReleaseNotes.New(assemblyVersion.Value, nugetVersion.Value, None, notes)

    /// Parse "complex" release notes text sequence
    let private parseAllComplex (text: seq<string>) =
        let rec findNextNotesBlock text =
            let isHeader line = String.startsWith "##" line
            let rec findEnd notes text =
                match text with
                | [] -> notes,[]
                | h :: rest -> if isHeader h then notes,text else findEnd (h :: notes) rest

            match text with
            | [] -> None
            | h :: rest -> if isHeader h then Some(h,findEnd [] rest) else findNextNotesBlock rest

        let rec loop releaseNotes text =
            match findNextNotesBlock text with
            | Some(header,(notes, rest)) ->
                let assemblyVer, nugetVer = parseVersions header
                let date = parseDate header
                let newReleaseNotes = ReleaseNotes.New(assemblyVer.Value,nugetVer.Value,date,notes |> List.filter (not << String.IsNullOrEmpty) |> List.rev)
                loop (newReleaseNotes::releaseNotes) rest
            | None -> releaseNotes

        let result = loop [] (text |> Seq.map (String.trimStartChars [|' '; '*'|] >> String.trimEndChars [|' '|]) |> Seq.toList)
        if List.isEmpty result then
            failwithf "release note files containing only top level headers are not allowed"
        else result


    /// Parses a Release Notes text and returns all release notes.
    ///
    /// ## Parameters
    ///  - `data` - Release notes text
    let parseAll (data: seq<string>) = 
        let data = data |> Seq.toList |> List.filter (not << String.IsNullOrWhiteSpace)
        match data with
        | [] -> failwith "Empty Release file."
        | h :: _ ->
            let (|Simple|Complex|Invalid|) = function '*' -> Simple | '#' -> Complex | _ -> Invalid
            let firstNonEmptyChar = h.Trim([|'-'; ' '|]).[0]
            match firstNonEmptyChar with
            | Simple -> List.map parseSimple data
            | Complex -> parseAllComplex data
            | Invalid -> failwith "Invalid Release Notes format."
            |> List.sortBy (fun x -> x.SemVer)
            |> List.rev

    
    /// Parses a Release Notes text and returns the lastest release notes.
    ///
    /// ## Parameters
    ///  - `data` - Release notes text
    let parse (data: seq<string>) =
        match data |> parseAll |> Seq.tryHead with
        | Some head -> head
        | None -> failwithf "The release notes document was not valid, see https://fake.build/apidocs/v5/fake-core-releasenotes.html for the allowed formats"

    /// Parses a Release Notes text file and returns the lastest release notes.
    ///
    /// ## Parameters
    ///  - `fileName` - Release notes text file name
    let load fileName = System.IO.File.ReadLines fileName |> parse


[<RequireQualifiedAccess>]
module DirectoryInfo =
    /// Creates a DirectoryInfo for the given path.
    let inline ofPath path = DirectoryInfo(path)

    /// Gets all subdirectories of a given directory.
    let inline getSubDirectories (dir : DirectoryInfo) = dir.GetDirectories()

    /// Gets all files in the directory.
    let inline getFiles (dir : DirectoryInfo) = dir.GetFiles()
        
    /// Checks if dir1 is a subfolder of dir2. If dir1 equals dir2 the function returns also true.
    let rec isSubfolderOf (dir2 : DirectoryInfo) (dir1 : DirectoryInfo) = 
        if Path.normalizeFileName dir1.FullName = Path.normalizeFileName dir2.FullName then true
        elif isNull dir1.Parent then false
        else dir1.Parent |> isSubfolderOf dir2
    
    /// Ensure that directory chain exists. Create necessary directories if necessary.
    let inline ensure (dir : DirectoryInfo) = if not dir.Exists then dir.Create()
 
    /// Performs the given actions on all files and subdirectories
    let rec private recursively dirF fileF (dir : DirectoryInfo) = 
        dir
        |> getSubDirectories
        |> Seq.iter (fun dir -> 
               recursively dirF fileF dir
               dirF dir)
        dir
        |> getFiles
        |> Seq.iter fileF
    
    /// Copies the file structure recursively, filtering files.
    let rec copyRecursiveToWithFilter overwrite filter (outputDir : DirectoryInfo) (dir : DirectoryInfo) = 
        let files = 
            dir
            |> getSubDirectories
            |> Seq.fold (fun acc (d : DirectoryInfo) -> 
                   let newDir = outputDir.FullName @@ d.Name |> ofPath
                   ensure newDir
                   d
                   |> copyRecursiveToWithFilter overwrite filter newDir
                   |> fun r -> r @ acc) []
        (dir
         |> getFiles
         |> Seq.filter (fun f -> filter outputDir f)
         |> Seq.map (fun f -> 
                let newFileName = outputDir.FullName @@ f.Name
                f.CopyTo(newFileName, overwrite) |> ignore
                newFileName)
         |> Seq.toList) @ files

    /// Copies the file structure recursively.
    let copyRecursiveTo overwrite (outputDir : DirectoryInfo) (dir : DirectoryInfo) = copyRecursiveToWithFilter overwrite (fun _ _ -> true) outputDir dir


module FileInfo =
    /// Creates a FileInfo for the given path.
    let inline ofPath path = new FileInfo(path)


[<RequireQualifiedAccess>]
module FileSystemInfo =
    /// Creates a FileInfo or a DirectoryInfo for the given path
    let inline ofPath path : FileSystemInfo = 
        if Directory.Exists path then upcast DirectoryInfo.ofPath path
        else upcast FileInfo.ofPath path

    /// Active pattern which discriminates between files and directories.
    let (|File|Directory|) (fileSysInfo : FileSystemInfo) = 
        match fileSysInfo with
        | :? FileInfo as file -> File(file)
        | :? DirectoryInfo as dir -> Directory(dir, dir.EnumerateFileSystemInfos())
        | _ -> failwith "No file or directory given."


module Directory =

    /// Checks if the given directory exists. If not then this functions creates the directory.
    let inline ensure dir = dir |> DirectoryInfo.ofPath |> DirectoryInfo.ensure

module Glob =

    open System
    open System.Text.RegularExpressions

    // Normalizes path for different OS
    let inline normalizePath (path : string) = 
        path.Replace('\\', Path.DirectorySeparatorChar).Replace('/', Path.DirectorySeparatorChar)

    type private SearchOption = 
        | Directory of string
        | Drive of string
        | Recursive
        | FilePattern of string

    let private checkSubDirs absolute (dir : string) root =
        if dir.Contains "*" then Directory.EnumerateDirectories(root, dir, SearchOption.TopDirectoryOnly) |> Seq.toList
        else 
            let path = Path.Combine(root, dir)
            let di = 
                if absolute then new DirectoryInfo (dir)
                else new DirectoryInfo (path)
            if di.Exists then [ di.FullName ]
            else []

    let rec private buildPaths acc (input : SearchOption list) =
        match input with
        | [] -> acc
        | Directory name :: t ->
            let subDirs = List.collect (checkSubDirs false name) acc
            buildPaths subDirs t
        | Drive name :: t ->
            let subDirs = List.collect (checkSubDirs true name) acc
            buildPaths subDirs t
        | Recursive :: [] ->
            let dirs = Seq.collect (fun dir -> Directory.EnumerateFileSystemEntries(dir, "*", SearchOption.AllDirectories)) acc
            buildPaths (acc @ Seq.toList dirs) []
        | Recursive :: t ->
            let dirs = Seq.collect (fun dir -> Directory.EnumerateDirectories(dir, "*", SearchOption.AllDirectories)) acc
            buildPaths (acc @ Seq.toList dirs) t
        | FilePattern pattern :: _ ->
            acc |> List.collect (fun dir ->
                if Directory.Exists (Path.Combine (dir, pattern)) then [Path.Combine (dir, pattern)]
                else
                    try
                        Directory.EnumerateFiles (dir, pattern) |> Seq.toList
                    with
                        | :? System.IO.PathTooLongException -> [])

    let private driveRegex = Regex(@"^[A-Za-z]:$", RegexOptions.Compiled)

    let inline private normalizeOutputPath (p : string) =
        p.Replace('\\', Path.DirectorySeparatorChar)
            .Replace('/', Path.DirectorySeparatorChar)
            .TrimEnd(Path.DirectorySeparatorChar)

    let internal search (baseDir : string) (input : string) = 
        let baseDir = normalizePath baseDir
        let input = normalizePath input
        let input =
            if String.IsNullOrEmpty baseDir
            then input
            else
                // The final \ (or /) makes sure to only match complete folder names (as one folder name could be a substring of the other)
                let start = baseDir.TrimEnd([|Path.DirectorySeparatorChar|]) + string Path.DirectorySeparatorChar
                // See https://github.com/fsharp/FAKE/issues/1925
                if input.StartsWith start then
                    input.Substring start.Length
                else input           

        let filePattern = Path.GetFileName(input)

        let splits = input.Split([| '/'; '\\' |], StringSplitOptions.None)
        let baseItems =
            let start, rest =
                if input.StartsWith "\\\\" && splits.Length >= 4 then
                    let serverName = splits.[2]
                    let share = splits.[3]
                    [ Directory (sprintf "\\\\%s\\%s" serverName share) ], splits |> Seq.skip 4
                elif splits.Length >= 2 && Path.IsPathRooted input && driveRegex.IsMatch splits.[0] then
                    [ Directory(splits.[0] + "\\") ], splits |> Seq.skip 1
                elif splits.Length >= 2 && Path.IsPathRooted input && input.StartsWith "/" then
                    [ Directory("/") ], splits |> Array.toSeq
                else
                    if Path.IsPathRooted input then failwithf "Unknown globbing input '%s', try to use a relative path and report an issue!" input
                    [], splits |> Array.toSeq
            let restList =
                rest    
                |> Seq.filter (String.IsNullOrEmpty >> not)
                |> Seq.map (function 
                       | "**" -> Recursive
                       | a when a = filePattern -> FilePattern(a)
                       | a -> Directory(a))
                |> Seq.toList
            start @ restList
        baseItems    
        |> buildPaths [ baseDir ]
        |> List.map normalizeOutputPath


open System.Collections.Generic

type IGlobbingPattern =
    inherit IEnumerable<string>
    abstract BaseDirectory : string
    abstract Includes : string list
    abstract Excludes : string list

type LazyGlobbingPattern =
    { BaseDirectory : string
      Includes : string list
      Excludes : string list }
    
    interface IGlobbingPattern with
        member this.BaseDirectory = this.BaseDirectory
        member this.Includes = this.Includes
        member this.Excludes = this.Excludes

    interface IEnumerable<string> with
        
        member this.GetEnumerator() = 
            let hashSet = HashSet()
            
            let excludes = 
                seq { 
                    for pattern in this.Excludes do
                        yield! Glob.search this.BaseDirectory pattern
                }
                |> Set.ofSeq
            
            let files = 
                seq { 
                    for pattern in this.Includes do
                        yield! Glob.search this.BaseDirectory pattern
                }
                |> Seq.filter (fun x -> not (Set.contains x excludes))
                |> Seq.filter (fun x -> hashSet.Add x)
            
            files.GetEnumerator()
        
        member this.GetEnumerator() = (this :> IEnumerable<string>).GetEnumerator() :> System.Collections.IEnumerator


[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module GlobbingPattern =
    let private defaultBaseDir = Path.GetFullPath "."

    /// Include files
    let create x = 
        { BaseDirectory = defaultBaseDir
          Includes = [ x ]
          Excludes = [] } :> IGlobbingPattern

/// Includes a single pattern and scans the files - !! x = AllFilesMatching x
let inline (!!) x = GlobbingPattern.create x

[<RequireQualifiedAccess>]
module Tools =

    let private (@@) path1 (path2:string) = Path.Combine(path1, path2.TrimStart [| '\\'; '/' |])

    /// Looks for a tool first in its default path, if not found the in ./packages/ and then
    /// in all subfolders of the root folder - returns the tool file name.
    let findToolInSubPath (toolname:string) (defaultPath:string) =
        try
            let tools = !! (defaultPath @@ "/**/" @@ toolname)
            if  Seq.isEmpty tools then 
                let packages = !! ("./packages/**/" @@ toolname)
                if Seq.isEmpty packages then
                    let root = !! ("./**/" @@ toolname)
                    Seq.head root
                else
                    Seq.head packages
            else
                Seq.head tools
        with
        | _ -> defaultPath @@ toolname



[<RequireQualifiedAccess>]
module Shell =

    /// Copies a single file to the target and overwrites the existing file.
    /// ## Parameters
    ///
    ///  - `target` - The target directory or file.
    ///  - `fileName` - The FileName.
    let copyFile target fileName =
        let fi = FileSystemInfo.ofPath fileName
        match fi with
        | FileSystemInfo.File f ->
            let targetName =
                match FileSystemInfo.ofPath target with
                | FileSystemInfo.Directory _ -> target @@ fi.Name
                | FileSystemInfo.File f' -> f'.FullName
            //TODO: logVerbosefn "Copy %s to %s" fileName targetName
            f.CopyTo(targetName, true) |> ignore
        | FileSystemInfo.Directory _ -> () //TODO: logVerbosefn "Ignoring %s, because it is a directory." fileName


    /// Renames the file or directory to the target name.
    /// ## Parameters
    ///
    ///  - `target` - The target file or directory name.
    ///  - `fileName` - The orginal file or directory name.
    let rename target fileName = (FileInfo.ofPath fileName).MoveTo target


    /// Cleans a directory by removing all files and sub-directories.
    let cleanDir path =
        let di = DirectoryInfo.ofPath path
        if di.Exists then
            () //TODO: logfn "Deleting contents of %s" path
            // delete all files
            Directory.GetFiles(path, "*.*", SearchOption.AllDirectories) |> Seq.iter (fun file ->
                                                                                let fi = FileInfo.ofPath file
                                                                                fi.IsReadOnly <- false
                                                                                fi.Delete())
            // deletes all subdirectories
            let rec deleteDirs actDir =
                Directory.GetDirectories(actDir) |> Seq.iter deleteDirs
                Directory.Delete(actDir, true)
            Directory.GetDirectories path |> Seq.iter deleteDirs
        else Directory.ensure path
        // set writeable
        File.SetAttributes(path, FileAttributes.Normal)

    /// Cleans multiple directories
    let cleanDirs dirs = Seq.iter cleanDir dirs

    /// Copies the file structure recursively.
    let copyRecursive dir outputDir overWrite = DirectoryInfo.copyRecursiveTo overWrite (DirectoryInfo.ofPath outputDir) (DirectoryInfo.ofPath dir)




// [<RequireQualifiedAccess>]
module Environment =

    open System

    /// Retrieves the environment variable with the given name
    let environVar name = System.Environment.GetEnvironmentVariable name

    /// Retrieves the environment variable with the given name or returns the default if no value was set
    let environVarOrDefault name defaultValue = 
        let var = environVar name
        if String.IsNullOrEmpty var then defaultValue
        else var

    /// Retrieves the environment variable or None
    let environVarOrNone name = 
        let var = environVar name
        if String.IsNullOrEmpty var then None
        else Some var

    /// Splits the entries of an environment variable and removes the empty ones.
    let splitEnvironVar name =
        let var = environVarOrNone name
        if var = None then [ ]
        else var.Value.Split([| Path.PathSeparator |]) |> Array.toList

    /// The path of the "Program Files" folder - might be x64 on x64 machine
    let ProgramFiles = Environment.GetFolderPath Environment.SpecialFolder.ProgramFiles

    /// The path of Program Files (x86)
    /// It seems this covers all cases where PROCESSOR\_ARCHITECTURE may misreport and the case where the other variable 
    /// PROCESSOR\_ARCHITEW6432 can be null
    let ProgramFilesX86 = 
        let wow64 = environVar "PROCESSOR_ARCHITEW6432"
        let globalArch = environVar "PROCESSOR_ARCHITECTURE"
        match wow64, globalArch with
        | "AMD64", "AMD64" 
        | null, "AMD64" 
        | "x86", "AMD64" -> environVar "ProgramFiles(x86)"
        | _ -> environVar "ProgramFiles"
        |> fun detected -> if isNull detected then @"C:\Program Files (x86)\" else detected

    /// The system root environment variable. Typically "C:\Windows"
    let SystemRoot = environVar "SystemRoot"

    /// Determines if the current system is an Unix system.
    /// See http://www.mono-project.com/docs/faq/technical/#how-to-detect-the-execution-platform
    let isUnix = 
        int System.Environment.OSVersion.Platform |> fun p -> (p = 4) || (p = 6) || (p = 128)

    /// Determines if the current system is a Windows system
    let isWindows =
        match System.Environment.OSVersion.Platform with
        | PlatformID.Win32NT | PlatformID.Win32S | PlatformID.Win32Windows | PlatformID.WinCE -> true
        | _ -> false

    /// Gets the list of valid directories included in the PATH environment variable.
    let pathDirectories =
        splitEnvironVar "PATH"
        |> Seq.map (fun value -> value.Trim())
        |> Seq.filter (not << String.IsNullOrEmpty)


module Trace =

    open System

    /// Defines Tracing information for TraceListeners
    type TraceData =
        | ImportantMessage of string
        | ErrorMessage of string
        | LogMessage of string * bool
        | TraceMessage of string * bool


    /// Defines a TraceListener interface
    type ITraceListener = 
        abstract Write : TraceData -> unit

    /// A default color map which maps TracePriorities to ConsoleColors
    let colorMap traceData = 
        match traceData with
        | ImportantMessage _ -> ConsoleColor.Yellow
        | ErrorMessage _ -> ConsoleColor.Red
        | LogMessage _ -> ConsoleColor.Gray
        | TraceMessage _ -> ConsoleColor.Green

    /// Implements a TraceListener for System.Console.
    /// ## Parameters
    ///  - `importantMessagesToStdErr` - Defines whether to trace important messages to StdErr.
    ///  - `colorMap` - A function which maps TracePriorities to ConsoleColors.
    type ConsoleTraceListener(colorMap) =
        let writeText stdErr color newLine text = 
            let curColor = Console.ForegroundColor
            try
              if curColor <> color then Console.ForegroundColor <- color
              let printer =
                match stdErr, newLine with
                | false, true -> printfn
                | false, false -> printf
                | true, true -> eprintfn
                | true, false -> eprintf
              printer "%s" text
            finally
              if curColor <> color then Console.ForegroundColor <- curColor
    
        interface ITraceListener with
            /// Writes the given message to the Console.
            member this.Write msg = 
                let color = colorMap msg
                match msg with
                | ImportantMessage text | ErrorMessage text ->
                    writeText true color true text
                | LogMessage(text, newLine) | TraceMessage(text, newLine) ->
                    writeText false color newLine text

    /// The default TraceListener for Console.
    let defaultConsoleTraceListener = ConsoleTraceListener(colorMap) :> ITraceListener

    /// A List with all registered listeners
    let listeners = new Collections.Generic.List<ITraceListener>()

    // register listeners
    listeners.Add defaultConsoleTraceListener

    /// Allows to post messages to all trace listeners
    let postMessage x = listeners.ForEach(fun listener -> listener.Write x)

    /// Logs the specified string        
    let log message = LogMessage(message, true) |> postMessage

    /// Writes a trace to the command line (in green)
    let trace message = postMessage (TraceMessage(message, true))

    /// Writes a message to the command line (in green)
    let tracefn fmt = Printf.ksprintf trace fmt

    let logfn fmt = Printf.ksprintf log fmt

    /// Logs the given files with the message.
    let logItems message items = items |> Seq.iter (log << sprintf "%s%s" message)


[<RequireQualifiedAccess>]
module ProcessUtils =

    /// Searches the given directories for all occurrences of the given file name
    let findFiles dirs file = 
        let files = 
            dirs
            |> Seq.map (fun (path : string) -> 
                   let replacedPath = 
                       path
                           .Replace("[ProgramFiles]"   , Environment.ProgramFiles   )
                           .Replace("[ProgramFilesX86]", Environment.ProgramFilesX86)
                           .Replace("[SystemRoot]"     , Environment.SystemRoot     )
                   try
                       let dir =
                           replacedPath   
                           |> DirectoryInfo.ofPath
                       if not dir.Exists then ""
                       else 
                           let fi = FileInfo.ofPath (dir.FullName @@ file)
                           if fi.Exists then fi.FullName else ""
                   with e -> raise <| exn(sprintf "Error while trying to find files like '%s' in path '%s' (replaced '%s'). Please report this issue to FAKE and reference https://github.com/fsharp/FAKE/issues/2136." file path replacedPath, e))
            |> Seq.filter ((<>) "")
            |> Seq.cache
        files

    /// Searches the given directories for all occurrences of the given file name
    let tryFindFile dirs file =
        let files = findFiles dirs file
        if not (Seq.isEmpty files) then Some (Seq.head files)
        else None

    /// Searches in PATH for the given file and returnes the result ordered by precendence
    let findFilesOnPath (file : string) : string seq =
        Environment.pathDirectories
        |> Seq.filter Path.isValidPath
        |> Seq.append [ "." ]
        |> fun path ->
            // See https://unix.stackexchange.com/questions/280528/is-there-a-unix-equivalent-of-the-windows-environment-variable-pathext
            if Environment.isWindows then
                // Prefer PATHEXT, see https://github.com/fsharp/FAKE/issues/1911
                // and https://github.com/fsharp/FAKE/issues/1899
                (Environment.environVarOrDefault "PATHEXT" ".COM;.EXE;.BAT")
                    .Split [|';'|]
                    |> Seq.collect (fun postFix -> findFiles path (file + postFix))
                    |> fun findings -> Seq.append findings (findFiles path file)
            else findFiles path file

    /// Searches the current directory and the directories within the PATH
    /// environment variable for the given file. If successful returns the full
    /// path to the file.
    /// ## Parameters
    ///  - `file` - The file to locate
    let tryFindFileOnPath (file : string) : string option = findFilesOnPath file |> Seq.tryHead

    /// Tries to find the tool via AppSettings. If no path has the right tool we are trying the PATH system variable.
    let tryFindPath fallbackValue tool =
        match tryFindFile fallbackValue tool with
        | None -> tryFindFileOnPath tool
        | x -> x

    /// Tries to find the tool via AppSettings. If no path has the right tool we are trying the PATH system variable.
    let findPath fallbackValue tool = defaultArg (tryFindPath fallbackValue tool) tool



type ProcessResult = { ExitCode : int; stdout : string; stderr : string }

let executeProcess (exe:string, cmdline:string, workingDir, (env:(string*string) list option)) =
    let psi =
        System.Diagnostics.ProcessStartInfo (exe, cmdline,
            UseShellExecute = false,
            RedirectStandardOutput = true,
            RedirectStandardError = true,
            CreateNoWindow = true,
            WorkingDirectory = defaultArg workingDir "")
    match env with 
    | Some pe->
        for var in pe do
            psi.EnvironmentVariables.Add (var) 
    | None -> ()
    let p = System.Diagnostics.Process.Start psi
    let output = new ResizeArray<_> ()
    let error  = new ResizeArray<_> ()
    p.OutputDataReceived.Add (fun args -> output.Add args.Data |> ignore)
    p.ErrorDataReceived.Add  (fun args -> error.Add  args.Data |> ignore)
    p.BeginErrorReadLine ()
    p.BeginOutputReadLine ()
    p.WaitForExit ()
    { ExitCode = p.ExitCode; stdout = String.toLines output; stderr = String.toLines error }




/// Contains tasks which allow to run FSharp.Formatting for generating documentation.
module FSFormatting =

    /// Specifies the fsformatting executable

    /// Runs fsformatting.exe with the given command in the given repository directory.
    let private run env command =
        let result = executeProcess ("dotnet", sprintf "tool run fsdocs %s" command, None, Some env)
        if 0 <> result.ExitCode
        then
            failwithf
                "fsdocs command %s failed with: %s" 
                command
                // workaround (fsformatting doesn't output errors to stderr)
                (if System.String.IsNullOrEmpty result.stderr then result.stdout else result.stderr)

    type FsDocsArguments =
        { SourceRepository : string
          Input: string
          OutputDirectory : string 
          TargetPath : string
          ProjectParameters : (string * string) list
          Projects : string }

    let defaultMetadataFormatArguments =
        { SourceRepository = ""
          OutputDirectory = "docs/"
          Projects = ""
          TargetPath = ""
          Input = "docsrc/content/"
          ProjectParameters = [] }

    let buildDocs (p:FsDocsArguments->FsDocsArguments) =
        let arguments = p defaultMetadataFormatArguments
        let projectParameters = arguments.ProjectParameters
        let env = ["TargetPath",arguments.TargetPath]
        projectParameters
            |> Seq.collect (fun (k, v) -> [ k; v ])
            |> Seq.append
                    ([  "build"; 
                        "--input"; arguments.Input
                        "--output"; arguments.OutputDirectory
                        "--sourcerepo"; arguments.SourceRepository
                        "--projects"; arguments.Projects
                        "--parameters" ])
            |> Seq.map (fun s -> if s.StartsWith "\"" then s else sprintf "\"%s\"" s)
            |> String.separated " "
            |> run env
        printfn "Successfully generated docs for : %s" arguments.Projects



/// Contains functions which allow basic operations on git repositories.
/// All operations assume that the CommandHelper can find git.exe.
module Git =
    open System

    let private GitPath = @"[ProgramFiles]\Git\cmd\;[ProgramFilesX86]\Git\cmd\;[ProgramFiles]\Git\bin\;[ProgramFilesX86]\Git\bin\;"

    /// Tries to locate the git.exe via the eviroment variable "GIT".
    let gitPath =
        if Environment.isUnix then "git"
        else
            let ev = Environment.environVar "GIT"
            if not (String.IsNullOrEmpty ev) then ev else ProcessUtils.findPath (GitPath.Split([| ';' |], StringSplitOptions.RemoveEmptyEntries)) "git.exe"

    /// Runs git.exe with the given command in the given repository directory.
    let runGitCommand repositoryDir command =
        let processResult = executeProcess (gitPath, command, Some repositoryDir, None)
        processResult.ExitCode = 0, [processResult.stdout], processResult.stderr

    /// Runs the given git command, waits for its completion and returns whether it succeeded.
    let directRunGitCommand repositoryDir command =
        let result = executeProcess (gitPath, command, Some repositoryDir, None)
        result.ExitCode = 0

    /// Runs the given git command, waits for its completion and fails when it didn't succeeded.
    let directRunGitCommandAndFail repositoryDir command =
        let result = executeProcess (gitPath, command, Some repositoryDir, None)
        result.ExitCode = 0
        |> fun ok -> if not ok then failwithf "Command failed. \n%A\n%A\n" result.stdout result.stderr

    /// Runs the git command and returns the first line of the result.
    let runSimpleGitCommand repositoryDir command =
        try
            let _,msg,errors = runGitCommand repositoryDir command
            let errorText = String.toLines msg + Environment.NewLine + errors
            if errorText.Contains "fatal: " then failwith errorText
            if msg.Length = 0 then "" else
            msg |> Seq.iter (Trace.logfn "%s")
            msg.[0]
        with exn -> failwithf "Could not run \"git %s\".\r\nError: %s" command exn.Message


    module Branches =
        /// Pushes all branches to the default remote.
        /// ## Parameters
        ///
        ///  - `repositoryDir` - The git repository.
        let push repositoryDir = directRunGitCommandAndFail repositoryDir "push"

    module Staging =
        /// Adds all files to the staging area
        let stageAll repositoryDir =
            "add . --all"
            |> runSimpleGitCommand repositoryDir
            |> Trace.trace

    module Commit =
        /// Commits all files in the given repository with the given message
        let exec repositoryDir message =
            sprintf "commit -m \"%s\"" message
            |> runSimpleGitCommand repositoryDir
            |> Trace.trace

    module Repository =
        /// Clones a single branch of a git repository.
        /// ## Parameters
        ///
        ///  - `workingDir` - The working directory.
        ///  - `repoUrl` - The URL to the origin.
        ///  - `branchname` - Specifes the target branch.
        ///  - `toPath` - Specifes the new target subfolder.
        let cloneSingleBranch workingDir repoUrl branchName toPath =
            sprintf "clone -b %s --single-branch %s %s" branchName repoUrl toPath
            |> runSimpleGitCommand workingDir
            |> Trace.trace
    module Config = 
        /// Get remote origin url
        /// ## Parameters
        ///
        ///  - `workingDir` - The working directory.
        let remoteOriginUrl workingDir =
            let url = 
                "config --get remote.origin.url"
                |> runSimpleGitCommand workingDir
            url.Trim([|'\n';'\r';'\t';' '|])
    


module DotNet =


    /// Runs dotnet.exe with the given command in the given repository directory.
    let runDotnetCommand repositoryDir command =
        let processResult = executeProcess ("dotnet", command, Some repositoryDir, None)
        processResult.ExitCode = 0, [processResult.stdout], processResult.stderr

    /// Runs the dotnet command and returns the first line of the result.
    let runSimpleDotnetCommand repositoryDir command =
        try
            let _,msg,errors = runDotnetCommand repositoryDir command
            let errorText = String.toLines msg + System.Environment.NewLine + errors
            if errorText.Contains "fatal: " then failwith errorText
            if msg.Length = 0 then "" else
            msg |> Seq.iter (Trace.logfn "%s")
            msg.[0]
        with exn -> failwithf "Could not run \"git %s\".\r\nError: %s" command exn.Message




module Target =
    let create str fn = 
        printfn "TARGET  --->  %s" str
        fn ()