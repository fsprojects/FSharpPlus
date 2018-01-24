// --------------------------------------------------------------------------------------
// Builds the documentation from `.fsx` and `.md` files in the 'docsrc/content' directory
// (the generated documentation is stored in the 'docs' directory)
// --------------------------------------------------------------------------------------

// Binaries that have XML documentation (in a corresponding generated XML file)
// Any binary output / copied to bin/projectName/projectName.dll will
// automatically be added as a binary to generate API docs for.
// for binaries output to root bin folder please add the filename only to the 
// referenceBinaries list below in order to generate documentation for the binaries.
// (This is the original behaviour of ProjectScaffold prior to multi project support)
let referenceBinaries = []
// Web site location for the generated documentation
let website = "/FSharpPlus"

let githubLink = "https://github.com/gusty/FSharpPlus"

// Specify more information about your project
let info =
  [ "project-name", "FSharpPlus"
    "project-author", "Gusty"
    "project-summary", "F#+ is a base library for F#."
    "project-github", githubLink
    "project-nuget", "http://nuget.org/packages/FSharpPlus" ]

// --------------------------------------------------------------------------------------
// For typical project, no changes are needed below
// --------------------------------------------------------------------------------------

#load "../../packages/FSharp.Formatting/FSharp.Formatting.fsx"
#I "../../packages/FAKE/tools/"

#I "../../bin/FSharpPlus/net45/"

#r "FSharpPlus.dll"
#r "FakeLib.dll"
open Fake
open System.IO
open Fake.FileHelper
open FSharp.Literate
open FSharp.MetadataFormat
open FSharp.Markdown
open FSharpPlus

// When called from 'build.fsx', use the public project URL as <root>
// otherwise, use the current 'output' directory.
#if RELEASE
let root = website
#else
let root = "file://" + (__SOURCE_DIRECTORY__ @@ "../../docs")
#endif

// Paths with template/source/output locations
let bin        = __SOURCE_DIRECTORY__ @@ "../../bin"
let content    = __SOURCE_DIRECTORY__ @@ "../content"
let output     = __SOURCE_DIRECTORY__ @@ "../../docs"
let files      = __SOURCE_DIRECTORY__ @@ "../files"
let templates  = __SOURCE_DIRECTORY__ @@ "templates"
let formatting = __SOURCE_DIRECTORY__ @@ "../../packages/FSharp.Formatting/"
let docTemplate = "docpage.cshtml"

// Where to look for *.csproj templates (in this order)
let layoutRootsAll = new System.Collections.Generic.Dictionary<string, string list>()
layoutRootsAll.Add("en",[ templates; formatting @@ "templates"
                          formatting @@ "templates/reference" ])
subDirectories (directoryInfo templates)
|> Seq.iter (fun d ->
                let name = d.Name
                if name.Length = 2 || name.Length = 3 then
                    layoutRootsAll.Add(
                            name, [templates @@ name
                                   formatting @@ "templates"
                                   formatting @@ "templates/reference" ]))

// Copy static files and CSS + JS from F# Formatting
let copyFiles () =
  CopyRecursive files output true |> Log "Copying file: "
  ensureDirectory (output @@ "content")
  CopyRecursive (formatting @@ "styles") (output @@ "content") true 
    |> Log "Copying styles and scripts: "

let binaries =
    let manuallyAdded = 
        referenceBinaries 
        |> List.map (fun b -> bin @@ b)
    
    let conventionBased = 
        directoryInfo bin 
        |> subDirectories
        |> Array.map (fun d -> d.FullName @@ "net45" @@(sprintf "%s.dll" d.Name))
        |> List.ofArray

    conventionBased @ manuallyAdded

let libDirs =
    let conventionBasedbinDirs =
        directoryInfo bin 
        |> subDirectories
        |> Array.map (fun d -> d.FullName)
        |> List.ofArray

    conventionBasedbinDirs @ [bin]

// Build API reference from XML comments
let buildReference () =
  CleanDir (output @@ "reference")
  MetadataFormat.Generate
    ( binaries, output @@ "reference", layoutRootsAll.["en"],
      parameters = ("root", root)::info,
      sourceRepo = githubLink @@ "tree/master",
      sourceFolder = __SOURCE_DIRECTORY__ @@ ".." @@ "..",
      publicOnly = true,libDirs = libDirs )

  // Exclude some Namespaces from the index
  let pathIndex = output @@ "reference" @@ "index.html"
  printfn "%s" pathIndex
  let ndx = 
    File.ReadAllLines pathIndex
    |> split [[|System.String.Empty|]]
    |> filter (fun x -> not (x |> exists (fun e -> e.Contains("BaseLib"))))
    |> filter (fun x -> not (x |> exists (fun e -> e.Contains("FSharpPlus.Internals Namespace"))))
    |> intercalate [|System.String.Empty|]
  File.WriteAllLines(pathIndex, ndx)


// PlantUml processing
open System.IO.Compression
open System.Text


let toUrl input =
    let encodeByte b =
        if   b < 10uy then 48uy + b
        elif b < 36uy then 55uy + b
        elif b < 62uy then 61uy + b
        elif b = 62uy then byte '-'
        elif b = 63uy then byte '_'
        else               byte '?'

    let encode3Bytes b1 b2 b3 =
        let c1 =  b1 >>> 2
        let c2 = (b2 >>> 4) ||| (b1 &&& 0x3uy <<< 4)
        let c3 = (b3 >>> 6) ||| (b2 &&& 0xFuy <<< 2)
        let c4 =                 b3 &&& 0x3Fuy
        [|
            encodeByte (c1 &&& 0x3Fuy)
            encodeByte (c2 &&& 0x3Fuy)
            encodeByte (c3 &&& 0x3Fuy)
            encodeByte (c4 &&& 0x3Fuy)
        |] |> Encoding.ASCII.GetChars

    let encode bytes =
        let c = Array.length bytes
        let s = StringBuilder ()
        for i in 0..3..c-1 do
            let b1 =                   bytes.[i]
            let b2 = if c > i + 1 then bytes.[i + 1] else 0uy
            let b3 = if c > i + 2 then bytes.[i + 2] else 0uy
            encode3Bytes b1 b2 b3 |> s.Append |> ignore
        string s

    use output = new MemoryStream ()
    let writeFrom (input:string) output =
        use writer = new StreamWriter(new DeflateStream(output, CompressionLevel.Optimal), Encoding.UTF8)
        writer.Write input
    output |> writeFrom input |> ignore
    output.ToArray () |> encode

let abstractions = content </> "abstractions.plantuml"
let plantUMLDiag = toUrl (File.ReadAllText abstractions)
let customize _ (doc:LiterateDocument) = doc.With (paragraphs = (doc.Paragraphs |>> function InlineBlock x -> InlineBlock (replace "{plantUMLDiag}" plantUMLDiag x) | x -> x))


// Build documentation from `fsx` and `md` files in `docs/content`
let buildDocumentation () =

  // First, process files which are placed in the content root directory.

  Literate.ProcessDirectory
    ( content, docTemplate, output, replacements = ("root", root)::info,
      layoutRoots = layoutRootsAll.["en"],
      generateAnchors = true,
      processRecursive = false,
      customizeDocument = customize
      )

  // And then process files which are placed in the sub directories
  // (some sub directories might be for specific language).

  let subdirs = Directory.EnumerateDirectories(content, "*", SearchOption.TopDirectoryOnly)
  for dir in subdirs do
    let dirname = (new DirectoryInfo(dir)).Name
    let layoutRoots =
        // Check whether this directory name is for specific language
        let key = layoutRootsAll.Keys
                  |> Seq.tryFind (fun i -> i = dirname)
        match key with
        | Some lang -> layoutRootsAll.[lang]
        | None -> layoutRootsAll.["en"] // "en" is the default language

    Literate.ProcessDirectory
      ( dir, docTemplate, output @@ dirname, replacements = ("root", root)::info,
        layoutRoots = layoutRoots,
        generateAnchors = true )

// Generate
copyFiles()
#if HELP
buildDocumentation()
#endif
#if REFERENCE
buildReference()
#endif