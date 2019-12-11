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

let githubLink = "https://github.com/fsprojects/FSharpPlus"

#load "./templates/template.fsx"
open Template
#load "./templates/plantuml.fsx"
open Plantuml
// Specify more information about your project

let properties:PropertyMeta =
    { Name = "FSharpPlus"
      Description = "F#+ is a base library for F#."
      Author = "Gusty"
      Github = githubLink
      NuGet = "http://nuget.org/packages/FSharpPlus" 
      Body = ""
      Title = ""
      Root =  fun v-> "."+v }
// --------------------------------------------------------------------------------------
// For typical project, no changes are needed below
// --------------------------------------------------------------------------------------
#load "../../.paket/load/netstandard2.0/docs/FSharp.Literate.fsx"
#load "../../.paket/load/netstandard2.0/docs/Fable.React.fsx"
#I "../../packages/docs/FAKE/tools/"
#I "../../packages/FSharp.Core/lib/net45/"
#I "../../bin/FSharpPlus/net45/"
#I @"../../src/FSharpPlus/bin/Release/net45/"

#r "FSharp.Core.dll"
#r "FSharpPlus.dll"
#r "FakeLib.dll"
open Fake
open System
open Fake.FileHelper
open FSharp.Literate
open FSharp.Markdown
open FSharpPlus

// When called from 'build.fsx', use the public project URL as <root>
// otherwise, use the current 'output' directory.
#if RELEASE
let root = website
#else
let root = "file://" + (__SOURCE_DIRECTORY__ @@ "../../docs")
#endif


// Copy static files and CSS + JS from F# Formatting

let (</>) x y = IO.Path.Combine(x,y)
module Path =
    // Paths with template/source/output locations
    let bin        = __SOURCE_DIRECTORY__ @@ "../../src/FSharpPlus/bin/Release/net45/"
    let content    = __SOURCE_DIRECTORY__ @@ "../content"
    let output     = __SOURCE_DIRECTORY__ @@ "../../docs"
    let files      = __SOURCE_DIRECTORY__ @@ "../files"
    let templates      = __SOURCE_DIRECTORY__ @@ "./templates"
    let formatting = __SOURCE_DIRECTORY__ @@ "../../packages/FSharp.Formatting/"

    let dir p = IO.Path.GetDirectoryName(p: string)
    let filename p = IO.Path.GetFileName(p: string)
    let changeExt ext p = IO.Path.ChangeExtension(p, ext)

module Directory =
    let ensure dir =
        if not (IO.Directory.Exists dir) then
            IO.Directory.CreateDirectory dir |> ignore

    let copyRecursive (path: string) dest =
        let path =
            if not (path.EndsWith(string IO.Path.DirectorySeparatorChar)) then
                path + string IO.Path.DirectorySeparatorChar
            else
                path
        let trim (p: string) =
            if p.StartsWith(path) then
                p.Substring(path.Length)
            else
                failwithf "Cannot find path root"
        IO.Directory.EnumerateFiles(path, "*", IO.SearchOption.AllDirectories)
        |> Seq.iter (fun p ->
            let target = dest </> trim p
            ensure(Path.dir target)
            IO.File.Copy(p, target, true))

let write path html =
    use writer = System.IO.File.CreateText(path)
    Fable.ReactServer.Raw.writeTo  writer (Fable.ReactServer.castHTMLNode html)

let docPackagePath  path =
    __SOURCE_DIRECTORY__ + @"/../../packages/docs/" + path
let includeDir path =
    "-I:" + docPackagePath path
let reference path =
    "-r:" + docPackagePath path
let evaluationOptions = 
    [| 
         includeDir "FSharp.Core/lib/netstandard2.0/"
         includeDir "FSharp.Literate/lib/netstandard2.0/" 
         includeDir "FSharp.Compiler.Service/lib/netstandard2.0/" 
         reference "FSharp.Compiler.Service/lib/netstandard2.0/FSharp.Compiler.Service.dll" |] 

let compilerOptions = 
    String.concat " " ( 
         "-r:System.Runtime"
         :: "-r:System.Net.WebClient"
         :: "-r:System.Runtime.Extensions"
         :: Array.toList evaluationOptions)

// PlantUml processing
let abstractions = Path.templates </> "abstractions.plantuml"
let plantUMLDiag = toUrl (IO.File.ReadAllText abstractions)
let customize (doc:LiterateDocument) = doc.With (paragraphs = (doc.Paragraphs |>> function InlineBlock (x,y) -> InlineBlock ((replace "{plantUMLDiag}" plantUMLDiag x),y) | x -> x))

let parseFsx path =

    let doc = 
      Literate.ParseScriptFile(
                  path = path,
                  compilerOptions = compilerOptions,
                  fsiEvaluator = FSharp.Literate.FsiEvaluator(evaluationOptions))
    
    let body = FSharp.Literate.Literate.FormatLiterateNodes(doc, OutputKind.Html, "", true, true) |> customize
    for err in doc.Errors do
        Printf.printfn "%A" err
    body, body.FormattedTips
    

let parseMd path =
    let doc = 
      Literate.ParseMarkdownFile(
                  path,
                  compilerOptions = compilerOptions,
                  fsiEvaluator = FSharp.Literate.FsiEvaluator(evaluationOptions))
    let body = FSharp.Literate.Literate.FormatLiterateNodes(doc, OutputKind.Html, "", true, true) |> customize
    for err in doc.Errors do
        Printf.printfn "%A" err
    body, body.FormattedTips

let format (doc: LiterateDocument) =
    Formatting.format doc.MarkdownDocument true OutputKind.Html

let processFile outdir path  =
    printfn "Processing help: %s" path
    let outfile = 
        let name = path |> Path.filename |> Path.changeExt ".html"
        outdir </> name

    let parse = 
        match IO.Path.GetExtension(path) with
        | ".fsx" -> parseFsx
        | ".md" -> parseMd
        | ext -> failwithf "Unable to process doc for %s files" ext

    let body, tips = 
        parse path
    let t =
        { properties with
            Body = format body
            Title = tips}
    t 
    |> template
    |> write outfile

let copyFiles () =
  CopyRecursive Path.files Path.output true |> Log "Copying file: "
  ensureDirectory (Path.output @@ "content")
  CopyRecursive (Path.formatting @@ "styles") (Path.output @@ "content") true 
    |> Log "Copying styles and scripts: "

let binaries =
    let manuallyAdded = 
        referenceBinaries 
        |> List.map (fun b -> Path.bin @@ b)
    
    let conventionBased = 
        directoryInfo Path.bin 
        |> subDirectories
        |> Array.map (fun d -> d.FullName @@ "net45" @@(sprintf "%s.dll" d.Name))
        |> List.ofArray

    conventionBased @ manuallyAdded

let libDirs =
    let conventionBasedbinDirs =
        directoryInfo Path.bin 
        |> subDirectories
        |> Array.map (fun d -> d.FullName)
        |> List.ofArray

    conventionBasedbinDirs @ [Path.bin]

// Build API reference from XML comments
let buildReference () =
  CleanDir (Path.output @@ "reference")
  (*
  RazorMetadataFormat.Generate
    ( binaries, output @@ "reference", layoutRootsAll.["en"],
      parameters = ("root", root)::info,
      sourceRepo = githubLink @@ "tree/master",
      sourceFolder = __SOURCE_DIRECTORY__ @@ ".." @@ "..",
      publicOnly = true,libDirs = libDirs )
    *)
  // Exclude some Namespaces from the index
  let pathIndex = Path.output @@ "reference" @@ "index.html"
  printfn "%s" pathIndex
  let ndx = 
    IO.File.ReadAllLines pathIndex
    |> split [[|System.String.Empty|]]
    |> filter (fun x -> not (x |> exists (fun e -> e.Contains("FSharpPlus.Control"))))
    |> filter (fun x -> not (x |> exists (fun e -> e.Contains("FSharpPlus.Internals Namespace"))))
    |> intercalate [|System.String.Empty|]
  IO.File.WriteAllLines(pathIndex, ndx)




// Build documentation from `fsx` and `md` files in `docs/content`
let buildDocumentation () =

  // First, process files which are placed in the content root directory.
    (*
  RazorLiterate.ProcessDirectory
    ( content, docTemplate, output, replacements = ("root", root)::info,
      layoutRoots = layoutRootsAll.["en"],
      generateAnchors = true,
      processRecursive = false,
      customizeDocument = customize
      )
    *)
  // And then process files which are placed in the sub directories
  // (some sub directories might be for specific language).
  Directory.copyRecursive Path.files Path.output
  //let subdirs = IO.Directory.EnumerateDirectories(content, "*", IO.SearchOption.TopDirectoryOnly)
  //printfn "Processing directories: %A" subdirs
  IO.Directory.EnumerateFiles Path.content
  |> Seq.iter (processFile Path.output)
// Generate
copyFiles()
#if HELP
buildDocumentation()
#endif
#if REFERENCE
buildReference()
#endif