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

let githubLink = "https://github.com/fsprojects/FSharpPlus"

#load "./templates/template.fsx"
open Template
#load "./templates/plantuml.fsx"
open Plantuml
#load "./tools.fsx"
open Tools
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
#load "../../.paket/load/netstandard2.0/docs/MathNet.Numerics.FSharp.fsx"
#I "../../packages/FSharp.Core/lib/netstandard2.0/"
#I "../../bin/FSharpPlus/netstandard2.0/"
#I @"../../src/FSharpPlus/bin/Release/netstandard2.0/"

#r "FSharp.Core.dll"
#r "FSharpPlus.dll"
open System
open FSharp.Literate
open FSharp.Markdown
open FSharpPlus

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
  Directory.copyRecursive Path.files Path.output
  Directory.ensure (Path.output </> "content")
  //Directory.copyRecursive (Path.formatting </> "styles") (Path.output </> "content")  

// Build documentation from `fsx` and `md` files in `docs/content`
let buildDocumentation () =
  Directory.copyRecursive Path.files Path.output
  IO.Directory.EnumerateFiles Path.content
  |> Seq.iter (processFile Path.output)
// Generate
copyFiles()
buildDocumentation()
