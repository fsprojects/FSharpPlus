#load "./tools.fsx"
//#load "../../.paket/load/netstandard2.1/docs/FSharp.Formatting.fsx"
#load "../../packages/docs/FSharp.Formatting/FSharp.Formatting.fsx" 
#I "../../packages/FSharp.Core/lib/net45/"
#I "../../bin/FSharpPlus/net45/"
#I @"../../src/FSharpPlus/bin/Release/net45/"
#r "FSharp.Core.dll"
#r "FSharpPlus.dll"
open Tools
open System
open FSharpPlus
open FSharp.Formatting.Razor
let referenceBinaries=["FSharpPlus.dll"]
let githubLink = "https://github.com/fsprojects/FSharpPlus"

open System
let copyFiles () =
  Directory.copyRecursive Path.files Path.output
  Directory.ensure (Path.output </> "content")
  Directory.copyRecursive (Path.formatting </> "styles") (Path.output </> "content") 
// Web site location for the generated documentation
let website = "/FSharpPlus"

// When called from 'build.fsx', use the public project URL as <root>
// otherwise, use the current 'output' directory.
#if RELEASE
let root = website
#else
let root = "file://" + (__SOURCE_DIRECTORY__ </> "../../docs")
#endif
let binaries =
    let manuallyAdded = 
        referenceBinaries 
        |> List.map (fun b -> Path.bin </> b)

    manuallyAdded
let libDirs = [Path.bin]
// Build API reference from XML comments
let buildReference () =
  //Directory.clean (Path.output </> "reference")
  let layoutRootsAll = new System.Collections.Generic.Dictionary<string, string list>()
  layoutRootsAll.Add("en",[ Path.templates; Path.formatting </> "templates"
                            Path.formatting </> "templates/reference" ])
  let root =""       
  RazorMetadataFormat.Generate
    ( binaries, Path.output </> "reference", layoutRootsAll.["en"],
      parameters = [("root", root)],
      sourceRepo = (githubLink </> "tree/master"),
      sourceFolder = (__SOURCE_DIRECTORY__ </> ".." </> ".."),
      publicOnly = true,libDirs = libDirs )
  // Exclude some Namespaces from the index
  let pathIndex = Path.output </> "reference" </> "index.html"
  printfn "%s" pathIndex
  let ndx = 
    IO.File.ReadAllLines pathIndex
    |> split [[|System.String.Empty|]]
    |> filter (fun x -> not (x |> exists (fun e -> e.Contains("FSharpPlus.Control"))))
    |> filter (fun x -> not (x |> exists (fun e -> e.Contains("FSharpPlus.Internals Namespace"))))
    |> intercalate [|System.String.Empty|]
  IO.File.WriteAllLines(pathIndex, ndx)


copyFiles ()
buildReference()
