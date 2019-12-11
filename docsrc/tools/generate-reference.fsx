#load "./tools.fsx"
#I "../../packages/FSharp.Core/lib/net45/"
#I "../../bin/FSharpPlus/net45/"
#I @"../../src/FSharpPlus/bin/Release/net45/"
#r "FSharp.Core.dll"
#r "FSharpPlus.dll"
open Tools
open System
open FSharpPlus


open System
let copyFiles () =
  Directory.copyRecursive Path.files Path.output
  Directory.ensure (Path.output </> "content")
  Directory.copyRecursive (Path.formatting </> "styles") (Path.output </> "content")  

// Build API reference from XML comments
let buildReference () =
  //Directory.clean (Path.output </> "reference")
  (*
  RazorMetadataFormat.Generate
    ( binaries, output </> "reference", layoutRootsAll.["en"],
      parameters = ("root", root)::info,
      sourceRepo = githubLink </> "tree/master",
      sourceFolder = __SOURCE_DIRECTORY__ </> ".." </> "..",
      publicOnly = true,libDirs = libDirs )
    *)
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
