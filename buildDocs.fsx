// Generate and Release Docs
// =========================
//
// Usage:
//
// Generate Docs 
//    -> fsi buildDocs.fsx
// Generate & Release Docs
//    -> fsi buildDocs.fsx ReleaseDocs
//


// Script Based on ProjectScaffold

#load "docsrc/tools/doclib.fsx"

#load "docsrc/tools/templates/plantuml.fsx"

open Doclib


// --------------------------------------------------------------------------------------
// START TODO: Provide project-specific details below
// --------------------------------------------------------------------------------------

// Information about the project are used
//  - to publish documentation on GitHub gh-pages
//  - for documentation, you also need to edit info in "docsrc/tools/generate.fsx"

// Git configuration (used for publishing documentation in gh-pages branch)
// The profile where the project is posted
let gitOwner = "fsprojects"
let gitHome = sprintf "%s/%s" "https://github.com" gitOwner

// The name of the project on GitHub
let gitName = "FSharpPlus"

let website = "/FSharpPlus"

let github_release_user = Environment.environVarOrDefault "github_release_user" gitOwner
let githubLink = sprintf "https://github.com/%s/%s" github_release_user gitName

// Specify more information about your project
let info =
  [ "project-name", "FSharpPlus"
    "project-author", "Gusty"
    "project-summary", "F#+ is a base library for F#."
    "project-github", githubLink
    "project-nuget", "http://nuget.org/packages/FSharpPlus" ]

// --------------------------------------------------------------------------------------
// END TODO: The rest of the file includes standard build steps
// --------------------------------------------------------------------------------------

// Read additional information from the release notes document
let release = ReleaseNotes.load "RELEASE_NOTES.md"


Target.create "CleanDocs" (fun _ ->
    Shell.cleanDirs ["docs"]
)


// --------------------------------------------------------------------------------------
// Build project

Target.create "Build" (fun _ ->
    DotNet.runSimpleDotnetCommand 
        __SOURCE_DIRECTORY__ 
        ("pack build.proj")
        |> Trace.trace
)


// --------------------------------------------------------------------------------------
// Restore FSharp.Formatting

let fsFormattingVersion = "3.1.0"
let tempDocsDir = "temp/tools"
Directory.ensure tempDocsDir

System.IO.File.WriteAllText(tempDocsDir @@ "docs.proj", """
<Project Sdk="Microsoft.NET.Sdk">
  <PropertyGroup>
    <TargetFramework>net47</TargetFramework>
    <RestorePackagesPath>packages</RestorePackagesPath>
  </PropertyGroup>
<ItemGroup>
    <PackageReference Include="FSharp.Formatting" Version="pinnedVersion">
        <PrivateAssets>all</PrivateAssets>
    </PackageReference>
    <PackageReference Include="FSharp.Formatting.CommandTool" Version="pinnedVersion">
        <PrivateAssets>all</PrivateAssets>
    </PackageReference>
</ItemGroup>
</Project>
""".Replace("pinnedVersion", fsFormattingVersion))

DotNet.runSimpleDotnetCommand 
    tempDocsDir 
    ("add package FSharp.Formatting -s https://ci.appveyor.com/nuget/fsharp-formatting -v " + fsFormattingVersion)
    |> Trace.trace

DotNet.runSimpleDotnetCommand
    tempDocsDir
    ("add package FSharp.Formatting.CommandTool -s https://ci.appveyor.com/nuget/fsharp-formatting -v " + fsFormattingVersion)
    |> Trace.trace


// --------------------------------------------------------------------------------------
// Generate the documentation

// Paths with template/source/output locations
let bin        = __SOURCE_DIRECTORY__ @@ "src"
let content    = __SOURCE_DIRECTORY__ @@ "docsrc/content"
let output     = __SOURCE_DIRECTORY__ @@ "docs"
let files      = __SOURCE_DIRECTORY__ @@ "docsrc/files"
let templates  = __SOURCE_DIRECTORY__ @@ "docsrc/tools/templates"
let formatting = __SOURCE_DIRECTORY__ @@ tempDocsDir @@ "packages" @@ "FSharp.Formatting" @@ fsFormattingVersion
let docTemplate = "docpage.cshtml"


let root = website

let referenceBinaries = []

let layoutRootsAll = new System.Collections.Generic.Dictionary<string, string list>()
layoutRootsAll.Add("en",[   templates; 
                            formatting @@ "templates"
                            formatting @@ "templates/reference" ])

Target.create "ReferenceDocs" (fun _ ->
    Directory.ensure (output @@ "reference")

    let binaries () =
        let manuallyAdded = referenceBinaries |> List.map (fun b -> bin @@ b)
   
        let conventionBased = 
            DirectoryInfo.getSubDirectories <| System.IO.DirectoryInfo bin
            |> Array.filter (fun x -> not ( x.FullName.EndsWith("FSharpPlus.Samples")))
            |> Array.collect (fun d ->
                let (name, d) =
                    let net45Bin = DirectoryInfo.getSubDirectories (DirectoryInfo.ofPath (d.FullName @@ "bin" @@ "Release")) |> Array.filter (fun x -> x.FullName.ToLower().Contains("net45"))
                    let net47Bin = DirectoryInfo.getSubDirectories (DirectoryInfo.ofPath (d.FullName @@ "bin" @@ "Release")) |> Array.filter (fun x -> x.FullName.ToLower().Contains("net47"))
                    if net45Bin.Length = 0 && net47Bin.Length = 0 then failwith "Failure: No binaries found."
                    if net45Bin.Length > 0 then d.Name, net45Bin.[0]
                    else d.Name, net47Bin.[0]
                d.GetFiles ()
                |> Array.filter (fun x -> x.Name.ToLower() = (sprintf "%s.dll" name).ToLower())
                |> Array.map (fun x -> x.FullName))
            |> List.ofArray

        conventionBased @ manuallyAdded

    binaries()
    |> FSFormatting.createDocsForDlls (fun args ->
        { args with
            OutputDirectory = output @@ "reference"
            LayoutRoots =  layoutRootsAll.["en"]
            ProjectParameters =  ("root", root)::info
            SourceRepository = githubLink @@ "tree/master" }
           )
)

let copyFiles () =
    Shell.copyRecursive files output true 
    |> Trace.logItems "Copying file: "
    Directory.ensure (output @@ "content")
    Shell.copyRecursive (formatting @@ "styles") (output @@ "content") true 
    |> Trace.logItems "Copying styles and scripts: "
        
Target.create "Docs" (fun _ ->
    System.IO.File.Delete "docsrc/content/release-notes.md"
    Shell.copyFile "docsrc/content/" "RELEASE_NOTES.md"
    Shell.rename "docsrc/content/release-notes.md" "docsrc/content/RELEASE_NOTES.md"

    System.IO.File.Delete "docsrc/content/license.md"
    Shell.copyFile "docsrc/content/" "LICENSE.txt"
    Shell.rename "docsrc/content/license.md" "docsrc/content/LICENSE.txt"

    
    DirectoryInfo.getSubDirectories (DirectoryInfo.ofPath templates)
    |> Seq.iter (fun d ->
                    let name = d.Name
                    if name.Length = 2 || name.Length = 3 then
                        layoutRootsAll.Add(
                                name, [templates @@ name
                                       formatting @@ "templates"
                                       formatting @@ "templates/reference" ]))
    copyFiles ()
    
    for dir in  [ content; ] do
        let langSpecificPath(lang, path:string) =
            path.Split([|'/'; '\\'|], System.StringSplitOptions.RemoveEmptyEntries)
            |> Array.exists(fun i -> i = lang)
        let layoutRoots =
            let key = layoutRootsAll.Keys |> Seq.tryFind (fun i -> langSpecificPath(i, dir))
            match key with
            | Some lang -> layoutRootsAll.[lang]
            | None -> layoutRootsAll.["en"] // "en" is the default language

        FSFormatting.createDocs (fun args ->
            { args with
                Source = content
                OutputDirectory = output 
                LayoutRoots = layoutRoots
                ProjectParameters  = ("root", root)::info
                Template = docTemplate } )
)

// --------------------------------------------------------------------------------------
// Post process here:

// Inject PlantUml
    
open Plantuml
let plantUmlDiag = templates @@ "abstractions.plantuml"
let abstractions = output    @@ "abstractions.html"
let plantUMLDiag = toUrl (System.IO.File.ReadAllText plantUmlDiag)
let abstractionsText = System.IO.File.ReadAllText abstractions
System.IO.File.WriteAllText (abstractions, abstractionsText.Replace ("{plantUMLDiag}", plantUMLDiag))



// --------------------------------------------------------------------------------------
// Release Scripts

if Array.contains "ReleaseDocs" fsi.CommandLineArgs then

    Target.create "ReleaseDocs" (fun _ ->
        let tempDocsDir = "temp/gh-pages"
        Shell.cleanDir tempDocsDir
        Git.Repository.cloneSingleBranch "" (gitHome + "/" + gitName + ".git") "gh-pages" tempDocsDir
        Shell.copyRecursive "docs" tempDocsDir true |> Trace.tracefn "%A"
        Git.Staging.stageAll tempDocsDir
        Git.Commit.exec tempDocsDir (sprintf "Update generated documentation for version %s" release.NugetVersion)
        Git.Branches.push tempDocsDir
    )

    Target.create "GenerateDocs" ignore