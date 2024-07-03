// Learn more about F# at http://fsharp.org

open System

open DocLib


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

let rootDir = __SOURCE_DIRECTORY__ @@ ".." @@ ".."


// --------------------------------------------------------------------------------------
// Generate the documentation

open Tools.Path

Target.create "Build" (fun _ ->
    let root = website+"/"
    FSFormatting.buildDocs (fun args ->
        { args with
            OutputDirectory = output
            ProjectParameters =  ("root", root)::info
            Projects = rootDir @@ "src/FSharpPlus/FSharpPlus.fsproj"
            TargetPath = rootDir @@ "src/FSharpPlus/bin/Release/net8.0/FSharpPlus.dll"
            SourceRepository = githubLink @@ "tree/master" }
           )
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




[<EntryPoint>]
let main argv =
    // Generate
    


    // --------------------------------------------------------------------------------------
    // Release Scripts

    if Array.contains "ReleaseDocs" argv then

        Target.create "ReleaseDocs" (fun _ ->
            let tempDocsDir = rootDir @@ "temp/gh-pages"
            Shell.cleanDir tempDocsDir
            let repoUrl = Git.Config.remoteOriginUrl rootDir
            Git.Repository.cloneSingleBranch rootDir repoUrl "gh-pages" tempDocsDir
            let docDir = rootDir @@ "docs"
            Shell.copyRecursive docDir tempDocsDir true |> Trace.tracefn "%A"
            Git.Staging.stageAll tempDocsDir
            Git.Commit.exec tempDocsDir (sprintf "Update generated documentation for version %s" release.NugetVersion)
            Git.Branches.push tempDocsDir
        )

        Target.create "GenerateDocs" ignore
    0 // return an integer exit code
