// Learn more about F# at http://fsharp.org

open System
open Generate

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
// Build project

Target.create "Build" (fun _ ->
    copyFiles()
    buildDocumentation()
)


// --------------------------------------------------------------------------------------
// Generate the documentation

let root = website

let referenceBinaries = []
open Tools.Path
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
    System.IO.File.Delete ( rootDir @@ "docsrc/content/release-notes.md" )
    Shell.copyFile (rootDir @@ "docsrc/content/") "RELEASE_NOTES.md"
    Shell.rename ( rootDir @@ "docsrc/content/release-notes.md" ) "docsrc/content/RELEASE_NOTES.md"

    System.IO.File.Delete ( rootDir @@ "docsrc/content/license.md" )
    Shell.copyFile ( rootDir @@ "docsrc/content/" ) "LICENSE.txt"
    Shell.rename ( rootDir @@ "docsrc/content/license.md" ) "docsrc/content/LICENSE.txt"

    
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




[<EntryPoint>]
let main argv =
    // Generate
    


    // --------------------------------------------------------------------------------------
    // Release Scripts

    if Array.contains "ReleaseDocs" argv then

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
    0 // return an integer exit code
