// --------------------------------------------------------------------------------------
// FAKE build script
// --------------------------------------------------------------------------------------

#r "paket: groupref buildfsx //"

#load ".fake/build.fsx/intellisense.fsx"

#if !FAKE
  #r "netstandard"
  #r "Facades/netstandard"
#endif

open System
open System.IO
open System.Collections.Generic
open System.Threading
open Fake
open Fake.Tools.Git
open Fake.DotNet
open Fake.IO
open Fake.IO.FileSystemOperators
open Fake.IO.Globbing.Operators
open Fake.Tools
open Fake.Core
open Fake.Api
open Fake.DotNet.Testing
open Fake.BuildServer

// --------------------------------------------------------------------------------------
// START TODO: Provide project-specific details below
// --------------------------------------------------------------------------------------

// Information about the project are used
//  - for version and project name in generated AssemblyInfo file
//  - by the generated NuGet package
//  - to run tests and to publish documentation on GitHub gh-pages
//  - for documentation, you also need to edit info in "docsrc/tools/generate.fsx"

// The name of the project
// (used by attributes in AssemblyInfo, name of a NuGet package and directory in 'src')
let project = "FSharpPlus"

// Short summary of the project
// (used as description in AssemblyInfo and as a short summary for NuGet package)
let summary = "F#+ is a base library for F#."

// Longer description of the project
// (used as a description for NuGet package; line breaks are automatically cleaned up)
let description = "A complete and extensible base library for F#."

// List of author names (for NuGet package)
let authors = [ "Gusty" ]

// Tags for your project (for NuGet package)
let tags = "FSharp Applicative Monad MonadTransformer Arrow Overloading"

// File system information
let solutionFile  = "FSharpPlus.sln"

// Default target configuration
let configuration = "Release"

// Pattern specifying assemblies to be tested using NUnit
let testAssemblies = "tests/**/bin" </> configuration </> "net452" </> "*Tests*.dll"

// Git configuration (used for publishing documentation in gh-pages branch)
// The profile where the project is posted
let gitOwner = "fsprojects"
let gitHome = sprintf "%s/%s" "https://github.com" gitOwner

// The name of the project on GitHub
let gitName = "FSharpPlus"

// The url for the raw files hosted

// --------------------------------------------------------------------------------------
// END TODO: The rest of the file includes standard build steps
// --------------------------------------------------------------------------------------

// Read additional information from the release notes document
let release = ReleaseNotes.load "RELEASE_NOTES.md"

module Util =
    let compileScript symbols outDir (fsxPath : string) =
        let dllFile = Path.ChangeExtension(Path.GetFileName fsxPath, ".dll")
        let opts = [
            yield Fsc.Out (Path.Combine(outDir, dllFile))
            yield Fsc.Target Fsc.TargetType.Library
            yield! symbols |> List.map Fsc.Define
        ]
        Fsc.compile opts [fsxPath]

    let normalizeVersion (version: string) =
        let i = version.IndexOf("-")
        if i > 0 then version.Substring(0, i) else version

    let assemblyInfo projectDir version extra =
        let version = normalizeVersion version
        let asmInfoPath = projectDir </> "AssemblyInfo.fs"
        (AssemblyInfo.Version version) :: extra
        |> AssemblyInfoFile.createFSharp asmInfoPath

// Patch build version if on AppVeyor
if BuildServer.buildServer = AppVeyor then
    AppVeyor.install false //.updateBuildVersion  (release.AssemblyVersion + "." + AppVeyor.AppVeyorEnvironment.BuildNumber)

// Helper active pattern for project types
let (|Fsproj|Csproj|Vbproj|Shproj|) (projFileName:string) =
    match projFileName with
    | f when f.EndsWith("fsproj") -> Fsproj
    | f when f.EndsWith("csproj") -> Csproj
    | f when f.EndsWith("vbproj") -> Vbproj
    | f when f.EndsWith("shproj") -> Shproj
    | _                           -> failwith (sprintf "Project file %s not supported. Unknown project type." projFileName)

// Generate assembly info files with the right version & up-to-date information
Target.create "AssemblyInfo" (fun _ ->
    let getAssemblyInfoAttributes projectName =
        [ AssemblyInfo.Title projectName
          AssemblyInfo.Product project
          AssemblyInfo.Description summary
          AssemblyInfo.Version release.AssemblyVersion
          AssemblyInfo.FileVersion release.AssemblyVersion ]

    let getProjectDetails projectPath =
        let projectName = System.IO.Path.GetFileNameWithoutExtension(projectPath)
        ( projectPath,
          projectName,
          System.IO.Path.GetDirectoryName(projectPath),
          (getAssemblyInfoAttributes projectName)
        )

    !! "src/**/*.??proj"
    |> Seq.map getProjectDetails
    |> Seq.iter (fun (projFileName, projectName, folderName, attributes) ->
        match projFileName with
        | Fsproj -> AssemblyInfoFile.createFSharp (folderName </> "AssemblyInfo.fs") attributes
        | Csproj -> AssemblyInfoFile.createCSharp ((folderName </> "Properties") </> "AssemblyInfo.cs") attributes
        | Vbproj -> AssemblyInfoFile.createVisualBasic ((folderName </> "My Project") </> "AssemblyInfo.vb") attributes
        | Shproj -> ()
        )
)

// Copies binaries from default VS location to expected bin folder
// But keeps a subdirectory structure for each project in the
// src folder to support multiple project outputs
Target.create "CopyBinaries" (fun _ ->
    !! "src/**/*.??proj"
    -- "src/**/*.shproj"
    |>  Seq.map (fun f -> ((System.IO.Path.GetDirectoryName f) </> "bin" </> configuration, "bin" </> (System.IO.Path.GetFileNameWithoutExtension f)))
    |>  Seq.iter (fun (fromDir, toDir) -> Shell.copyDir toDir fromDir (fun _ -> true))
)

// --------------------------------------------------------------------------------------
// Clean build results
let (nugetVersionPrefix,nugetVersionSuffix) =
    match release.NugetVersion.Split('-') |> Array.toList with
    | prefix::_ when BuildServer.AppVeyor.detect() -> (prefix, Git.Information.getCurrentHash())
    | prefix::[]->(prefix,"")
    | prefix::suffix::[]->(prefix,suffix)
    | _-> failwith "failed to recognise version"

let vsProjProps = [
#if MONO
     ("DefineConstants","MONO")
#else
     ("Platform", "Any CPU") 
#endif
     ("Configuration", configuration)
     ("PackageReleaseNotes", release.Notes |> List.head)
     ("VersionSuffix", nugetVersionSuffix)
     ("VersionPrefix", nugetVersionPrefix)
]

let buildParams (defaults: MSBuildParams) =
    { defaults with
          DoRestore = true
          Properties =
                [
                    "Optimize", "True"
                    "DebugSymbols", "True"
                    "Configuration", configuration
#if MONO
                    "DefineConstants","MONO"
#else
                    "Platform", "Any CPU"
#endif
                    "PackageReleaseNotes", release.Notes |> List.head
                    "VersionSuffix", nugetVersionSuffix
                    "VersionPrefix", nugetVersionPrefix
                    ]}

Target.create "Clean" (fun _ ->
    !! solutionFile |> MSBuild.runRelease buildParams "" "Clean" |> ignore
    Shell.cleanDirs ["bin"; "temp"; "docs"; "src"</>project</>"bin"; "src"</>project</>"obj" ]
)

// --------------------------------------------------------------------------------------
// Build library & test project

Target.create "Build" (fun _ ->
    !! solutionFile
    |> MSBuild.runRelease buildParams "" "Rebuild"
    |> ignore
)

Target.create "Restore" (fun _ ->
    DotNet.restore id solutionFile
)

// --------------------------------------------------------------------------------------
// Run the unit tests using test runner

Target.create "RunTests" (fun _ ->
    !! testAssemblies
    |> NUnit3.run (fun p ->
        { p with
            ShadowCopy = true
            TimeOut = TimeSpan.FromMinutes 20.
            ToolPath = "./packages/NUnit.ConsoleRunner/tools/nunit3-console.exe"
            OutputDir = "./TestResults.xml" })
(*TODO:
    // Import test result file if on AppVeyor
    if BuildServer.buildServer = AppVeyor then
        AppVeyor.UploadTestResultsFile AppVeyor.TestResultsType.NUnit "TestResults.xml"
        *)
)


// --------------------------------------------------------------------------------------
// Build a NuGet package

Target.create "NuGet" (fun _ ->
  !! ("src" </> project </> (sprintf "%s.fsproj" project))
  |> MSBuild.runRelease buildParams "" "pack"
  |> ignore
)

Target.create "CopyNuGet" (fun _ ->
    !! ("src" </> project </> "bin" </> "**" </> configuration </> (sprintf "%s*.nupkg" project))
    |> Shell.copyTo "bin"
)

Target.create "PublishNuget" (fun _ ->
    Paket.push(fun p ->
        { p with
            PublishUrl = "https://www.nuget.org"
            WorkingDir = "bin" })
)


// --------------------------------------------------------------------------------------
// Generate the documentation

let fsiExe = (__SOURCE_DIRECTORY__ @@ "packages" @@ "docs" @@ "FSharp.Compiler.Tools" @@ "tools" @@ "fsi.exe")

/// Run the given buildscript with FAKE.exe
let executeWithOutput configStartInfo =
    let exitCode =
        Process.execRaw
            configStartInfo
            TimeSpan.MaxValue false ignore ignore
    Threading.Thread.Sleep 1000
    exitCode

let executeWithRedirect errorF messageF configStartInfo =
    let exitCode =
        Process.execRaw
            configStartInfo
            TimeSpan.MaxValue true errorF messageF
    Threading.Thread.Sleep 1000
    exitCode

let executeHelper executer traceMsg failMessage configStartInfo =
    Trace.trace traceMsg
    let exit = executer configStartInfo
    if exit <> 0 then
        failwith failMessage
    ()

let execute = executeHelper executeWithOutput
// Documentation

let buildDocumentationTarget fsiargs target =
    execute
        (sprintf "Building documentation, (%s) this could take some time, please wait..." target)
        (sprintf "generating documentation %s failed" target)
        (fun p -> { p with 
                       FileName = "dotnet"
                       Arguments = sprintf " fsi --exec generate.fsx %s" fsiargs
                       WorkingDirectory = __SOURCE_DIRECTORY__ @@ "docsrc" @@ "tools" } )

Target.create "GenerateReferenceDocs" ignore // not currently implemented

let generateHelp' fail debug =
    let args =
        if debug then ""
        else "--define:RELEASE "
    try
        buildDocumentationTarget args "Default"
        Trace.log "Help generated"
    with
    | e when not fail ->
        Trace.log "generating help documentation failed"

let generateHelp fail =
    generateHelp' fail false

Target.create "GenerateHelp" (fun _ ->
    Shell.rm "docsrc/content/release-notes.md"
    Shell.copyFile "docsrc/content/" "RELEASE_NOTES.md"
    Shell.rename "docsrc/content/release-notes.md" "docsrc/content/RELEASE_NOTES.md"

    Shell.rm "docsrc/content/license.md"
    Shell.copyFile "docsrc/content/" "LICENSE.txt"
    Shell.rename "docsrc/content/license.md" "docsrc/content/LICENSE.txt"

    generateHelp true
)

Target.create "GenerateHelpDebug" (fun _ ->
    Shell.rm "docsrc/content/release-notes.md"
    Shell.copyFile "docsrc/content/" "RELEASE_NOTES.md"
    Shell.rename "docsrc/content/release-notes.md" "docsrc/content/RELEASE_NOTES.md"

    Shell.rm "docsrc/content/license.md"
    Shell.copyFile "docsrc/content/" "LICENSE.txt"
    Shell.rename "docsrc/content/license.md" "docsrc/content/LICENSE.txt"

    generateHelp' true true
)

Target.create "GenerateDocs" ignore

let createIndexFsx lang =
    let content = """(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use
// it to define helpers that you do not want to show in the documentation.
#I "../../../bin"

(**
F# Project Scaffold ({0})
=========================
*)
"""
    let targetDir = "docsrc/content" </> lang
    let targetFile = targetDir </> "index.fsx"
    Directory.ensure targetDir
    System.IO.File.WriteAllText(targetFile, System.String.Format(content, lang))

Target.create "AddLangDocs" (fun _ ->
    let args = System.Environment.GetCommandLineArgs()
    if args.Length < 4 then
        failwith "Language not specified."

    args.[3..]
    |> Seq.iter (fun lang ->
        if lang.Length <> 2 && lang.Length <> 3 then
            failwithf "Language must be 2 or 3 characters (ex. 'de', 'fr', 'ja', 'gsw', etc.): %s" lang

        let templateFileName = "template.cshtml"
        let templateDir = "docsrc/tools/templates"
        let langTemplateDir = templateDir </> lang
        let langTemplateFileName = langTemplateDir </> templateFileName

        if System.IO.File.Exists(langTemplateFileName) then
            failwithf "Documents for specified language '%s' have already been added." lang

        Directory.ensure langTemplateDir
        Shell.copy langTemplateDir [ templateDir </> templateFileName ]

        createIndexFsx lang)
)

// --------------------------------------------------------------------------------------
// Release Scripts

Target.create "ReleaseDocs" (fun _ ->
    let tempDocsDir = "temp/gh-pages"
    Shell.cleanDir tempDocsDir
    Repository.cloneSingleBranch "" (gitHome + "/" + gitName + ".git") "gh-pages" tempDocsDir

    Shell.copyRecursive "docs" tempDocsDir true |> Trace.logfn "%A"
    Git.Staging.stageAll tempDocsDir
    Git.Commit.exec tempDocsDir (sprintf "Update generated documentation for version %s" release.NugetVersion)
    Branches.push tempDocsDir
)

Target.create "Release" (fun _ ->
    let user =
        match Environment.GetEnvironmentVariable "github-user" with
        | s when not (String.IsNullOrWhiteSpace s) -> s
        | _ -> UserInput.getUserInput "Username: "
    let pw =
        match Environment.GetEnvironmentVariable "github-pw" with
        | s when not (String.IsNullOrWhiteSpace s) -> s
        | _ -> UserInput.getUserPassword "Password: "
    let remote =
        Git.CommandHelper.getGitResult "" "remote -v"
        |> Seq.filter (fun (s: string) -> s.EndsWith("(push)"))
        |> Seq.tryFind (fun (s: string) -> s.Contains(gitOwner + "/" + gitName))
        |> function None -> gitHome + "/" + gitName | Some (s: string) -> s.Split().[0]

    Git.Staging.stageAll ""
    Git.Commit.exec "" (sprintf "Bump version to %s" release.NugetVersion)
    Branches.pushBranch "" remote (Information.getBranchName "")

    Branches.tag "" release.NugetVersion
    Branches.pushTag "" remote release.NugetVersion
    // See https://github.com/fsharp/FAKE/blob/5.8.5/src/app/Fake.Api.GitHub/GitHub.fs :
    // release on github
    GitHub.createClient user pw
    |> GitHub.draftNewRelease gitOwner gitName release.NugetVersion (release.SemVer.PreRelease <> None) release.Notes
    // TODO: |> uploadFile "PATH_TO_FILE"
    |>  GitHub.publishDraft
    |> Async.RunSynchronously
)

Target.create "BuildDocs" ignore
Target.create "BuildPackage" ignore

// --------------------------------------------------------------------------------------
// Run all targets by default. Invoke 'build <Target>' to override

Target.create "All" ignore

open Fake.Core.TargetOperators

"Clean"
  ==> "AssemblyInfo"
  ==> "Restore"
  ==> "Build"
  ==> "CopyBinaries"
  ==> "RunTests"
  ==> "NuGet"
  ==> "CopyNuGet"
  ==> "BuildPackage"
  ==> "BuildDocs"
  ==> "All"
  =?> ("ReleaseDocs",BuildServer.isLocalBuild)

"CopyBinaries"
  ==> "GenerateHelp"
  ==> "GenerateReferenceDocs"
  ==> "GenerateDocs"

"GenerateReferenceDocs"
  ==> "GenerateDocs"
  ==> "BuildDocs"

//"GenerateHelpDebug"
//  ==> "KeepRunning"

"Clean"
  ==> "Release"

"BuildPackage"
  ==> "PublishNuget"
  ==> "Release"

"BuildDocs"
  ==> "ReleaseDocs"
  ==> "Release"

Target.runOrDefault "All"
