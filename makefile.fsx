#I @"packages/FAKE/tools/"
#r @"FakeLib.dll"

open Fake
open System
open System.IO

let buildDir = "./build"
let nugetDir = "./packages"
let packagesDir = "./packages"

Target "Clean" (fun _ -> CleanDirs [buildDir])

Target "RestorePackages" RestorePackages

Target "BuildSolution" (fun _ ->
    MSBuildWithDefaults "Build" ["./FsControl.Core/FsControl.Core.fsproj"]
    |> Log "Core Build-Output: "
    MSBuildWithDefaults "Build" ["./FsControl.BaseLib/FsControl.BaseLib.csproj"]
    |> Log "BaseLib Build-Output: "
)

"BuildSolution" <== ["Clean"; "RestorePackages"]

RunTargetOrDefault "BuildSolution"
