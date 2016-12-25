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
    MSBuildWithDefaults "Build" ["./FSharpPlus/FSharpPlus.fsproj"]
    |> Log "Core Build-Output: "
    MSBuildWithDefaults "Build" ["./FSharpPlus.BaseLib/FSharpPlus.BaseLib.csproj"]
    |> Log "BaseLib Build-Output: "
)

"BuildSolution" <== ["Clean"; "RestorePackages"]

RunTargetOrDefault "BuildSolution"
