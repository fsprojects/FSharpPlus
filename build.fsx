#if run_with_bin_sh 
  # See why this works at http://stackoverflow.com/a/21948918/637783 
  exec fsharpi --define:mono_posix --exec $0 $*
#endif

(*
 * Crossplatform FSharp Makefile Bootstrapper
 * Apache licensed - Copyright 2014 Jay Tuley <jay+code@tuley.name>
 * v 2.0 https://gist.github.com/jbtule/9243729
 * 
 * How to use:
 *   On Windows `fsi --exec build.fsx <buildtarget>
 *
 *  On Mac Or Linux `./build.fsx <buildtarget>`
 *    *Note:* But if you have trouble then use `sh build.fsx <buildtarget>`
 *
 *)

open System
open System.IO
open System.Diagnostics

(* helper functions *)
#if mono_posix
#r "Mono.Posix.dll"
open Mono.Unix.Native
let applyExecutionPermissionUnix path =
    let _,stat = Syscall.lstat(path)
    Syscall.chmod(path, FilePermissions.S_IXUSR ||| stat.st_mode) |> ignore
#else
let applyExecutionPermissionUnix path = ()
#endif

let doesNotExist path =
    path |> Path.GetFullPath |> File.Exists |> not

let execAt (workingDir:string) (exePath:string) (args:string seq) =
    let processStart (psi:ProcessStartInfo) =
        let ps = Process.Start(psi)
        ps.WaitForExit ()
        ps.ExitCode
    let fullExePath = exePath |> Path.GetFullPath
    applyExecutionPermissionUnix fullExePath
    let exitCode = ProcessStartInfo(
                        fullExePath,
                        args |> String.concat " ",
                        WorkingDirectory = (workingDir |> Path.GetFullPath),
                        UseShellExecute = false) 
                   |> processStart
    if exitCode <> 0 then
        exit exitCode
    ()

let exec = execAt Environment.CurrentDirectory

let downloadNugetTo path =
    let fullPath = path |> Path.GetFullPath;
    if doesNotExist fullPath then 
        printf "Downloading NuGet..."
        use webClient = new System.Net.WebClient()
        fullPath |> Path.GetDirectoryName |> Directory.CreateDirectory |> ignore
        webClient.DownloadFile("https://nuget.org/nuget.exe", path |> Path.GetFullPath)
        printfn "Done."

let passedArgs = fsi.CommandLineArgs.[1..] |> Array.toList

(* execution script customize below *)

let nugetExe = "packages/NuGet/NuGet.exe"
let fakeExe = "packages/FAKE/tools/FAKE.exe"

downloadNugetTo nugetExe

if doesNotExist fakeExe then 
    exec nugetExe ["install"; "FAKE"; "-OutputDirectory packages"; "-ExcludeVersion"; "-Prerelease"]
exec fakeExe ("makefile.fsx"::passedArgs)"
