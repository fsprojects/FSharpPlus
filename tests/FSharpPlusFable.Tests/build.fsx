open System
open System.Collections.Generic
open System.IO

let args : string array = fsi.CommandLineArgs |> Array.tail

let separated delimiter (items : string seq) = String.Join((delimiter: string), Array.ofSeq items)

let toLines text = separated Environment.NewLine text

type ProcessResult = { ExitCode : int; stdout : string; stderr : string }

let executeProcess (exe, cmdline, workingDir) =
    let psi =
        new System.Diagnostics.ProcessStartInfo (exe, cmdline,
            UseShellExecute = false,
            RedirectStandardOutput = true,
            RedirectStandardError = true,
            CreateNoWindow = true,
            WorkingDirectory = defaultArg workingDir "")
    let p = System.Diagnostics.Process.Start psi
    let output = new ResizeArray<_> ()
    let error  = new ResizeArray<_> ()
    p.OutputDataReceived.Add (fun args -> output.Add args.Data |> ignore)
    p.ErrorDataReceived.Add  (fun args -> error.Add  args.Data |> ignore)
    p.BeginErrorReadLine ()
    p.BeginOutputReadLine ()
    p.WaitForExit ()
    { ExitCode = p.ExitCode; stdout = toLines output; stderr = toLines error }


type TraceData =
        | ImportantMessage of string
        | ErrorMessage of string
        | LogMessage of string * bool
        | TraceMessage of string * bool

type ITraceListener = 
        abstract Write : TraceData -> unit

type ConsoleTraceListener(colorMap) =
        let writeText stdErr color newLine text = 
            let curColor = Console.ForegroundColor
            try
              if curColor <> color then Console.ForegroundColor <- color
              let printer =
                match stdErr, newLine with
                | false, true -> printfn
                | false, false -> printf
                | true, true -> eprintfn
                | true, false -> eprintf
              printer "%s" text
            finally
              if curColor <> color then Console.ForegroundColor <- curColor
    
        interface ITraceListener with
            /// Writes the given message to the Console.
            member this.Write msg = 
                let color = colorMap msg
                match msg with
                | ImportantMessage text | ErrorMessage text ->
                    writeText true color true text
                | LogMessage(text, newLine) | TraceMessage(text, newLine) ->
                    writeText false color newLine text


let listeners = new Collections.Generic.List<ITraceListener>()

let postMessage x = listeners.ForEach(fun listener -> listener.Write x)

let log message = LogMessage(message, true) |> postMessage

let logfn fmt = Printf.ksprintf log fmt


/// Runs dotnet.exe with the given command in the given repository directory.
let runCommand com repositoryDir command =
        let processResult = executeProcess (com, command, Some repositoryDir)
        processResult.ExitCode = 0, [processResult.stdout], processResult.stderr

/// Runs the dotnet command and returns the first line of the result.
let runSimpleCommand com repositoryDir command =
        try
            let _,msg,errors = runCommand com repositoryDir command
            let errorText = toLines msg + System.Environment.NewLine + errors
            if errorText.Contains "fatal: " then failwith errorText
            if msg.Length = 0 then "" else
            msg |> Seq.iter (logfn "%s")
            msg.[0]
        with exn -> failwithf "Could not run \"git %s\".\r\nError: %s" command exn.Message





match args.[0] with
| "npminstall" -> printfn "%s" (runSimpleCommand "npm" "." "install")
| "fabletest" -> printfn "%s" (runSimpleCommand "npm" "." "run pretest --verbose")
                 printfn "%s" (runSimpleCommand "npm" "." "test --verbose")

| "fsharptest" -> printfn "%s" (runSimpleCommand "dotnet" "." "build")
                  printfn "%s" (runSimpleCommand "dotnet" "." "run")   

| _ -> printfn "Command not recognised"
