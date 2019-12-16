// Learn more about F# at http://fsharp.org


open System
open Fake.IO.FileSystemOperators
open Fake.IO

let refreshWebpageEvent = new Event<string>()

type Configuration = {
    SiteBaseUrl         : Uri
    GitHubRepoUrl       : Uri
    RepositoryRoot      : IO.DirectoryInfo
    DocsOutputDirectory : IO.DirectoryInfo
    DocsSourceDirectory : IO.DirectoryInfo
    ProjectName         : string
    ProjectFilesGlob    : IGlobbingPattern
    ReleaseVersion      : string
}

let docsApiDir docsDir = docsDir @@ "Api_Reference"

type DisposableList =
        {
            disposables : IDisposable list
        } interface IDisposable with
            member x.Dispose () =
                x.disposables |> List.iter(fun s -> s.Dispose())


module ProjInfo =
    open System.IO

    type References = FileInfo []
    type TargetPath = FileInfo

    type ProjInfo = {
        References : References
        TargetPath : TargetPath
    }

    open Dotnet.ProjInfo.Workspace
    open Dotnet.ProjInfo.Workspace.FCS
    let createFCS () =
        let checker =
            FCS_Checker.Create(
              projectCacheSize = 200,
              keepAllBackgroundResolutions = true,
              keepAssemblyContents = true)
        checker.ImplicitlyStartBackgroundWork <- true
        checker

    let createLoader () =
        let msbuildLocator = MSBuildLocator()
        let config = LoaderConfig.Default msbuildLocator
        let loader = Loader.Create(config)
        let netFwconfig = NetFWInfoConfig.Default msbuildLocator
        let netFwInfo = NetFWInfo.Create(netFwconfig)

        loader, netFwInfo

    let [<Literal>] RefPrefix = "-r:"

    let findTargetPath targetPath =
        if File.exists targetPath then
            FileInfo targetPath
        else
            //HACK: Need to get dotnet-proj-info to handle configurations when extracting data
            let debugFolder = sprintf "%cDebug%c" Path.DirectorySeparatorChar Path.DirectorySeparatorChar
            let releaseFolder = sprintf "%cRelease%c" Path.DirectorySeparatorChar Path.DirectorySeparatorChar
            let debugFolderAlt = sprintf "%cDebug%c" Path.DirectorySeparatorChar Path.AltDirectorySeparatorChar
            let releaseFolderAlt = sprintf "%cRelease%c" Path.DirectorySeparatorChar Path.AltDirectorySeparatorChar

            let releasePath = targetPath.Replace(debugFolder, releaseFolder).Replace(debugFolderAlt, releaseFolderAlt)
            if releasePath |> File.exists then
                releasePath |> FileInfo
            else
                failwithf "Couldn't find a dll to generate documentationfrom %s or %s" targetPath releasePath

    let findReferences projPath : ProjInfo=
        let fcs = createFCS ()
        let loader, netFwInfo = createLoader ()
        loader.LoadProjects [ projPath ]
        let fcsBinder = FCSBinder(netFwInfo, loader, fcs)
        match fcsBinder.GetProjectOptions(projPath) with
        | Ok options ->
            let references =
                options.OtherOptions
                |> Array.filter(fun s ->
                    s.StartsWith(RefPrefix)
                )
                |> Array.map(fun s ->
                    // removes "-r:" from beginning of reference path
                    s.Remove(0,RefPrefix.Length)
                    |> FileInfo
                )

            let dpwPo =
                match options.ExtraProjectInfo with
                | Some (:? ProjectOptions as dpwPo) -> dpwPo
                | x -> failwithf "invalid project info %A" x
            let targetPath = findTargetPath dpwPo.ExtraProjectInfo.TargetPath
            { References = references ; TargetPath = targetPath}

        | Error e ->
            failwithf "Couldn't read project %s %A" projPath e


module GenerateDocs =
    open DocsTool
    open Fake.Core
    open Fake.IO.Globbing.Operators
    open Fake.IO
    open Fable.React
    open Fable.React.Helpers
    open FSharp.Literate
    open System.IO
    open FSharp.MetadataFormat


    type GeneratedDoc = {
        SourcePath : FileInfo option
        OutputPath : FileInfo
        Content : ReactElement list
        Title : string
    }


    let docsFileGlob docsSrcDir =
        !! (docsSrcDir @@ "**/*.fsx")
        ++ (docsSrcDir @@ "**/*.md")

    let render html =
        fragment [] [
            RawText "<!doctype html>"
            RawText "\n"
            html ]
        |> Fable.ReactServer.renderToString

    let renderWithMasterTemplate masterCfg navBar titletext bodytext pageSource =
        Master.masterTemplate masterCfg navBar titletext bodytext pageSource
        |> render

    let renderWithMasterAndWrite masterCfg (outPath : FileInfo) navBar titletext bodytext pageSource   =
        let contents = renderWithMasterTemplate masterCfg navBar titletext bodytext pageSource
        IO.Directory.CreateDirectory(outPath.DirectoryName) |> ignore

        IO.File.WriteAllText(outPath.FullName, contents)
        Fake.Core.Trace.tracefn "Rendered to %s" outPath.FullName

    let generateNav (cfg : Configuration) (generatedDocs : GeneratedDoc list) =
        let docsDir = cfg.DocsOutputDirectory.FullName
        let pages =
                generatedDocs
                |> List.map(fun gd -> gd.OutputPath)
                |> List.filter(fun f -> f.FullName.StartsWith(docsDir </> "content") |> not)
                |> List.filter(fun f -> f.FullName.StartsWith(docsDir </> "files") |> not)
                |> List.filter(fun f -> f.FullName.StartsWith(docsDir </> "index.html") |> not)

        let topLevelNavs : Nav.TopLevelNav = {
            DocsRoot = IO.DirectoryInfo docsDir
            DocsPages = pages
        }

        let navCfg : Nav.NavConfig = {
            SiteBaseUrl = cfg.SiteBaseUrl
            GitHubRepoUrl = cfg.GitHubRepoUrl
            ProjectName = cfg.ProjectName
            TopLevelNav = topLevelNavs
        }

        Nav.generateNav navCfg

    let renderGeneratedDocs (cfg : Configuration)  (generatedDocs : GeneratedDoc list) =
        let nav = generateNav cfg generatedDocs
        let masterCfg : Master.MasterTemplateConfig = {
            SiteBaseUrl = cfg.SiteBaseUrl
            GitHubRepoUrl = cfg.GitHubRepoUrl
            ProjectName = cfg.ProjectName
            ReleaseVersion = cfg.ReleaseVersion
            ReleaseDate = DateTimeOffset.Now
            RepositoryRoot = cfg.RepositoryRoot
        }
        generatedDocs
        |> Seq.iter(fun gd ->
            let pageSource =
                gd.SourcePath
                |> Option.map(fun sp ->
                    sp.FullName.Replace(cfg.RepositoryRoot.FullName, "").Replace("\\", "/")
                )
            renderWithMasterAndWrite masterCfg gd.OutputPath nav gd.Title gd.Content pageSource
        )


    let copyAssets (cfg : Configuration) =
        Shell.copyDir (cfg.DocsOutputDirectory.FullName </> "content")   ( cfg.DocsSourceDirectory.FullName </> "content") (fun _ -> true)
        Shell.copyDir (cfg.DocsOutputDirectory.FullName </> "files")   ( cfg.DocsSourceDirectory.FullName </> "files") (fun _ -> true)


    let regexReplace (cfg : Configuration) source =
        let replacements =
            [
                "{{siteBaseUrl}}", (cfg.SiteBaseUrl.ToString().TrimEnd('/'))
            ]
        (source, replacements)
        ||> List.fold(fun state (pattern, replacement) ->
            Text.RegularExpressions.Regex.Replace(state, pattern, replacement)
        )

    let generateDocs (libDirs : ProjInfo.References) (docSourcePaths : IGlobbingPattern) (cfg : Configuration) =
        let parse (fileName : string) source =
            let doc =
                let references =
                    libDirs
                    |> Array.map(fun fi -> fi.DirectoryName)
                    |> Array.distinct
                    |> Array.map(sprintf "-I:%s")
                let runtimeDeps =
                    [|
                        "-r:System.Runtime"
                        "-r:System.Net.WebClient"
                    |]
                let compilerOptions = String.Join(' ', Array.concat [runtimeDeps; references])
                let fsiEvaluator = FSharp.Literate.FsiEvaluator(references)
                match Path.GetExtension fileName with
                | ".fsx" ->
                    Literate.ParseScriptString(
                        source,
                        path = fileName,
                        compilerOptions = compilerOptions,
                        fsiEvaluator = fsiEvaluator)
                | ".md" ->
                    let source = regexReplace cfg source
                    Literate.ParseMarkdownString(
                        source,
                        path = fileName,
                        compilerOptions = compilerOptions,
                        fsiEvaluator = fsiEvaluator
                    )
                | others -> failwithf "FSharp.Literal does not support %s file extensions" others
            FSharp.Literate.Literate.FormatLiterateNodes(doc, OutputKind.Html, "", true, true)

        let format (doc: LiterateDocument) =
            if not <| Seq.isEmpty doc.Errors
            then
                failwithf "error while formatting file %s. Errors are:\n%A" doc.SourceFile doc.Errors
            else
                Formatting.format doc.MarkdownDocument true OutputKind.Html
                + doc.FormattedTips



        docSourcePaths
        |> Array.ofSeq
        |> Seq.map(fun filePath ->

            Fake.Core.Trace.tracefn "Rendering %s" filePath
            let file = IO.File.ReadAllText filePath
            let outPath =
                let changeExtension ext path = IO.Path.ChangeExtension(path,ext)
                filePath.Replace(cfg.DocsSourceDirectory.FullName, cfg.DocsOutputDirectory.FullName)
                |> changeExtension ".html"
                |> FileInfo
            let fs =
                file
                |> parse filePath
                |> format
            let contents =
                [div [] [
                    fs
                    |> RawText
                ]]

            {
                SourcePath = FileInfo filePath |> Some
                OutputPath = outPath
                Content = contents
                Title = sprintf "%s-%s" outPath.Name cfg.ProjectName
            }
        )
        |> Seq.toList


    let generateAPI (projInfos : ProjInfo.ProjInfo array) (cfg : Configuration) =
        let generate (projInfo :  ProjInfo.ProjInfo) =
            Trace.tracefn "Generating API Docs for %s" projInfo.TargetPath.FullName
            let mscorlibDir =
                (Uri(typedefof<System.Runtime.MemoryFailPoint>.GetType().Assembly.CodeBase)) //Find runtime dll
                    .AbsolutePath // removes file protocol from path
                    |> Path.GetDirectoryName
            let references =
                projInfo.References
                |> Array.toList
                |> List.map(fun fi -> fi.DirectoryName)
                |> List.distinct
            let libDirs = mscorlibDir :: references
            let targetApiDir = docsApiDir cfg.DocsOutputDirectory.FullName @@ IO.Path.GetFileNameWithoutExtension(projInfo.TargetPath.Name)

            let generatorOutput =
                MetadataFormat.Generate(
                    projInfo.TargetPath.FullName,
                    libDirs = libDirs,
                    sourceFolder = cfg.RepositoryRoot.FullName,
                    sourceRepo = (cfg.GitHubRepoUrl |> Uri.simpleCombine "tree/master" |> string),
                    markDownComments = false
                    )

            let fi = FileInfo <| targetApiDir @@ (sprintf "%s.html" generatorOutput.AssemblyGroup.Name)
            let indexDoc = {
                SourcePath = None
                OutputPath = fi
                Content = [Namespaces.generateNamespaceDocs generatorOutput.AssemblyGroup generatorOutput.Properties]
                Title = sprintf "%s-%s" fi.Name cfg.ProjectName
            }

            let moduleDocs =
                generatorOutput.ModuleInfos
                |> List.map (fun m ->
                    let fi = FileInfo <| targetApiDir @@ (sprintf "%s.html" m.Module.UrlName)
                    let content = Modules.generateModuleDocs m generatorOutput.Properties
                    {
                        SourcePath = None
                        OutputPath = fi
                        Content = content
                        Title = sprintf "%s-%s" m.Module.Name cfg.ProjectName
                    }
                )
            let typeDocs =
                generatorOutput.TypesInfos
                |> List.map (fun m ->
                    let fi = FileInfo <| targetApiDir @@ (sprintf "%s.html" m.Type.UrlName)
                    let content = Types.generateTypeDocs m generatorOutput.Properties
                    {
                        SourcePath = None
                        OutputPath = fi
                        Content = content
                        Title = sprintf "%s-%s" m.Type.Name cfg.ProjectName
                    }
                )
            [ indexDoc ] @ moduleDocs @ typeDocs
        projInfos
        |> Seq.collect(generate)
        |> Seq.toList

    let buildDocs (projInfos : ProjInfo.ProjInfo array) (cfg : Configuration) =
        let refs = projInfos |> Seq.collect (fun p -> p.References) |> Seq.distinct |> Seq.toArray
        copyAssets cfg
        let generateDocs =
            async {
                return generateDocs refs (docsFileGlob cfg.DocsSourceDirectory.FullName) cfg
            }
        let generateAPI =
            async {
                return (generateAPI projInfos cfg)
            }
        Async.Parallel [generateDocs; generateAPI]
        |> Async.RunSynchronously
        |> Array.toList
        |> List.collect id

    let renderDocs (cfg : Configuration) =
        let projInfos = cfg.ProjectFilesGlob |> Seq.map(ProjInfo.findReferences) |> Seq.toArray
        buildDocs projInfos cfg
        |> renderGeneratedDocs cfg

    let watchDocs (cfg : Configuration) =
        let projInfos = cfg.ProjectFilesGlob |> Seq.map(ProjInfo.findReferences) |> Seq.toArray
        let initialDocs = buildDocs projInfos cfg
        initialDocs |> renderGeneratedDocs cfg

        let refs = projInfos |> Seq.collect (fun p -> p.References) |> Seq.distinct |> Seq.toArray
        let d1 =
            docsFileGlob cfg.DocsSourceDirectory.FullName
            |> ChangeWatcher.run (fun changes ->
                printfn "changes %A" changes
                changes
                |> Seq.iter (fun m ->
                    printfn "watching %s" m.FullPath
                    let generated = generateDocs refs (!! m.FullPath) cfg
                    initialDocs
                    |> List.filter(fun x -> generated |> List.exists(fun y -> y.OutputPath =  x.OutputPath) |> not )
                    |> List.append generated
                    |> List.distinctBy(fun gd -> gd.OutputPath.FullName)
                    |> renderGeneratedDocs cfg
                )
                refreshWebpageEvent.Trigger "m.FullPath"
            )
        let d2 =
            !! (cfg.DocsSourceDirectory.FullName </> "content" </> "**/*")
            ++ (cfg.DocsSourceDirectory.FullName </> "files"  </> "**/*")
            |> ChangeWatcher.run(fun changes ->
                printfn "changes %A" changes
                copyAssets cfg
                refreshWebpageEvent.Trigger "Assets"
            )


        let d3 =
            projInfos
            |> Seq.map(fun p -> p.TargetPath.FullName)
            |> Seq.fold ((++)) (!! "")

            |> ChangeWatcher.run(fun changes ->
                changes
                |> Seq.iter(fun c -> Trace.logf "Regenerating API docs due to %s" c.FullPath )
                let generated = generateAPI projInfos cfg
                initialDocs
                |> List.filter(fun x -> generated |> List.exists(fun y -> y.OutputPath =  x.OutputPath) |> not )
                |> List.append generated
                |> List.distinctBy(fun gd -> gd.OutputPath.FullName)
                |> renderGeneratedDocs cfg
                refreshWebpageEvent.Trigger "Api"
            )
        { disposables = [d1; d2; d3] } :> IDisposable


module WebServer =
    open Microsoft.AspNetCore.Hosting
    open Microsoft.AspNetCore.Builder
    open Microsoft.Extensions.FileProviders
    open Microsoft.AspNetCore.Http
    open System.Net.WebSockets
    open System.Diagnostics

    let hostname = "localhost"
    let port = 5000

    /// Helper to determine if port is in use
    let waitForPortInUse (hostname : string) port =
        let mutable portInUse = false
        while not portInUse do
            Async.Sleep(10) |> Async.RunSynchronously
            use client = new Net.Sockets.TcpClient()
            try
                client.Connect(hostname,port)
                portInUse <- client.Connected
                client.Close()
            with e ->
                client.Close()

    /// Async version of IApplicationBuilder.Use
    let useAsync (middlware : HttpContext -> (unit -> Async<unit>) -> Async<unit>) (app:IApplicationBuilder) =
        app.Use(fun env next ->
            middlware env (next.Invoke >> Async.AwaitTask)
            |> Async.StartAsTask
            :> System.Threading.Tasks.Task
        )

    let createWebsocketForLiveReload (httpContext : HttpContext) (next : unit -> Async<unit>) = async {
        if httpContext.WebSockets.IsWebSocketRequest then
            let! websocket = httpContext.WebSockets.AcceptWebSocketAsync() |> Async.AwaitTask
            use d =
                refreshWebpageEvent.Publish
                |> Observable.subscribe (fun m ->
                    let segment = ArraySegment<byte>(m |> Text.Encoding.UTF8.GetBytes)
                    websocket.SendAsync(segment, WebSocketMessageType.Text, true, httpContext.RequestAborted)
                    |> Async.AwaitTask
                    |> Async.Start

                )
            while websocket.State <> WebSocketState.Closed do
                do! Async.Sleep(1000)
        else
            do! next ()
    }

    let configureWebsocket (appBuilder : IApplicationBuilder) =
        appBuilder.UseWebSockets()
        |> useAsync (createWebsocketForLiveReload)
        |> ignore

    let startWebserver docsDir (url : string) =
        WebHostBuilder()
            .UseKestrel()
            .UseUrls(url)
            .Configure(fun app ->
                let opts =
                    StaticFileOptions(
                        FileProvider =  new PhysicalFileProvider(docsDir)
                    )
                app.UseStaticFiles(opts) |> ignore
                configureWebsocket app
            )
            .Build()
            .Run()

    let openBrowser url =
        //https://github.com/dotnet/corefx/issues/10361
        let psi = ProcessStartInfo(FileName = url, UseShellExecute = true)
        let proc = Process.Start psi
        proc.WaitForExit()
        if proc.ExitCode <> 0 then failwithf "opening browser failed"

    let serveDocs docsDir =
        async {
            waitForPortInUse hostname port
            sprintf "http://%s:%d/index.html" hostname port |> openBrowser
        } |> Async.Start
        startWebserver docsDir (sprintf "http://%s:%d" hostname port)


open Argu
open Fake.IO.Globbing.Operators
open DocsTool.CLIArgs
[<EntryPoint>]
let main argv =

    let defaultConfig = {
        SiteBaseUrl = Uri(sprintf "http://%s:%d/" WebServer.hostname WebServer.port )
        GitHubRepoUrl = Uri "https://github.com"
        RepositoryRoot = IO.DirectoryInfo (__SOURCE_DIRECTORY__ @@ "..")
        DocsOutputDirectory = IO.DirectoryInfo "docs"
        DocsSourceDirectory = IO.DirectoryInfo "docsSrc"
        ProjectName = ""
        ProjectFilesGlob = !! ""
        ReleaseVersion = "0.1.0"
    }

    let errorHandler = ProcessExiter(colorizer = function ErrorCode.HelpText -> None | _ -> Some ConsoleColor.Red)
    let programName =
        let name = System.Reflection.Assembly.GetExecutingAssembly().GetName().Name
        if Fake.Core.Environment.isWindows then
            sprintf "%s.exe" name
        else
            name



    let parser = ArgumentParser.Create<CLIArguments>(programName = programName, errorHandler = errorHandler)
    let parsedArgs = parser.Parse argv
    match parsedArgs.GetSubCommand() with
    | Build args ->
        let config =
            (defaultConfig, args.GetAllResults())
            ||> List.fold(fun state next ->
                match next with
                | BuildArgs.SiteBaseUrl url ->  { state with SiteBaseUrl = Uri url }
                | BuildArgs.ProjectGlob glob -> { state with ProjectFilesGlob = !! glob}
                | BuildArgs.DocsOutputDirectory outdir -> { state with DocsOutputDirectory = IO.DirectoryInfo outdir}
                | BuildArgs.DocsSourceDirectory srcdir -> { state with DocsSourceDirectory = IO.DirectoryInfo srcdir}
                | BuildArgs.GitHubRepoUrl url -> { state with GitHubRepoUrl = Uri url}
                | BuildArgs.ProjectName repo -> { state with ProjectName = repo}
                | BuildArgs.ReleaseVersion version -> { state with ReleaseVersion = version}
            )
        GenerateDocs.renderDocs config
    | Watch args ->
        let config =
            (defaultConfig, args.GetAllResults())
            ||> List.fold(fun state next ->
                match next with
                | WatchArgs.ProjectGlob glob -> {state with ProjectFilesGlob = !! glob}
                | WatchArgs.DocsOutputDirectory outdir -> { state with DocsOutputDirectory = IO.DirectoryInfo outdir}
                | WatchArgs.DocsSourceDirectory srcdir -> { state with DocsSourceDirectory = IO.DirectoryInfo srcdir}
                | WatchArgs.GitHubRepoUrl url -> { state with GitHubRepoUrl = Uri url}
                | WatchArgs.ProjectName repo -> { state with ProjectName = repo}
                | WatchArgs.ReleaseVersion version -> { state with ReleaseVersion = version}
            )
        use ds = GenerateDocs.watchDocs config
        WebServer.serveDocs config.DocsOutputDirectory.FullName
    0 // return an integer exit code
