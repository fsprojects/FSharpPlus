module Tools
open System
let (</>) x y = IO.Path.Combine(x,y)


module Path =
    // Paths with template/source/output locations
    let bin        = __SOURCE_DIRECTORY__ </> "../../src/FSharpPlus/bin/Release/net8.0/"
    let content    = __SOURCE_DIRECTORY__ </> "../content"
    let output     = __SOURCE_DIRECTORY__ </> "../../docs"
    let templates      = __SOURCE_DIRECTORY__ </> "./templates"
    let formatting = __SOURCE_DIRECTORY__ </> "../../packages/docs/FSharp.Formatting/"

    let dir p = IO.Path.GetDirectoryName(p: string)
    let filename p = IO.Path.GetFileName(p: string)
    let changeExt ext p = IO.Path.ChangeExtension(p, ext)

module Directory =
    let ensure dir =
        if not (IO.Directory.Exists dir) then
            IO.Directory.CreateDirectory dir |> ignore

    let copyRecursive (path: string) dest =
        let path =
            if not (path.EndsWith(string IO.Path.DirectorySeparatorChar)) then
                path + string IO.Path.DirectorySeparatorChar
            else
                path
        let trim (p: string) =
            if p.StartsWith(path) then
                p.Substring(path.Length)
            else
                failwithf "Cannot find path root"
        IO.Directory.EnumerateFiles(path, "*", IO.SearchOption.AllDirectories)
        |> Seq.iter (fun p ->
            let target = dest </> trim p
            ensure(Path.dir target)
            IO.File.Copy(p, target, true))
