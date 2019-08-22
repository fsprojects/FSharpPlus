$fsiExitCode = 0
Push-Location
try {
    $FSI = [IO.Path]::Combine($PSScriptRoot, ".." , "..", "packages", "docs","FSharp.Compiler.Tools","tools","fsi.exe")

    & $FSI --exec generate.fsx @Args
    $fsiExitCode = $LastExitCode
}
finally {
    Pop-Location
}
exit($fsiExitCode)