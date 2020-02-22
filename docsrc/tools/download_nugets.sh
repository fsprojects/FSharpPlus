#!/bin/bash
pushd $(dirname "${0}") > /dev/null
cd ../../
dotnet tool restore
mkdir -p ./bin
mkdir -p ./packages/docs/
NUGET="./bin/nuget.exe"
if test ! -f "$NUGET"; then
  curl -o ./bin/nuget.exe https://dist.nuget.org/win-x86-commandline/latest/nuget.exe
fi
if test "$OS" = "Windows_NT"; then
  MONO=""
else
  MONO="mono"
fi

$MONO $NUGET install FSharp.Core "-ExcludeVersion" "-version" 4.6.2 "-source" https://www.nuget.org/api/v2 "-OutputDirectory" packages/docs/
$MONO $NUGET install System.Runtime "-ExcludeVersion" "-version" 4.3.1  "-source" https://www.nuget.org/api/v2 "-OutputDirectory" packages/docs/
$MONO $NUGET install MathNet.Numerics.FSharp "-ExcludeVersion" "-version" 4.8.1  -source https://www.nuget.org/api/v2 "-OutputDirectory" packages/docs/
$MONO $NUGET install FSharp.Literate "-ExcludeVersion" "-version" 4.0.0-alpha03  -source https://www.nuget.org/api/v2 "-OutputDirectory" packages/docs/
$MONO $NUGET install FSharp.Compiler.Service "-ExcludeVersion" "-source" https://www.nuget.org/api/v2 "-OutputDirectory" packages/docs/
$MONO $NUGET install FSharp.Formatting "-ExcludeVersion" "-version" 4.0.0-alpha03 "-source" https://www.nuget.org/api/v2 "-OutputDirectory" packages/docs/
