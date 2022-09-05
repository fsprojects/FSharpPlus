#!/bin/bash
pushd $(dirname "${0}") > /dev/null
cd ../../
# Restore fake and paket
dotnet tool restore

./docsrc/tools/download_nugets.sh

# Build
# Note: some bug means can't build debug
dotnet build -c Release FSharpPlus.sln

# Gen docs
dotnet run --project ./docsrc/tools 

# To release docs run:
# dotnet run --project ./docsrc/tools ReleaseDocs

# To run the documentation without going through docsrc/tools:
# (export TargetPath=src/FSharpPlus/bin/Release/net45/FSharpPlus.dll ;dotnet fsdocs build --projects src/FSharpPlus/FSharpPlus.fsproj --input docsrc/content/ --output docs/)
