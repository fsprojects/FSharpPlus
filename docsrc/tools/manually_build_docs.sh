#!/bin/bash
pushd $(dirname "${0}") > /dev/null
cd ../../
# Restore fake and paket
dotnet tool restore

# Build
# Note: some bug means can't build debug
dotnet build -c Release FSharpPlus.sln

# Gen docs
#mkdir -p bin/FSharpPlus/netstandard2.0/
#cp ./src/FSharpPlus/bin/Release/netstandard2.0/* bin/FSharpPlus/netstandard2.0/
dotnet run --project ./docsrc/tools

# (export TargetPath=src/FSharpPlus/bin/Debug/netstandard2.0/FSharpPlus.dll ;dotnet fsdocs build --projects src/FSharpPlus/FSharpPlus.fsproj --input docsrc/content/ --output docs/)
