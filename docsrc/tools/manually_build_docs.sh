#!/bin/bash
pushd $(dirname "${0}") > /dev/null
cd ../../
# Restore fake and paket
dotnet tool restore

# Build
# Note: some bug means can't build debug
dotnet build -c Release

# Gen docs
#mkdir -p bin/FSharpPlus/netstandard2.0/
#cp ./src/FSharpPlus/bin/Release/netstandard2.0/* bin/FSharpPlus/netstandard2.0/
dotnet run --project ./docsrc/tools

