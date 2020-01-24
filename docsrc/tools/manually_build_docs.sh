#!/bin/bash

# Restore fake and paket
dotnet tool restore

# Restore paket deps
dotnet paket restore

# Build
# Note: some bug means can't build debug
dotnet build -c Release

# Gen docs
#mkdir -p bin/FSharpPlus/netstandard2.0/
#cp ./src/FSharpPlus/bin/Release/netstandard2.0/* bin/FSharpPlus/netstandard2.0/
./docsrc/tools/generate.sh
