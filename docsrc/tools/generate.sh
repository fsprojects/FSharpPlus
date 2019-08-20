#!/usr/bin/env bash
pushd $(dirname "${0}") > /dev/null
BASEDIR=$(pwd -L)
FSI="../../packages/docs/FSharp.Compiler.Tools/tools/fsi.exe"

if [[ "$OS" != "Windows_NT" ]]
then
    mono $FSI --exec generate.fsx "$@" 
else
    $FSI --exec generate.fsx "$@" 
fi

# Sample usage 
#  ./docsrc/tools/generate.sh --define:RELEASE --define:REFERENCE
#  ./docsrc/tools/generate.sh --define:RELEASE --define:HELP
