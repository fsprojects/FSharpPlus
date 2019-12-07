#!/usr/bin/env bash
pushd $(dirname "${0}") > /dev/null
BASEDIR=$(pwd -L)

dotnet fsi --exec generate.fsx "$@" 

# Sample usage 
#  ./docsrc/tools/generate.sh --define:RELEASE --define:REFERENCE
#  ./docsrc/tools/generate.sh --define:RELEASE --define:HELP
