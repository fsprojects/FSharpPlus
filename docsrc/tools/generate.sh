#!/usr/bin/env bash
pushd $(dirname "${0}") > /dev/null
BASEDIR=$(pwd -L)

dotnet fsi --exec generate.fsx $@ 
