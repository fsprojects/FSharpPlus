#!/bin/bash
pushd $(dirname "${0}") > /dev/null
dotnet pack build.proj
