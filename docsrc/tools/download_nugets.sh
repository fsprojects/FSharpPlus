#!/bin/bash
pushd $(dirname "${0}") > /dev/null
cd ../../
mkdir -p packages/docs/
nuget install FSharp.Core  -ExcludeVersion -version 4.6.2  -source https://www.nuget.org/api/v2   -OutputDirectory packages/docs/
nuget install System.Runtime  -ExcludeVersion -version 4.3.1  -source https://www.nuget.org/api/v2   -OutputDirectory packages/docs/
nuget install MathNet.Numerics.FSharp  -ExcludeVersion -version 4.8.1  -source https://www.nuget.org/api/v2   -OutputDirectory packages/docs/
nuget install FSharp.Literate  -ExcludeVersion -version 4.0.0-alpha03  -source https://www.nuget.org/api/v2   -OutputDirectory packages/docs/
nuget install FSharp.Compiler.Service -ExcludeVersion  -source https://www.nuget.org/api/v2   -OutputDirectory packages/docs/
