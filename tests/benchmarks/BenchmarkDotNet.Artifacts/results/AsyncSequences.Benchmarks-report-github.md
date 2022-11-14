``` ini

BenchmarkDotNet=v0.12.1, OS=arch 
Intel Core i5-10210U CPU 1.60GHz, 1 CPU, 8 logical and 4 physical cores
.NET Core SDK=7.0.100-preview.7.22377.5
  [Host]     : .NET Core 6.0.9 (CoreCLR 6.0.922.41905, CoreFX 6.0.922.41905), X64 RyuJIT DEBUG
  DefaultJob : .NET Core 6.0.9 (CoreCLR 6.0.922.41905, CoreFX 6.0.922.41905), X64 RyuJIT


```
|               Method | times | threads |        Mean |   Error |  StdDev | Ratio | RatioSD |
|--------------------- |------ |-------- |------------:|--------:|--------:|------:|--------:|
|                 **Base** |    **10** |       **2** |    **133.3 ms** | **0.43 ms** | **0.40 ms** |  **1.00** |    **0.00** |
| StartImmediateAsTask |    10 |       2 |    133.2 ms | 0.24 ms | 0.21 ms |  1.00 |    0.00 |
| ManualResetEventSlim |    10 |       2 |    134.4 ms | 2.35 ms | 2.09 ms |  1.01 |    0.02 |
|                      |       |         |             |         |         |       |         |
|                 **Base** |    **10** |       **3** |    **133.2 ms** | **0.33 ms** | **0.30 ms** |  **1.00** |    **0.00** |
| StartImmediateAsTask |    10 |       3 |    133.4 ms | 0.43 ms | 0.38 ms |  1.00 |    0.00 |
| ManualResetEventSlim |    10 |       3 |    133.3 ms | 0.36 ms | 0.34 ms |  1.00 |    0.00 |
|                      |       |         |             |         |         |       |         |
|                 **Base** |   **100** |       **2** |  **1,332.8 ms** | **1.22 ms** | **1.14 ms** |  **1.00** |    **0.00** |
| StartImmediateAsTask |   100 |       2 |  1,331.9 ms | 0.93 ms | 0.83 ms |  1.00 |    0.00 |
| ManualResetEventSlim |   100 |       2 |  1,333.3 ms | 1.14 ms | 1.06 ms |  1.00 |    0.00 |
|                      |       |         |             |         |         |       |         |
|                 **Base** |   **100** |       **3** |  **1,332.7 ms** | **1.48 ms** | **1.16 ms** |  **1.00** |    **0.00** |
| StartImmediateAsTask |   100 |       3 |  1,332.5 ms | 1.20 ms | 1.13 ms |  1.00 |    0.00 |
| ManualResetEventSlim |   100 |       3 |  1,333.0 ms | 1.45 ms | 1.36 ms |  1.00 |    0.00 |
|                      |       |         |             |         |         |       |         |
|                 **Base** |  **1000** |       **2** | **13,334.9 ms** | **2.62 ms** | **2.32 ms** |  **1.00** |    **0.00** |
| StartImmediateAsTask |  1000 |       2 | 13,334.8 ms | 2.39 ms | 1.99 ms |  1.00 |    0.00 |
| ManualResetEventSlim |  1000 |       2 | 13,333.9 ms | 2.24 ms | 1.99 ms |  1.00 |    0.00 |
|                      |       |         |             |         |         |       |         |
|                 **Base** |  **1000** |       **3** | **13,335.0 ms** | **2.47 ms** | **2.19 ms** |  **1.00** |    **0.00** |
| StartImmediateAsTask |  1000 |       3 | 13,334.8 ms | 2.74 ms | 2.29 ms |  1.00 |    0.00 |
| ManualResetEventSlim |  1000 |       3 | 13,334.4 ms | 2.64 ms | 2.21 ms |  1.00 |    0.00 |
