open sequence

[<EntryPoint>]
let main argv =
    let summary = BenchmarkDotNet.Running.BenchmarkRunner.Run<Benchmarks>()
    0