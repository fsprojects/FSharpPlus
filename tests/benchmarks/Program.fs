open AsyncSequences

[<EntryPoint>]
let main _ =
    do BenchmarkDotNet.Running.BenchmarkRunner.Run<Benchmarks>() |> ignore
    0