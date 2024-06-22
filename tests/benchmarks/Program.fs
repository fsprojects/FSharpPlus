[<EntryPoint>]
let main _ =
    // do BenchmarkDotNet.Running.BenchmarkRunner.Run<ExtensionsBenchmarks.OptionBenchmarks>() |> ignore
    // do BenchmarkDotNet.Running.BenchmarkRunner.Run<ExtensionsBenchmarks.VOptionBenchmarks>() |> ignore
    do BenchmarkDotNet.Running.BenchmarkRunner.Run<ExtensionsBenchmarks.ResultBenchmarks>() |> ignore
    0
