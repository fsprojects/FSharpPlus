module ExtensionsBenchmarks

open FSharpPlus
open BenchmarkDotNet.Attributes
open Microsoft.FSharp.Core.CompilerServices

[<AutoOpen>]
module Common =
    let mkArray s n invalidAtEnd arraySize =
        if invalidAtEnd then
            seq {
                yield! Array.init arraySize (fun _ -> s(1))
                yield n
            }
            |> Seq.toArray
        else
            seq {
                yield! Array.init (arraySize / 2) (fun _ -> s(1))
                yield n
                yield! Array.init (arraySize / 2) (fun _ -> s(1))
            }
            |> Seq.toArray

module OptionOldVersion =
    let sequence (t: seq<option<'T>>) =
        let mutable ok = true
        let res = Seq.toArray (seq {
            use e = t.GetEnumerator ()
            while e.MoveNext () && ok do
                match e.Current with
                | Some v -> yield v
                | None   -> ok <- false })
        if ok then Some (Array.toSeq res) else None
        
module VOptionOldVersion =
    let sequence (t: seq<voption<'T>>) =
        let mutable ok = true
        let res = Seq.toArray (seq {
            use e = t.GetEnumerator ()
            while e.MoveNext () && ok do
                match e.Current with
                | ValueSome v -> yield v
                | ValueNone   -> ok <- false })
        if ok then ValueSome (Array.toSeq res) else ValueNone

module OptionNewVersion =

    let sequence (t: seq<option<'T>>) =
        let mutable accumulator = ArrayCollector<'T> ()
        let mutable noneFound = false
        use e = t.GetEnumerator ()
        while e.MoveNext () && noneFound do
            match e.Current with
            | Some v -> accumulator.Add v
            | None -> noneFound <-  true
            
        if noneFound
        then None
        else
            Some (accumulator.Close () |> Array.toSeq)

module VOptionNewVersion =
         
    let sequence (t: seq<voption<'T>>) =
        let mutable accumulator = ArrayCollector<'T> ()
        let mutable noneFound = false
        use e = t.GetEnumerator ()
        while e.MoveNext () && noneFound do
            match e.Current with
            | ValueSome v -> accumulator.Add v
            | ValueNone -> noneFound <-  true
            
        if noneFound
        then ValueNone
        else
            ValueSome (accumulator.Close () |> Array.toSeq)

module ResultOldVersion =
    let sequence (t: seq<Result<_, _>>) =
        let mutable error = None
        let res = Seq.toArray (seq {
            use e = t.GetEnumerator ()
            while e.MoveNext () && error.IsNone do
                match e.Current with
                | Ok v -> yield v
                | Error e -> error <- Some e })

        match error with
        | None -> Ok (Array.toSeq res)
        | Some e -> Error e

module ResultNewVersion =
         
    let sequence (t: seq<Result<int, string>>) : Result<int seq, string> =
        let mutable accumulator = ArrayCollector<_> ()
        let mutable error = None
        use e = t.GetEnumerator ()
        while e.MoveNext () && error.IsNone do
            match e.Current with
            | Ok v -> accumulator.Add v
            | Error e -> error <- Some e
            
        match error with
        | None -> Ok (accumulator.Close () |> Array.toSeq)
        | Some e -> Error e

type Values<'t> = { Title: string; Values: 't }

[<MemoryDiagnoser>]
type OptionBenchmarks() =
    
    member this.runArray =
        seq {
            yield { Title = "1000_M"; Values = mkArray Some None false 1000 }
            yield { Title = "10000_M"; Values = mkArray Some None false 10000 }
            yield { Title = "100000_M"; Values = mkArray Some None false 100000 }
            yield { Title = "1000_E"; Values = mkArray Some None true 1000 }
            yield { Title = "10000_E"; Values = mkArray Some None true 10000 }
            yield { Title = "100000_E"; Values = mkArray Some None true 100000 }
        }

    [<Benchmark(Baseline = true)>]
    [<ArgumentsSource(nameof(Unchecked.defaultof<OptionBenchmarks>.runArray))>]
    member this.Base (v: Values<int option array>) = OptionOldVersion.sequence v.Values

    [<Benchmark>]
    [<ArgumentsSource(nameof(Unchecked.defaultof<OptionBenchmarks>.runArray))>]
    member this.NewVersion (v: Values<int option array>) = OptionNewVersion.sequence v.Values

[<MemoryDiagnoser>]
type VOptionBenchmarks() =
    
    member this.runArray =
        seq {
            yield { Title = "1000_M"; Values = mkArray ValueSome ValueNone false 1000 }
            yield { Title = "10000_M"; Values = mkArray ValueSome ValueNone false 10000 }
            yield { Title = "100000_M"; Values =  mkArray ValueSome ValueNone false 100000 }
            yield { Title = "1000_E"; Values = mkArray ValueSome ValueNone true 1000 }
            yield { Title = "10000_E"; Values = mkArray ValueSome ValueNone true 10000 }
            yield { Title = "100000_E"; Values =  mkArray ValueSome ValueNone true 100000 }
        }

    [<Benchmark(Baseline = true)>]
    [<ArgumentsSource(nameof(Unchecked.defaultof<VOptionBenchmarks>.runArray))>]
    member this.VOptionBase (v: Values<int voption array>) = VOptionOldVersion.sequence v.Values

    [<Benchmark>]
    [<ArgumentsSource(nameof(Unchecked.defaultof<VOptionBenchmarks>.runArray))>]
    member this.NewVersion (v: Values<int voption array>) = VOptionNewVersion.sequence v.Values

[<MemoryDiagnoser>]
type ResultBenchmarks() =
    
    member this.runArray =
        seq {
            yield { Title = "1000_M"; Values = mkArray Ok (Error "error") false 1000 }
            yield { Title = "10000_M"; Values = mkArray Ok (Error "error") false 10000 }
            yield { Title = "100000_M"; Values = mkArray Ok (Error "error") false 100000 }
            yield { Title = "1000_E"; Values = mkArray Ok (Error "error") true 1000 }
            yield { Title = "10000_E"; Values = mkArray Ok (Error "error") true 10000 }
            yield { Title = "100000_E"; Values = mkArray Ok (Error "error") true 100000 }
        }

    [<Benchmark(Baseline = true)>]
    [<ArgumentsSource(nameof(Unchecked.defaultof<ResultBenchmarks>.runArray))>]
    member this.ResultBase (v: Values<Result<int, string> array>) = ResultOldVersion.sequence v.Values

    [<Benchmark>]
    [<ArgumentsSource(nameof(Unchecked.defaultof<ResultBenchmarks>.runArray))>]
    member this.NewVersion (v: Values<Result<int, string> array>) = ResultNewVersion.sequence v.Values
