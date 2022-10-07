namespace Diagnosers

open System
open BenchmarkDotNet.Configs
open BenchmarkDotNet.Diagnosers
open BenchmarkDotNet.Reports
open BenchmarkDotNet.Columns

type private CompletedWorkItemCountMetricDescriptor() =
    static member Instance =
        CompletedWorkItemCountMetricDescriptor()

    interface IMetricDescriptor with
        member this.DisplayName = "CompletedWorkItems"
        member this.Id = "CompletedWorkItemCount"

        member this.Legend =
            "The number of work () items that have been processed in ThreadPool (per single operation)"

        member this.NumberFormat = "#0.0000 ()"
        member this.TheGreaterTheBetter = false
        member this.Unit = "Count"
        member this.UnitType = UnitType.Dimensionless

type private LockContentionCountMetricDescriptor() =
    static member Instance =
        LockContentionCountMetricDescriptor()

    interface IMetricDescriptor with
        member this.DisplayName = "LockContentions"
        member this.Id = "LockContentionCount"

        member this.Legend =
            "The number of times there was contention upon trying to take a Monitor's lock (per single operation)"

        member this.NumberFormat = "#0.0000"
        member this.TheGreaterTheBetter = false
        member this.Unit = "Count"
        member this.UnitType = UnitType.Dimensionless

type CustomerThreadingDiagnoser() =
    static member Default = CustomerThreadingDiagnoser()

    interface IDiagnoser with
        member this.DisplayResults _ = ()
        member this.GetRunMode _ = RunMode.NoOverhead
        member this.Handle(_, _) = ()

        member this.ProcessResults(results) =
            seq {
                let completedWorkItemCount =
                    double results.ThreadingStats.CompletedWorkItemCount

                let totalOperations =
                    double results.ThreadingStats.TotalOperations

                let lockContentionCount =
                    double results.ThreadingStats.LockContentionCount

                printfn $"completedWorkItemCount: {completedWorkItemCount}"
                printfn $"totalOperations: {totalOperations}"
                printfn $"lockContentionCount: {lockContentionCount}"

                yield Metric(CompletedWorkItemCountMetricDescriptor.Instance, completedWorkItemCount / totalOperations)
                yield Metric(LockContentionCountMetricDescriptor.Instance, lockContentionCount / totalOperations)
            }

        member this.Validate _ = Array.empty
        member this.Analysers = Array.empty
        member this.Exporters = Array.empty

        member this.Ids =
            [| nameof CustomerThreadingDiagnoser |]

[<AttributeUsage(AttributeTargets.Class)>]
type CustomerThreadingDiagnoserAttribute() =
    inherit Attribute()

    interface IConfigSource with
        member this.Config =
            ManualConfig
                .CreateEmpty()
                .AddDiagnoser(CustomerThreadingDiagnoser.Default)