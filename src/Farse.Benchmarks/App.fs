open BenchmarkDotNet.Configs
open BenchmarkDotNet.Reports
open BenchmarkDotNet.Running
open Farse.Benchmarks

let config =
    ManualConfig
        .Create(DefaultConfig.Instance)
        .WithSummaryStyle(SummaryStyle.Default)
        .HideColumns("Error", "StdDev", "RatioSD")

let summary =
    BenchmarkRunner
        .Run<ParserBenchmarks>(config)