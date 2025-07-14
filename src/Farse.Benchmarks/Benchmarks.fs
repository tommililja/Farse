open System
open System.IO
open System.Text.Json
open BenchmarkDotNet.Attributes
open BenchmarkDotNet.Configs
open BenchmarkDotNet.Order
open BenchmarkDotNet.Reports
open BenchmarkDotNet.Running
open Newtonsoft.Json
open Newtonsoft.Json.Linq
open Farse
open Farse.Operators
open Thoth.Json
open Thoth.Json.Core

type Base = {
    Int: int
    Decimal: decimal
    String: string
    Bool: bool
    Guid: Guid
    DateTime: DateTime
}

type Benchmark = {
    Int: int
    Decimal: decimal
    String: string
    Bool: bool
    Guid: Guid
    DateTime: DateTime
    Array: Base list
    Object: Base
}

[<MemoryDiagnoser(true)>]
[<Orderer(SummaryOrderPolicy.FastestToSlowest)>]
type BenchmarksNested() =

    let mutable json = String.Empty

    [<GlobalSetup>]
    member _.Setup() =
        json <- File.ReadAllText("Benchmarks.Nested.json")

    [<Benchmark(Description = "Farse: Path / Single")>]
    member _.FarsePathSingle() =
        parser {
            let! _ = "object.object.string" &= Parse.string

            return ()
        }
        |> Parser.parse json
        |> Result.defaultWith failwith

    [<Benchmark(Description = "Farse: Path / Multiple")>]
    member _.FarsePathMultiple() =
        parser {
            let! _ = "object.object.string" &= Parse.string
            let! _ = "object.object.int" &= Parse.int
            let! _ = "object.object.guid" &= Parse.guid

            return ()
        }
        |> Parser.parse json
        |> Result.defaultWith failwith

    [<Benchmark(Description = "Farse: Objects / Single")>]
    member _.FarseObjectsSingle() =
        parser {
            let! _ = "object" &= parser {
                 return! "object" &= parser {
                    let! _ = "string" &= Parse.string

                    return ()
                }
            }

            return ()
        }
        |> Parser.parse json
        |> Result.defaultWith failwith

    [<Benchmark(Description = "Farse: Objects / Multiple")>]
    member _.FarseObjectsMany() =
        parser {
            let! _ = "object" &= parser {
                 return! "object" &= parser {
                    let! _ = "string" &= Parse.string
                    let! _ = "int" &= Parse.int
                    let! _ = "guid" &= Parse.guid

                    return ()
                }
            }

            return ()
        }
        |> Parser.parse json
        |> Result.defaultWith failwith

[<MemoryDiagnoser(true)>]
[<Orderer(SummaryOrderPolicy.FastestToSlowest)>]
type Benchmarks() =

    let mutable json = String.Empty

    [<GlobalSetup>]
    member _.Setup() =
        json <- File.ReadAllText("Benchmarks.json")

    [<Benchmark(Description = "Newtonsoft.Json*")>]
    member _.NewtonsoftJsonSerialization() =
        JsonConvert.DeserializeObject<Benchmark>(json)

    [<Benchmark(Description = "Newtonsoft.Json")>]
    member _.NewtonsoftJson() =
        let o = JObject.Parse(json)

        let object (o:JObject) =
            let _ = o.Property("int").Value.Value<int>()
            let _ = o.Property("decimal").Value.Value<decimal>()
            let _ = o.Property("string").Value.Value<string>()
            let _ = o.Property("bool").Value.Value<bool>()
            let _ = o.Property("guid").Value.Value<string>() |> Guid.Parse
            let _ = o.Property("dateTime").Value.Value<DateTime>()

            ()

        object o

        let _ =
            o["array"] :?> JArray
            |> Seq.map (fun x -> x :?> JObject)
            |> Seq.map object
            |> Seq.toList

        let _ = o["object"] :?> JObject |> object

        ()

    [<Benchmark(Description = "System.Text.Json")>]
    member _.SystemTextJson() =
        use doc = JsonDocument.Parse(json)
        let e = doc.RootElement

        let object (e:JsonElement) =
            let _ = e.GetProperty("int").GetInt32()
            let _ = e.GetProperty("decimal").GetDecimal()
            let _ = e.GetProperty("string").GetString()
            let _ = e.GetProperty("bool").GetBoolean()
            let _ = e.GetProperty("guid").GetGuid()
            let _ = e.GetProperty("dateTime").GetDateTime()

            ()

        object e

        use arr = e.GetProperty("array").EnumerateArray()

        let _ =
            arr
            |> Seq.map object
            |> Seq.toList

        let _ = e.GetProperty("object") |> object

        ()

    [<Benchmark(Description = "System.Text.Json*")>]
    member _.SystemTextJsonSerialization() =
        System.Text.Json.JsonSerializer.Deserialize<Benchmark>(
            json, JsonSerializerOptions(PropertyNameCaseInsensitive = true))

    [<Benchmark(Description = "Thoth.Json.Net")>]
    member _.ThothJsonNet() =
        let object =
            Thoth.Json.Net.Decode.object (fun get ->
                let _ = get.Required.Field "int" Thoth.Json.Net.Decode.int
                let _ = get.Required.Field "decimal" Thoth.Json.Net.Decode.decimal
                let _ = get.Required.Field "string" Thoth.Json.Net.Decode.string
                let _ = get.Required.Field "bool" Thoth.Json.Net.Decode.bool
                let _ = get.Required.Field "guid" Thoth.Json.Net.Decode.guid
                let _ = get.Required.Field "dateTime" Thoth.Json.Net.Decode.datetimeLocal

                ()
            )

        let decoder =
            Thoth.Json.Net.Decode.object (fun get ->
                let _ = get.Required.Raw object
                let _ = get.Required.Field "array" (Thoth.Json.Net.Decode.list object)
                let _ = get.Required.Field "object" object

                ()
            )

        json
        |> Thoth.Json.Net.Decode.fromString decoder
        |> Result.defaultWith failwith

    [<Benchmark(Description = "Thoth.System.Text.Json")>]
    member _.ThothSystemTextJson() =
        let object =
            Decode.object (fun get ->
                let _ = get.Required.Field "int" Decode.int
                let _ = get.Required.Field "decimal" Decode.decimal
                let _ = get.Required.Field "string" Decode.string
                let _ = get.Required.Field "bool" Decode.bool
                let _ = get.Required.Field "guid" Decode.guid
                let _ = get.Required.Field "dateTime" Decode.datetimeLocal

                ()
            )

        let decoder =
            Decode.object (fun get ->
                let _ = get.Required.Raw object
                let _ = get.Required.Field "array" (Decode.list object)
                let _ = get.Required.Field "object" object

                ()
            )

        json
        |> System.Text.Json.Decode.fromString decoder
        |> Result.defaultWith failwith

    [<Benchmark(Description = "Farse", Baseline = true)>]
    member _.Farse() =
        let object =
            parser {
                let! _ = "int" &= Parse.int
                let! _ = "decimal" &= Parse.decimal
                let! _ = "string" &= Parse.string
                let! _ = "bool" &= Parse.bool
                let! _ = "guid" &= Parse.guid
                let! _ = "dateTime" &= Parse.dateTime

                return ()
            }

        parser {
            do! object

            let! _ = "array" &= Parse.list object
            let! _ = "object" &= object

            return ()
        }
        |> Parser.parse json
        |> Result.defaultWith failwith

let config =
    ManualConfig
        .Create(DefaultConfig.Instance)
        .WithSummaryStyle(SummaryStyle.Default)
        .HideColumns("Error", "StdDev", "RatioSD")

let summary = BenchmarkRunner.Run<Benchmarks>(config)