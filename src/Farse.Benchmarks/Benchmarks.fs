open System
open System.IO
open System.Text.Json
open BenchmarkDotNet.Attributes
open BenchmarkDotNet.Configs
open BenchmarkDotNet.Order
open BenchmarkDotNet.Reports
open BenchmarkDotNet.Running
open Microsoft.FSharpLu.Json
open Newtonsoft.Json
open Newtonsoft.Json.Linq
open Thoth.Json
open Thoth.Json.Core
open Farse
open Farse.Operators

[<CLIMutable>]
[<NoComparison>]
type Subscription = {
    Plan: string
    IsCanceled: bool
    RenewsAt: DateTime option
}

[<CLIMutable>]
[<NoComparison>]
type User = {
    Id: Guid
    Name: string
    Age: byte option
    Email: string
    Profiles: Guid Set
    Subscription: Subscription
}

[<MemoryDiagnoser(true)>]
[<Orderer(SummaryOrderPolicy.FastestToSlowest)>]
type Benchmarks() =

    let mutable json = String.Empty

    [<GlobalSetup>]
    member _.Setup() = json <- File.ReadAllText("Benchmarks.100.json")

    [<Benchmark(Description = "Newtonsoft.Json*")>]
    member _.NewtonsoftJsonSerialization() =
        let settings = JsonSerializerSettings()
        settings.Converters.Add(CompactUnionJsonConverter())
        JsonConvert.DeserializeObject<User array>(json, settings)

    [<Benchmark(Description = "System.Text.Json*")>]
    member _.SystemTextJsonSerialization() =
        System.Text.Json.JsonSerializer.Deserialize<User array>(
            json, JsonSerializerOptions(PropertyNameCaseInsensitive = true))

    [<Benchmark(Description = "Newtonsoft.Json")>]
    member this.NewtonsoftJson() =
        let users = JArray.Parse(json)

        let inline asOption fn (x:JToken) =
            match x with
            | x when x.Type <> JTokenType.Null -> Some <| fn x
            | _ -> None

        for user in users do
            let user = user :?> JObject
            let _ = user.GetValue("id").Value<string>() |> Guid.Parse
            let _ = user.GetValue("name").Value<string>()
            let _ = user.GetValue("age") |> asOption _.Value<byte>()
            let _ = user.GetValue("email") |> _.Value<string>()
            let _ = user.GetValue("profiles") |> Seq.map (_.Value<string>() >> Guid.Parse)

            let subscription = user.GetValue("subscription") :?> JObject
            let _ = subscription.GetValue("plan").Value<string>()
            let _ = subscription.GetValue("isCanceled").Value<bool>()
            let _ = subscription.GetValue("renewsAt") |> asOption _.Value<DateTime>()

            ()

    [<Benchmark(Description = "System.Text.Json")>]
    member _.SystemTextJson() =
        use doc = JsonDocument.Parse(json)
        let users = doc.RootElement.EnumerateArray()

        let inline asOption fn (x:JsonElement) =
            match x with
            | x when x.ValueKind <> JsonValueKind.Null -> Some <| fn x
            | _ -> None

        for user in users do
            let _ = user.GetProperty("id").GetGuid()
            let _ = user.GetProperty("name").GetString()
            let _ = user.GetProperty("age") |> asOption _.GetByte()
            let _ = user.GetProperty("email").GetString()
            let _ = user.GetProperty("profiles").EnumerateArray() |> Seq.map _.GetGuid()

            let subscription = user.GetProperty("subscription")
            let _ = subscription.GetProperty("plan").GetString()
            let _ = subscription.GetProperty("isCanceled").GetBoolean()
            let _ = subscription.GetProperty("renewsAt") |> asOption _.GetDateTime()

            ()

    [<Benchmark(Description = "Thoth.Json.Net")>]
    member _.ThothJsonNet() =
        let subscription =
            Thoth.Json.Net.Decode.object (fun get ->
                let _ = get.Required.Field "plan" Thoth.Json.Net.Decode.string
                let _ = get.Required.Field "isCanceled" Thoth.Json.Net.Decode.bool
                let _ = get.Optional.Field "renewsAt" Thoth.Json.Net.Decode.datetimeLocal

                ()
            )

        let decoder =
            Thoth.Json.Net.Decode.object (fun get ->
                let _ = get.Required.Field "id" Thoth.Json.Net.Decode.guid
                let _ = get.Required.Field "name" Thoth.Json.Net.Decode.string
                let _ = get.Optional.Field "age" Thoth.Json.Net.Decode.byte
                let _ = get.Required.Field "email" Thoth.Json.Net.Decode.string
                let _ = get.Required.Field "profiles" (Thoth.Json.Net.Decode.array Thoth.Json.Net.Decode.guid)
                let _ = get.Required.Field "subscription" subscription

                ()
            )

        json
        |> Thoth.Json.Net.Decode.fromString (Thoth.Json.Net.Decode.array decoder)
        |> Result.defaultWith failwith

    [<Benchmark(Description = "Thoth.System.Text.Json")>]
    member _.ThothSystemTextJson() =
        let subscription =
            Decode.object (fun get ->
                let _ = get.Required.Field "plan" Decode.string
                let _ = get.Required.Field "isCanceled" Decode.bool
                let _ = get.Optional.Field "renewsAt" Decode.datetimeLocal

                ()
            )

        let decoder =
            Decode.object (fun get ->
                let _ = get.Required.Field "id" Decode.guid
                let _ = get.Required.Field "name" Decode.string
                let _ = get.Optional.Field "age" Decode.byte
                let _ = get.Required.Field "email" Decode.string
                let _ = get.Required.Field "profiles" (Decode.array Decode.guid)
                let _ = get.Required.Field "subscription" subscription

                ()
            )

        json
        |> System.Text.Json.Decode.fromString (Decode.array decoder)
        |> Result.defaultWith failwith

    [<Benchmark(Description = "Farse", Baseline = true)>]
    member _.Farse() =
        let parser =
            parser {
                let! _ = "id" &= Parse.guid
                let! _ = "name" &= Parse.string
                let! _ = "age" ?= Parse.byte
                let! _ = "email" &= Parse.string
                let! _ = "profiles" &= Parse.array Parse.guid

                let! _ = "subscription" &= parser {
                    let! _ = "plan" &= Parse.string
                    let! _ = "isCanceled" &= Parse.bool
                    let! _ = "renewsAt" ?= Parse.dateTime

                    return ()
                }

                return ()
            }

        parser
        |> Parse.array
        |> Parser.parse json
        |> Result.defaultWith failwith

let config =
    ManualConfig
        .Create(DefaultConfig.Instance)
        .WithSummaryStyle(SummaryStyle.Default)
        .HideColumns("Error", "StdDev", "RatioSD")

let summary = BenchmarkRunner.Run<Benchmarks>(config)