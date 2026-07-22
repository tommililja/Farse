open System
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
type Subscription = {
    Plan: string
    IsCanceled: bool
    RenewsAt: DateTime option
}

[<CLIMutable>]
type User = {
    Id: Guid
    Name: string
    Age: byte option
    Email: string
    Profiles: Guid array
    Subscription: Subscription
    Tags: string array
}

module BenchmarkData =

    let json n =
        [ 1 .. n ]
        |> JObj.arr (fun _ ->
            [
                "id", JStr "c8eae96a-025d-4bc9-88f8-f204e95f2883"
                "name", JStr "Alice"
                "age", JNil
                "email", JStr "alice@domain.com"
                "profiles",
                    JStr.arr id [
                        "01458283-b6e3-4ae7-ae54-a68eb587cdc0"
                        "927eb20f-cd62-470c-aafc-c3ce6b9248b0"
                        "bf00d1e2-ee53-4969-9507-86bed7e96432"
                    ]
                "subscription",
                    JObj [
                        "plan", JStr "pro"
                        "isCanceled", JBit false
                        "renewsAt", JStr "2026-12-25T10:30:00Z"
                    ]
                "tags",
                    JArr [
                        JStr "beta"
                        JStr "verified"
                    ]
            ]
        )

module NewtonsoftJson =

    let asOption fn (t:JToken) =
        match t with
        | x when x.Type <> JTokenType.Null -> Some <| fn x
        | _ -> None

    let asArray fn (t:JToken) =
        let array = t :?> JArray
        let items = Array.zeroCreate array.Count
        for i in 0 .. array.Count - 1 do
            items[i] <- fn array[i]
        items

module SystemTextJson =

    let asOption fn (e:JsonElement) =
        match e with
        | x when x.ValueKind <> JsonValueKind.Null -> Some <| fn x
        | _ -> None

    let asArray fn (e:JsonElement) =
        let items = Array.zeroCreate <| e.GetArrayLength()
        let mutable enumerator = e.EnumerateArray()
        let mutable i = 0
        while enumerator.MoveNext() do
            items[i] <- fn enumerator.Current
            i <- i + 1
        items

[<MemoryDiagnoser(true); Orderer(SummaryOrderPolicy.FastestToSlowest)>]
type ParserBenchmarks() =

    let json =
        BenchmarkData.json 100
        |> Json.asString Indented

    let farse =
        parser {
            let! id = "id" &= Parse.guid
            and! name = "name" &= Parse.string
            and! age = "age" ?= Parse.byte
            and! email = "email" &= Parse.string
            and! profiles = "profiles" &= Parse.array Parse.guid

            and! subscription = "subscription" &= parser {
                let! plan = "plan" &= Parse.string
                and! isCanceled = "isCanceled" &= Parse.bool
                and! renewsAt = "renewsAt" ?= Parse.dateTime

                return {
                    Plan = plan
                    IsCanceled = isCanceled
                    RenewsAt = renewsAt
                }
            }

            and! tags = "tags" &= Parse.array Parse.string

            return {
                Id = id
                Name = name
                Age = age
                Email = email
                Profiles = profiles
                Subscription = subscription
                Tags = tags
            }
        }
        |> Parse.array

    let thothJsonNet =
        let subscription =
            Thoth.Json.Net.Decode.object (fun get ->
                {
                    Plan = get.Required.Field "plan" Thoth.Json.Net.Decode.string
                    IsCanceled = get.Required.Field "isCanceled" Thoth.Json.Net.Decode.bool
                    RenewsAt = get.Optional.Field "renewsAt" Thoth.Json.Net.Decode.datetimeLocal
                }
            )

        Thoth.Json.Net.Decode.object (fun get ->
            {
                Id = get.Required.Field "id" Thoth.Json.Net.Decode.guid
                Name = get.Required.Field "name" Thoth.Json.Net.Decode.string
                Age = get.Optional.Field "age" Thoth.Json.Net.Decode.byte
                Email = get.Required.Field "email" Thoth.Json.Net.Decode.string
                Profiles = get.Required.Field "profiles" (Thoth.Json.Net.Decode.array Thoth.Json.Net.Decode.guid)
                Subscription = get.Required.Field "subscription" subscription
                Tags = get.Required.Field "tags" (Thoth.Json.Net.Decode.array Thoth.Json.Net.Decode.string)
            }
        )
        |> Thoth.Json.Net.Decode.array

    let thothSystemTextJson =
        let subscription =
            Decode.object (fun get ->
                {
                    Plan = get.Required.Field "plan" Decode.string
                    IsCanceled = get.Required.Field "isCanceled" Decode.bool
                    RenewsAt = get.Optional.Field "renewsAt" Decode.datetimeLocal
                }
            )

        Decode.object (fun get ->
            {
                Id = get.Required.Field "id" Decode.guid
                Name = get.Required.Field "name" Decode.string
                Age = get.Optional.Field "age" Decode.byte
                Email = get.Required.Field "email" Decode.string
                Profiles = get.Required.Field "profiles" (Decode.array Decode.guid)
                Subscription = get.Required.Field "subscription" subscription
                Tags = get.Required.Field "tags" (Decode.array Decode.string)
            }
        )
        |> Decode.array

    let options = JsonSerializerOptions (
        PropertyNameCaseInsensitive = true
    )

    let settings =
        let settings = JsonSerializerSettings()
        settings.Converters.Add(CompactUnionJsonConverter())
        settings

    [<Benchmark(Description = "Newtonsoft.Json*")>]
    member _.NewtonsoftJsonSerialization() =
        JsonConvert.DeserializeObject<User array>(json, settings)

    [<Benchmark(Description = "System.Text.Json*")>]
    member _.SystemTextJsonSerialization() =
        System.Text.Json.JsonSerializer.Deserialize<User array>(json, options)

    [<Benchmark(Description = "Newtonsoft.Json")>]
    member _.NewtonsoftJson() =
        JArray.Parse(json)
        |> NewtonsoftJson.asArray (fun user ->
            let user = user :?> JObject
            let subscription = user.GetValue("subscription") :?> JObject
            {
                Id = user.GetValue("id").Value<string>() |> Guid.Parse
                Name = user.GetValue("name").Value<string>()
                Age = user.GetValue("age") |> NewtonsoftJson.asOption _.Value<byte>()
                Email = user.GetValue("email").Value<string>()
                Profiles = user.GetValue("profiles") |> NewtonsoftJson.asArray (_.Value<string>() >> Guid.Parse)
                Subscription =
                    {
                        Plan = subscription.GetValue("plan").Value<string>()
                        IsCanceled = subscription.GetValue("isCanceled").Value<bool>()
                        RenewsAt = subscription.GetValue("renewsAt") |> NewtonsoftJson.asOption _.Value<DateTime>()
                    }
                Tags = user.GetValue("tags") |> NewtonsoftJson.asArray _.Value<string>()
            }
        )

    [<Benchmark(Description = "System.Text.Json")>]
    member _.SystemTextJson() =
        use doc = JsonDocument.Parse(json)
        doc.RootElement
        |> SystemTextJson.asArray (fun user ->
            let subscription = user.GetProperty("subscription")
            {
                Id = user.GetProperty("id").GetGuid()
                Name = user.GetProperty("name").GetString()
                Age = user.GetProperty("age") |> SystemTextJson.asOption _.GetByte()
                Email = user.GetProperty("email").GetString()
                Profiles = user.GetProperty("profiles") |> SystemTextJson.asArray _.GetGuid()
                Subscription = {
                    Plan = subscription.GetProperty("plan").GetString()
                    IsCanceled = subscription.GetProperty("isCanceled").GetBoolean()
                    RenewsAt = subscription.GetProperty("renewsAt") |> SystemTextJson.asOption _.GetDateTime()
                }
                Tags = user.GetProperty("tags") |> SystemTextJson.asArray _.GetString()
            }
        )

    [<Benchmark(Description = "Thoth.Json.Net")>]
    member _.ThothJsonNet() =
        json
        |> Thoth.Json.Net.Decode.fromString thothJsonNet
        |> Result.defaultWith failwith

    [<Benchmark(Description = "Thoth.System.Text.Json")>]
    member _.ThothSystemTextJson() =
        json
        |> System.Text.Json.Decode.fromString thothSystemTextJson
        |> Result.defaultWith failwith

    [<Benchmark(Description = "Farse", Baseline = true)>]
    member _.Farse() =
        farse
        |> Parser.parse json
        |> Result.mapError ParserError.asString
        |> Result.defaultWith failwith

let config =
    ManualConfig
        .Create(DefaultConfig.Instance)
        .WithSummaryStyle(SummaryStyle.Default)
        .HideColumns("Error", "StdDev", "RatioSD")

let summary =
    BenchmarkRunner
        .Run<ParserBenchmarks>(config)