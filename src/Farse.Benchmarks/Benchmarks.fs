namespace Farse.Benchmarks

open System
open System.Text.Json
open BenchmarkDotNet.Attributes
open BenchmarkDotNet.Order
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
        |> JObj.array (fun _ ->
            [
                "id", JStr "c8eae96a-025d-4bc9-88f8-f204e95f2883"
                "name", JStr "Alice"
                "age", JNil
                "email", JStr "alice@domain.com"
                "profiles",
                    JStr.array id [
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
        |> JToken.asArray (fun user ->
            let user = user :?> JObject
            let subscription = user.GetValue("subscription") :?> JObject
            {
                Id = user.GetValue("id").Value<string>() |> Guid.Parse
                Name = user.GetValue("name").Value<string>()
                Age = user.GetValue("age") |> JToken.asOption _.Value<byte>()
                Email = user.GetValue("email").Value<string>()
                Profiles = user.GetValue("profiles") |> JToken.asArray (_.Value<string>() >> Guid.Parse)
                Subscription =
                    {
                        Plan = subscription.GetValue("plan").Value<string>()
                        IsCanceled = subscription.GetValue("isCanceled").Value<bool>()
                        RenewsAt = subscription.GetValue("renewsAt") |> JToken.asOption _.Value<DateTime>()
                    }
                Tags = user.GetValue("tags") |> JToken.asArray _.Value<string>()
            }
        )

    [<Benchmark(Description = "System.Text.Json")>]
    member _.SystemTextJson() =
        use doc = JsonDocument.Parse(json)
        doc.RootElement
        |> JsonElement.asArray (fun e ->
            let subscription = e.GetProperty("subscription")
            {
                Id = e.GetProperty("id").GetGuid()
                Name = e.GetProperty("name").GetString()
                Age = e.GetProperty("age") |> JsonElement.asOption _.GetByte()
                Email = e.GetProperty("email").GetString()
                Profiles = e.GetProperty("profiles") |> JsonElement.asArray _.GetGuid()
                Subscription = {
                    Plan = subscription.GetProperty("plan").GetString()
                    IsCanceled = subscription.GetProperty("isCanceled").GetBoolean()
                    RenewsAt = subscription.GetProperty("renewsAt") |> JsonElement.asOption _.GetDateTime()
                }
                Tags = e.GetProperty("tags") |> JsonElement.asArray _.GetString()
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