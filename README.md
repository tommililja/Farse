![Build](https://github.com/tommililja/Farse/actions/workflows/dotnet.yml/badge.svg)
[![NuGet Version](https://img.shields.io/nuget/v/Farse.svg)](https://www.nuget.org/packages/Farse)

# Farse

>Simple parsing library for F# using System.Text.Json.

Heavily inspired by Thoth.Json.Net and its composability; Farse uses a slightly different syntax, includes a computational expression, and a few custom operators that simplify parsing. It also tries to keep a low overhead while still providing utility and acceptable error messages.

## Installation

Things are still changing but a pre-release version is available.

```shell
dotnet add package Farse --prerelease
```

## Benchmarks

There are some initial benchmarks [here](https://github.com/tommililja/Farse/blob/main/src/Farse.Benchmarks/Benchmarks.fs).

```shell
BenchmarkDotNet v0.15.2, macOS Sequoia 15.5 (24F74) [Darwin 24.5.0]
Apple M1 Pro, 1 CPU, 8 logical and 8 physical cores
.NET SDK 9.0.203
  [Host]     : .NET 9.0.4 (9.0.425.16305), Arm64 RyuJIT AdvSIMD DEBUG
  DefaultJob : .NET 9.0.4 (9.0.425.16305), Arm64 RyuJIT AdvSIMD
```

```shell
| Method                 | Mean      | Ratio | Gen0   | Gen1   | Allocated | Alloc Ratio |
|----------------------- |----------:|------:|-------:|-------:|----------:|------------:|
| System.Text.Json       |  3.660 us |  0.75 | 0.1106 |      - |     696 B |        0.21 |
| System.Text.Json*      |  3.663 us |  0.75 | 0.4082 | 0.0076 |    2562 B |        0.77 |
| Farse                  |  4.877 us |  1.00 | 0.5264 |      - |    3320 B |        1.00 |
| Newtonsoft.Json*       |  6.257 us |  1.28 | 1.5182 | 0.0229 |    9544 B |        2.87 |
| Thoth.System.Text.Json |  8.195 us |  1.68 | 1.5869 | 0.0305 |    9944 B |        3.00 |
| Newtonsoft.Json        |  8.461 us |  1.73 | 2.8229 | 0.1373 |   17720 B |        5.34 |
| Thoth.Json.Net         | 10.340 us |  2.12 | 3.3569 | 0.1526 |   21136 B |        6.37 |        6.37 |
```

\* Serialization

## Example

Given the following JSON string.

```json
{
    "id": "c8eae96a-025d-4bc9-88f8-f204e95f2883",
    "name": "Alice",
    "age": null,
    "email": "alice@domain.com",
    "groups": [
        "01458283-b6e3-4ae7-ae54-a68eb587cdc0",
        "bf00d1e2-ee53-4969-9507-86bed7e96432",
        "927eb20f-cd62-470c-aafc-c3ce6b9248b0"
    ],
    "subscription": {
        "plan": "Pro",
        "isCanceled": true,
        "renewsAt": null
    }
}
```

And the two (optional) operators.

```fsharp
/// <summary>
/// Parses a required property.
/// </summary>
let (&=) = Parse.req

/// <summary>
/// Parses an optional property.
/// </summary>
let (?=) = Parse.opt
```

We can create the following parser.

```fsharp
open Farse
open Farse.Operators

module User =
    open Parse

    let parser =
        parser {
            let! id = "id" &= UserId.parser
            let! name = "name" &= string
            let! age = "age" ?= int
            let! email = "email" &= Email.parser
            let! groups = "groups" &= list GroupId.parser

            let! subscription = "subscription" &= parser {
                let! plan = "plan" &= Plan.parser
                let! isCanceled = "isCanceled" &= bool
                let! renewsAt = "renewsAt" ?= dateTime
    
                return {
                    Plan = plan
                    IsCanceled = isCanceled
                    RenewsAt = renewsAt
                }
            }

            // Simple "path" support, which can be very useful
            // when we just want to parse a (few) nested value(s).
            let! isCanceled = "subscription.isCanceled" &= bool
      
            return {
                Id = id
                Name = name
                Age = age
                Email = email
                Groups = groups
                Subscription = subscription
            }
        }
```

With the following types.

```fsharp
open Farse

type UserId = UserId of Guid

module UserId =

    let asString (UserId x) =
        string x

    let parser =
        Parse.guid
        |> Parser.map UserId

type Email = Email of string

module Email =

    let asString (Email x) = x

    let fromString str =
        // Some validation
        Ok (Email str)

    let parser =
        Parse.string
        |> Parser.validate fromString

type GroupId = GroupId of Guid

module GroupId =

    let asString (GroupId x) =
        string x

    let parser =
        Parse.guid
        |> Parser.map GroupId

type Plan =
    | Pro
    | Standard
    | Free

module Plan =

    let fromString = function
        | "Pro" -> Ok Pro
        | "Standard" -> Ok Standard
        | "Free" -> Ok Free
        | invalid -> Error $"Invalid plan: %s{invalid}."

    let asString = function
        | Pro -> "Pro"
        | Standard -> "Standard"
        | Free -> "Free"

    let parser =
        Parse.string
        |> Parser.validate fromString

type Subscription = {
    Plan: Plan
    IsCanceled: bool
    RenewsAt: DateTime option
}

type User = {
    Id: UserId
    Name: string
    Age: int option
    Email: Email
    Groups: GroupId list
    Subscription: Subscription
}
```

Then we can just run the parser.

```fsharp
let user =
    User.parser
    |> Parser.parse json
    |> Result.defaultWith failwith

printfn "%s" user.Name
```

We can also construct JSON strings.

```fsharp
let jsonString =
    JsonString.create [
        "id", JStr (UserId.asString user.Id)
        "name", JStr user.Name
        "age",
            user.Age
            |> Option.map (Int >> JNum)
            |> JOpt
        "email", JStr (Email.asString user.Email)
        "groups",
            user.Groups
            |> List.map (GroupId.asString >> JStr)
            |> JArr
        "subscription",
            JObj [
                "plan", JStr (Plan.asString user.Subscription.Plan)
                "isCanceled", JBit user.Subscription.IsCanceled
                "renewsAt",
                    user.Subscription.RenewsAt
                    |> Option.map (DateTime.asString >> JStr)
                    |> JOpt
            ]
    ]

let json =
    jsonString
    |> JsonString.asString

printfn "%s" json
```
