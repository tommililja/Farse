![Build](https://github.com/tommililja/Farse/actions/workflows/dotnet.yml/badge.svg)
[![NuGet Version](https://img.shields.io/nuget/v/Farse.svg)](https://www.nuget.org/packages/Farse)

# Farse

>Simple parsing library for F# using System.Text.Json.

Inspired by [Thoth.Json.Net](https://github.com/thoth-org/Thoth.Json.Net) and its composability.

Farse uses a slightly different syntax, includes a computation expression, and a few custom operators that simplify parsing. It also tries to keep a low overhead while still providing utility and acceptable error messages.

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
| System.Text.Json       |  3.652 us |  0.73 | 0.1106 |      - |     696 B |        0.22 |
| System.Text.Json*      |  3.668 us |  0.74 | 0.4082 | 0.0076 |    2562 B |        0.80 |
| Farse                  |  4.989 us |  1.00 | 0.5112 |      - |    3208 B |        1.00 |
| Newtonsoft.Json*       |  6.106 us |  1.22 | 1.5182 | 0.0229 |    9544 B |        2.98 |
| Thoth.System.Text.Json |  8.169 us |  1.64 | 1.5869 | 0.0305 |    9944 B |        3.10 |
| Newtonsoft.Json        |  8.900 us |  1.78 | 2.8229 | 0.1373 |   17720 B |        5.52 |
| Thoth.Json.Net         | 10.102 us |  2.02 | 3.3569 | 0.1526 |   21136 B |        6.59 |

* Serialization
```

## Example

Given the following JSON string.

```json
{
    "id": "c8eae96a-025d-4bc9-88f8-f204e95f2883",
    "name": "Alice",
    "age": null,
    "email": "alice@domain.com",
    "profiles": [
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
            let! age = "age" ?= Age.parser
            let! email = "email" &= Email.parser
            let! profiles = "profiles" &= set ProfileId.parser

            // Inlined parser example.
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

            // Simple "path" example, which can be very useful
            // when we just want to parse a (few) nested value(s).
            let! isCanceled = "subscription.isCanceled" &= bool
      
            return {
                Id = id
                Name = name
                Age = age
                Email = email
                Profiles = profiles
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

type Age = Age of byte

module Age =

    let asByte (Age x) = x

    let fromByte age =
        match age with
        | age when age >= 12uy -> Ok (Age age)
        | age -> Error $"Invalid age: %u{age}."

    let parser =
        Parse.byte
        |> Parser.validate fromByte

type Email = Email of string

module Email =

    let asString (Email x) = x

    let fromString str =
        // Some validation
        Ok (Email str)

    let parser =
        Parse.string
        |> Parser.validate fromString

type ProfileId = ProfileId of Guid

module ProfileId =

    let asString (ProfileId x) =
        string x

    let parser =
        Parse.guid
        |> Parser.map ProfileId

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
    Age: Age option
    Email: Email
    Profiles: ProfileId Set
    Subscription: Subscription
}
```

Then we can just run the parser.

```fsharp
let user =
    User.parser
    |> Parser.parse json
    |> Result.defaultWith failwith

printf "%s" user.Name
```

## JsonString

We can also create JSON strings.

```fsharp
let jsonString =
    JsonString.create [
        "id", JStr <| UserId.asString user.Id
        "name", JStr user.Name
        "age",
            user.Age
            |> Option.map (Age.asByte >> Byte >> JNum)
            |> JOpt
        "email", JStr <| Email.asString user.Email
        "profiles",
            user.Profiles
            |> List.ofSeq
            |> List.map (ProfileId.asString >> JStr)
            |> JArr
        "subscription",
            JObj [
                "plan", JStr <| Plan.asString user.Subscription.Plan
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

printf "%s" json
```

## Errors

More examples can be found [here](https://github.com/tommililja/Farse/blob/main/src/Farse.Tests/Verify).

```code
﻿Error: Could not parse property 'prop'.
Expected: Number, actual: String.
Object:
{
    "prop": "1"
}
```

>Farse does not throw exceptions and it only catches **JsonException**, that is thrown when supplying invalid JSON.
