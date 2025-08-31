![Build](https://img.shields.io/github/actions/workflow/status/tommililja/Farse/dotnet.yml?branch=main&label=Build)
[![NuGet](https://img.shields.io/nuget/v/Farse.svg?label=Version)](https://www.nuget.org/packages/Farse)

# Farse

> Simple parsing library for F# using System.Text.Json.

Inspired by [Thoth.Json](https://github.com/thoth-org/Thoth.Json) and its composability.

Farse uses a slightly different syntax, includes a computation expression, and a few custom operators that simplify parsing. It also tries to keep a low overhead while still producing acceptable error messages.

## Installation

```shell
dotnet add package Farse
```

## Benchmarks

There are some initial benchmarks [here](https://github.com/tommililja/Farse/blob/main/src/Farse.Benchmarks/Benchmarks.fs).

```shell
BenchmarkDotNet v0.15.2, macOS Sequoia 15.6 (24G84) [Darwin 24.6.0]
Apple M1 Pro, 1 CPU, 8 logical and 8 physical cores
.NET SDK 9.0.203
  [Host]     : .NET 9.0.4 (9.0.425.16305), Arm64 RyuJIT AdvSIMD DEBUG
  DefaultJob : .NET 9.0.4 (9.0.425.16305), Arm64 RyuJIT AdvSIMD
```

```shell
| Method                 | Mean     | Ratio | Gen0    | Gen1    | Allocated | Alloc Ratio |
|----------------------- |---------:|------:|--------:|--------:|----------:|------------:|
| System.Text.Json       | 109.8 us |  0.71 |  4.1504 |       - |  25.85 KB |        0.25 |
| System.Text.Json*      | 113.4 us |  0.73 | 13.0615 |  1.7090 |  80.19 KB |        0.77 |
| Farse                  | 154.6 us |  1.00 | 16.8457 |       - | 104.19 KB |        1.00 |
| Thoth.System.Text.Json | 247.3 us |  1.60 | 55.1758 | 18.0664 | 338.76 KB |        3.25 |
| Newtonsoft.Json*       | 248.7 us |  1.61 | 59.5703 |  7.8125 | 365.69 KB |        3.51 |
| Newtonsoft.Json        | 306.6 us |  1.98 | 75.6836 |  6.8359 | 464.07 KB |        4.45 |
| Thoth.Json.Net         | 366.9 us |  2.37 | 94.7266 | 44.9219 | 581.86 KB |        5.58 |

* Serialization
```

## Example

Given the JSON string.

```json
{
    "id": "c8eae96a-025d-4bc9-88f8-f204e95f2883",
    "name": "Alice",
    "age": null,
    "email": "alice@domain.com",
    "profiles": [
        "01458283-b6e3-4ae7-ae54-a68eb587cdc0",
        "927eb20f-cd62-470c-aafc-c3ce6b9248b0",
        "bf00d1e2-ee53-4969-9507-86bed7e96432"
    ],
    "subscription": {
        "plan": "Pro",
        "isCanceled": false,
        "renewsAt": "2026-12-25T10:30:00Z"
    }
}
```

And the two (optional) operators.

```fsharp
// Parses a required property.
let (&=) = Parse.req

// Parses an optional property.
let (?=) = Parse.opt
```

We can create a simple parser.

```fsharp
open Farse
open Farse.Operators

module User =
    open Parse

    let parser =
        parser {
            let! id = "id" &= userId // Optimized parser example #1.
            let! name = "name" &= string
            let! age = "age" ?= Age.parser
            let! email = "email" &= email // Optimized parser example #2.
            let! profiles = "profiles" &= set ProfileId.parser

            // Inlined parser example.
            let! subscription = "subscription" &= parser {
                let! plan = "plan" &= Plan.parser
                let! isCanceled = "isCanceled" &= bool
                let! renewsAt = "renewsAt" ?= instant // Custom parser example.
    
                return {
                    Plan = plan
                    IsCanceled = isCanceled
                    RenewsAt = renewsAt
                }
            }

            // Simple "path" example, which can be very useful
            // when we just want to parse a (few) nested value(s).
            let! _isCanceled = "subscription.isCanceled" &= bool
      
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
open NodaTime
open System

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

    let fromByte = function
        | age when age >= 12uy -> Ok <| Age age
        | age -> Error $"Invalid age: %u{age}."

    let parser =
        Parse.byte
        |> Parser.validate fromByte

type Email = Email of string

module Email =

    let asString (Email x) = x

    let fromString =
        Email >> Ok // Some validation.

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
        | str -> Error $"Invalid plan: %s{str}."

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
    RenewsAt: Instant option
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

## Custom parsers

Use Parse.custom to create parsers for third-party types, or to create optimized parsers for user-defined types that avoid unnecessary operations such as map, bind, and validate.

```fsharp
open Farse
open NodaTime
open NodaTime.Text
open System.Text.Json

module Parse =

    // Optimized parser example #1.
    let userId =
        Parse.custom (fun element ->
            match element.TryGetGuid() with
            | true, guid -> Ok <| UserId guid
            | _ -> Error String.Empty // No additional info.
        ) JsonValueKind.String // Expected kind.

    // Optimized parser example #2.
    let email =
        Parse.custom (fun element ->
            element.GetString()
            |> Email.fromString // Error added as additional information.
        ) JsonValueKind.String // Expected kind.

    // Custom parser example.
    let instant =
        Parse.custom (fun element ->
            let string = element.GetString()
            match InstantPattern.General.Parse(string) with
            | result when result.Success -> Ok result.Value
            | result -> Error result.Exception.Message // Error added as additional information.
        ) JsonValueKind.String // Expected kind.
```

> Note: Use True or False for any bool value.

## Creating JSON

We can create JSON strings with [Json](https://github.com/tommililja/Farse/blob/main/src/Farse/Json.fs).

This creates an object, but you can create any type and convert it with Json.asString.

```fsharp
JObj [
    "id", JStr <| UserId.asString user.Id
    "name", JStr user.Name
    "age",
        user.Age
        |> Option.map (Age.asByte >> JNum)
        |> JNil
    "email", JStr <| Email.asString user.Email
    "profiles",
        user.Profiles
        |> Seq.map (ProfileId.asString >> JStr)
        |> JArr
    "subscription", JObj [
        "plan", JStr <| Plan.asString user.Subscription.Plan
        "isCanceled", JBit user.Subscription.IsCanceled
        "renewsAt",
            user.Subscription.RenewsAt
            |> Option.map (_.ToString() >> JStr)
            |> JNil
    ]
]
```

> Note: Use JNum<'a> to be explicit.

## Errors

More examples can be found [here](https://github.com/tommililja/Farse/blob/main/src/Farse.Tests/Verify).

```code
Error: Could not parse property 'plan'.
Message: Failed to parse 'Max' as Plan. Invalid plan: Max.
Object:
```
```json
{
  "plan": "Max",
  "isCanceled": false,
  "renewsAt": "2026-12-25T10:30:00Z"
}
```

> Note: Farse doesn't throw exceptions and only catches JsonException, that is thrown for invalid JSON.
