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
BenchmarkDotNet v0.15.3, macOS 26.0 (25A354) [Darwin 25.0.0]
Apple M1 Pro, 1 CPU, 8 logical and 8 physical cores
.NET SDK 9.0.305
  [Host]     : .NET 9.0.9 (9.0.9, 9.0.925.41916), Arm64 RyuJIT armv8.0-a DEBUG
  DefaultJob : .NET 9.0.9 (9.0.9, 9.0.925.41916), Arm64 RyuJIT armv8.0-a
```

```shell
| Method                 | Mean     | Ratio | Gen0    | Gen1    | Allocated | Alloc Ratio |
|----------------------- |---------:|------:|--------:|--------:|----------:|------------:|
| System.Text.Json       | 109.1 us |  0.83 |  4.1504 |       - |  25.85 KB |        0.60 |
| System.Text.Json*      | 112.1 us |  0.86 | 13.0615 |  1.7090 |  80.19 KB |        1.86 |
| Farse                  | 130.9 us |  1.00 |  6.8359 |       - |  43.02 KB |        1.00 |
| Thoth.System.Text.Json | 247.5 us |  1.89 | 55.1758 | 18.0664 | 338.76 KB |        7.87 |
| Newtonsoft.Json*       | 248.1 us |  1.89 | 58.5938 |  5.8594 | 365.01 KB |        8.48 |
| Newtonsoft.Json        | 274.0 us |  2.09 | 75.6836 | 22.9492 | 464.07 KB |       10.79 |
| Thoth.Json.Net         | 365.5 us |  2.79 | 94.7266 | 44.9219 | 581.86 KB |       13.52 |

* Serialization
```

## Example

The complete example can be found [here](https://github.com/tommililja/Farse/blob/main/src/Farse.Tests/Example.fs).

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
            let! id = "id" &= userId // Custom parser example.
            and! name = "name" &= string
            and! age = "age" ?= age
            and! email = "email" &= email
            and! profiles = "profiles" &= set profileId

            // Inlined parser example.
            and! subscription = "subscription" &= parser {
                let! plan = "plan" &= plan
                and! isCanceled = "isCanceled" &= bool
                and! renewsAt = "renewsAt" ?= instant // Custom parser example.

                return {
                    Plan = plan
                    IsCanceled = isCanceled
                    RenewsAt = renewsAt
                }
            }

            // "Path" example, which can be very useful
            // when we just want to parse a (few) nested value(s).
            and! _isCanceled = "subscription.isCanceled" &= bool

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

> Note: In this example, our parsers are defined under the same module name as included parsers.

With the following types.

```fsharp
type UserId = UserId of Guid

module UserId =

    let asString (UserId x) =
        string x

type Age = Age of byte

module Age =

    let fromByte = function
        | age when age >= 12uy -> Ok <| Age age
        | age -> Error $"Invalid age '%u{age}'."
        
    let asByte (Age x) = x

type Email = Email of string

module Email =

    let fromString =
        // Some validation.
        Email >> Ok
        
    let asString (Email x) = x

type ProfileId = ProfileId of Guid

module ProfileId =

    let asString (ProfileId x) =
        string x

type Plan =
    | Pro
    | Standard
    | Free

module Plan =

    let fromString = function
        | "Pro" -> Ok Pro
        | "Standard" -> Ok Standard
        | "Free" -> Ok Free
        | str -> Error $"Invalid plan '%s{str}'."

    let asString = function
        | Pro -> "Pro"
        | Standard -> "Standard"
        | Free -> "Free"

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

## Building parsers

Parsers can be built by combining the included parsers with map and validate. We can also use Parse.custom to build parsers for third-party types, or to just avoid unnecessary operations.

```fsharp
open Farse

module Parse =

    // Custom parser example.
    let instant =
        Parse.custom (fun element ->
            let string = element.GetString()
            match InstantPattern.General.Parse(string) with
            | result when result.Success -> Ok result.Value
            | result -> Error <| Some result.Exception.Message // Added as details.
        ) ExpectedKind.String

    // Custom parser example.
    let userId =
        Parse.custom (fun element ->
            match element.TryGetGuid() with
            | true, guid -> Ok <| UserId guid
            | _ -> Error None // No details.
        ) ExpectedKind.String

    // Combined parsers example.

    let profileId =
        Parse.guid
        |> Parser.map ProfileId

    let email =
        Parse.string
        |> Parser.validate Email.fromString

    let age =
        Parse.byte
        |> Parser.validate Age.fromByte

    let plan =
        Parse.string
        |> Parser.validate Plan.fromString
```

## Creating JSON

We can create JSON strings with [Json](https://github.com/tommililja/Farse/blob/main/src/Farse/Json.fs).

This creates an object, but you can create any type and convert it with Json.asString.

```fsharp
open Farse

module User =
    
    let asJson user =
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
            "subscription",
                JObj [
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

#### Object

```code
Error: Could not parse property 'renewsAt'.
Message: Tried parsing '202612-25T10:30:00Z' to Instant. Details: The value string [...]
Object:
```
```json
{
    "plan": "Pro",
    "isCanceled": false,
    "renewsAt": "202612-25T10:30:00Z"
}
```

#### Array

```code
Error: Could not parse property 'profiles[1]'.
Message: Tried parsing '927eb20f-cd62-470c-aafc-c3ce6b9248' to Guid.
Array:
```
```json
[
    "01458283-b6e3-4ae7-ae54-a68eb587cdc0",
    "927eb20f-cd62-470c-aafc-c3ce6b9248",
    "bf00d1e2-ee53-4969-9507-86bed7e96432"
]
```

> Note: Farse does not throw exceptions unless something unexpected occurs.
