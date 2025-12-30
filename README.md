![Build](https://img.shields.io/github/actions/workflow/status/tommililja/Farse/dotnet.yml?branch=main&label=Build)
[![NuGet](https://img.shields.io/nuget/v/Farse.svg?label=Version)](https://www.nuget.org/packages/Farse)

# Farse

> Simple parsing library for F# using System.Text.Json.

Inspired by [Thoth.Json](https://github.com/thoth-org/Thoth.Json) and its composability.

Farse uses a slightly different syntax, includes a computation expression, and a few custom operators that simplify parsing. It also tries to keep a low overhead while still producing acceptable error messages.

## Installation

```shell
dotnet package add Farse
```

## Benchmarks

There are some initial benchmarks [here](https://github.com/tommililja/Farse/blob/main/src/Farse.Benchmarks/Benchmarks.fs).

```shell
BenchmarkDotNet v0.15.8, macOS Tahoe 26.2 (25C56) [Darwin 25.2.0]
Apple M1 Pro, 1 CPU, 8 logical and 8 physical cores
.NET SDK 10.0.100
  [Host]     : .NET 10.0.0 (10.0.0, 10.0.25.52411), Arm64 RyuJIT armv8.0-a DEBUG
  DefaultJob : .NET 10.0.0 (10.0.0, 10.0.25.52411), Arm64 RyuJIT armv8.0-a
```

```shell
| Method                 | Mean     | Ratio | Gen0    | Gen1    | Allocated | Alloc Ratio |
|----------------------- |---------:|------:|--------:|--------:|----------:|------------:|
| System.Text.Json       | 108.0 us |  0.85 |  4.1504 |       - |  25.85 KB |        0.60 |
| System.Text.Json*      | 108.8 us |  0.86 | 12.9395 |  1.5869 |  79.97 KB |        1.86 |
| Farse                  | 127.1 us |  1.00 |  6.8359 |       - |  43.02 KB |        1.00 |
| Newtonsoft.Json*       | 193.8 us |  1.53 | 42.7246 |  5.8594 | 262.27 KB |        6.10 |
| Thoth.System.Text.Json | 215.0 us |  1.69 | 55.1758 | 18.3105 | 338.76 KB |        7.87 |
| Newtonsoft.Json        | 223.3 us |  1.76 | 75.6836 | 23.9258 | 464.07 KB |       10.79 |
| Thoth.Json.Net         | 308.1 us |  2.42 | 94.7266 | 44.9219 | 581.86 KB |       13.52 |

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
let (&=) = Prop.req

// Parses an optional property.
let (?=) = Prop.opt
```

We can create a simple parser.

```fsharp
open Farse
open Farse.Operators

module User =
    open Parse

    let parser =
        parser {
            let! id = "id" &= guid |> Parser.map UserId
            and! name = "name" &= string
            and! age = "age" ?= byte |> Parser.validate Age.fromByte
            and! email = "email" &= string |> Parser.validate Email.fromString
            and! profiles = "profiles" &= set profileId // Custom parser example.

            // Inlined parser example.
            and! subscription = "subscription" &= parser {
                let! plan = "plan" &= string |> Parser.validate Plan.fromString
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

> Note: In this example, our custom parsers are defined under the same module name as included parsers.

With the following types.

```fsharp
type UserId = UserId of Guid

module UserId =

    let asString (UserId x) =
        string x

type Age = Age of byte

module Age =

    [<Literal>]
    let private AllowedAge = 12uy

    let fromByte = function
        | age when age >= AllowedAge -> Ok <| Age age
        | _ -> Error $"The lowest allowed age is '%u{AllowedAge}'."
        
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
        | str -> Error $"Plan '%s{str}' not found."

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

## Custom parsers

Parse.custom can be used to build parsers for third-party types, or to just avoid unnecessary operations.

```fsharp
open Farse

module Parse =

    let profileId =
        Parse.custom (fun element ->
            match element.TryGetGuid() with
            | true, guid -> Ok <| ProfileId guid
            | _ -> Error None // No details.
        ) ExpectedKind.String

    let instant =
        Parse.custom (fun element ->
            let string = element.GetString()
            match InstantPattern.General.Parse(string) with
            | result when result.Success -> Ok result.Value
            | result -> Error <| Some result.Exception.Message // Added as details.
        ) ExpectedKind.String
```

## Validation

There is a few different options available depending on your use case.

#### Parser.validate 

Passes along the error string from the validation function.  
Works for both optional and non-optional values (SRTP).

```fsharp
let! age = "age" ?= byte |> Parser.validate Age.fromByte
```

```fsharp
The lowest allowed age is '12'.
```

#### Parse.valid

Produces detailed error messages when validation fails.

```fsharp
let! age = "age" ?= valid byte Age.fromByte
```

```code
Error: Could not parse property 'age'.
Message: Tried parsing '10' to Age.
Details: The lowest allowed age is '12'.
```
```json
{
    "id": "c8eae96a-025d-4bc9-88f8-f204e95f2883",
    "name": "Alice",
    "age": 10
}
```

#### Parse.custom

Produces detailed error messages when validation fails.  
Helpful error messages are recommended for when parsing fails.

```fsharp
let age =
    Parse.custom (fun element ->
        match e.TryGetByte() with
        | true, byte -> Age.fromByte byte |> Result.mapError Some
        | _ -> Error <| Some "Invalid byte."
    ) ExpectedKind.Number

let! age = "age" ?= age
```

```code
Error: Could not parse property 'age'.
Message: Tried parsing '10' to Age.
Details: The lowest allowed age is '12'.
```
```json
{
    "id": "c8eae96a-025d-4bc9-88f8-f204e95f2883",
    "name": "Alice",
    "age": 10
}
```

## Creating JSON

We can create JSON strings with the [Json](https://github.com/tommililja/Farse/blob/main/src/Farse/Json.fs) type.

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
                |> JNum.nil Age.asByte
            "email", JStr <| Email.asString user.Email
            "profiles",
                user.Profiles
                |> JStr.arr ProfileId.asString 
            "subscription",
                JObj [
                    "plan", JStr <| Plan.asString user.Subscription.Plan
                    "isCanceled", JBit user.Subscription.IsCanceled
                    "renewsAt",
                        user.Subscription.RenewsAt
                        |> JStr.nil _.ToString()
                ]
        ]
```

> Note: Use JNum<'a> to be explicit.

## Errors

More examples can be found [here](https://github.com/tommililja/Farse/blob/main/src/Farse.Tests/Verify).

#### Object

```code
Error: Could not parse property 'renewsAt'.
Message: Tried parsing '202612-25T10:30:00Z' to Instant.
Details: The value string does not [...]
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
Message: Tried parsing '927eb20f-cd62-470c-aafc-c3ce6b9' to ProfileId.
```
```json
[
    "01458283-b6e3-4ae7-ae54-a68eb587cdc0",
    "927eb20f-cd62-470c-aafc-c3ce6b9",
    "bf00d1e2-ee53-4969-9507-86bed7e9643c"
]
```

> Note: Farse does not throw exceptions unless something unexpected occurs.
