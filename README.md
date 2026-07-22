![Farse](https://raw.githubusercontent.com/tommililja/Farse/main/logo/farse-128.png)

# Farse
[![NuGet](https://img.shields.io/nuget/v/Farse.svg?label=NuGet)](https://www.nuget.org/packages/Farse)
> Simple, explicit JSON parsing for F# using System.Text.Json.

Inspired by [Thoth.Json](https://github.com/thoth-org/Thoth.Json) and its composability.

Farse uses a slightly different syntax, includes a computation expression, and a few custom operators that simplify parsing. It also tries to keep a low overhead while producing detailed and helpful error messages.

## Installation

Farse currently targets .NET 8.0 and above.

```shell
dotnet package add Farse
```

## Benchmarks

The benchmarks can be found [here](https://github.com/tommililja/Farse/blob/main/src/Farse.Benchmarks/Benchmarks.fs).

```shell
BenchmarkDotNet v0.15.8, macOS Tahoe 26.5.2 (25F84) [Darwin 25.5.0]
Apple M1 Pro, 1 CPU, 8 logical and 8 physical cores
.NET SDK 10.0.302
  [Host]     : .NET 10.0.10 (10.0.10, 10.0.1026.32716), Arm64 RyuJIT armv8.0-a DEBUG
  DefaultJob : .NET 10.0.10 (10.0.10, 10.0.1026.32716), Arm64 RyuJIT armv8.0-a
```

```shell
| Method                 | Mean     | Ratio | Gen0     | Gen1    | Allocated | Alloc Ratio |
|----------------------- |---------:|------:|---------:|--------:|----------:|------------:|
| System.Text.Json       | 140.2 us |  0.88 |   7.0801 |  0.7324 |  44.63 KB |        0.81 |
| Farse                  | 159.3 us |  1.00 |   8.7891 |  1.2207 |  54.80 KB |        1.00 |
| System.Text.Json*      | 121.9 us |  0.77 |  10.9863 |  1.4648 |  67.47 KB |        1.23 |
| Newtonsoft.Json*       | 221.9 us |  1.39 |  41.5039 |  5.6152 | 254.46 KB |        4.64 |
| Thoth.System.Text.Json | 241.6 us |  1.52 |  67.6270 | 16.8457 | 416.76 KB |        7.60 |
| Newtonsoft.Json        | 270.5 us |  1.70 |  88.3789 | 34.6680 | 542.18 KB |        9.89 |
| Thoth.Json.Net         | 374.0 us |  2.35 | 113.2813 | 33.2031 | 696.61 KB |       12.71 |

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
        "plan": "pro",
        "isCanceled": false,
        "renewsAt": "2026-12-25T10:30:00Z"
    },
    "tags": [ 
        "beta", 
        "verified"
    ]
}
```

And the three (optional) operators.

```fsharp
// Parses a required property.
let (&=) = Prop.get

// Parses an optional property, returning an option.
let (?=) = Prop.tryGet

// Parses an optional property, distinguishing between a missing property and a null value.
let (??=) = Prop.tryGet2
```

We can create this simple parser.

```fsharp
open Farse
open Farse.Operators

module User =
    open Parse

    let parser =
        parser {
            let! id = "id" &= guid |> Parser.map UserId
            and! name = "name" &= string
            and! age = "age" ?= refine byte Age.fromByte
            and! email = "email" &= refine string Email.fromString
            and! profiles = "profiles" &= set profileId // Custom parser example.

            // Inlined parser example.
            and! subscription = "subscription" &= parser {
                let! plan = "plan" &= refine string Plan.fromString
                and! isCanceled = "isCanceled" &= bool
                and! renewsAt = "renewsAt" ?= instant // Custom parser example.

                return {
                    Plan = plan
                    IsCanceled = isCanceled
                    RenewsAt = renewsAt
                }
            }
            
            and! tags = "tags" &= list (refine string Tag.fromString)

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
                Tags = tags
            }
        }
```

> Note: The custom parsers are defined under the same module name as included parsers.

For the following types.

```fsharp
type UserId = UserId of Guid

module UserId =

    let asString (UserId x) =
        string x

type Age = Age of byte

module Age =

    [<Literal>]
    let private MinAge = 12uy

    let fromByte = function
        | age when age >= MinAge -> Ok <| Age age
        | _ -> Error $"The minimum age is %u{MinAge}."
        
    let asByte (Age x) = x

type Email = Email of string

module Email =

    let fromString = Email >> Ok // Some validation.
        
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
        | "pro" -> Ok Pro
        | "standard" -> Ok Standard
        | "free" -> Ok Free
        | string -> Error $"Plan '%s{string}' not found."

    let asString = function
        | Pro -> "pro"
        | Standard -> "standard"
        | Free -> "free"

type Subscription = {
    Plan: Plan
    IsCanceled: bool
    RenewsAt: Instant option
}

type Tag =
    | Beta
    | Verified

module Tag =

    let fromString = function
        | "beta" -> Ok Beta
        | "verified" -> Ok Verified
        | string -> Error $"Tag '%s{string}' not found."

    let asString = function
        | Beta -> "beta"
        | Verified -> "verified"

type User = {
    Id: UserId
    Name: string
    Age: Age option
    Email: Email
    Profiles: ProfileId Set
    Subscription: Subscription
    Tags: Tag list
}
```

Then we can just run the parser.

```fsharp
let user =
    User.parser
    |> Parser.parse json
    |> Result.mapError ParserError.asString
    |> Result.defaultWith failwith

printf "%s" user.Name
```

It can also be run asynchronously from a stream.

```fsharp
task {
    let! result =
        User.parser
        |> Parser.parseAsync stream ct
        
    let user =
        result
        |> Result.mapError ParserError.asString
        |> Result.defaultWith failwith

    return printf "%s" user.Name
}
```

## Custom parsers

Parse.custom can be used to build parsers for third-party types or to just avoid unnecessary operations.

```fsharp
open Farse

module Parse =

    let profileId =
        Parse.custom (fun element ->
            match element.TryGetGuid() with
            | true, guid -> Ok <| ProfileId guid
            | _ -> Error "Expected a Guid string." // Added as details.
        ) ExpectedKind.String

    let instant =
        Parse.custom (fun element ->
            let string = element.GetString()
            match InstantPattern.General.Parse(string) with
            | result when result.Success -> Ok result.Value
            | result -> Error result.Exception.Message // Added as details.
        ) ExpectedKind.String
```
> Note: This is recommended for frequently parsed types.

### Errors

ProfileId

```code
Parser failed with 1 error[s].

Error[0]:
  at $.profiles[1]
   | Tried parsing 'ProfileId.
   | Expected a Guid string.
   = "invalid"
```

Instant

```code
Parser failed with 1 error[s].

Error[0]:
  at $.subscription.renewsAt
   | Tried parsing 'Instant.
   | The value string does not [...]
   = "202612-25T10:30:00Z"
```

## One-of

For objects with a string discriminator.

```fsharp
let! x = "prop" &= oneOf "disc" [ "a", a; "b", b ]
```

Which is equal to matching but less flexible.

```fsharp
let! disc = "prop.disc" &= string
let! x =
    match disc with
    | "a" -> "prop" &= a
    | "b" -> "prop" &= b
    | x -> Parser.fail $"Discriminator '%s{x}' is missing a parser."
```

We can also try each parser in order.

```fsharp
let! x = "prop" &= attempt [ a; b ]
```

## Validation

There are a few different ways to validate parsed values.

```fsharp
let! age = "age" ?= age // Custom parser that uses Age.fromByte.
let! age = "age" ?= refine byte Age.fromByte
let! age = "age" ?= verify byte (fun x -> x >= 12uy) "The minimum age is 12."
```

Validation can also be combined with sequences.

```fsharp
let! tags = "tags" &= list tag
let! tags = "tags" &= list (refine string Tag.fromString)
```

### Errors

Age

```code
Parser failed with 1 error[s].

Error[0]:
  at $.age
   | Tried parsing 'Age.
   | The minimum age is 12.
   = 10
```

Tag

```code
Parser failed with 1 error[s].

Error[0]:
  at $.tags[0]
   | Tried parsing 'Tag.
   | Tag 'user' not found.
   = "user"
```

## Creating JSON

We can create JSON strings with the [Json](https://github.com/tommililja/Farse/blob/main/src/Farse/Json.fs) type.

```fsharp
open Farse

module User =
    
    let asJson user =
        JObj [
            "id", JStr (UserId.asString user.Id)
            "name", JStr user.Name
            "age", JNum.nil Age.asByte user.Age
            "email", JStr (Email.asString user.Email)
            "profiles", JStr.arr ProfileId.asString user.Profiles
            "subscription",
                JObj [
                    "plan", JStr (Plan.asString user.Subscription.Plan)
                    "isCanceled", JBit user.Subscription.IsCanceled
                    "renewsAt", JStr.nil InstantPattern.General.Format user.Subscription.RenewsAt
                ]
            "tags", JStr.arr Tag.asString user.Tags
        ]
        
    let asJsonString =
        asJson >> Json.asString Indented
```

Which is the same as the following.

```fsharp
let asJson user =
    JObj [
        "id", JStr (UserId.asString user.Id)
        "name", JStr user.Name
        "age",
            user.Age
            |> Option.map (Age.asByte >> JNum)
            |> Option.defaultValue JNil
        "email", JStr (Email.asString user.Email)
        "profiles",
            user.Profiles
            |> List.ofSeq
            |> List.map (ProfileId.asString >> JStr)
            |> JArr
        "subscription",
            JObj [
                "plan", JStr (Plan.asString user.Subscription.Plan)
                "isCanceled", JBit user.Subscription.IsCanceled
                "renewsAt",
                    user.Subscription.RenewsAt
                    |> Option.map (InstantPattern.General.Format >> JStr)
                    |> Option.defaultValue JNil
            ]
        "tags",
            user.Tags
            |> List.map (Tag.asString >> JStr)
            |> JArr
    ]
```
> Note: Use JNum<'a> and JNum.nil<'a, 'b> to be explicit.

### From string

Parsing a string to Json.

```fsharp
let json =
    string
    |> Json.fromString
    |> Result.defaultWith (_.Message >> failwith)
```

Parsing asynchronously from a stream.

```fsharp
task {
    let! result = Json.fromStreamAsync ct stream
    return Result.defaultWith (_.Message >> failwith) result
}
```

### To string

Converting Json to a string.

```fsharp
type JsonFormat =
    | Indented
    | Custom of JsonSerializerOptions
    | Raw
```

```fsharp
let string = Json.asString Indented json
```

Writing directly to a stream or buffer writer.

```fsharp
task {
    use writer = new Utf8JsonWriter(ctx.Response.BodyWriter)
    Json.asStringTo writer json
    do! writer.FlushAsync()
}
```

## Errors

More examples can be found [here](https://github.com/tommililja/Farse/tree/main/src/Farse.Tests/Verify).

ParserError can be converted to a formatted string.

```fsharp
let msg = ParserError.asString error
```

We can also build custom error messages.

```fsharp
let msg =
    match error with
    | Json exn -> $"Parser failed: %s{exn.Message}" // Invalid JSON.
    | Errors list ->
        list
        |> List.map (_.Path >> JsonPath.asString >> sprintf "Parser failed at: %s")
        |> String.concat "\n"
```

From the following information.

```fsharp
type ParseError = {
    Path: JsonPath          // The full JSON path where the error occurred.
    Element: JsonElement    // The element that was parsed.
    Index: int option       // If the error occurred directly in an array.
    Value: string option    // Parsing succeeded, but validation failed.
    Type: Type              // The Type that was parsed.
    Details: string         // Parsing error or validation error details.
    Exn: exn option         // Full exn if one occurred.
}
```
> Note: Farse does not throw exceptions unless something unexpected occurs.
