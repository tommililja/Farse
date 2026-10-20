![Farse](https://raw.githubusercontent.com/tommililja/Farse/main/logo/farse-128.png)

# Farse
[![NuGet](https://img.shields.io/nuget/v/Farse.svg?label=NuGet)](https://www.nuget.org/packages/Farse)
> Simple, explicit JSON parsing for F#

Inspired by [Thoth.Json](https://github.com/thoth-org/Thoth.Json) and its composability.

Farse uses a computation expression and a few custom operators that simplify defining and building composable, type-safe parsers. It also produces detailed and helpful error messages, aims to keep overhead low, and performs similarly to pure `System.Text.Json` parsing.

## Installation

Farse targets `.NET 8.0` and above.

```shell
dotnet package add Farse
```

## Benchmarks

The benchmarks can be found [here](https://github.com/tommililja/Farse/blob/main/src/Farse.Benchmarks/Benchmarks.fs).

```shell
BenchmarkDotNet v0.15.8, macOS 27.0.1 (26A434) [Darwin 27.0.0]
Apple M1 Pro, 1 CPU, 8 logical and 8 physical cores
.NET SDK 10.0.302
  [Host]     : .NET 10.0.10 (10.0.10, 10.0.1026.32716), Arm64 RyuJIT armv8.0-a DEBUG
  DefaultJob : .NET 10.0.10 (10.0.10, 10.0.1026.32716), Arm64 RyuJIT armv8.0-a
```

```shell
| Method                 | Mean     | Ratio | Gen0     | Gen1    | Allocated | Alloc Ratio |
|----------------------- |---------:|------:|---------:|--------:|----------:|------------:|
| System.Text.Json       | 139.3 us |  0.90 |   7.0801 |  0.7324 |  44.63 KB |        0.81 |
| Farse                  | 155.5 us |  1.00 |   8.7891 |  1.2207 |   54.8 KB |        1.00 |
| System.Text.Json*      | 120.7 us |  0.78 |  10.9863 |  1.4648 |  67.47 KB |        1.23 |
| Newtonsoft.Json*       | 220.2 us |  1.42 |  41.5039 |  5.6152 | 254.46 KB |        4.64 |
| Thoth.System.Text.Json | 241.6 us |  1.55 |  67.6270 | 16.8457 | 416.76 KB |        7.60 |
| Newtonsoft.Json        | 269.8 us |  1.74 |  88.3789 | 34.6680 | 542.18 KB |        9.89 |
| Thoth.Json.Net         | 373.2 us |  2.40 | 113.2813 | 33.2031 | 696.61 KB |       12.71 |

* Automatic deserialization
```

## Example

The complete example can be found [here](https://github.com/tommililja/Farse/blob/main/src/Farse.Tests/Example.fs).

Given the JSON document:

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

And the three operators:

```fsharp
// Parses a required property.
let (&=) = Prop.get
```

```fsharp
// Parses an optional property, returning an option.
let (?=) = Prop.tryGet
```

```fsharp
// Parses an optional property, returning an option option.
let (??=) = Prop.tryGet2
```

We can define a `Parser` like this:

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

> Note: Custom parsers are defined in a Parse module, alongside the built-in ones.

For the types:

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

The `Parser<User>` can then be run:

```fsharp
let user =
    User.parser
    |> Parser.parse json
    |> Result.mapError ParserError.asString
    |> Result.defaultWith failwith
```

We can also parse a `Stream` asynchronously:

```fsharp
task {
    let! result =
        User.parser
        |> Parser.parseAsync stream ct
        
    let user =
        result
        |> Result.mapError ParserError.asString
        |> Result.defaultWith failwith

    return user
}
```

## Validation

There are a few different ways to validate parsed values:

```fsharp
let! age = "age" ?= age // Custom parser that uses Age.fromByte.
let! age = "age" ?= refine byte Age.fromByte
let! age = "age" ?= verify byte (fun x -> x >= 12uy) "The minimum age is 12."
```

Validation can also be combined with sequences:

```fsharp
let! tags = "tags" &= list tag
let! tags = "tags" &= list (refine string Tag.fromString)
let! tags = "tags" &= list (verify string (String.IsNullOrWhiteSpace >> not) "Tag was empty.")
```

Age:

```fsharp
Parser yielded 1 error[s].

  Error[0]:
    at $.age
     | Tried parsing 'Age.
     | The minimum age is 12.
     = 10uy
```

Tag:

```fsharp
Parser yielded 1 error[s].

  Error[0]:
    at $.tags[0]
     | Tried parsing 'Tag.
     | Tag 'user' not found.
     = "user"
```

## Custom

`Parse.custom` can be used to build parsers for third-party types or to avoid unnecessary operations:

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

ProfileId:

```fsharp
Parser yielded 1 error[s].

  Error[0]:
    at $.profiles[1]
     | Tried parsing 'ProfileId.
     | Expected a Guid string.
     = "invalid"
```

Instant:

```fsharp
Parser yielded 1 error[s].

  Error[0]:
    at $.subscription.renewsAt
     | Tried parsing 'Instant.
     | The value string does not [...]
     = "202612-25T10:30:00Z"
```

## One-of

For objects with a string discriminator:

```fsharp
let! x = "prop" &= oneOf "disc" [ "a", a; "b", b ]
```

Which is similar to matching, but less flexible:

```fsharp
let! disc = "prop.disc" &= string
let! x =
    match disc with
    | "a" -> "prop" &= a
    | "b" -> "prop" &= b
    | x -> Parser.fail $"Unknown discriminator '%s{x}'."
```

We can also try each `Parser` in order:

```fsharp
let! x = "prop" &= attempt [ a; b ]
```

## Recursive

Use `Parse.delay` to define recursive parsers:

```fsharp
type Tree =
    | Leaf of int
    | Branch of Tree * Tree

let rec tree () =
    oneOf "type" [
        "leaf",
            parser {
                let! value = "value" &= int
                return Leaf value
            }
        "branch",
            parser {
                let! left = "left" &= delay tree
                let! right = "right" &= delay tree
                return Branch (left, right)
            }
    ]
```

Parsers can also reference each other:

```fsharp
type Value = {
    Id: string
    Fields: Field array
}

and Field = {
    Name: string
    Values: Value array
}

let rec value () =
    parser {
        let! id = "id" &= string
        and! fields = "fields" &= array (delay field)
        return { Id = id; Fields = fields }
    }

and field () =
    parser {
        let! name = "name" &= string
        and! values = "values" &= array (delay value)
        return { Name = name; Values = values }
    }
```

## Computation expression

Parsers are immutable and can be reused, so define them once as values.

- `let!` runs parsers in order and returns on the first error.  
- `and!` runs every parser in the group and collects errors.

```fsharp
let parser =
    parser {
        let! x = "x" &= int
        and! y = "y" &= int

        do! "z" &= unit

        if x = 0 then
            do! Parser.fail "Parser failed."

        return x + y
    }
```

Parsers may be rebuilt on every parse, so create expensive objects outside:

```fsharp
let private slugRegex = Regex("^[a-z0-9-]+$")

let parser =
    parser {
        let! id = "id" &= int
        let! slug = "slug" &= regex slugRegex // Created once, not on every parse.
        return id, slug
    }
```

Prefer `and!` for independent values, as it performs better than `let!`.

## Operators

Tables that describe operator behavior.

```shell
| Operator          | {}    | { "a": null } | { "a": 1 }    |
|------------------ |------ |-------------- |-------------- |
| "a" &= int        | Error | Error         | 1             |
| "a" &= option int | Error | None          | Some 1        |
| "a" ?= int        | None  | None          | Some 1        |
| "a" ??= int       | None  | Some None     | Some (Some 1) |
```

```shell
| Operator            | {}    | { "a": null } | { "a": {} } | { "a": { "b": null } } | { "a": { "b": 1 } } |
|-------------------- |------ |-------------- |------------ |----------------------- |-------------------- |
| "a.b" &= int        | Error | Error         | Error       | Error                  | 1                   |
| "a.b" &= option int | Error | Error         | Error       | None                   | Some 1              |
| "a.b" ?= int        | None  | None          | None        | None                   | Some 1              |
| "a.b" ??= int       | None  | None          | None        | Some None              | Some (Some 1)       |
```

> Note: A missing or null segment anywhere in the path counts as missing.

## Errors

More examples can be found [here](https://github.com/tommililja/Farse/tree/main/src/Farse.Tests/Verify).

`ParserError` can be converted to a formatted string:

```fsharp
let msg = ParserError.asString error
```

We can also build custom error messages:

```fsharp
let msg =
    match error with
    | Json exn -> $"Parser failed: %s{exn.Message}" // Invalid JSON.
    | Errors list ->
        list
        |> List.map (_.Path >> JsonPath.asString >> sprintf "Parser failed at: %s")
        |> String.concat "\n"
```

With the available information:

```fsharp
type ParseError = {
    Path: JsonPath          // Path where the error occurred.
    Index: int option       // Index in the innermost array.
    Details: string         // Parsing or validation error details.
    Value: string option    // Raw JSON value or the parsed value.
    Type: Type              // Type that was being parsed.
    Exn: exn option         // Exception, if one occurred.
}
```
> Note: Farse _should_ not throw when parsing, but some parsers may throw on invalid arguments.

## Creating JSON

We can create JSON structures using the [`Json`](https://github.com/tommililja/Farse/blob/main/src/Farse/Json.fs) type.

This example builds an object and converts it to an indented string:

```fsharp
open Farse

module User =

    let asJson user =
        JObj [
            "id", JStr (UserId.asString user.Id)
            "name", JStr user.Name
            "age", JNum.option Age.asByte user.Age
            "email", JStr (Email.asString user.Email)
            "profiles", JStr.array ProfileId.asString user.Profiles
            "subscription",
                JObj [
                    "plan", JStr (Plan.asString user.Subscription.Plan)
                    "isCanceled", JBit user.Subscription.IsCanceled
                    "renewsAt", JStr.option Instant.asString user.Subscription.RenewsAt
                ]
            "tags", JStr.array Tag.asString user.Tags
        ]

    let asJsonString =
        asJson >> Json.asString Indented
```
> Note: JNum uses implicit conversions. Use JNum.number<int> to be explicit.

Which is the same as:

```fsharp
let asJson user =
    JObj [
        "id", JStr (UserId.asString user.Id)
        "name", JStr user.Name
        "age",
            user.Age
            |> Option.map (Age.asByte >> JNum.number)
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
                    |> Option.map (Instant.asString >> JStr)
                    |> Option.defaultValue JNil
            ]
        "tags",
            user.Tags
            |> List.map (Tag.asString >> JStr)
            |> JArr
    ]
```

### Comparison

There are a few different ways to compare `Json` values:

```fsharp
// Structural, compares the JSON as written.
let equal = x = y
```

```fsharp
// Ignores property order and compares numbers by value.
let equal = Json.equal x y
```

```fsharp
// Same as Json.equal, but returns a diff message.
match Json.diff x y with
| Some msg -> failwith msg
| None -> ()
```

### Parsing

Parsing a string:

```fsharp
let json =
    string
    |> Json.fromString
    |> Result.defaultWith (_.Message >> failwith)
```

Parsing a `Stream` asynchronously:

```fsharp
task {
    let! result = Json.fromStreamAsync token stream
    return result |> Result.defaultWith (_.Message >> failwith)
}
```

### Converting

Converting a `Json` to a string:

```fsharp
type JsonFormat =
    | Indented
    | Custom of JsonWriterOptions
    | Raw
```

```fsharp
let string = Json.asString Indented json
```

Writing directly to a `Stream` or `IBufferWriter` asynchronously:

```fsharp
task {
    use writer = new Utf8JsonWriter(ctx.Response.BodyWriter)
    Json.writeTo writer json
    do! writer.FlushAsync()
}
```

## Contributing

Bug reports, feature requests, and pull requests are welcome.

See [CONTRIBUTING.md](CONTRIBUTING.md).
