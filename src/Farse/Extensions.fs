namespace Farse

#nowarn 104

open System
open System.Collections.Generic
open System.Text.Json
open System.Text.Json.Nodes

[<RequireQualifiedAccess>]
type ExpectedKind =
    | Undefined
    | Object
    | Array
    | String
    | Number
    | Bool
    | Null
    | Any

[<AutoOpen>]
module internal Extensions =

    type Kind = JsonValueKind

    module Kind =

        let inline asString kind =
            match kind with
            | Kind.Undefined -> "Undefined"
            | Kind.Object -> "Object"
            | Kind.Array -> "Array"
            | Kind.String -> "String"
            | Kind.Number -> "Number"
            | Kind.True
            | Kind.False -> "Bool"
            | Kind.Null -> "Null"

    module ExpectedKind =

        let inline fromKind kind =
            match kind with
            | Kind.Undefined -> ExpectedKind.Undefined
            | Kind.Object -> ExpectedKind.Object
            | Kind.Array -> ExpectedKind.Array
            | Kind.String -> ExpectedKind.String
            | Kind.Number -> ExpectedKind.Number
            | Kind.True -> ExpectedKind.Bool
            | Kind.False -> ExpectedKind.Bool
            | Kind.Null -> ExpectedKind.Null

        let inline asString expectedKind =
            match expectedKind with
            | ExpectedKind.Undefined -> "Undefined"
            | ExpectedKind.Object -> "Object"
            | ExpectedKind.Array -> "Array"
            | ExpectedKind.String -> "String"
            | ExpectedKind.Number -> "Number"
            | ExpectedKind.Bool -> "Bool"
            | ExpectedKind.Null -> "Null"
            | ExpectedKind.Any -> "Any"

    module JsonSerializerOptions =

        let preset = JsonSerializerOptions(
            WriteIndented = true,
            IndentSize = 4
        )

    module JsonElement =

        let inline getProperty (name:string) (element:JsonElement) =
            element.TryGetProperty(name) |> snd

        let inline tryGetProperty (name:string) (element:JsonElement) =
            match element.TryGetProperty(name) with
            | true, prop when prop.ValueKind <> Kind.Null -> Some prop
            | _ -> None

        let inline asString (element:JsonElement) =
            element.ToString()

        let inline asRawString (element:JsonElement) =
            element.GetRawText()

        let inline asJsonString (element:JsonElement) =
            JsonSerializer.Serialize(element, JsonSerializerOptions.preset)

    module JsonNode =

        let create<'a> x =
            JsonValue.Create<'a>(x)
                .Root

    module JsonArray =

        let asJsonNode (x:JsonNode array) =
            JsonArray(x).Root

    module JsonObject =

        let asJsonNode (x:KeyValuePair<string, JsonNode> seq) =
            JsonObject(x).Root

    module ResultOption =

        let inline bind fn = function
            | Ok x ->
                match x with
                | Some x -> fn x
                | None -> Ok None
            | Error e -> Error e

    module Seq =

        let inline ofSeq x = x :> seq<_>

    module KeyValuePairs =

        let inline ofSeq x = Seq.map KeyValuePair.Create x

    module Dictionary =

        let inline ofSeq x = dict x

    [<AutoOpen>]
    module ActivePatterns =

        let inline (|Flat|Nested|) (str:string) =
            if str.Contains('.')
            then Nested (str.Split('.', StringSplitOptions.RemoveEmptyEntries))
            else Flat str

    [<AutoOpen>]
    module Builders =

        type StringBuilder() =

            member _.Yield(line:string) = [ line ]

            member _.Yield(line:string option) =
                line
                |> Option.map List.singleton
                |> Option.defaultValue []

            member _.YieldFrom(lines:string list) = lines

            member _.Combine(a, b) = a @ b

            member _.Delay(fn) = fn()

            member _.Zero() = []

            member _.Run(lines) =
                lines
                |> Seq.filter (String.IsNullOrWhiteSpace >> not)
                |> String.concat "\n"

        type ResultBuilder() =

            member inline _.Return(x) = Ok x

            member inline _.ReturnFrom(x) = x

            member inline _.Delay([<InlineIfLambda>] fn) = fn ()

            member inline _.Zero() = Ok ()

            member inline _.Bind(x, [<InlineIfLambda>] fn) = Result.bind fn x

        let string = StringBuilder()

        let result = ResultBuilder()