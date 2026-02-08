namespace Farse

open System
open System.Text.Json

// Ignore enum match warning.
#nowarn 104

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

type JsonPath = JsonPath of string list

module JsonPath =

    let prop name = JsonPath [ $".%s{name}" ]

    let index n = JsonPath [ $"[%i{n}]" ]

    let empty = JsonPath []

    let append (JsonPath a) (JsonPath b) =
        List.append a b
        |> JsonPath

    let asString (JsonPath list) =
        list
        |> List.append [ "$" ]
        |> String.concat String.Empty

[<AutoOpen>]
module internal Extensions =

    type Kind = JsonValueKind

    module Kind =

        let asString = function
            | Kind.Undefined -> "Undefined"
            | Kind.Object -> "Object"
            | Kind.Array -> "Array"
            | Kind.String -> "String"
            | Kind.Number -> "Number"
            | Kind.True | Kind.False -> "Bool"
            | Kind.Null -> "Null"

    module ExpectedKind =

        let inline fromKind kind =
            match kind with
            | Kind.Undefined -> ExpectedKind.Undefined
            | Kind.Object -> ExpectedKind.Object
            | Kind.Array -> ExpectedKind.Array
            | Kind.String -> ExpectedKind.String
            | Kind.Number -> ExpectedKind.Number
            | Kind.True | Kind.False -> ExpectedKind.Bool
            | Kind.Null -> ExpectedKind.Null

        let asString = function
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

        let inline getValue (element:JsonElement) =
            match element.ValueKind with
            | JsonValueKind.Null
            | JsonValueKind.Undefined -> None
            | _ -> Some <| element.GetRawText()

        let inline isBool (element:JsonElement) =
            element.ValueKind = Kind.True || element.ValueKind = Kind.False

    module Type =

        let rec getName (x:Type) =
            match x with
            | x when x.IsGenericType ->
                let name = x.Name.Substring(0, x.Name.IndexOf('`'))
                let args =
                    x.GetGenericArguments()
                    |> Array.map getName
                    |> String.concat ", "

                $"%s{name}<%s{args}>"
            | x -> x.Name

    module Result =

        let inline bindError fn = function
            | Ok x -> Ok x
            | Error e -> fn e

    module ResultOption =

        let inline bind fn = function
            | Ok (Some x) -> fn x
            | Ok None -> Ok None
            | Error e -> Error e

    [<AutoOpen>]
    module ActivePatterns =

        let inline (|Prop|Path|) (str:string) =
            if str.Contains('.')
            then Path (str.Split('.', StringSplitOptions.RemoveEmptyEntries))
            else Prop str

    [<AutoOpen>]
    module Builders =

        type StringBuilder() =

            member inline _.Yield(line:string) = [ line ]

            member inline _.Yield(line:string option) =
                line
                |> Option.map List.singleton
                |> Option.defaultValue []

            member inline _.YieldFrom(lines:string list) = lines

            member inline _.Combine(a, b) = a @ b

            member inline _.Delay([<InlineIfLambda>] fn) = fn ()

            member inline _.Zero() = []

            member inline _.Run(lines) =
                lines
                |> Seq.filter (String.IsNullOrWhiteSpace >> not)
                |> String.concat "\n"

        type ResultBuilder() =

            member inline _.Bind(x, [<InlineIfLambda>] fn) = Result.bind fn x

            member inline _.Return(x) = Ok x

            member inline _.ReturnFrom(x) = x

            member inline _.Delay([<InlineIfLambda>] fn) = fn ()

            member inline _.Zero() = Ok ()

        let string = StringBuilder()

        let result = ResultBuilder()