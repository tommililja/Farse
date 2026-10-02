namespace Farse

open System
open System.Text.Json

[<Struct>]
type JsonPath = JsonPath of string

module JsonPath =

    let private special = [| '.'; '\''; '\\'; '['; ']' |]

    let internal segment (name:string) =
        if name.Length = 0 || name.IndexOfAny(special) >= 0
        then
            let name = name.Replace("\\", "\\\\").Replace("'", "\\'")
            $"['%s{name}']"
        else $".%s{name}"

    let internal empty =
        JsonPath String.Empty

    let internal prop (name:string) =
        JsonPath (segment name)

    let internal index n =
        JsonPath $"[%i{n}]"

    let internal append (JsonPath a) (JsonPath b) =
        JsonPath (a + b)

    /// <summary>Converts a <c>JsonPath</c> to a <c>string</c>.</summary>
    /// <example><code>let string = JsonPath.asString path</code></example>
    let asString (JsonPath string) =
        "$" + string

type internal Kind = JsonValueKind

module internal Kind =

    let asString = function
        | Kind.Array -> "Array"
        | Kind.Null -> "Null"
        | Kind.Number -> "Number"
        | Kind.Object -> "Object"
        | Kind.String -> "String"
        | Kind.True | Kind.False -> "Bool"
        | Kind.Undefined -> "Undefined"

/// <summary>Represents the expected <c>JsonValueKind</c> of a <c>JsonElement</c>, excluding <c>Undefined</c>.</summary>
[<RequireQualifiedAccess>]
type ExpectedKind =
    | Any
    | Array
    | Bool
    | Null
    | Number
    | Object
    | String

module internal ExpectedKind =

    let asString = function
        | ExpectedKind.Any -> "Any"
        | ExpectedKind.Array -> "Array"
        | ExpectedKind.Bool -> "Bool"
        | ExpectedKind.Null -> "Null"
        | ExpectedKind.Number -> "Number"
        | ExpectedKind.Object -> "Object"
        | ExpectedKind.String -> "String"