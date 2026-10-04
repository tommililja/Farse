namespace Farse

open System
open System.Text
open System.Text.Json

[<Struct>]
type JsonPath = JsonPath of string

module JsonPath =

    // RFC 9535 member-name-shorthand, restricted to ASCII.
    // Non-ASCII names fall back to brackets, which are always valid.
    let private isShorthand (name:string) =
        (Char.IsAsciiLetter(name[0]) || name[0] = '_')
        && name |> Seq.forall (fun c -> Char.IsAsciiLetterOrDigit(c) || c = '_')

    // RFC 9535 single-quoted string escaping.
    let private escape (name:string) =
        let sb = StringBuilder()
        for c in name do
            match c with
            | '\\' -> sb.Append("\\\\")
            | '\'' -> sb.Append("\\'")
            | '\b' -> sb.Append("\\b")
            | '\u000C' -> sb.Append("\\f")
            | '\n' -> sb.Append("\\n")
            | '\r' -> sb.Append("\\r")
            | '\t' -> sb.Append("\\t")
            | c when c < ' ' -> sb.Append($"\\u%04x{int c}")
            | c -> sb.Append(c)
            |> ignore
        sb.ToString()

    let internal segment = function
        | name when String.IsNullOrEmpty(name) -> "['']"
        | name when isShorthand name -> $".%s{name}"
        | name -> $"['%s{escape name}']"

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