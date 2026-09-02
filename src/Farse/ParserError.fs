namespace Farse

open System
open System.Text.Json

[<NoComparison>]
type ParseError = {
    Path: JsonPath
    Element: JsonElement
    Index: int option
    Details: string
    Value: string option
    Type: Type
    Exn: exn option
}

module ParseError =

    // Functions for appending the path.

    let private append path x =
        { x with Path = JsonPath.append path x.Path }

    let internal withProp name x =
        append (JsonPath.prop name) x

    let internal withIndex n x =
        append (JsonPath.index n) x

    let internal withPath path x =
        append path x

    // Errors

    let internal required path type' element  =
        {
            Path = path
            Element = JsonElement.clone element
            Index = None
            Details = "Missing required property."
            Value = None
            Type = type'
            Exn = None
        }

    let internal invalid value details type' element =
        {
            Path = JsonPath.empty
            Element = JsonElement.clone element
            Index = None
            Details = details
            Value = Some value
            Type = type'
            Exn = None
        }

    let internal details details type' element =
        {
            Path = JsonPath.empty
            Element = JsonElement.clone element
            Index = None
            Details = details
            Value = None
            Type = type'
            Exn = None
        }

    let internal exn (exn:exn) type' element =
        {
            Path = JsonPath.empty
            Element = JsonElement.clone element
            Index = None
            Details = exn.Message
            Value = None
            Type = type'
            Exn = Some exn
        }

    let internal expected expected path type' element =
        {
            Path = path
            Element = JsonElement.clone element
            Index = None
            Details = $"Expected %s{ExpectedKind.asString expected}, but got %s{Kind.asString element.ValueKind}."
            Value = None
            Type = type'
            Exn = None
        }

    let internal index n type' element =
        {
            Path = JsonPath.index n
            Element = JsonElement.clone element
            Index = Some n
            Details = "Index was out of range."
            Value = None
            Type = type'
            Exn = None
        }

    /// <summary>Converts a <c>ParseError</c> to a formatted <c>string</c>.</summary>
    /// <example><code>let string = ParseError.asString error</code></example>
    let asString error =
        string {
            $"at %s{JsonPath.asString error.Path}"
            $" | Tried parsing '%s{Type.getName error.Type}."

            if String.isNotEmpty error.Details then
                $" | %s{error.Details}"

            error.Value
            |> Option.orElse (JsonElement.tryGetValue error.Element)
            |> Option.map (sprintf " = %s")
        }

[<NoComparison>]
type ParserError =
    | Json of exn
    | Errors of ParseError list

module ParserError =

    /// <summary>Converts a <c>ParserError</c> to a formatted <c>string</c>.</summary>
    /// <example><code>let string = ParserError.asString error</code></example>
    let asString error =
        match error with
        | Json exn -> $"Could not parse JSON: %s{exn.Message}"
        | Errors list ->
            let errors =
                list
                |> List.mapi (fun i error ->
                    let msg =
                        error
                        |> ParseError.asString
                        |> String.indent 2

                    $"Error[%i{i}]:\n%s{msg}"
                )
                |> String.concat "\n\n"
                |> String.indent 2

            $"Parser yielded %i{list.Length} error[s].\n\n%s{errors}"