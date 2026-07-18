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

    let internal validation value details type' element =
        {
            Path = JsonPath.empty
            Element = JsonElement.clone element
            Index = None
            Details = details
            Value = Some value
            Type = type'
            Exn = None
        }

    let internal invalid details type' element =
        {
            Path = JsonPath.empty
            Element = JsonElement.clone element
            Index = None
            Details = details
            Value = None
            Type = type'
            Exn = None
        }

    let internal invalidEx (exn:exn) type' element =
        {
            Path = JsonPath.empty
            Element = JsonElement.clone element
            Index = None
            Details = exn.Message
            Value = None
            Type = type'
            Exn = Some exn
        }

    let internal expectedKind expected path type' element =
        {
            Path = path
            Element = JsonElement.clone element
            Index = None
            Details = $"Expected %s{ExpectedKind.asString expected}, but got %s{Kind.asString element.ValueKind}."
            Value = None
            Type = type'
            Exn = None
        }

    let internal expectedValue actual expected type' element =
        {
            Path = JsonPath.empty
            Element = JsonElement.clone element
            Index = None
            Details = $"Expected %A{expected}, but got %A{actual}."
            Value = None
            Type = type'
            Exn = None
        }

    let internal invalidIndex n type' element =
        {
            Path = JsonPath.index n
            Element = JsonElement.clone element
            Index = Some n
            Details = "Index was out of range."
            Value = None
            Type = type'
            Exn = None
        }

    let internal invalidTuple actual expected type' element =
        {
            Path = JsonPath.empty
            Element = JsonElement.clone element
            Index = None
            Details = $"Expected a tuple of %i{expected}, but got %i{actual}."
            Value = None
            Type = type'
            Exn = None
        }

    let internal missingParser disc type' element =
        {
            Path = JsonPath.empty
            Element = JsonElement.clone element
            Index = None
            Details = $"Discriminator '%s{disc}' is missing a parser."
            Value = None
            Type = type'
            Exn = None
        }

    let internal attempt parsers type' element =
        {
            Path = JsonPath.empty
            Element = JsonElement.clone element
            Index = None
            Details = $"Tried %i{parsers} parsers without success."
            Value = None
            Type = type'
            Exn = None
        }

    let internal emptyParsers type' element =
        {
            Path = JsonPath.empty
            Element = JsonElement.clone element
            Index = None
            Details = "No parsers given."
            Value = None
            Type = type'
            Exn = None
        }

    let internal duplicateKey key type' element =
        {
            Path = JsonPath.prop key
            Element = JsonElement.clone element
            Index = None
            Details = "Duplicate key."
            Value = None
            Type = type'
            Exn = None
        }

    /// <summary>Converts a ParseError to a formatted string.</summary>
    /// <example><code>let string = ParseError.asString error</code></example>
    /// <param name="error">The ParseError to convert.</param>
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

    /// <summary>Converts a ParserError to a formatted string.</summary>
    /// <param name="error">The ParserError to convert.</param>
    let asString error =
        match error with
        | Json exn -> $"Could not parse JSON: %s{exn.Message}"
        | Errors list ->
            let errors =
                list
                |> List.mapi (fun i e ->
                    let string = ParseError.asString e
                    let error =
                        string.Split('\n')
                        |> Array.map (sprintf "  %s")
                        |> String.concat "\n"

                    $"Error[%i{i}]:\n%s{error}"
                )
                |> String.concat "\n\n"

            $"Parser failed with %i{List.length list} error[s].\n\n%s{errors}"