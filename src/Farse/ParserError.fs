namespace Farse

open System
open System.Text.Json

type JsonPath = JsonPath of string list

module JsonPath =

    let empty = JsonPath []

    let inline internal prop name =
        JsonPath [ $".%s{name}" ]

    let inline internal index n =
        JsonPath [ $"[%i{n}]" ]

    let inline internal append (JsonPath a) (JsonPath b) =
        List.append a b
        |> JsonPath

    /// <summary>Converts the JsonPath to a string.</summary>
    let asString (JsonPath list) =
        list
        |> List.append [ "$" ]
        |> String.concat String.Empty

type ParseError = {
    Path: JsonPath
    Element: JsonElement
    Index: int option
    Value: string option
    Type: Type
    Details: string
    Exn: exn option
}

module ParseError =

    // Errors

    let internal validation details type' value element =
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
            Value = JsonElement.tryGetValue element
            Type = type'
            Exn = None
        }

    let internal invalidEx details type' exn element =
        {
            Path = JsonPath.empty
            Element = JsonElement.clone element
            Index = None
            Details = details
            Value = JsonElement.tryGetValue element
            Type = type'
            Exn = Some exn
        }

    let expectedKind expectedKind path type' element =
        {
            Path = path
            Element = JsonElement.clone element
            Index = None
            Details = $"Expected %s{ExpectedKind.asString expectedKind}, but got %s{Kind.asString element.ValueKind}."
            Type = type'
            Value = JsonElement.tryGetValue element
            Exn = None
        }

    let internal invalidIndex n type' element =
        {
            Path = JsonPath.index n
            Element = JsonElement.clone element
            Index = Some n
            Details = "Index was out of range."
            Type = type'
            Value = JsonElement.tryGetValue element
            Exn = None
        }

    let internal invalidTuple actual expected type' element =
        {
            Path = JsonPath.empty
            Element = JsonElement.clone element
            Index = None
            Details = $"Expected a tuple of %i{expected}, but got %i{actual}."
            Type = type'
            Value = JsonElement.tryGetValue element
            Exn = None
        }

    let internal invalidOneOf value type' element =
        {
            Path = JsonPath.empty
            Element = JsonElement.clone element
            Index = None
            Details = $"Missing parser for discriminator '%s{value}'."
            Type = type'
            Value = None
            Exn = None
        }

    let attempt parsers type' element =
        {
            Path = JsonPath.empty
            Element = JsonElement.clone element
            Index = None
            Value = None
            Type = type'
            Details = $"Tried %i{parsers} parsers without success."
            Exn = None
        }

    let internal duplicateKey key type' element =
        {
            Path = JsonPath.empty
            Element = JsonElement.clone element
            Index = None
            Details = $"Duplicate key '%s{key}'."
            Type = type'
            Value = None
            Exn = None
        }

    // Functions for appending the path.

    let private append path x =
        { x with Path = JsonPath.append path x.Path }

    let internal withProp name x =
        append (JsonPath.prop name) x

    let internal withIndex n x =
        append (JsonPath.index n) x

    let internal withPath path x =
        append path x

    /// <summary>Converts the ParseError to a formatted string.</summary>
    /// <param name="error">The ParseError to convert.</param>
    let asString error =
        string {
            $"at %s{JsonPath.asString error.Path}"
            $" | Tried parsing '%s{Type.getName error.Type}."

            if not (String.IsNullOrWhiteSpace error.Details) then
                $" | %s{error.Details}"

            Option.map (sprintf " = %s") error.Value
        }

type ParserError =
    | Json of exn
    | Errors of ParseError list

module ParserError =

    /// <summary>Converts the ParserError to a formatted string.</summary>
    let asString = function
        | Json exn -> $"Could not parse JSON: %s{exn.Message}"
        | Errors list ->
            list
            |> List.mapi (fun i e ->
                ParseError.asString e
                |> String.indentLines
                |> sprintf "Error[%i]:\n%s" i
            )
            |> String.concat "\n\n"
            |> sprintf "Parser failed with %i error[s].\n\n%s" (List.length list)