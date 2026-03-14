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
    Value: string option
    Type: Type
    Details: string
    Exn: exn option
}

module ParseError =

    // Errors

    let inline internal validation details type' value element = {
        Path = JsonPath.empty
        Element = element
        Details = details
        Value = Some value
        Type = type'
        Exn = None
    }

    let inline internal invalid details type' element = {
        Path = JsonPath.empty
        Element = element
        Details = details
        Value = JsonElement.getValue element
        Type = type'
        Exn = None
    }

    let inline internal invalidEx details type' exn element = {
        Path = JsonPath.empty
        Element = element
        Details = details
        Value = JsonElement.getValue element
        Type = type'
        Exn = Some exn
    }

    let inline internal expectedKind expectedKind path type' element = {
        Path = path
        Element = element
        Details = $"Expected %s{ExpectedKind.asString expectedKind}, but got %s{Kind.asString element.ValueKind}."
        Type = type'
        Value = JsonElement.getValue element
        Exn = None
    }

    let inline internal invalidIndex n type' element = {
        Path = JsonPath.index n
        Element = element
        Details = "Index was out of range."
        Type = type'
        Value = JsonElement.getValue element
        Exn = None
    }

    let inline internal invalidTuple actual expected type' element = {
        Path = JsonPath.empty
        Element = element
        Details = $"Expected a tuple of %i{expected}, but got %i{actual}."
        Type = type'
        Value = JsonElement.getValue element
        Exn = None
    }

    let inline internal invalidOneOf value type' element = {
        Path = JsonPath.empty
        Element = element
        Details = $"Missing parser for discriminator '%s{value}'."
        Type = type'
        Value = None
        Exn = None
    }

    let inline internal duplicateKey key type' element = {
        Path = JsonPath.empty
        Element = element
        Details = $"Duplicate key '%s{key}'."
        Type = type'
        Value = None
        Exn = None
    }

    // Functions for appending the path.

    let inline private append path x =
        { x with Path = JsonPath.append path x.Path }

    let inline internal withProp name x =
        append (JsonPath.prop name) x

    let inline internal withIndex n x =
        append (JsonPath.index n) x

    let inline internal withPath path x =
        append path x

    /// <summary>Converts the ParseError to a formatted string.</summary>
    /// <param name="error">The ParseError to convert.</param>
    let asString error =
        string {
            $"at %s{JsonPath.asString error.Path}"
            $" | Tried parsing '%s{Type.getName error.Type}."
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
            string {
                $"Parser failed with %i{List.length list} error[s].\n"

                list
                |> List.mapi (fun i e ->
                    let error =
                        ParseError.asString e
                        |> String.indentLines

                    $"Error[%i{i}]:\n%s{error}"
                )
                |> String.concat "\n\n"
            }