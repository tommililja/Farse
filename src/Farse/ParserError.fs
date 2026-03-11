namespace Farse

open System
open System.Text.Json

type JsonPath = JsonPath of string list

module JsonPath =

    let inline internal prop name =
        JsonPath [ $".%s{name}" ]

    let inline internal index n =
        JsonPath [ $"[%i{n}]" ]

    let inline internal append (JsonPath a) (JsonPath b) =
        List.append a b
        |> JsonPath

    let empty = JsonPath []

    let asString (JsonPath list) =
        list
        |> List.append [ "$" ]
        |> String.concat String.Empty

type Details = {
    Path: JsonPath
    Element: JsonElement
    Details: string
    Value: string option
    Type: Type
}

type ParserError =
    | Json of exn
    | Failure of Details
    | Other of string

module ParserError =

    // Errors

    let inline internal validation details type' element value =
        Failure {
            Path = JsonPath.empty
            Element = element
            Details = details
            Value = Some value
            Type = type'
        }

    let inline internal invalid details type' element =
        Failure {
            Path = JsonPath.empty
            Element = element
            Details = defaultArg details "Invalid value."
            Value = JsonElement.getValue element
            Type = type'
        }

    let inline internal expectedKind expectedKind path type' element =
        Failure {
            Path = path
            Element = element
            Details = $"Expected %s{ExpectedKind.asString expectedKind}, but got %s{Kind.asString element.ValueKind}."
            Type = type'
            Value = JsonElement.getValue element
        }

    let inline internal invalidIndex n type' element =
        Failure {
            Path = JsonPath.index n
            Element = element
            Details = "Index was out of range."
            Type = type'
            Value = JsonElement.getValue element
        }

    let inline internal invalidTuple actual expected type' element =
        Failure {
            Path = JsonPath.empty
            Element = element
            Details = $"Expected a tuple of %i{expected}, but got %i{actual}."
            Type = type'
            Value = None
        }

    let inline internal invalidOneOf value type' element =
        Failure {
            Path = JsonPath.empty
            Element = element
            Details = $"Missing parser for discriminator '%s{value}'."
            Type = type'
            Value = None
        }

    let inline internal duplicateKey key type' element =
        Failure {
            Path = JsonPath.empty
            Element = element
            Details = $"Duplicate key '%s{key}'."
            Type = type'
            Value = None
        }

    // Functions for appending the path.

    let inline private append path x =
        Failure { x with Path = JsonPath.append path x.Path }

    let inline internal withProp name = function
        | Failure x -> append (JsonPath.prop name) x
        | x -> x

    let inline internal withIndex n = function
        | Failure x -> append (JsonPath.index n) x
        | x -> x

    let inline internal withPath path = function
        | Failure x -> append path x
        | x -> x

    // Public

    /// <summary>Converts the ParserError to a formatted string.</summary>
    /// <param name="error">The ParserError to convert.</param>
    let asString error =
        match error with
        | Failure details ->
            string {
                $"at %s{JsonPath.asString details.Path}"
                $" | Tried parsing '%s{Type.getName details.Type}"
                $" | %s{details.Details}"

                Option.map (sprintf " = %s") details.Value
            }
        | Json exn ->
            string {
                $"| Could not parse JSON."
                $"| %s{exn.Message}"
            }
        | Other msg -> $"| %s{msg}"

    /// <summary>Converts the ParserError list to a formatted string.</summary>
    /// <param name="list">The ParserError list to convert.</param>
    let asStringList list =
        string {
            $"Parser failed with %i{List.length list} error[s].\n"

            list
            |> List.mapi (fun i e ->
                let error =
                    asString e
                    |> _.Split('\n')
                    |> Array.map (sprintf "  %s")
                    |> String.concat "\n"

                $"Error[%i{i}]:\n%s{error}"
            )
            |> String.concat "\n\n"
        }