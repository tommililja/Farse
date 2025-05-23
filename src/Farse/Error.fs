namespace Farse

open System
open System.Text.Json

exception private ArrayException of Error:string

module internal Error =

    let create list =
        list
        |> String.concat "\n"
        |> Error

    let couldNotParse (element:JsonElement) (expectedType:Type) =
        $"The value '%s{JsonElement.getRawText element}' is not valid for %s{expectedType.FullName}."

    let invalidElement (expected:JsonValueKind) (actual:JsonValueKind) =
        $"Expected: %s{string expected}, actual: %s{string actual}."

    let element (element:JsonElement) =
        $"%s{string element.ValueKind}:\n%s{JsonElement.getJson element}"

    // Errors

    let notObject (e:JsonElement) =
        create [
            invalidElement JsonValueKind.Object e.ValueKind
            element e
        ]

    let nullProperty name e =
        create [
            $"Error: Required property '%s{name}' was null or not found."
            element e
        ]

    let parseError name msg e =
        create [
            $"Error: Could not parse property '%s{name}'."
            msg
            element e
        ]

    let nullOrEmptyJson () =
        create [
            "Error: Could not parse JSON string."
            "The string was null or empty."
        ]

    let invalidJson str (exn:exn) =
        create [
            "Error: Could not parse JSON string."
            exn.Message
            $"String: '%s{str}'."
        ]