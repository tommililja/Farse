namespace Farse

open System
open System.Text.Json

module internal Error =

    let create list =
        list
        |> String.concat "\n"
        |> Error

    let couldNotParse (element:JsonElement) (expectedType:Type) =
        match element.ValueKind with
        | JsonValueKind.Array
        | JsonValueKind.Object -> $"%s{string element.ValueKind} is not a valid value for %s{expectedType.FullName}."
        | _ -> $"The value '%s{JsonElement.getRawText element}' is not valid for %s{expectedType.FullName}."

    let couldNotParseDateTime (element:JsonElement) format =
        $"The value '%s{JsonElement.getRawText element}' is not valid for %s{typeof<DateTime>.FullName} with format %s{format}."

    let invalidElement (expected:JsonValueKind) (actual:JsonValueKind) =
        $"Expected: %s{string expected}, actual: %s{string actual}."

    let printElement (element:JsonElement) =
        $"%s{string element.ValueKind}:\n%s{JsonElement.getJson element}"

    let notObject name (element:JsonElement) =
        create [
            $"Error: Could not parse property '%s{name}'."
            $"Cannot read property from %s{string element.ValueKind}."
            printElement element
        ]

    let nullProperty name element =
        create [
            $"Error: Required property '%s{name}' was null."
            printElement element
        ]

    let missingProperty name element =
        create [
            $"Error: Required property '%s{name}' was not found."
            printElement element
        ]

    let parseError name (error:string) element =
        // Quick ugly fix for nested parser errors.
        if error.StartsWith("Error:")
        then create [ error ]
        else
            create [
                $"Error: Could not parse property '%s{name}'."
                error
                printElement element
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