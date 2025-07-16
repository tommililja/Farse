namespace Farse

open System
open System.Text.Json

module internal Error =

    let create strings =
        strings
        |> String.concat "\n"
        |> Error

    let couldNotParse (expectedType:Type) (element:JsonElement) =
        $"The value '%s{JsonElement.getRawText element}' is not valid for %s{expectedType.FullName}."

    let couldNotParseDateTime format (element:JsonElement) =
        $"The value '%s{JsonElement.getRawText element}' is not valid for %s{typeof<DateTime>.FullName} with format '%s{format}'."

    let invalidElement (expected:Kind) (actual:Kind) =
        let expected =
            match expected with
            | Kind.True | Kind.False -> "Bool"
            | kind -> string kind

        $"Expected: %s{expected}, actual: %s{string actual}."

    let print (element:JsonElement) =
        $"%s{string element.ValueKind}:\n%s{JsonElement.getJson element}"

    let notObject name (element:JsonElement) =
        create [
            $"Error: Could not parse property '%s{name}'."
            $"Cannot read property from %s{string element.ValueKind}."
            print element
        ]

    let nullProperty name element =
        create [
            $"Error: Required property '%s{name}' was null."
            print element
        ]

    let missingProperty name element =
        create [
            $"Error: Required property '%s{name}' was not found."
            print element
        ]

    let parseError name msg element =
        create [
            $"Error: Could not parse property '%s{name}'."
            msg
            print element
        ]

    let invalidString () =
        create [
            "Error: Could not parse JSON string."
            "The string was null or empty."
        ]

    let invalidJson str (exn:exn) =
        create [
            "Error: Could not parse JSON string."
            exn.Message
            $"JSON: '%s{str}'."
        ]