namespace Farse

open System
open System.Text.Json

module internal Error =

    let create strings =
        strings
        |> String.concat "\n"
        |> Error

    let couldNotParse (expectedType:Type) element =
        $"The value '%s{JsonElement.getRawText element}' is not valid for %s{expectedType.FullName}."

    let invalidElement (expected:Kind) (actual:Kind) =
        let expected =
            match expected with
            | Kind.True | Kind.False -> "Bool"
            | kind -> string kind

        $"Expected: %s{expected}, actual: %s{string actual}."

    let print (element:JsonElement) =
        match element.ValueKind with
        | Kind.Null | Kind.Undefined -> String.Empty
        | kind -> $"%s{string kind}:\n%s{JsonElement.getJson element}"

    let couldNotRead name (element:JsonElement) =
        create [
            $"Error: Could not read property '%s{name}'."
            invalidElement Kind.Object element.ValueKind
            print element
        ]

    let notObject name (previous:JsonElement) (element:JsonElement) =
        create [
            $"Error: Could not parse property '%s{name}'."
            invalidElement Kind.Object element.ValueKind
            print previous
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

    let invalidJson json (exn:exn) =
        create [
            "Error: Could not parse JSON string."
            exn.Message
            $"JSON: '%s{json}'."
        ]