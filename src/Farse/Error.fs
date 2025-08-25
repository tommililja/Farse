namespace Farse

open System
open System.Text.Json

module internal Error =

    let private create strings =
        strings
        |> String.concat "\n"
        |> Error

    let private print (element:JsonElement) =
        match element.ValueKind with
        | Kind.Null | Kind.Undefined -> String.Empty
        | kind -> $"%s{string kind}:\n%s{JsonElement.getJson element}"

    let invalidValue msg (expectedType:Type) element =
        let additionalInfo =
            match msg with
            | String msg -> $": %s{msg}"
            | Invalid -> "."

        $"Could not parse '%s{JsonElement.getRawText element}' into %s{expectedType.FullName}%s{additionalInfo}"

    let invalidKind (expected:Kind) (actual:Kind) =
        let expected =
            match expected with
            | Kind.True | Kind.False -> "Bool"
            | kind -> string kind

        $"Expected: %s{expected}, actual: %s{string actual}."

    let couldNotRead name (element:JsonElement) =
        create [
            $"Error: Could not read property '%s{name}'."
            invalidKind Kind.Object element.ValueKind
            print element
        ]

    let notObject name previous (element:JsonElement) =
        create [
            $"Error: Could not parse property '%s{name}'."
            invalidKind Kind.Object element.ValueKind
            print previous
        ]

    let couldNotParse name msg element =
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