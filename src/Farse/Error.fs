namespace Farse

open System
open System.Text.Json

module internal Error =

    let private create =
        String.concat "\n"

    let private print (element:JsonElement) =
        match element.ValueKind with
        | Kind.Null | Kind.Undefined -> String.Empty
        | kind -> $"%s{string kind}:\n%s{JsonElement.asJsonString element}"

    let invalidValue msg (expectedType:Type) element =
        let value = JsonElement.asString element
        let details =
            match msg with
            | String msg -> $" Details: %s{msg}"
            | Empty -> String.Empty

        $"Tried parsing '%s{value}' to %s{expectedType.Name}.%s{details}"

    let invalidKind (expected:Kind) (element:JsonElement) =
        let expected = Kind.asString expected
        let actual = Kind.asString element.ValueKind
        let value =
            match element.ValueKind with
            | Kind.Null | Kind.Undefined | Kind.Object | Kind.Array -> String.Empty
            | _ -> $". Value: '%s{JsonElement.asString element}'"

        $"Expected %s{expected}, but got %s{actual}%s{value}."

    let couldNotRead name (element:JsonElement) =
        create [
            $"Error: Could not read property '%s{name}'."
            $"Message: %s{invalidKind Kind.Object element}"
            print element
        ]

    let notObject name (object:JsonElement) (element:JsonElement) =
        create [
            $"Error: Could not parse property '%s{name}'."
            $"Message: %s{invalidKind Kind.Object element}"
            print object
        ]

    let couldNotParse name msg element =
        create [
            $"Error: Could not parse property '%s{name}'."
            $"Message: %s{msg}"
            print element
        ]

    let invalidJson (exn:exn) json =
        create [
            "Error: Could not parse JSON string."
            $"Message: %s{exn.Message}"
            $"JSON: '%s{json}'."
        ]