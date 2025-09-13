namespace Farse

open System
open System.Text.Json

module internal Error =

    let private create =
        String.concat "\n"

    let private print (element:JsonElement) =
        match element.ValueKind with
        | Kind.Null | Kind.Undefined -> String.Empty
        | kind -> $"%s{string kind}:\n%s{JsonElement.getJson element}"

    let invalidValue msg (expectedType:Type) element =
        let msg =
            match msg with
            | String msg ->
                match msg[msg.Length - 1] with
                | '.' | '!' | '?' -> $": %s{msg}"
                | _ -> $": %s{msg}."
            | Invalid -> "."

        $"Failed when parsing '%s{JsonElement.asString element}' as %s{expectedType.Name}%s{msg}"

    let invalidKind (expected:Kind) (actual:Kind) =
        let expected =
            match expected with
            | Kind.True | Kind.False -> "Bool"
            | kind -> string kind

        $"Expected '%s{expected}', but got '%s{string actual}'."

    let couldNotRead name (element:JsonElement) =
        create [
            $"Error: Could not read property '%s{name}'."
            $"Message: %s{invalidKind Kind.Object element.ValueKind}"
            print element
        ]

    let notObject name (object:JsonElement) (element:JsonElement) =
        create [
            $"Error: Could not parse property '%s{name}'."
            $"Message: %s{invalidKind Kind.Object element.ValueKind}"
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