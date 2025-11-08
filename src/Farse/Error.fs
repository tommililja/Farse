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

    let invalidValue msg (expectedType:Type) (element:JsonElement) =
        let value =
            match element.ValueKind with
            | Kind.Number | Kind.String | Kind.True | Kind.False -> $"'%s{JsonElement.asString element}'"
            | kind -> Kind.asString kind

        match msg with
        | Some msg when String.isNotEmpty msg -> $"Tried parsing %s{value} to %s{expectedType.Name}. Details: %s{msg}"
        | _ -> $"Tried parsing %s{value} to %s{expectedType.Name}."

    let invalidKind (expected:ExpectedKind) (element:JsonElement) =
        let expected = ExpectedKind.asString expected
        let actual = Kind.asString element.ValueKind
        match element.ValueKind with
        | Kind.Null | Kind.Undefined | Kind.Object | Kind.Array -> $"Expected %s{expected}, but got %s{actual}."
        | _ -> $"Expected %s{expected}, but got %s{actual}. Value: '%s{JsonElement.asString element}'."

    let duplicateKey key =
        $"Duplicate key '%s{key}'."

    let invalidTuple expected actual =
        $"Expected a tuple of %i{expected}, actual %i{actual}."

    let couldNotRead name (element:JsonElement) =
        create [
            $"Error: Could not read property '%s{name}'."
            $"Message: %s{invalidKind ExpectedKind.Object element}"
            print element
        ]

    let notObject name (parent:JsonElement) (element:JsonElement) =
        create [
            $"Error: Could not parse property '%s{name}'."
            $"Message: %s{invalidKind ExpectedKind.Object element}"
            print parent
        ]

    let couldNotParse name msg parent =
        create [
            $"Error: Could not parse property '%s{name}'."
            $"Message: %s{msg}"
            print parent
        ]

    let invalidJson (exn:exn) json =
        create [
            "Error: Could not parse JSON string."
            $"Message: %s{exn.Message}"
            $"JSON: '%s{json}'."
        ]