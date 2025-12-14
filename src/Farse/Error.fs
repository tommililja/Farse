namespace Farse

open System
open System.Text.Json

module internal Error =

    let private create =
        String.concat "\n"

    let private print (element:JsonElement) =
        match element.ValueKind with
        | Kind.Null | Kind.Undefined -> String.Empty
        | _ -> $"\n%s{JsonElement.asJsonString element}"

    let rec private getTypeName (x:Type) =
        match x with
        | x when x.IsGenericType ->
            let genericName = x.Name.Substring(0, x.Name.IndexOf('`'))
            let args =
                x.GetGenericArguments()
                |> Array.map getTypeName
                |> String.concat ", "

            $"%s{genericName}<%s{args}>"
        | x -> x.Name

    let invalidValue msg (expectedType:Type) (element:JsonElement) =
        let value =
            match element.ValueKind with
            | Kind.Number | Kind.String | Kind.True | Kind.False -> $"'%s{JsonElement.asString element}'"
            | kind -> Kind.asString kind

        create [
            $"Tried parsing %s{value} to %s{getTypeName expectedType}."

            match msg with
            | Some msg when String.isNotEmpty msg -> $"Details: %s{msg}"
            | _ -> ()
        ]

    let invalidKind (expected:ExpectedKind) (element:JsonElement) =
        let expected = ExpectedKind.asString expected
        let actual = Kind.asString element.ValueKind
        $"Expected %s{expected}, but got %s{actual}."

    let duplicateKey key =
        $"Duplicate key '%s{key}'."

    let invalidTuple expected actual =
        $"Expected a tuple of %i{expected}, but got %i{actual}."

    let invalidIndex = "Index out of range."

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