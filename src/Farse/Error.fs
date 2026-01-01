namespace Farse

open System
open System.Text.Json

module internal Error =

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

    let invalidValue (expectedType:Type) (element:JsonElement) =
        let value =
            match element.ValueKind with
            | Kind.Number | Kind.String | Kind.True | Kind.False -> $"'%s{JsonElement.asString element}'"
            | kind -> Kind.asString kind

        $"Tried parsing %s{value} to %s{getTypeName expectedType}."

    let invalidKind (expected:ExpectedKind) (element:JsonElement) =
        $"Expected %s{ExpectedKind.asString expected}, but got %s{Kind.asString element.ValueKind}."

    let duplicateKey key =
        $"Duplicate key '%s{key}'."

    let invalidTuple expected actual =
        $"Expected a tuple of %i{expected}, but got %i{actual}."

    let invalidIndex =
        "Index out of range."

    let couldNotRead name element =
        string {
            $"Error: Could not read property '%s{name}'."
            $"Message: %s{invalidKind ExpectedKind.Object element}"

            print element
        }

    let couldNotParse name msg details parent =
        string {
            $"Error: Could not parse property '%s{name}'."
            $"Message: %s{msg}"

            details
            |> Option.map (fun msg ->
                $"Details: %s{msg}"
            )

            print parent
        }

    let notObject name parent element =
        string {
            $"Error: Could not parse property '%s{name}'."
            $"Message: %s{invalidKind ExpectedKind.Object element}"

            print parent
        }

    let invalidJson (exn:exn) json =
        string {
            "Error: Could not parse JSON string."
            $"Message: %s{exn.Message}"
            $"JSON: '%s{json}'."
        }