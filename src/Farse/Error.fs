namespace Farse

open System
open System.Text.Json

module internal Error =

    let private print (element:JsonElement option) =
        match element with
        | Some kind when kind.ValueKind = Kind.Null || kind.ValueKind = Kind.Undefined -> String.Empty
        | Some element -> $"\n%s{JsonElement.asJsonString element}"
        | None -> String.Empty

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
        "Index was out of range."

    let message path msg details parent =
        string {
            $"Path: %s{JsonPath.asString path}"
            $"Message: %s{msg}"

            details
            |> Option.map (sprintf "Details: %s")

            print parent
        }

    let invalidJson name (exn:exn) json =
        string {
            $"Error: Could not parse JSON %s{name}."
            $"Message: %s{exn.Message}"

            json
            |> Option.map (function
                | null -> "JSON: null"
                | str -> $"JSON: \"%s{str}\""
            )
        }