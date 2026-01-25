namespace Farse

module internal Error =

    let invalidValue actual =
        $"Tried parsing %s{Type.getFullName actual}."

    let invalidKind expected actual =
        $"Expected %s{ExpectedKind.asString expected}, but got %s{Kind.asString actual}."

    let duplicateKey key =
        $"Duplicate key '%s{key}'."

    let invalidTuple expected actual =
        $"Expected a tuple of %i{expected}, but got %i{actual}."

    let invalidIndex =
        "Index was out of range."

    let message path msg details element =
        string {
            $"Path: %s{JsonPath.asString path}"
            $"Message: %s{msg}"

            details
            |> Option.map (sprintf "Details: %s")

            element
            |> Option.bind (
                JsonElement.getValue
                >> Option.map (sprintf "Value: %s")
            )
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