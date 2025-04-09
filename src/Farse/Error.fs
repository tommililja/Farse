namespace Farse

open System
open System.Text.Json

exception private ArrayException of Error:string

module internal Error =

    let create list =
        list
        |> String.concat "\n"
        |> Error

    let couldNotParseJson =
        "Error: Could not parse JSON string."

    let nullOrEmpty =
        "The string was null or empty."

    let invalidJson (exn:exn) =
        exn.Message

    let jsonString str =
        $"String: '%s{str}'."

    let parsingProperty name =
         $"Error: Could not parse property '%s{name}'."

    let couldNotParse (element:JsonElement) (expectedType:Type) =
        $"The value '%s{JsonElement.getRawText element}' is not valid for %s{expectedType.FullName}."

    let invalidElement (expected:JsonValueKind) (actual:JsonValueKind) =
        $"Expected: %s{string expected}, actual: %s{string actual}."

    let nullProperty name =
        $"Error: Required property '%s{name}' was null or not found."

    let element (element:JsonElement) =
        $"%s{string element.ValueKind}:\n%s{JsonElement.getJson element}"

    let notObject (element:JsonElement) =
        invalidElement JsonValueKind.Object element.ValueKind