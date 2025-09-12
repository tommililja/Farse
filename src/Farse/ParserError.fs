namespace Farse

open System
open System.Text.Json

module internal Error =

    let private create lines =
        lines
        |> String.concat "\n"

    let private print (element:JsonElement) =
        match element.ValueKind with
        | Kind.Null | Kind.Undefined -> String.Empty
        | kind -> $"%s{string kind}:\n%s{JsonElement.getJson element}"

    let invalidValue msg (expectedType:Type) element =
        let msg =
            match msg with
            | String msg ->
                match msg[msg.Length - 1] with
                | '.' | '!' | '?' -> $" %s{msg}"
                | _ -> $" %s{msg}."
            | Invalid -> String.Empty

        $"Failed to parse '%s{JsonElement.asString element}' as %s{expectedType.Name}.%s{msg}"

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

    let invalidString () =
        create [
            "Error: Could not parse JSON string."
            "Message: The string was null or empty."
        ] |> Error

    let invalidJson json (exn:exn) =
        create [
            "Error: Could not parse JSON string."
            $"Message: %s{exn.Message}"
            $"JSON: '%s{json}'."
        ] |> Error

[<NoComparison>]
type ParserError =
    | ArrayError of index:int * array:JsonElement * error:ParserError
    | CouldNotParse of name:string * msg:string * element:JsonElement
    | CouldNotRead of name:string * element:JsonElement
    | InvalidKind of expected:Kind * actual:Kind
    | InvalidValue of msg:string * expected:Type * element:JsonElement
    | NotObject of name:string * object:JsonElement * element:JsonElement
    | Other of msg:string

module ParserError =
    open Error

    let rec internal enrich name object = function
        | ArrayError (index, array, error) ->
            let name = $"%s{name}[%i{index}]"
            enrich name array error
        | CouldNotRead(_, element) ->
            NotObject (name, object, element)
        | InvalidKind (expected, actual) ->
            let msg = invalidKind expected actual
            CouldNotParse (name, msg, object)
        | InvalidValue (msg, expected, element) ->
            let msg = invalidValue msg expected element
            CouldNotParse (name, msg, object)
        | other -> other

    let rec asString = function
        | ArrayError (_, _, msg) -> asString msg
        | CouldNotParse (name, msg, element) -> couldNotParse name msg element
        | CouldNotRead (name, element) -> couldNotRead name element
        | InvalidKind (expected, actual) -> invalidKind expected actual
        | InvalidValue (msg, expected, element) -> invalidValue msg expected element
        | NotObject (name, object, element) -> notObject name object element
        | Other msg -> msg