namespace Farse

open System
open System.Text.Json

module internal Error =

    let triedParsing type' =
        $"Tried parsing %s{Type.getName type'}."

    let invalidKind expected actual =
        $"Expected %s{ExpectedKind.asString expected}, but got %s{Kind.asString actual}."

    let invalidTuple expected actual =
        $"Expected a tuple of %i{expected}, but got %i{actual}."

    let invalidIndex =
        "Index was out of range."

    let duplicateKey key =
        $"Duplicate key '%s{key}'."

    let message path msg details value =
        string {
            $"Path: %s{JsonPath.asString path}"
            $"Message: %s{msg}"

            details
            |> Option.map (sprintf "Details: %s")

            value
            |> Option.map (sprintf "Value: %s")
        }

    let invalidStream (exn:exn) =
        string {
            "Error: Could not parse JSON stream."
            $"Message: %s{exn.Message}"
        } |> Error

    let invalidJson (exn:exn) json =
        string {
            "Error: Could not parse JSON string."
            $"Message: %s{exn.Message}"

            match json with
            | null -> "JSON: null"
            | str -> $"JSON: \"%s{str}\""
        } |> Error

type ParserErrorType =
    | ArrayItem of error:ParserErrorType
    | ArrayIndex
    | CouldNotParse of msg:string * details:string option * value:string option
    | InvalidKind of expected:ExpectedKind * actual:Kind * value:string option
    | InvalidValue of details:string option * type':Type * value:string option
    | KeyValue of error:ParserErrorType
    | Other of msg:string

type ParserError = {
    Path: JsonPath
    ErrorType: ParserErrorType
}

module InvalidValue =

    let create msg type' element =
        Error {
            Path = JsonPath.empty
            ErrorType = InvalidValue (msg, type', element)
        }

module InvalidKind =

    let create expected element =
        let value = JsonElement.getValue element
        Error {
            Path = JsonPath.empty
            ErrorType = InvalidKind (expected, element.ValueKind, value)
        }

module ArrayItem =

    let create n error =
        Error {
            Path = JsonPath.append (JsonPath.index n) error.Path
            ErrorType = ArrayItem error.ErrorType
        }

module ArrayIndex =

    let create n =
        Error {
            Path = JsonPath.index n
            ErrorType = ArrayIndex
        }

module KeyValue =

    let create name error =
        Error {
            Path = JsonPath.prop name
            ErrorType = KeyValue error
        }

module Other =

    let create msg =
        Error {
            Path = JsonPath.empty
            ErrorType = Other msg
        }

module CouldNotParse =

    let invalidKind path (element:JsonElement) expected =
        let msg = Error.invalidKind expected element.ValueKind
        let value = JsonElement.getValue element
        Error {
            Path = path
            ErrorType = CouldNotParse (msg, None, value)
        }

module ParserError =

    let internal enrich path error =
        let rec getError = function
            | ArrayItem error ->
                getError error
            | ArrayIndex ->
                let msg = Error.invalidIndex
                CouldNotParse (msg, None, None)
            | CouldNotParse (msg, details, value) ->
                CouldNotParse (msg, details, value)
            | InvalidKind (expected, actual, value) ->
                let msg = Error.invalidKind expected actual
                CouldNotParse (msg, None, value)
            | InvalidValue (details, type', value) ->
                let msg = Error.triedParsing type'
                CouldNotParse (msg, details, value)
            | KeyValue error ->
                getError error
            | Other msg -> Other msg

        Error {
            error with
                Path = JsonPath.append path error.Path
                ErrorType = getError error.ErrorType
        }

    let asString x =
        let rec getMessage path = function
            | ArrayItem error ->
                getMessage path error
            | ArrayIndex ->
                let msg = Error.invalidIndex
                Error.message path msg None None
            | CouldNotParse (msg, details, value) ->
                Error.message path msg details value
            | InvalidKind (expected, actual, value) ->
                let msg = Error.invalidKind expected actual
                Error.message path msg None value
            | InvalidValue (details, type', value) ->
                let msg = Error.triedParsing type'
                Error.message path msg details value
            | KeyValue error ->
                getMessage path error
            | Other msg -> msg

        getMessage x.Path x.ErrorType