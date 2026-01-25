namespace Farse

open System
open System.Text.Json

type ParserErrorType =
    | ArrayItem of error:ParserErrorType
    | ArrayIndex
    | CouldNotParse of msg:string * details:string option * element:JsonElement option
    | InvalidKind of expected:ExpectedKind * element:JsonElement
    | InvalidValue of details:string option * expected:Type * element:JsonElement
    | KeyValue of error:ParserErrorType

type ParserError = {
    Path: JsonPath
    ErrorType: ParserErrorType
}

module ParserError =

    let fromType errorType = {
        Path = JsonPath.empty
        ErrorType = errorType
    }

    let internal create path errorType = {
        Path = path
        ErrorType = errorType
    }

    let internal enrich path error =
        let rec getError = function
            | ArrayItem error ->
                getError error
            | ArrayIndex ->
                let msg = Error.invalidIndex
                CouldNotParse (msg, None, None)
            | CouldNotParse(msg, details, element) ->
                CouldNotParse (msg, details, element)
            | InvalidKind (expected, actual) ->
                let msg = Error.invalidKind expected actual.ValueKind
                CouldNotParse (msg, None, Some actual)
            | InvalidValue (details, expected, element) ->
                let msg = Error.invalidValue expected
                CouldNotParse (msg, details, Some element)
            | KeyValue error ->
                getError error

        {
            error with
                Path = JsonPath.append path error.Path
                ErrorType = getError error.ErrorType
        }

    let internal enriched path =
        fromType >> enrich path

    let asString x =
        let rec getMessage path = function
            | ArrayItem error ->
                getMessage path error
            | ArrayIndex ->
                let msg = Error.invalidIndex
                Error.message path msg None None
            | CouldNotParse (msg, details, element) ->
                Error.message path msg details element
            | InvalidKind (expected, actual) ->
                let msg = Error.invalidKind expected actual.ValueKind
                Error.message path msg None (Some actual)
            | InvalidValue (details, expected, element) ->
                let msg = Error.invalidValue expected
                Error.message path msg details (Some element)
            | KeyValue error ->
                getMessage path error

        getMessage x.Path x.ErrorType