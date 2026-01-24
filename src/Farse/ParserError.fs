namespace Farse

open System
open System.Text.Json

type ParserErrorType =
    | ArrayItem of array:JsonElement * error:ParserErrorType
    | ArrayIndex of array:JsonElement
    | CouldNotParse of msg:string * details:string option * parent:JsonElement option
    | InvalidKind of expected:ExpectedKind * element:JsonElement
    | InvalidValue of details:string option * expected:Type * element:JsonElement
    | KeyValue of error:ParserErrorType * object:JsonElement
    | Other of msg:string

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

    let internal enrich path parent error =
        let rec getError parent = function
            | ArrayItem (array, error) ->
                getError array error
            | ArrayIndex _ ->
                let msg = Error.invalidIndex
                CouldNotParse (msg, None, Some parent)
            | CouldNotParse(msg, details, parent) ->
                CouldNotParse (msg, details, parent)
            | InvalidKind (expected, actual) ->
                let msg = Error.invalidKind expected actual
                CouldNotParse (msg, None, Some parent)
            | InvalidValue (details, expected, element) ->
                let msg = Error.invalidValue expected element
                CouldNotParse (msg, details, Some parent)
            | KeyValue (error, parent) ->
                getError parent error
            | Other msg -> Other msg

        {
            error with
                Path = JsonPath.append path error.Path
                ErrorType = getError parent error.ErrorType
        }

    let internal enriched path parent =
        fromType >> enrich path parent

    let asString x =
        let rec getMessage path parent = function
            | ArrayItem (array, error) ->
                getMessage path (Some array) error
            | ArrayIndex array ->
                let msg = Error.invalidIndex
                Error.message path msg None (Some array)
            | CouldNotParse (msg, details, parent) ->
                Error.message path msg details parent
            | InvalidKind (expected, actual) ->
                let msg = Error.invalidKind expected actual
                Error.message path msg None parent
            | InvalidValue (details, expected, element) ->
                let msg = Error.invalidValue expected element
                Error.message path msg details parent
            | KeyValue (error, object) ->
                getMessage path (Some object) error
            | Other msg -> msg

        getMessage x.Path None x.ErrorType