namespace Farse

open System
open System.Text.Json

type ParserErrorType =
    | ArrayItem of index:int * array:JsonElement * error:ParserErrorType
    | ArrayIndex
    | CouldNotParse of name:string * msg:string * details:string option * parent:JsonElement
    | InvalidKind of expected:ExpectedKind * element:JsonElement
    | InvalidValue of details:string option * expected:Type * element:JsonElement
    | KeyValue of name:string * error:ParserErrorType * parent:JsonElement
    | Other of msg:string

type ParserError = {
    Path: JsonPath
    ErrorType: ParserErrorType
}

module ParserError =
    open Error

    let fromType errorType = {
        Path = JsonPath.empty
        ErrorType = errorType
    }

    let internal create path errorType = {
        Path = path
        ErrorType = errorType
    }

    let internal enrich name path parent error =
        let rec enrichError name parent = function
            | ArrayItem (index, array, error) ->
                let name = $"%s{name}[%i{index}]"
                enrichError name array error
            | ArrayIndex ->
                CouldNotParse (name, invalidIndex, None, parent)
            | InvalidKind (expected, actual) ->
                let msg = invalidKind expected actual
                CouldNotParse (name, msg, None, parent)
            | InvalidValue (details, expected, element) ->
                let msg = invalidValue expected element
                CouldNotParse (name, msg, details, parent)
            | KeyValue (name, error, parent) ->
                enrichError name parent error
            | other -> other

        {
            error with
                Path = JsonPath.append path error.Path
                ErrorType = enrichError name parent error.ErrorType
        }

    let internal enriched name path parent =
        fromType >> enrich name path parent

    let asString x =
        let rec getMessage path = function
            | ArrayItem (_, _, error) -> getMessage path error
            | ArrayIndex -> invalidIndex
            | CouldNotParse (name, msg, details, parent) -> couldNotParse name path msg details parent
            | InvalidKind (expected, actual) -> invalidKind expected actual
            | InvalidValue (_, expected, element) -> invalidValue expected element
            | KeyValue (_, error, _) -> getMessage path error
            | Other msg -> msg

        getMessage x.Path x.ErrorType