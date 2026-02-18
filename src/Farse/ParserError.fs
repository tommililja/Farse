namespace Farse

open System
open System.Text.Json
open Farse

module internal Error =

    let inline list x =
        List.singleton x
        |> Error

    let triedParsing type' =
        $"Tried parsing '%s{Type.getName type'}."

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
            $"  at %s{JsonPath.asString path}"
            $"   | %s{msg}"

            details
            |> Option.map (sprintf "   | %s")

            value
            |> Option.map (sprintf "   = %s")
        }

    let invalidStream (exn:exn) =
        string {
            "Error: Could not parse JSON stream."
            $"Message: %s{exn.Message}"
        }

    let invalidJson (exn:exn) json =
        string {
            "Error: Could not parse JSON string."
            $"Message: %s{exn.Message}"

            match json with
            | null -> "JSON: null"
            | str -> $"JSON: \"%s{str}\""
        }

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

    let create msg type' element = {
        Path = JsonPath.empty
        ErrorType = InvalidValue (msg, type', element)
    }

module InvalidKind =

    let create expected element =
        let value = JsonElement.getValue element
        {
            Path = JsonPath.empty
            ErrorType = InvalidKind (expected, element.ValueKind, value)
        }

module ArrayItem =

    let create n error = {
        Path = JsonPath.append (JsonPath.index n) error.Path
        ErrorType = ArrayItem error.ErrorType
    }

module ArrayIndex =

    let create n = {
        Path = JsonPath.index n
        ErrorType = ArrayIndex
    }

module KeyValue =

    let create name error = {
        Path = JsonPath.prop name
        ErrorType = KeyValue error.ErrorType
    }

module Other =

    let create msg = {
        Path = JsonPath.empty
        ErrorType = Other msg
    }

module CouldNotParse =

    let invalidKind path (element:JsonElement) expected =
        let msg = Error.invalidKind expected element.ValueKind
        let value = JsonElement.getValue element
        {
            Path = path
            ErrorType = CouldNotParse (msg, None, value)
        }

module ParserError =

    let rec private getError = function
        | ArrayItem error -> getError error
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
        | KeyValue error -> getError error
        | Other msg -> Other msg

    let internal enrich path error =
        {
            error with
                Path = JsonPath.append path error.Path
                ErrorType = getError error.ErrorType
        }

    let asString error =
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
            | Other msg -> $"   | %s{msg}"

        getMessage error.Path error.ErrorType

module ParserErrors =

    let asString errors =
        string {
            $"Parser failed with [%i{List.length errors}] error[s].\n"

            errors
            |> List.mapi (fun i x -> $"error[%i{i + 1}]:\n%s{ParserError.asString x}")
            |> String.concat "\n\n"
        }