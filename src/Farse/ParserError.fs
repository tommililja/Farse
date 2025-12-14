namespace Farse

open System
open System.Text.Json

[<NoComparison>]
type ParserError =
    | ArrayError of index:int * array:JsonElement * error:ParserError
    | ArrayLengthError
    | CouldNotParse of name:string * msg:string * parent:JsonElement
    | KeyValueError of name:string * error:ParserError * parent:JsonElement
    | CouldNotRead of name:string * element:JsonElement
    | InvalidKind of expected:ExpectedKind * element:JsonElement
    | InvalidValue of msg:string option * expected:Type * element:JsonElement
    | InvalidTuple of expected:int * actual:int * element:JsonElement
    | DuplicateKeys of key:string * element:JsonElement
    | NotObject of name:string * parent:JsonElement * element:JsonElement
    | Other of msg:string

module ParserError =
    open Error

    let rec internal enrich name parent = function
        | ArrayError (index, array, error) ->
            let name = $"%s{name}[%i{index}]"
            enrich name array error
        | ArrayLengthError ->
            CouldNotParse (name, invalidIndex, parent)
        | CouldNotRead(_, element) ->
            NotObject (name, parent, element)
        | KeyValueError (name, error, parent) ->
            enrich name parent error
        | InvalidKind (expected, actual) ->
            let msg = invalidKind expected actual
            CouldNotParse (name, msg, parent)
        | InvalidValue (msg, expected, element) ->
            let msg = invalidValue msg expected element
            CouldNotParse (name, msg, parent)
        | DuplicateKeys (key, element) ->
            let msg = duplicateKey key
            CouldNotParse (name, msg, element)
        | InvalidTuple (expected, actual, array) ->
            let msg = invalidTuple expected actual
            CouldNotParse (name, msg, array)
        | other -> other

    let rec asString = function
        | ArrayError (_, _, error) -> asString error
        | ArrayLengthError -> invalidIndex
        | CouldNotParse (name, msg, parent) -> couldNotParse name msg parent
        | CouldNotRead (name, element) -> couldNotRead name element
        | InvalidKind (expected, actual) -> invalidKind expected actual
        | InvalidValue (msg, expected, element) -> invalidValue msg expected element
        | KeyValueError (_, error, _) -> asString error
        | InvalidTuple (expected, actual, _) -> invalidTuple expected actual
        | DuplicateKeys (key, _) -> duplicateKey key
        | NotObject (name, parent, element) -> notObject name parent element
        | Other msg -> msg