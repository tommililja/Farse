namespace Farse

open System
open System.Text.Json

type ParserError =
    | ArrayItem of index:int * array:JsonElement * error:ParserError
    | ArrayLength
    | CouldNotParse of name:string * msg:string * details:string option * parent:JsonElement
    | CouldNotRead of name:string * element:JsonElement
    | InvalidKind of expected:ExpectedKind * element:JsonElement
    | InvalidValue of details:string option * expected:Type * element:JsonElement
    | KeyValue of name:string * error:ParserError * parent:JsonElement
    | NotObject of name:string * parent:JsonElement * element:JsonElement
    | Other of msg:string

module ParserError =
    open Error

    let rec internal enrich name parent = function
        | ArrayItem (index, array, error) ->
            let name = $"%s{name}[%i{index}]"
            enrich name array error
        | ArrayLength ->
            CouldNotParse (name, invalidIndex, None, parent)
        | CouldNotRead (_, element) ->
            NotObject (name, parent, element)
        | InvalidKind (expected, actual) ->
            let msg = invalidKind expected actual
            CouldNotParse (name, msg, None, parent)
        | InvalidValue (details, expected, element) ->
            let msg = invalidValue expected element
            CouldNotParse (name, msg, details, parent)
        | KeyValue (name, error, parent) ->
            enrich name parent error
        | other -> other

    let rec asString = function
        | ArrayItem (_, _, error) -> asString error
        | ArrayLength -> invalidIndex
        | CouldNotParse (name, msg, details, parent) -> couldNotParse name msg details parent
        | CouldNotRead (name, element) -> couldNotRead name element
        | InvalidKind (expected, actual) -> invalidKind expected actual
        | InvalidValue (_, expected, element) -> invalidValue expected element
        | KeyValue (_, error, _) -> asString error
        | NotObject (name, parent, element) -> notObject name parent element
        | Other msg -> msg