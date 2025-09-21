namespace Farse

open System
open System.Text.Json

[<NoComparison>]
type ParserError =
    | ArrayError of index:int * array:JsonElement * error:ParserError
    | CouldNotParse of name:string * msg:string * element:JsonElement
    | CouldNotRead of name:string * element:JsonElement
    | InvalidKind of expected:Kind * element:JsonElement
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