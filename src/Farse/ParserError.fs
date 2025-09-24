namespace Farse

open System
open System.Text.Json

[<NoComparison>]
type ParserError =
    | ArrayError of index:int * array:JsonElement * error:ParserError
    | CouldNotParse of name:string * msg:string * parent:JsonElement
    | CouldNotRead of name:string * element:JsonElement
    | InvalidKind of expected:ExpectedKind * element:JsonElement
    | InvalidValue of msg:string option * expected:Type * element:JsonElement
    | NotObject of name:string * parent:JsonElement * element:JsonElement
    | Other of msg:string

module ParserError =
    open Error

    let rec internal enrich name parent = function
        | ArrayError (index, array, error) ->
            let name = $"%s{name}[%i{index}]"
            enrich name array error
        | CouldNotRead(_, element) ->
            NotObject (name, parent, element)
        | InvalidKind (expected, actual) ->
            let msg = invalidKind expected actual
            CouldNotParse (name, msg, parent)
        | InvalidValue (msg, expected, element) ->
            let msg = invalidValue msg expected element
            CouldNotParse (name, msg, parent)
        | other -> other

    let rec asString = function
        | ArrayError (_, _, msg) -> asString msg
        | CouldNotParse (name, msg, parent) -> couldNotParse name msg parent
        | CouldNotRead (name, element) -> couldNotRead name element
        | InvalidKind (expected, actual) -> invalidKind expected actual
        | InvalidValue (msg, expected, element) -> invalidValue msg expected element
        | NotObject (name, parent, element) -> notObject name parent element
        | Other msg -> msg