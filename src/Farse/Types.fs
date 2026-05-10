namespace Farse

open System.Text.Json

[<Struct; NoComparison>]
type internal JsonElementType =
    | Element of JsonElement
    | Undefined of JsonElement
    | Null of JsonElement

type internal Kind = JsonValueKind

module internal Kind =

    let asString = function
        | Kind.Array -> "Array"
        | Kind.Null -> "Null"
        | Kind.Number -> "Number"
        | Kind.Object -> "Object"
        | Kind.String -> "String"
        | Kind.True | Kind.False -> "Bool"
        | Kind.Undefined -> "Undefined"

[<RequireQualifiedAccess>]
type ExpectedKind =
    | Any
    | Array
    | Bool
    | Null
    | Number
    | Object
    | String
    | Undefined

module internal ExpectedKind =

    let fromKind = function
        | Kind.Array -> ExpectedKind.Array
        | Kind.Null -> ExpectedKind.Null
        | Kind.Number -> ExpectedKind.Number
        | Kind.Object -> ExpectedKind.Object
        | Kind.String -> ExpectedKind.String
        | Kind.True | Kind.False -> ExpectedKind.Bool
        | Kind.Undefined -> ExpectedKind.Undefined

    let asString = function
        | ExpectedKind.Any -> "Any"
        | ExpectedKind.Array -> "Array"
        | ExpectedKind.Bool -> "Bool"
        | ExpectedKind.Null -> "Null"
        | ExpectedKind.Number -> "Number"
        | ExpectedKind.Object -> "Object"
        | ExpectedKind.String -> "String"
        | ExpectedKind.Undefined -> "Undefined"