namespace Farse

open System.Text.Json

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

module private ExpectedKind =

    let fromKind = function
        | JsonValueKind.Array -> ExpectedKind.Array
        | JsonValueKind.Null -> ExpectedKind.Null
        | JsonValueKind.Number -> ExpectedKind.Number
        | JsonValueKind.Object -> ExpectedKind.Object
        | JsonValueKind.String -> ExpectedKind.String
        | JsonValueKind.True | JsonValueKind.False -> ExpectedKind.Bool
        | JsonValueKind.Undefined -> ExpectedKind.Undefined

    let asString = function
        | ExpectedKind.Any -> "Any"
        | ExpectedKind.Array -> "Array"
        | ExpectedKind.Bool -> "Bool"
        | ExpectedKind.Null -> "Null"
        | ExpectedKind.Number -> "Number"
        | ExpectedKind.Object -> "Object"
        | ExpectedKind.String -> "String"
        | ExpectedKind.Undefined -> "Undefined"