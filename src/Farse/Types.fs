namespace Farse

open System.Text.Json

// Ignore enum match warning.
#nowarn 104

[<RequireQualifiedAccess>]
type ExpectedKind =
    | Undefined
    | Object
    | Array
    | String
    | Number
    | Bool
    | Null
    | Any

module ExpectedKind =

    let fromKind = function
        | JsonValueKind.Undefined -> ExpectedKind.Undefined
        | JsonValueKind.Object -> ExpectedKind.Object
        | JsonValueKind.Array -> ExpectedKind.Array
        | JsonValueKind.String -> ExpectedKind.String
        | JsonValueKind.Number -> ExpectedKind.Number
        | JsonValueKind.True | JsonValueKind.False -> ExpectedKind.Bool
        | JsonValueKind.Null -> ExpectedKind.Null

    let asString = function
        | ExpectedKind.Undefined -> "Undefined"
        | ExpectedKind.Object -> "Object"
        | ExpectedKind.Array -> "Array"
        | ExpectedKind.String -> "String"
        | ExpectedKind.Number -> "Number"
        | ExpectedKind.Bool -> "Bool"
        | ExpectedKind.Null -> "Null"
        | ExpectedKind.Any -> "Any"