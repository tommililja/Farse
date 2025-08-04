namespace Farse.Tests

open Xunit
open Farse

module JsonString =

    [<Fact>]
    let ``Should create expected JSON object`` () =
        JObj [
            "int", JNum <| Int 123456789
            "float", JNum <| Float 123.12
            "decimal", JNum <| Decimal 123.12m
            "byte", JNum <| Byte 128uy
            "string", JStr "string"
            "bool", JBit true
            "object", JObj [
                "string", JStr "string"
            ]
            "array", JArr [
                JStr "string"
            ]
            "option", JOpt <| Some (JStr "string")
            "nil", JNil
        ]
        |> Json.asString
        |> Expect.string

    [<Fact>]
    let ``Should create expected JSON array`` () =
        JArr [
            JStr "item 1"
            JStr "item 2"
            JStr "item 3"
        ]
        |> Json.asString
        |> Expect.json