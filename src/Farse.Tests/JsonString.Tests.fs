namespace Farse.Tests

open Xunit
open Farse

module JsonString =

    [<Fact>]
    let ``Should create expected JSON string`` () =
        let jsonString =
            JsonString.create [
                "int", JNum <| Int 123456789
                "float", JNum <| Float 123.12
                "decimal", JNum <| Decimal 123.12m
                "string", JStr "string"
                "bool", JBit true
                "object",
                    JObj [
                        "string", JStr "string"
                    ]
                "array",
                    JArr [
                        JStr "string"
                    ]
                "option", JOpt <| Some (JStr "string")
                "nil", JNil
            ]

        jsonString
        |> JsonString.asString
        |> Verify.json