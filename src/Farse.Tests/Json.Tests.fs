namespace Farse.Tests

open Xunit
open Farse

module Json =

    [<Fact>]
    let ``Should create expected JSON object`` () =
        JObj [
            "int", JNum 123
            "float", JNum 123.12
            "decimal", JNum 123.12m
            "byte", JNum 123uy
            "string", JStr "string"
            "bool", JBit true
            "object", JObj [
                "string", JStr "string"
            ]
            "array", JArr [
                JStr "string"
                JNon
            ]
            "some", JNil <| Some (JStr "string")
            "none", JNil None
            "missing", JNon
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