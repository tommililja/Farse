namespace Farse.Tests

open System
open Xunit
open Farse

module ErrorTests =

    module Parser =

        [<Fact>]
        let ``Should return Error when parsing invalid JSON`` () =
            Parse.none
            |> Parser.parse "invalid"
            |> Expect.errorString

        [<Fact>]
        let ``Should return Error when parsing a null string`` () =
            Parse.none
            |> Parser.parse null
            |> Expect.errorString

        [<Fact>]
        let ``Should return Error when parsing an empty string`` () =
            Parse.none
            |> Parser.parse String.Empty
            |> Expect.errorString

        [<Fact>]
        let ``Should return Error when validation fails`` () =
            Parse.int
            |> Parser.validate (fun int ->
                if int > 1 then Ok int
                else Error "Value too small."
            )
            |> Parser.parse "1"
            |> Expect.errorString

        [<Fact>]
        let ``Should return Error when optional validation fails`` () =
            Parser.opt "prop" Parse.int
            |> Parser.validate (fun int ->
                if int > 1 then Ok int
                else Error "Value too small."
            )
            |> Parser.parse """{ "prop": 1 }"""
            |> Expect.errorString

        [<Fact>]
        let ``Should return Error when creating a Parser from an Error`` () =
            Error "Error"
            |> Parser.fromResult
            |> Parser.parse "1"
            |> Expect.errorString

    module Parsing =

        [<Fact>]
        let ``Should return Error when parsing an incorrect value`` () =
            Parser.req "prop" Parse.int
            |> Parser.parse """{ "prop": "1" }"""
            |> Expect.errorString

        [<Fact>]
        let ``Should return Error when parsing a nested incorrect value`` () =
            Parser.req "prop.prop2" Parse.int
            |> Parser.parse """{ "prop": { "prop2": "1" } }"""
            |> Expect.errorString

        [<Fact>]
        let ``Should return Error when try parsing an incorrect value`` () =
            Parser.opt "prop" Parse.int
            |> Parser.parse """{ "prop": "1" }"""
            |> Expect.errorString

        [<Fact>]
        let ``Should return Error when try parsing a nested incorrect value`` () =
            Parser.opt "prop.prop2" Parse.int
            |> Parser.parse """{ "prop": { "prop2": "1" } }"""
            |> Expect.errorString

        [<Fact>]
        let ``Should return Error when parsing a property from an element that is not an object`` () =
            Parser.req "prop" Parse.int
            |> Parser.parse """[]"""
            |> Expect.errorString

        [<Fact>]
        let ``Should return Error when parsing a nested property from an element that is not an object`` () =
            Parser.req "prop.prop2" Parse.int
            |> Parser.parse """{ "prop": [] }"""
            |> Expect.errorString

        [<Fact>]
        let ``Should return Error when try parsing a property from an element that is not an object`` () =
            Parser.opt "prop" Parse.int
            |> Parser.parse """[]"""
            |> Expect.errorString

        [<Fact>]
        let ``Should return Error when try parsing a nested property from an element that is not an object`` () =
            Parser.opt "prop.prop2" Parse.int
            |> Parser.parse """{ "prop": [] }"""
            |> Expect.errorString

        [<Fact>]
        let ``Should return Error when parsing a value when the value is an object`` () =
            Parser.req "prop" Parse.int
            |> Parser.parse """{ "prop": { "prop2": null } }"""
            |> Expect.errorString

        [<Fact>]
        let ``Should return Error when parsing a nested value when the value is an object`` () =
            Parser.req "prop.prop2" Parse.int
            |> Parser.parse """{ "prop": { "prop2": { "prop3": 1 } } }"""
            |> Expect.errorString

        [<Fact>]
        let ``Should return Error when try parsing a value when the value is an object`` () =
            Parser.opt "prop" Parse.int
            |> Parser.parse """{ "prop": { "prop2": null } }"""
            |> Expect.errorString

        [<Fact>]
        let ``Should return Error when try parsing a nested value when the value is an object`` () =
            Parser.opt "prop.prop2" Parse.int
            |> Parser.parse """{ "prop": { "prop2": { "prop3": 1 } } }"""
            |> Expect.errorString

        [<Fact>]
        let ``Should return Error when parsing a value when the value is an array`` () =
            Parser.req "prop" Parse.int
            |> Parser.parse """{ "prop": [] }"""
            |> Expect.errorString

        [<Fact>]
        let ``Should return Error when parsing a nested value when the value is an array`` () =
            Parser.req "prop.prop2" Parse.int
            |> Parser.parse """{ "prop": { "prop2": [] } }"""
            |> Expect.errorString

        [<Fact>]
        let ``Should return Error when try parsing a value when the value is an array`` () =
            Parser.opt "prop" Parse.int
            |> Parser.parse """{ "prop": [] }"""
            |> Expect.errorString

        [<Fact>]
        let ``Should return Error when try parsing a nested value when the value is an array`` () =
            Parser.opt "prop.prop2" Parse.int
            |> Parser.parse """{ "prop": { "prop2": [] } }"""
            |> Expect.errorString

        [<Fact>]
        let ``Should return Error when parsing an incorrect value in an array`` () =
            Parser.req "prop" (Parse.list Parse.string)
            |> Parser.parse """{ "prop": [ "1", 2, "3" ] }"""
            |> Expect.errorString

        [<Fact>]
        let ``Should return Error when parsing an incorrect value in a nested array`` () =
            let parser = Parse.list Parse.string
            Parser.req "prop" (Parse.list parser)
            |> Parser.parse """{ "prop": [ [ "1" ], [ "1", 2, "3" ] ] }"""
            |> Expect.errorString

        [<Fact>]
        let ``Should return Error when parsing a nested incorrect value in an array`` () =
            Parser.req "prop.prop2" (Parse.list Parse.string)
            |> Parser.parse """{ "prop": { "prop2": [ "1", 2, "3" ] } }"""
            |> Expect.errorString

        [<Fact>]
        let ``Should return Error when parsing a nested incorrect value in a nested array`` () =
            let parser = Parse.list Parse.string
            Parser.req "prop.prop2" (Parse.list parser)
            |> Parser.parse """{ "prop": { "prop2": [ [ "1" ], [ "1", 2, "3" ] ] } }"""
            |> Expect.errorString

        [<Fact>]
        let ``Should return Error when try parsing an incorrect value in an array`` () =
            Parser.opt "prop" (Parse.list Parse.string)
            |> Parser.parse """{ "prop": [ "1", 2, "3"] }"""
            |> Expect.errorString

        [<Fact>]
        let ``Should return Error when try parsing an incorrect value in a nested array`` () =
            let parser = Parse.list Parse.string
            Parser.opt "prop" (Parse.list parser)
            |> Parser.parse """{ "prop": [ [ "1" ], [ "1", 2, "3" ] ] }"""
            |> Expect.errorString

        [<Fact>]
        let ``Should return Error when try parsing a nested incorrect value in an array`` () =
            Parser.opt "prop.prop2" (Parse.list Parse.string)
            |> Parser.parse """{ "prop": { "prop2": [ "1", 2, "3" ] } }"""
            |> Expect.errorString

        [<Fact>]
        let ``Should return Error when try parsing a nested incorrect value in an nested array`` () =
            let parser = Parse.list Parse.string
            Parser.opt "prop.prop2" (Parse.list parser)
            |> Parser.parse """{ "prop": { "prop2": [ [ "1" ], [ "1", 2, "3" ] ] } }"""
            |> Expect.errorString

        [<Fact>]
        let ``Should return Error when parsing an object in an array that contains another kind`` () =
            let parser = Parser.req "prop2" Parse.string
            Parser.req "prop" (Parse.list parser)
            |> Parser.parse """{ "prop": [ { "prop2": "1" }, 2, 3 ] }"""
            |> Expect.errorString

        [<Fact>]
        let ``Should return Error when parsing a nested object in an array that contains another kind`` () =
            let parser = Parser.req "prop3" Parse.string
            Parser.req "prop.prop2" (Parse.list parser)
            |> Parser.parse """{ "prop": { "prop2": [ { "prop3": "1" }, 2, 3 ] } }"""
            |> Expect.errorString

        [<Fact>]
        let ``Should return Error when try parsing an object in an array that contains another kind`` () =
            let parser = Parser.opt "prop2" Parse.string
            Parser.opt "prop" (Parse.list parser)
            |> Parser.parse """{ "prop": [ { "prop2": "1" }, 2, 3 ] }"""
            |> Expect.errorString

        [<Fact>]
        let ``Should return Error when try parsing a nested object in an array that contains another kind`` () =
            let parser = Parser.req "prop3" Parse.string
            Parser.opt "prop.prop2" (Parse.list parser)
            |> Parser.parse """{ "prop": { "prop2": [ { "prop3": "1" }, 2, 3 ] } }"""
            |> Expect.errorString

        [<Fact>]
        let ``Should return Error when parsing an array that is an object`` () =
            Parser.req "prop" (Parse.array Parse.string)
            |> Parser.parse """{ "prop": { "prop2": 1 } }"""
            |> Expect.errorString

        [<Fact>]
        let ``Should return Error when parsing a nested array that is an object`` () =
            Parser.req "prop.prop2" (Parse.array Parse.string)
            |> Parser.parse """{ "prop": { "prop2": { "prop3": 1 } } }"""
            |> Expect.errorString

        [<Fact>]
        let ``Should return Error when try parsing an array that is an object`` () =
            Parser.opt "prop" (Parse.array Parse.string)
            |> Parser.parse """{ "prop": { "prop2": 1 } }"""
            |> Expect.errorString

        [<Fact>]
        let ``Should return Error when try parsing a nested array that is an object`` () =
            Parser.opt "prop.prop2" (Parse.array Parse.string)
            |> Parser.parse """{ "prop": { "prop2": { "prop3": 1 } } }"""
            |> Expect.errorString

    module Null =

        [<Fact>]
        let ``Should return Error when parsing a null value`` () =
            Parser.req "prop" Parse.int
            |> Parser.parse """{ "prop": null }"""
            |> Expect.errorString

        [<Fact>]
        let ``Should return Error when parsing a null value with composed parsers`` () =
            Parser.req "prop" (Parser.req "prop2" Parse.string)
            |> Parser.parse """{ "prop": { "prop2": null } }"""
            |> Expect.errorString

        [<Fact>]
        let ``Should return Error when try parsing a null value with composed parsers`` () =
            Parser.opt "prop" (Parser.req "prop2" Parse.string)
            |> Parser.parse """{ "prop": { "prop2": null } }"""
            |> Expect.errorString

        [<Fact>]
        let ``Should return Error when parsing a nested null value`` () =
            Parser.req "prop.prop2" Parse.int
            |> Parser.parse """{ "prop": { "prop2": null } }"""
            |> Expect.errorString

        [<Fact>]
        let ``Should return Error when parsing a nested null value with required parsers`` () =
            let parser = Parser.req "prop3" Parse.int
            Parser.req "prop.prop2" parser
            |> Parser.parse """{ "prop": { "prop2": { "prop3": null } } }"""
            |> Expect.errorString

        [<Fact>]
        let ``Should return Error when try parsing a nested null value with mixed parsers`` () =
            let parser = Parser.req "prop3" Parse.int
            Parser.opt "prop.prop2" parser
            |> Parser.parse """{ "prop": { "prop2": { "prop3": null } } }"""
            |> Expect.errorString

        [<Fact>]
        let ``Should return Error when an object is null`` () =
            Parser.req "prop.prop2" Parse.int
            |> Parser.parse """{ "prop": null }"""
            |> Expect.errorString

        [<Fact>]
        let ``Should return Error when an object is null with required parsers`` () =
            let parser = Parser.req "prop3" Parse.int
            Parser.req "prop.prop2" parser
            |> Parser.parse """{ "prop": null }"""
            |> Expect.errorString

        [<Fact>]
        let ``Should return Error when a nested object is null with required parsers`` () =
            let parser = Parser.req "prop3" Parse.int
            Parser.req "prop.prop2" parser
            |> Parser.parse """{ "prop": { "prop2": null } }"""
            |> Expect.errorString

        [<Fact>]
        let ``Should return Error when a nested object is null with mixed parsers`` () =
            let parser = Parser.opt "prop3" Parse.int
            Parser.req "prop.prop2" parser
            |> Parser.parse """{ "prop": { "prop2": null } }"""
            |> Expect.errorString

    module Missing =

        [<Fact>]
        let ``Should return Error when parsing a missing value`` () =
            Parser.req "missing" Parse.int
            |> Parser.parse """{ "prop": "1" }"""
            |> Expect.errorString

        [<Fact>]
        let ``Should return Error when parsing a missing value with composed parsers`` () =
            Parser.req "prop" (Parser.req "missing" Parse.string)
            |> Parser.parse """{ "prop": { "prop2": "1" } }"""
            |> Expect.errorString

        [<Fact>]
        let ``Should return Error when try parsing a missing value with composed parsers`` () =
            Parser.opt "prop" (Parser.req "missing" Parse.string)
            |> Parser.parse """{ "prop": { "prop2": "1" } }"""
            |> Expect.errorString

        [<Fact>]
        let ``Should return Error when parsing a nested missing value`` () =
            Parser.req "prop.missing" Parse.int
            |> Parser.parse """{ "prop": { "prop2": 1 } }"""
            |> Expect.errorString

        [<Fact>]
        let ``Should return Error when parsing a nested missing value with required parsers`` () =
            let parser = Parser.req "missing" Parse.int
            Parser.req "prop.prop2" parser
            |> Parser.parse """{ "prop": { "prop2": { "prop3": 1 } } }"""
            |> Expect.errorString

        [<Fact>]
        let ``Should return Error when try parsing a nested missing value with mixed parsers`` () =
            let parser = Parser.req "missing" Parse.int
            Parser.opt "prop.prop2" parser
            |> Parser.parse """{ "prop": { "prop2": { "prop3": 1 } } }"""
            |> Expect.errorString

        [<Fact>]
        let ``Should return Error when an object is missing`` () =
            Parser.req "missing.prop2" Parse.int
            |> Parser.parse """{ "prop": { "prop2": 1 } }"""
            |> Expect.errorString

        [<Fact>]
        let ``Should return Error when an object is missing with required parsers`` () =
            let parser = Parser.req "prop3" Parse.int
            Parser.req "missing.prop2" parser
            |> Parser.parse """{ "prop": { "prop2": { "prop3": 1 } } }"""
            |> Expect.errorString

        [<Fact>]
        let ``Should return Error when a nested object is missing with required parsers`` () =
            let parser = Parser.req "prop3" Parse.int
            Parser.req "prop.missing" parser
            |> Parser.parse """{ "prop": { "prop2": { "prop3": 1 } } }"""
            |> Expect.errorString

        [<Fact>]
        let ``Should return Error when a nested object is missing with mixed parsers`` () =
            let parser = Parser.opt "prop3" Parse.int
            Parser.req "prop.missing" parser
            |> Parser.parse """{ "prop": { "prop2": { "prop3": 1 } } }"""
            |> Expect.errorString

    module Parse =

        [<Fact>]
        let ``Should return Error when custom parser does not match expected kind`` () =
            let parser : Parser<int> =
                Parse.custom (fun _ ->
                    Ok 1
                ) ExpectedKind.Number
            Parser.req "prop" parser
            |> Parser.parse """{ "prop": "1" }"""
            |> Expect.errorString

        [<Fact>]
        let ``Should return Error when custom parser throws an exception`` () =
            let parser : Parser<int> =
                Parse.custom (fun _ ->
                    failwith "Failed."
                ) ExpectedKind.Number
            Parser.req "prop" parser
            |> Parser.parse """{ "prop": 1 }"""
            |> Expect.errorString

        [<Fact>]
        let ``Should return Error with details when custom parser fails`` () =
            let parser : Parser<int> =
                Parse.custom (fun _ ->
                    Error <| Some "Failed."
                ) ExpectedKind.Number
            Parser.req "prop" parser
            |> Parser.parse """{ "prop": 1 }"""
            |> Expect.errorString

        [<Fact>]
        let ``Should return Error without details when custom parser fails`` () =
            let parser : Parser<int> =
                Parse.custom (fun _ ->
                    Error <| None
                ) ExpectedKind.Number
            Parser.req "prop" parser
            |> Parser.parse """{ "prop": 1 }"""
            |> Expect.errorString

        [<Fact>]
        let ``Should return Error when value is not valid`` () =
            let fromInt (_:int) : Result<string, _> = Error "Not valid."
            Parser.req "prop" (Parse.valid Parse.int fromInt)
            |> Parser.parse """{ "prop": 1 }"""
            |> Expect.errorString

        [<Fact>]
        let ``Should return Error when parsing Map with duplicate keys`` () =
            Parser.req "prop" (Parse.map Parse.int)
            |> Parser.parse """{ "prop": { "key2": 1, "key2": 2, "key3": 3 } }"""
            |> Expect.errorString

        [<Fact>]
        let ``Should return Error when parsing Dictionary fails`` () =
            Parser.req "prop" (Parse.dict Parse.int)
            |> Parser.parse """{ "prop": { "key1": 1, "key2": "2", "key3": 3 } }"""
            |> Expect.errorString

        [<Fact>]
        let ``Should return Error when parsing Dictionary with duplicate keys`` () =
            Parser.req "prop" (Parse.dict Parse.int)
            |> Parser.parse """{ "prop": { "key2": 1, "key2": 2, "key3": 3 } }"""
            |> Expect.errorString

        [<Fact>]
        let ``Should return Error when parsing KeyValuePair seq with duplicate keys`` () =
            Parser.req "prop" (Parse.keyValuePairs Parse.int)
            |> Parser.parse """{ "prop": { "key2": 1, "key2": 2, "key3": 3 } }"""
            |> Expect.errorString

        [<Fact>]
        let ``Should return Error when parsing tuple seq with duplicate keys`` () =
            Parser.req "prop" (Parse.tuples Parse.int)
            |> Parser.parse """{ "prop": { "key2": 1, "key2": 2, "key3": 3 } }"""
            |> Expect.errorString

        [<Fact>]
        let ``Should return Error when trying to parse string seq with duplicate keys`` () =
            Parser.req "prop" Parse.keys
            |> Parser.parse """{ "prop": { "key2": 1, "key2": 2, "key3": 3 } }"""
            |> Expect.errorString

        [<Fact>]
        let ``Should return Error when tuple parsing fails`` () =
            Parser.req "prop" (Parse.tuple2 Parse.guid Parse.int)
            |> Parser.parse """{ "prop": [ "1", 1 ] }"""
            |> Expect.errorString

        [<Fact>]
        let ``Should return Error when parsing tuple with incorrect length`` () =
            Parser.req "prop" (Parse.tuple2 Parse.string Parse.int)
            |> Parser.parse """{ "prop": [ "1", 1, 1 ] }"""
            |> Expect.errorString

        [<Fact>]
        let ``Should return Error when index parsing fails`` () =
            Parser.req "prop" (Parse.index 1 Parse.int)
            |> Parser.parse """{ "prop": [ 1, "2", 3 ] }"""
            |> Expect.errorString

        [<Fact>]
        let ``Should return Error when index is out of range`` () =
            Parser.req "prop" (Parse.index 3 Parse.int)
            |> Parser.parse """{ "prop": [ 1, 2, 3 ] }"""
            |> Expect.errorString

        [<Fact>]
        let ``Should return Error when parsing a TimeOnly exact with an incorrect format`` () =
            Parser.req "prop" (Parse.timeOnlyExact "HHmmss")
            |> Parser.parse """{ "prop": "17:28:45" }"""
            |> Expect.errorString

        [<Fact>]
        let ``Should return Error when parsing a TimeSpan exact with an incorrect format`` () =
            Parser.req "prop" (Parse.timeSpanExact @"hh\mm\ss")
            |> Parser.parse """{ "prop": "01:28:45" }"""
            |> Expect.errorString

        [<Fact>]
        let ``Should return Error when parsing a DateOnly exact with an incorrect format`` () =
            Parser.req "prop" (Parse.dateOnlyExact "yyyyMMdd")
            |> Parser.parse """{ "prop": "2025-05-13" }"""
            |> Expect.errorString

        [<Fact>]
        let ``Should return Error when parsing a DateTime exact with an incorrect format`` () =
            Parser.req "prop" (Parse.dateTimeExact "yyyy-MM-dd HH:mm:ss")
            |> Parser.parse """{ "prop": "2025-05-13T17:28:45" }"""
            |> Expect.errorString

        [<Fact>]
        let ``Should return Error when parsing a DateTimeOffset exact with an incorrect format`` () =
            Parser.req "prop" (Parse.dateTimeOffsetExact "yyyyMMdd HH:mm:ss zzz")
            |> Parser.parse """{ "prop": "2025-05-13 17:28 +02:00" }"""
            |> Expect.errorString