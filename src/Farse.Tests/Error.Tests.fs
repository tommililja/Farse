namespace Farse.Tests

open System
open Xunit
open Farse

module ErrorTests =

    module Parser =

        [<Fact>]
        let ``Should return error when parsing invalid JSON`` () =
            Parse.none
            |> Parser.parse "invalid"
            |> Expect.errorString

        [<Fact>]
        let ``Should return error when parsing a null string`` () =
            Parse.none
            |> Parser.parse null
            |> Expect.errorString

        [<Fact>]
        let ``Should return error when parsing an empty string`` () =
            Parse.none
            |> Parser.parse String.Empty
            |> Expect.errorString

    module Parsing =

        [<Fact>]
        let ``Should return error when parsing an incorrect value`` () =
            Parse.req "prop" Parse.int
            |> Parser.parse """{ "prop": "1" }"""
            |> Expect.errorString

        [<Fact>]
        let ``Should return error when parsing a nested incorrect value`` () =
            Parse.req "prop.prop2" Parse.int
            |> Parser.parse """{ "prop": { "prop2": "1" } }"""
            |> Expect.errorString

        [<Fact>]
        let ``Should return error when try parsing an incorrect value`` () =
            Parse.opt "prop" Parse.int
            |> Parser.parse """{ "prop": "1" }"""
            |> Expect.errorString

        [<Fact>]
        let ``Should return error when try parsing a nested incorrect value`` () =
            Parse.opt "prop.prop2" Parse.int
            |> Parser.parse """{ "prop": { "prop2": "1" } }"""
            |> Expect.errorString

        [<Fact>]
        let ``Should return error when parsing a property from an element that is not an object`` () =
            Parse.req "prop" Parse.int
            |> Parser.parse """[]"""
            |> Expect.errorString

        [<Fact>]
        let ``Should return error when parsing a nested property from an element that is not an object`` () =
            Parse.req "prop.prop2" Parse.int
            |> Parser.parse """{ "prop": [] }"""
            |> Expect.errorString

        [<Fact>]
        let ``Should return error when try parsing a property from an element that is not an object`` () =
            Parse.opt "prop" Parse.int
            |> Parser.parse """[]"""
            |> Expect.errorString

        [<Fact>]
        let ``Should return error when try parsing a nested property from an element that is not an object`` () =
            Parse.opt "prop.prop2" Parse.int
            |> Parser.parse """{ "prop": [] }"""
            |> Expect.errorString

        [<Fact>]
        let ``Should return error when parsing a value when the value is an object`` () =
            Parse.req "prop" Parse.int
            |> Parser.parse """{ "prop": { "prop2": null } }"""
            |> Expect.errorString

        [<Fact>]
        let ``Should return error when parsing a nested value when the value is an object`` () =
            Parse.req "prop.prop2" Parse.int
            |> Parser.parse """{ "prop": { "prop2": { "prop3": 1 } } }"""
            |> Expect.errorString

        [<Fact>]
        let ``Should return error when try parsing a value when the value is an object`` () =
            Parse.opt "prop" Parse.int
            |> Parser.parse """{ "prop": { "prop2": null } }"""
            |> Expect.errorString

        [<Fact>]
        let ``Should return error when try parsing a nested value when the value is an object`` () =
            Parse.opt "prop.prop2" Parse.int
            |> Parser.parse """{ "prop": { "prop2": { "prop3": 1 } } }"""
            |> Expect.errorString

        [<Fact>]
        let ``Should return error when parsing a value when the value is an array`` () =
            Parse.req "prop" Parse.int
            |> Parser.parse """{ "prop": [] }"""
            |> Expect.errorString

        [<Fact>]
        let ``Should return error when parsing a nested value when the value is an array`` () =
            Parse.req "prop.prop2" Parse.int
            |> Parser.parse """{ "prop": { "prop2": [] } }"""
            |> Expect.errorString

        [<Fact>]
        let ``Should return error when try parsing a value when the value is an array`` () =
            Parse.opt "prop" Parse.int
            |> Parser.parse """{ "prop": [] }"""
            |> Expect.errorString

        [<Fact>]
        let ``Should return error when try parsing a nested value when the value is an array`` () =
            Parse.opt "prop.prop2" Parse.int
            |> Parser.parse """{ "prop": { "prop2": [] } }"""
            |> Expect.errorString

        [<Fact>]
        let ``Should return error when parsing an incorrect value in an array`` () =
            Parse.req "prop" (Parse.list Parse.string)
            |> Parser.parse """{ "prop": [ 1, 2, 3 ] }"""
            |> Expect.errorString

        [<Fact>]
        let ``Should return error when parsing a nested incorrect value in an array`` () =
            Parse.req "prop.prop2" (Parse.list Parse.string)
            |> Parser.parse """{ "prop": { "prop2": [ 1, 2, 3 ] } }"""
            |> Expect.errorString

        [<Fact>]
        let ``Should return error when try parsing an incorrect value in an array`` () =
            Parse.opt "prop" (Parse.list Parse.string)
            |> Parser.parse """{ "prop": [ 1, 2, 3 ] }"""
            |> Expect.errorString

        [<Fact>]
        let ``Should return error when try parsing a nested incorrect value in an array`` () =
            Parse.opt "prop.prop2" (Parse.list Parse.string)
            |> Parser.parse """{ "prop": { "prop2": [ 1, 2, 3 ] } }"""
            |> Expect.errorString

        [<Fact>]
        let ``Should return error when parsing an object in an array that contains another kind`` () =
            let parser = Parse.req "prop2" Parse.string
            Parse.req "prop" (Parse.list parser)
            |> Parser.parse """{ "prop": [ { "prop2": "1" }, 2, 3 ] }"""
            |> Expect.errorString

        [<Fact>]
        let ``Should return error when parsing a nested object in an array that contains another kind`` () =
            let parser = Parse.req "prop3" Parse.string
            Parse.req "prop.prop2" (Parse.list parser)
            |> Parser.parse """{ "prop": { "prop2": [ { "prop3": "1" }, 2, 3 ] } }"""
            |> Expect.errorString

        [<Fact>]
        let ``Should return error when try parsing an object in an array that contains another kind`` () =
            let parser = Parse.opt "prop2" Parse.string
            Parse.opt "prop" (Parse.list parser)
            |> Parser.parse """{ "prop": [ { "prop2": "1" }, 2, 3 ] }"""
            |> Expect.errorString

        [<Fact>]
        let ``Should return error when try parsing a nested object in an array that contains another kind`` () =
            let parser = Parse.req "prop3" Parse.string
            Parse.opt "prop.prop2" (Parse.list parser)
            |> Parser.parse """{ "prop": { "prop2": [ { "prop3": "1" }, 2, 3 ] } }"""
            |> Expect.errorString

        [<Fact>]
        let ``Should return error when parsing an array that is an object`` () =
            Parse.req "prop" (Parse.array Parse.string)
            |> Parser.parse """{ "prop": { "prop2": 1 } }"""
            |> Expect.errorString

        [<Fact>]
        let ``Should return error when parsing a nested array that is an object`` () =
            Parse.req "prop.prop2" (Parse.array Parse.string)
            |> Parser.parse """{ "prop": { "prop2": { "prop3": 1 } } }"""
            |> Expect.errorString

        [<Fact>]
        let ``Should return error when try parsing an array that is an object`` () =
            Parse.opt "prop" (Parse.array Parse.string)
            |> Parser.parse """{ "prop": { "prop2": 1 } }"""
            |> Expect.errorString

        [<Fact>]
        let ``Should return error when try parsing a nested array that is an object`` () =
            Parse.opt "prop.prop2" (Parse.array Parse.string)
            |> Parser.parse """{ "prop": { "prop2": { "prop3": 1 } } }"""
            |> Expect.errorString

    module Null =

        [<Fact>]
        let ``Should return error when parsing a null value`` () =
            Parse.req "prop" Parse.int
            |> Parser.parse """{ "prop": null }"""
            |> Expect.errorString

        [<Fact>]
        let ``Should return error when parsing a null value with composed parsers`` () =
            Parse.req "prop" (Parse.req "prop2" Parse.string)
            |> Parser.parse """{ "prop": { "prop2": null } }"""
            |> Expect.errorString

        [<Fact>]
        let ``Should return error when try parsing a null value with composed parsers`` () =
            Parse.opt "prop" (Parse.req "prop2" Parse.string)
            |> Parser.parse """{ "prop": { "prop2": null } }"""
            |> Expect.errorString

        [<Fact>]
        let ``Should return error when parsing a nested null value`` () =
            Parse.req "prop.prop2" Parse.int
            |> Parser.parse """{ "prop": { "prop2": null } }"""
            |> Expect.errorString

        [<Fact>]
        let ``Should return error when parsing a nested null value with required parsers`` () =
            let parser = Parse.req "prop3" Parse.int
            Parse.req "prop.prop2" parser
            |> Parser.parse """{ "prop": { "prop2": { "prop3": null } } }"""
            |> Expect.errorString

        [<Fact>]
        let ``Should return error when try parsing a nested null value with mixed parsers`` () =
            let parser = Parse.req "prop3" Parse.int
            Parse.opt "prop.prop2" parser
            |> Parser.parse """{ "prop": { "prop2": { "prop3": null } } }"""
            |> Expect.errorString

        [<Fact>]
        let ``Should return error when an object is null`` () =
            Parse.req "prop.prop2" Parse.int
            |> Parser.parse """{ "prop": null }"""
            |> Expect.errorString

        [<Fact>]
        let ``Should return error when an object is null with required parsers`` () =
            let parser = Parse.req "prop3" Parse.int
            Parse.req "prop.prop2" parser
            |> Parser.parse """{ "prop": null }"""
            |> Expect.errorString

        [<Fact>]
        let ``Should return error when a nested object is null with required parsers`` () =
            let parser = Parse.req "prop3" Parse.int
            Parse.req "prop.prop2" parser
            |> Parser.parse """{ "prop": { "prop2": null } }"""
            |> Expect.errorString

        [<Fact>]
        let ``Should return error when a nested object is null with mixed parsers`` () =
            let parser = Parse.opt "prop3" Parse.int
            Parse.req "prop.prop2" parser
            |> Parser.parse """{ "prop": { "prop2": null } }"""
            |> Expect.errorString

    module Missing =

        [<Fact>]
        let ``Should return error when parsing a missing value`` () =
            Parse.req "missing" Parse.int
            |> Parser.parse """{ "prop": "1" }"""
            |> Expect.errorString

        [<Fact>]
        let ``Should return error when parsing a missing value with composed parsers`` () =
            Parse.req "prop" (Parse.req "missing" Parse.string)
            |> Parser.parse """{ "prop": { "prop2": "1" } }"""
            |> Expect.errorString

        [<Fact>]
        let ``Should return error when try parsing a missing value with composed parsers`` () =
            Parse.opt "prop" (Parse.req "missing" Parse.string)
            |> Parser.parse """{ "prop": { "prop2": "1" } }"""
            |> Expect.errorString

        [<Fact>]
        let ``Should return error when parsing a nested missing value`` () =
            Parse.req "prop.missing" Parse.int
            |> Parser.parse """{ "prop": { "prop2": 1 } }"""
            |> Expect.errorString

        [<Fact>]
        let ``Should return error when parsing a nested missing value with required parsers`` () =
            let parser = Parse.req "missing" Parse.int
            Parse.req "prop.prop2" parser
            |> Parser.parse """{ "prop": { "prop2": { "prop3": 1 } } }"""
            |> Expect.errorString

        [<Fact>]
        let ``Should return error when try parsing a nested missing value with mixed parsers`` () =
            let parser = Parse.req "missing" Parse.int
            Parse.opt "prop.prop2" parser
            |> Parser.parse """{ "prop": { "prop2": { "prop3": 1 } } }"""
            |> Expect.errorString

        [<Fact>]
        let ``Should return error when an object is missing`` () =
            Parse.req "missing.prop2" Parse.int
            |> Parser.parse """{ "prop": { "prop2": 1 } }"""
            |> Expect.errorString

        [<Fact>]
        let ``Should return error when an object is missing with required parsers`` () =
            let parser = Parse.req "prop3" Parse.int
            Parse.req "missing.prop2" parser
            |> Parser.parse """{ "prop": { "prop2": { "prop3": 1 } } }"""
            |> Expect.errorString

        [<Fact>]
        let ``Should return error when a nested object is missing with required parsers`` () =
            let parser = Parse.req "prop3" Parse.int
            Parse.req "prop.missing" parser
            |> Parser.parse """{ "prop": { "prop2": { "prop3": 1 } } }"""
            |> Expect.errorString

        [<Fact>]
        let ``Should return error when a nested object is missing with mixed parsers`` () =
            let parser = Parse.opt "prop3" Parse.int
            Parse.req "prop.missing" parser
            |> Parser.parse """{ "prop": { "prop2": { "prop3": 1 } } }"""
            |> Expect.errorString

    module Parse =

        // TODO: Add tests for other Parse.* functions.

        [<Fact>]
        let ``Should return error when parsing a datetime exact with an incorrect format`` () =
            Parse.req "prop" (Parse.dateTimeExact "yyyy-MM-dd HH:mm:ss")
            |> Parser.parse """{ "prop": "2025-05-13T17:28:45" }"""
            |> Expect.errorString

        [<Fact>]
        let ``Should return error when parsing a datetime exact when the value is not a string`` () =
            Parse.req "prop" (Parse.dateTimeExact "yyyy-MM-dd HH:mm:ss")
            |> Parser.parse """{ "prop": 1 }"""
            |> Expect.errorString