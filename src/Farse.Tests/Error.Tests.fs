namespace Farse.Tests

open System
open Xunit
open Farse

module Error =

    [<Fact>]
    let ``Should return correct error message with parsing invalid json test`` () =
        Parse.int
        |> Parser.parse "invalid"
        |> Expect.error
        |> Verify.string

    [<Fact>]
    let ``Should return correct error message with string is null`` () =
        Parse.int
        |> Parser.parse null
        |> Expect.error
        |> Verify.string

    [<Fact>]
    let ``Should return correct error message when string is empty`` () =
        Parse.int
        |> Parser.parse String.Empty
        |> Expect.error
        |> Verify.string

    [<Fact>]
    let ``Should return correct error message when parsing value`` () =
        Parse.req "prop" Parse.int
        |> Parser.parse """{ "prop": "1" }"""
        |> Expect.error
        |> Verify.string

    [<Fact>]
    let ``Should return correct error message when parsing required value`` () =
        Parse.req "prop" Parse.int
        |> Parser.parse """{ "prop": "1" }"""
        |> Expect.error
        |> Verify.string

    [<Fact>]
    let ``Should return correct error message when parsing optional value`` () =
        Parse.opt "prop" Parse.int
        |> Parser.parse """{ "prop": "1" }"""
        |> Expect.error
        |> Verify.string

    [<Fact>]
    let ``Should return correct error message when try parsing value`` () =
        Parse.opt "prop" Parse.int
        |> Parser.parse """{ "prop": "1" }"""
        |> Expect.error
        |> Verify.string

    [<Fact>]
    let ``Should return correct error message when parsing value from object`` () =
        Parse.req "prop" Parse.int
        |> Parser.parse """{ "prop": { "prop2": null } }"""
        |> Expect.error
        |> Verify.string

    [<Fact>]
    let ``Should return correct error message when parsing value from array`` () =
        Parse.req "prop" Parse.int
        |> Parser.parse """{ "prop": [] }"""
        |> Expect.error
        |> Verify.string

    [<Fact>]
    let ``Should return correct error message when parsing datetime exact`` () =
        Parse.req "prop" (Parse.dateTimeExact "yyyy-MM-dd HH:mm:ss")
        |> Parser.parse """{ "prop": "2025-05-13T17:28:45" }"""
        |> Expect.error
        |> Verify.string

    [<Fact>]
    let ``Should return correct error message when parsing datetime exact when value is not string`` () =
        Parse.req "prop" (Parse.dateTimeExact "yyyy-MM-dd HH:mm:ss")
        |> Parser.parse """{ "prop": 100 }"""
        |> Expect.error
        |> Verify.string

    [<Fact>]
    let ``Should return correct error message when property is null`` () =
        Parse.req "prop" Parse.int
        |> Parser.parse """{ "prop": null }"""
        |> Expect.error
        |> Verify.string

    [<Fact>]
    let ``Should return correct error message when property is missing`` () =
        Parse.req "missing" Parse.int
        |> Parser.parse """{ "prop": "1" }"""
        |> Expect.error
        |> Verify.string

    [<Fact>]
    let ``Should return correct error message when parsing value in array`` () =
        Parse.req "prop" (Parse.list Parse.string)
        |> Parser.parse """{ "prop": [ 1, 2, 3 ] }"""
        |> Expect.error
        |> Verify.string

    [<Fact>]
    let ``Should return correct error message when parsing object in array`` () =
        let parser = Parse.req "prop2" Parse.string
        Parse.req "prop" (Parse.list parser)
        |> Parser.parse """{ "prop": [ 1, 2, 3 ] }"""
        |> Expect.error
        |> Verify.string

    [<Fact>]
    let ``Should return correct error message when parsing object`` () =
        Parse.req "prop.prop2" Parse.string
        |> Parser.parse """{ "prop": [ 1, 2, 3 ] }"""
        |> Expect.error
        |> Verify.string

    [<Fact>]
    let ``Should return correct error message when parsing array`` () =
        Parse.req "prop" (Parse.array Parse.string)
        |> Parser.parse """{ "prop": { "prop2": 1 } }"""
        |> Expect.error
        |> Verify.string

    [<Fact>]
    let ``Should return correct error message when parsing array as list`` () =
        Parse.req "prop" (Parse.list Parse.string)
        |> Parser.parse """{ "prop": { "prop2": 1 } }"""
        |> Expect.error
        |> Verify.string

    [<Fact>]
    let ``Should return correct error message when parsing nested value`` () =
        let json =
            """
                {
                    "prop": {
                        "prop2": {
                            "prop3": "1"
                        }
                    }
                }
            """

        Parse.req "prop.prop2.prop3" Parse.int
        |> Parser.parse json
        |> Expect.error
        |> Verify.string

    [<Fact>]
    let ``Should return correct error message when try parsing nested value`` () =
        let json =
            """
                {
                    "prop": {
                        "prop2": {
                            "prop3": "1"
                        }
                    }
                }
            """

        Parse.opt "prop.prop2.prop3" Parse.int
        |> Parser.parse json
        |> Expect.error
        |> Verify.string

    [<Fact>]
    let ``Should return correct error message when parsing nested value in array`` () =
        let json =
            """
                {
                    "prop": {
                        "prop2": {
                            "prop3": [ "1" ]
                        }
                    }
                }
            """

        Parse.req "prop.prop2.prop3" (Parse.list Parse.int)
        |> Parser.parse json
        |> Expect.error
        |> Verify.string

    [<Fact>]
    let ``Should return correct error message when try parsing nested value in array`` () =
        let json =
            """
                {
                    "prop": {
                        "prop2": {
                            "prop3": [ "1" ]
                        }
                    }
                }
            """

        Parse.opt "prop.prop2.prop3" (Parse.list Parse.int)
        |> Parser.parse json
        |> Expect.error
        |> Verify.string

    [<Fact>]
    let ``Should return correct error message when parsing nested array as object`` () =
        let json =
            """
                {
                    "prop": {
                        "prop2": []
                    }
                }
            """

        Parse.req "prop.prop2.prop3" Parse.int
        |> Parser.parse json
        |> Expect.error
        |> Verify.string

    [<Fact>]
    let ``Should return correct error message when try parsing nested array as object`` () =
        let json =
            """
                {
                    "prop": {
                        "prop2": []
                    }
                }
            """

        Parse.opt "prop.prop2.prop3" Parse.int
        |> Parser.parse json
        |> Expect.error
        |> Verify.string

    [<Fact>]
    let ``Should return correct error message when parsing array as object`` () =
        Parse.req "prop" Parse.int
        |> Parser.parse "[]"
        |> Expect.error
        |> Verify.string

    [<Fact>]
    let ``Should return correct error message when try parsing array as object`` () =
        Parse.opt "prop" Parse.int
        |> Parser.parse "[]"
        |> Expect.error
        |> Verify.string

    [<Fact>]
    let ``Should return correct error message when parsing nested null property`` () =
        let json =
            """
                {
                    "prop": {
                        "prop2": null
                    }
                }
            """

        Parse.req "prop.prop2.prop3" Parse.int
        |> Parser.parse json
        |> Expect.error
        |> Verify.string

    [<Fact>]
    let ``Should return correct error message when parsing missing property`` () =
        Parse.req "missing" Parse.int
        |> Parser.parse """{ "prop": null }"""
        |> Expect.error
        |> Verify.string

    [<Fact>]
    let ``Should return correct error message when parsing nested missing property`` () =
        let json =
            """
                {
                    "prop": {
                        "prop2": {
                            "prop3": "100"
                        }
                    }
                }
            """

        Parse.req "prop.prop2.missing" Parse.int
        |> Parser.parse json
        |> Expect.error
        |> Verify.string

    [<Fact>]
    let ``Should return correct error message for nested parsers`` () =
        let json =
            """
                {
                    "prop": {
                        "prop2": {
                            "prop3": null
                        }
                    }
                }
            """

        Parse.req "prop.prop2.prop3" Parse.int
        |> Parser.parse json
        |> Expect.error
        |> Verify.string