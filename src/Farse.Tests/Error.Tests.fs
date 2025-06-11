namespace Farse.Tests

open System
open Xunit
open Farse

module Error =

    [<Fact>]
    let ``Should return correct error message with parsing invalid json`` () =
        Parse.int
        |> Parser.parse "invalid" []
        |> Expect.error
        |> Verify.string

    [<Fact>]
    let ``Should return correct error message with string is null`` () =
        Parse.int
        |> Parser.parse null []
        |> Expect.error
        |> Verify.string

    [<Fact>]
    let ``Should return correct error message when string is empty`` () =
        Parse.int
        |> Parser.parse String.Empty []
        |> Expect.error
        |> Verify.string

    [<Fact>]
    let ``Should return correct error message when parsing value`` () =
        Parse.int
        |> Parser.parse """{ "prop": "1" }""" [ "prop" ]
        |> Expect.error
        |> Verify.string

    [<Fact>]
    let ``Should return correct error message when parsing required value`` () =
        Parse.req "prop" Parse.int
        |> Parser.parse """{ "prop": "1" }""" []
        |> Expect.error
        |> Verify.string

    [<Fact>]
    let ``Should return correct error message when parsing optional value`` () =
        Parse.opt "prop" Parse.int
        |> Parser.parse """{ "prop": "1" }""" []
        |> Expect.error
        |> Verify.string

    [<Fact>]
    let ``Should return correct error message when try parsing value`` () =
        Parse.int
        |> Parser.tryParse """{ "prop": "1" }""" [ "prop" ]
        |> Expect.error
        |> Verify.string

    [<Fact>]
    let ``Should return correct error message when parsing value from object`` () =
        Parse.int
        |> Parser.parse """{ "prop": { "prop2": null } }""" [ "prop" ]
        |> Expect.error
        |> Verify.string

    [<Fact>]
    let ``Should return correct error message when parsing value from array`` () =
        Parse.int
        |> Parser.parse """{ "prop": [] }""" [ "prop" ]
        |> Expect.error
        |> Verify.string

    [<Fact>]
    let ``Should return correct error message when parsing datetime exact`` () =
        Parse.dateTimeExact "yyyy-MM-dd HH:mm:ss"
        |> Parser.parse """{ "prop": "2025-05-13T17:28:45" }""" [ "prop" ]
        |> Expect.error
        |> Verify.string

    [<Fact>]
    let ``Should return correct error message when parsing datetime exact when value is not string`` () =
        Parse.dateTimeExact "yyyy-MM-dd HH:mm:ss"
        |> Parser.parse """{ "prop": 100 }""" [ "prop" ]
        |> Expect.error
        |> Verify.string

    [<Fact>]
    let ``Should return correct error message when property is null`` () =
        Parse.int
        |> Parser.parse """{ "prop": null }""" [ "prop" ]
        |> Expect.error
        |> Verify.string

    [<Fact>]
    let ``Should return correct error message when property is missing`` () =
        Parse.int
        |> Parser.parse """{ "prop": "1" }""" [ "missing" ]
        |> Expect.error
        |> Verify.string

    [<Fact>]
    let ``Should return correct error message when parsing value in array`` () =
        Parse.list Parse.string
        |> Parser.parse """{ "prop": [ 1, 2, 3 ] }""" [ "prop" ]
        |> Expect.error
        |> Verify.string

    [<Fact>]
    let ``Should return correct error message when parsing object in array`` () =
        let parser = Parse.req "prop2" Parse.string
        Parse.list parser
        |> Parser.parse """{ "prop": [ 1, 2, 3 ] }""" [ "prop" ]
        |> Expect.error
        |> Verify.string

    [<Fact>]
    let ``Should return correct error message when parsing object`` () =
        Parse.req "prop2" Parse.string
        |> Parser.parse """{ "prop": [ 1, 2, 3 ] }""" [ "prop" ]
        |> Expect.error
        |> Verify.string

    [<Fact>]
    let ``Should return correct error message when parsing array`` () =
        Parse.array Parse.string
        |> Parser.parse """{ "prop": { "prop2": 1 } }""" [ "prop" ]
        |> Expect.error
        |> Verify.string

    [<Fact>]
    let ``Should return correct error message when parsing array as list`` () =
        Parse.list Parse.string
        |> Parser.parse """{ "prop": { "prop2": 1 } }""" [ "prop" ]
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

        Parse.int
        |> Parser.parse json [ "prop"; "prop2"; "prop3" ]
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

        Parse.int
        |> Parser.tryParse json [ "prop"; "prop2"; "prop3" ]
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

        Parse.list Parse.int
        |> Parser.parse json [ "prop"; "prop2"; "prop3" ]
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

        Parse.list Parse.int
        |> Parser.tryParse json [ "prop"; "prop2"; "prop3" ]
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

        Parse.int
        |> Parser.parse json [ "prop"; "prop2"; "prop3" ]
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

        Parse.int
        |> Parser.tryParse json [ "prop"; "prop2"; "prop3" ]
        |> Expect.error
        |> Verify.string

    [<Fact>]
    let ``Should return correct error message when parsing array as object`` () =
        Parse.int
        |> Parser.parse "[]" [ "prop" ]
        |> Expect.error
        |> Verify.string

    [<Fact>]
    let ``Should return correct error message when try parsing array as object`` () =
        Parse.int
        |> Parser.tryParse "[]" [ "prop" ]
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

        Parse.int
        |> Parser.parse json [ "prop"; "prop2"; "prop3" ]
        |> Expect.error
        |> Verify.string

    [<Fact>]
    let ``Should return correct error message when parsing missing property`` () =
        Parse.int
        |> Parser.parse """{ "prop": null }""" [ "missing" ]
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

        Parse.int
        |> Parser.parse json [ "prop"; "prop2"; "missing" ]
        |> Expect.error
        |> Verify.string