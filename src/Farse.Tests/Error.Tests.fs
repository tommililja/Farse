namespace Farse.Tests

open System
open Xunit
open Farse

module Error =

    [<Fact>]
    let ``Should return correct error message with parsing invalid JSON`` () =
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
        |> Parser.parse """{ "prop": [ 1, 2, 3] }"""
        |> Expect.error
        |> Verify.string

    [<Fact>]
    let ``Should return correct error message when parsing object in array`` () =
        let parser = Parse.req "prop2" Parse.string
        Parse.req "prop" (Parse.list parser)
        |> Parser.parse """{ "prop": [ 1, 2, 3] }"""
        |> Expect.error
        |> Verify.string

    [<Fact>]
    let ``Should return correct error message when parsing object`` () =
        let parser = Parse.req "prop2" Parse.string
        Parse.req "prop" parser
        |> Parser.parse """{ "prop": [ 1, 2, 3] }"""
        |> Expect.error
        |> Verify.string

    [<Fact>]
    let ``Should return correct error message when parsing array`` () =
        Parse.req "prop" (Parse.array Parse.string)
        |> Parser.parse """{ "prop": { "prop2": 1 }}"""
        |> Expect.error
        |> Verify.string

    [<Fact>]
    let ``Should return correct error message when parsing array as list`` () =
        Parse.req "prop" (Parse.list Parse.string)
        |> Parser.parse """{ "prop": { "prop2": 1 }}"""
        |> Expect.error
        |> Verify.string

    [<Fact>]
    let ``Should return correct error message when parsing property from non-object`` () =
        Parse.req "prop" (Parse.list Parse.string)
        |> Parser.parse "[]"
        |> Expect.error
        |> Verify.string

    module TraverseTests =

        [<Fact>]
        let ``Should return correct error message when value is incorrect`` () =
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
            |> Parser.traverse [ "prop"; "prop2"; "prop3" ]
            |> Parser.parse json
            |> Expect.error
            |> Verify.string

        [<Fact>]
        let ``Should return correct error message when value is incorrect in array`` () =
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
            |> Parser.traverse [ "prop"; "prop2"; "prop3" ]
            |> Parser.parse json
            |> Expect.error
            |> Verify.string

        [<Fact>]
        let ``Should return correct error message when prop is missing`` () =
            let json =
                """
                    {
                        "missing": null
                    }
                """

            Parse.int
            |> Parser.traverse [ "prop"; "prop2"; "prop3" ]
            |> Parser.parse json
            |> Expect.error
            |> Verify.string

        [<Fact>]
        let ``Should return correct error message when prop is array`` () =
            let json =
                """
                    {
                        "prop": {
                            "prop2": []
                        }
                    }
                """

            Parse.int
            |> Parser.traverse [ "prop"; "prop2"; "prop3" ]
            |> Parser.parse json
            |> Expect.error
            |> Verify.string

        [<Fact>]
        let ``Should return correct error message when object is array`` () =
            let json = "[]"

            Parse.int
            |> Parser.traverse [ "prop"; "prop2"; "prop3" ]
            |> Parser.parse json
            |> Expect.error
            |> Verify.string

        [<Fact>]
        let ``Should return correct error message when nested prop is missing`` () =
            let json =
                """
                    {
                        "prop": {
                            "prop2": {
                                "missing": "100"
                            }
                        }
                    }
                """

            Parse.int
            |> Parser.traverse [ "prop"; "prop2"; "prop3" ]
            |> Parser.parse json
            |> Expect.error
            |> Verify.string

        [<Fact>]
        let ``Should return correct error message when prop is null`` () =
            let json =
                """
                    {
                        "prop": {
                            "prop2": null
                        }
                    }
                """

            Parse.int
            |> Parser.traverse [ "prop"; "prop2"; "prop3" ]
            |> Parser.parse json
            |> Expect.error
            |> Verify.string

        module TryTraverse =

            [<Fact>]
            let ``Should return correct error message when value is incorrect`` () =
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
                |> Parser.tryTraverse [ "prop"; "prop2"; "prop3" ]
                |> Parser.parse json
                |> Expect.error
                |> Verify.string

            [<Fact>]
            let ``Should return correct error message when value is incorrect in array`` () =
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
                |> Parser.tryTraverse [ "prop"; "prop2"; "prop3" ]
                |> Parser.parse json
                |> Expect.error
                |> Verify.string

            [<Fact>]
            let ``Should return correct error message when prop is array`` () =
                let json =
                    """
                        {
                            "prop": {
                                "prop2": []
                            }
                        }
                    """

                Parse.int
                |> Parser.tryTraverse [ "prop"; "prop2"; "prop3" ]
                |> Parser.parse json
                |> Expect.error
                |> Verify.string

            [<Fact>]
            let ``Should return correct error message when object is array`` () =
                let json = "[]"

                Parse.int
                |> Parser.tryTraverse [ "prop"; "prop2"; "prop3" ]
                |> Parser.parse json
                |> Expect.error
                |> Verify.string