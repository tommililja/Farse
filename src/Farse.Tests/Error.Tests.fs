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
        Parse.req "prop" (Parse.list Parse.string)
        |> Parser.parse """{ "prop": { "prop2": 1 }}"""
        |> Expect.error
        |> Verify.string