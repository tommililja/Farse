namespace Farse.Tests

open System
open Xunit
open Farse

module Parse =

    [<Fact>]
    let ``Should parse required value as int`` () =
        let expected = 1
        let actual =
            Parse.req "prop" Parse.int
            |> Parser.parse """{ "prop": 1 }"""
            |> Expect.ok
        Expect.equal actual expected

    [<Fact>]
    let ``Should parse optional value as int`` () =
        let expected = Some 1
        let actual =
            Parse.opt "prop" Parse.int
            |> Parser.parse """{ "prop": 1 }"""
            |> Expect.ok
        Expect.equal actual expected

    [<Fact>]
    let ``Should parse optional value as None`` () =
        let expected = None
        let actual =
            Parse.opt "prop" Parse.int
            |> Parser.parse """{ "prop": null }"""
            |> Expect.ok
        Expect.equal actual expected

    [<Fact>]
    let ``Should parse value as int`` () =
        let expected = 1
        let actual =
            Parse.req "prop" Parse.int
            |> Parser.parse """{ "prop": 1 }"""
            |> Expect.ok
        Expect.equal actual expected

    [<Fact>]
    let ``Should parse value as float`` () =
        let expected = 1.1
        let actual =
            Parse.req "prop" Parse.float
            |> Parser.parse """{ "prop": 1.1 }"""
            |> Expect.ok
        Expect.equal actual expected

    [<Fact>]
    let ``Should parse value as decimal`` () =
        let expected = 3.333333333333m
        let actual =
            Parse.req "prop" Parse.decimal
            |> Parser.parse """{ "prop": 3.333333333333 }"""
            |> Expect.ok
        Expect.equal actual expected

    [<Fact>]
    let ``Should parse value as string`` () =
        let expected = "text"
        let actual =
            Parse.req "prop" Parse.string
            |> Parser.parse """{ "prop": "text" }"""
            |> Expect.ok
        Expect.equal actual expected

    [<Fact>]
    let ``Should parse value as bool`` () =
        let expected = true
        let actual =
            Parse.req "prop" Parse.bool
            |> Parser.parse """{ "prop": true }"""
            |> Expect.ok
        Expect.equal actual expected

    [<Fact>]
    let ``Should parse value as guid`` () =
        let expected = Guid.Empty
        let actual =
            Parse.req "prop" Parse.guid
            |> Parser.parse """{ "prop": "00000000-0000-0000-0000-000000000000" }"""
            |> Expect.ok
        Expect.equal actual expected

    [<Fact>]
    let ``Should parse value as datetime`` () =
        let expected = DateTime.Parse("2025-05-13T17:28:45")
        let actual =
            Parse.req "prop" Parse.dateTime
            |> Parser.parse """{ "prop": "2025-05-13T17:28:45" }"""
            |> Expect.ok
        Expect.equal actual expected

    [<Fact>]
    let ``Should parse value as datetime utc`` () =
        let now = DateTime(DateOnly(2025, 05, 25), TimeOnly(10, 00))
        let expected = now.ToUniversalTime()
        let actual =
            Parse.req "prop" Parse.dateTimeUtc
            |> Parser.parse $$"""{ "prop": "%%s{{now.ToString("yyyy-MM-ddTHH:mm")}}" }"""
            |> Expect.ok
        Expect.equal actual expected

    [<Fact>]
    let ``Should parse value as datetime exact`` () =
        let expected = DateTime.Parse("2025-05-13 17:28")
        let actual =
            Parse.req "prop" (Parse.dateTimeExact "yyyy-MM-dd HH:mm")
            |> Parser.parse """{ "prop": "2025-05-13 17:28" }"""
            |> Expect.ok
        Expect.equal actual expected

    [<Fact>]
    let ``Should parse value as datetime offset`` () =
        let expected = DateTime.Parse("2025-05-13T17:28:45+02:00")
        let actual =
            Parse.req "prop" Parse.dateTime
            |> Parser.parse """{ "prop": "2025-05-13T17:28:45+02:00" }"""
            |> Expect.ok
        Expect.equal actual expected

    [<Fact>]
    let ``Should parse value as list`` () =
        let expected = [ 1; 2; 3; 4; 5 ]
        let actual =
            Parse.req "prop" (Parse.list Parse.int)
            |> Parser.parse """{ "prop": [ 1, 2, 3, 4, 5 ] }"""
            |> Expect.ok
        Expect.equal actual expected

    [<Fact>]
    let ``Should parse value as array`` () =
        let expected = [| 1; 2; 3; 4; 5 |]
        let actual =
            Parse.req "prop" (Parse.array Parse.int)
            |> Parser.parse """{ "prop": [ 1, 2, 3, 4, 5 ] }"""
            |> Expect.ok
        Expect.equal actual expected