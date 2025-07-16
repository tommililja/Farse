namespace Farse.Tests

open System
open System.Text.Json
open Xunit
open Farse

module Parse =

    [<Fact>]
    let ``Should parse optional value as Some`` () =
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
    let ``Should parse value as byte`` () =
        let expected = 123uy
        let actual =
            Parse.req "prop" Parse.byte
            |> Parser.parse """{ "prop": 123 }"""
            |> Expect.ok
        Expect.equal actual expected

    [<Fact>]
    let ``Should parse value as sbyte`` () =
        let expected = 123y
        let actual =
            Parse.req "prop" Parse.sbyte
            |> Parser.parse """{ "prop": 123 }"""
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
    let ``Should parse value as int16`` () =
        let expected = 1s
        let actual =
            Parse.req "prop" Parse.int16
            |> Parser.parse """{ "prop": 1 }"""
            |> Expect.ok
        Expect.equal actual expected

    [<Fact>]
    let ``Should parse value as int64`` () =
        let expected = 4738291056482917L
        let actual =
            Parse.req "prop" Parse.int64
            |> Parser.parse """{ "prop": 4738291056482917 }"""
            |> Expect.ok
        Expect.equal actual expected

    [<Fact>]
    let ``Should parse value as uint16`` () =
        let expected = 1us
        let actual =
            Parse.req "prop" Parse.uint16
            |> Parser.parse """{ "prop": 1 }"""
            |> Expect.ok
        Expect.equal actual expected

    [<Fact>]
    let ``Should parse value as uint32`` () =
        let expected = 1u
        let actual =
            Parse.req "prop" Parse.uint32
            |> Parser.parse """{ "prop": 1 }"""
            |> Expect.ok
        Expect.equal actual expected

    [<Fact>]
    let ``Should parse value as uint64`` () =
        let expected = 4738291056482917UL
        let actual =
            Parse.req "prop" Parse.uint64
            |> Parser.parse """{ "prop": 4738291056482917 }"""
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
    let ``Should parse value as float32`` () =
        let expected = 1.1f
        let actual =
            Parse.req "prop" Parse.float32
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
    let ``Should parse value as char`` () =
        let expected = 'x'
        let actual =
            Parse.req "prop" Parse.char
            |> Parser.parse """{ "prop": "x" }"""
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

    [<Fact>]
    let ``Should parse value as set`` () =
        let expected = set [ 1; 2; 3; 4; 5 ]
        let actual =
            Parse.req "prop" (Parse.set Parse.int)
            |> Parser.parse """{ "prop": [ 1, 2, 3, 4, 5 ] }"""
            |> Expect.ok
        Expect.equal actual expected

    [<Fact>]
    let ``Should parse object property count as int`` () =
        let expected = 1
        let actual =
            Parse.req "prop" Parse.propertyCount
            |> Parser.parse """{ "prop": { "prop2": 100 } }"""
            |> Expect.ok
        Expect.equal actual expected

    [<Fact>]
    let ``Should parse array length as int`` () =
        let expected = 5
        let actual =
            Parse.req "prop" Parse.arrayLength
            |> Parser.parse """{ "prop": [ 1, 2, 3, 4, 5 ] }"""
            |> Expect.ok
        Expect.equal actual expected

    [<Fact>]
    let ``Should parse element kind as JsonValueKind`` () =
        let expected = JsonValueKind.Number
        let actual =
            Parse.req "prop" Parse.kind
            |> Parser.parse """{ "prop": 100 }"""
            |> Expect.ok
        Expect.equal actual expected
