namespace Farse.Tests

open System
open System.Text.Json
open Xunit
open Farse

module ParseTests =

    [<Fact>]
    let ``Should parse number as Some int`` () =
        let expected = Some 1
        let actual =
            Parse.opt "prop" Parse.int
            |> Parser.parse """{ "prop": 1 }"""
            |> Expect.ok
        Expect.equal actual expected

    [<Fact>]
    let ``Should parse null as None`` () =
        let expected = None
        let actual =
            Parse.opt "prop" Parse.int
            |> Parser.parse """{ "prop": null }"""
            |> Expect.ok
        Expect.equal actual expected

    [<Fact>]
    let ``Should parse nested number as int`` () =
        let expected = 1
        let actual =
            Parse.req "prop.prop2.prop3" Parse.int
            |> Parser.parse """{ "prop": { "prop2": { "prop3": 1 } } }"""
            |> Expect.ok
        Expect.equal actual expected

    [<Fact>]
    let ``Should parse nested number as int with required parsers`` () =
        let expected = 1
        let actual =
            let parser = Parse.req "prop3" Parse.int
            Parse.req "prop.prop2" parser
            |> Parser.parse """{ "prop": { "prop2": { "prop3": 1 } } }"""
            |> Expect.ok
        Expect.equal actual expected

    [<Fact>]
    let ``Should parse nested null as None with optional parsers`` () =
        let expected = Some None
        let actual =
            let parser = Parse.opt "prop3" Parse.int
            Parse.opt "prop.prop2" parser
            |> Parser.parse """{ "prop": { "prop2": { "prop3": null } } }"""
            |> Expect.ok
        Expect.equal actual expected

    [<Fact>]
    let ``Should parse nested number as Some int with mixed parsers`` () =
        let expected = Some 1
        let actual =
            let parser = Parse.req "prop3" Parse.int
            Parse.opt "prop.prop2" parser
            |> Parser.parse """{ "prop": { "prop2": { "prop3": 1 } } }"""
            |> Expect.ok
        Expect.equal actual expected

    [<Fact>]
    let ``Should parse nested null as None``() =
        let expected = None
        let actual =
            Parse.opt "prop.prop2.prop3" Parse.int
            |> Parser.parse """ { "prop": { "prop2": { "prop3": null } } } """
            |> Expect.ok
        Expect.equal actual expected

    [<Fact>]
    let ``Should parse nested null object as None`` () =
        let expected = None
        let actual =
            Parse.opt "prop.prop2.prop3" Parse.int
            |> Parser.parse """ { "prop": { "prop2": null } } """
            |> Expect.ok
        Expect.equal actual expected

    [<Fact>]
    let ``Should parse nested null object as None with mixed parsers`` () =
        let expected = None
        let actual =
            let parser = Parse.req "prop3" Parse.int
            Parse.opt "prop.prop2" parser
            |> Parser.parse """ { "prop": { "prop2": null } } """
            |> Expect.ok
        Expect.equal actual expected

    [<Fact>]
    let ``Should parse number as byte`` () =
        let expected = 1uy
        let actual =
            Parse.req "prop" Parse.byte
            |> Parser.parse """{ "prop": 1 }"""
            |> Expect.ok
        Expect.equal actual expected

    [<Fact>]
    let ``Should parse number as sbyte`` () =
        let expected = 1y
        let actual =
            Parse.req "prop" Parse.sbyte
            |> Parser.parse """{ "prop": 1 }"""
            |> Expect.ok
        Expect.equal actual expected

    [<Fact>]
    let ``Should parse number as int`` () =
        let expected = 1
        let actual =
            Parse.req "prop" Parse.int
            |> Parser.parse """{ "prop": 1 }"""
            |> Expect.ok
        Expect.equal actual expected

    [<Fact>]
    let ``Should parse number as int16`` () =
        let expected = 1s
        let actual =
            Parse.req "prop" Parse.int16
            |> Parser.parse """{ "prop": 1 }"""
            |> Expect.ok
        Expect.equal actual expected

    [<Fact>]
    let ``Should parse number as int64`` () =
        let expected = 1L
        let actual =
            Parse.req "prop" Parse.int64
            |> Parser.parse """{ "prop": 1 }"""
            |> Expect.ok
        Expect.equal actual expected

    [<Fact>]
    let ``Should parse number as uint16`` () =
        let expected = 1us
        let actual =
            Parse.req "prop" Parse.uint16
            |> Parser.parse """{ "prop": 1 }"""
            |> Expect.ok
        Expect.equal actual expected

    [<Fact>]
    let ``Should parse number as uint32`` () =
        let expected = 1u
        let actual =
            Parse.req "prop" Parse.uint32
            |> Parser.parse """{ "prop": 1 }"""
            |> Expect.ok
        Expect.equal actual expected

    [<Fact>]
    let ``Should parse number as uint64`` () =
        let expected = 1UL
        let actual =
            Parse.req "prop" Parse.uint64
            |> Parser.parse """{ "prop": 1 }"""
            |> Expect.ok
        Expect.equal actual expected

    [<Fact>]
    let ``Should parse number as float`` () =
        let expected = 1.1
        let actual =
            Parse.req "prop" Parse.float
            |> Parser.parse """{ "prop": 1.1 }"""
            |> Expect.ok
        Expect.equal actual expected

    [<Fact>]
    let ``Should parse number as float32`` () =
        let expected = 1.1f
        let actual =
            Parse.req "prop" Parse.float32
            |> Parser.parse """{ "prop": 1.1 }"""
            |> Expect.ok
        Expect.equal actual expected

    [<Fact>]
    let ``Should parse number as decimal`` () =
        let expected = 1.1m
        let actual =
            Parse.req "prop" Parse.decimal
            |> Parser.parse """{ "prop": 1.1 }"""
            |> Expect.ok
        Expect.equal actual expected

    [<Fact>]
    let ``Should parse string as char`` () =
        let expected = 'c'
        let actual =
            Parse.req "prop" Parse.char
            |> Parser.parse """{ "prop": "c" }"""
            |> Expect.ok
        Expect.equal actual expected

    [<Fact>]
    let ``Should parse string as string`` () =
        let expected = "text"
        let actual =
            Parse.req "prop" Parse.string
            |> Parser.parse """{ "prop": "text" }"""
            |> Expect.ok
        Expect.equal actual expected

    [<Fact>]
    let ``Should parse bool as bool`` () =
        let expected = true
        let actual =
            Parse.req "prop" Parse.bool
            |> Parser.parse """{ "prop": true }"""
            |> Expect.ok
        Expect.equal actual expected

    [<Fact>]
    let ``Should parse string as Guid`` () =
        let expected = Guid.Empty
        let actual =
            Parse.req "prop" Parse.guid
            |> Parser.parse """{ "prop": "00000000-0000-0000-0000-000000000000" }"""
            |> Expect.ok
        Expect.equal actual expected

    [<Fact>]
    let ``Should parse null as unit`` () =
        let expected = ()
        let actual =
            Parse.req "prop" Parse.unit
            |> Parser.parse """{ "prop": null }"""
            |> Expect.ok
        Expect.equal actual expected

    [<Fact>]
    let ``Should parse string as TimeOnly`` () =
        let expected = TimeOnly.Parse("17:28:45")
        let actual =
            Parse.req "prop" Parse.timeOnly
            |> Parser.parse """{ "prop": "2025-05-13T17:28:45" }"""
            |> Expect.ok
        Expect.equal actual expected

    [<Fact>]
    let ``Should parse string as TimeOnly exact`` () =
        let expected = TimeOnly.Parse("17:28:45")
        let actual =
            Parse.req "prop" (Parse.timeOnlyExact "HHmmss")
            |> Parser.parse """{ "prop": "172845" }"""
            |> Expect.ok
        Expect.equal actual expected

    [<Fact>]
    let ``Should parse string as DateOnly`` () =
        let expected = DateOnly.Parse("2025-05-13")
        let actual =
            Parse.req "prop" Parse.dateOnly
            |> Parser.parse """{ "prop": "2025-05-13T17:28:45" }"""
            |> Expect.ok
        Expect.equal actual expected

    [<Fact>]
    let ``Should parse string as DateOnly exact`` () =
        let expected = DateOnly.Parse("2025-05-13")
        let actual =
            Parse.req "prop" (Parse.dateOnlyExact "yyyyMMdd")
            |> Parser.parse """{ "prop": "20250513" }"""
            |> Expect.ok
        Expect.equal actual expected

    [<Fact>]
    let ``Should parse string as DateTime`` () =
        let expected = DateTime.Parse("2025-05-13T17:28:45")
        let actual =
            Parse.req "prop" Parse.dateTime
            |> Parser.parse """{ "prop": "2025-05-13T17:28:45" }"""
            |> Expect.ok
        Expect.equal actual expected

    [<Fact>]
    let ``Should parse string as DateTime UTC`` () =
        let now = DateTime(DateOnly(2025, 05, 25), TimeOnly(10, 00))
        let expected = now.ToUniversalTime()
        let actual =
            Parse.req "prop" Parse.dateTimeUtc
            |> Parser.parse $$"""{ "prop": "%%s{{now.ToString("yyyy-MM-ddTHH:mm")}}" }"""
            |> Expect.ok
        Expect.equal actual expected

    [<Fact>]
    let ``Should parse string as DateTime exact`` () =
        let expected = DateTime.Parse("2025-05-13T17:28:45")
        let actual =
            Parse.req "prop" (Parse.dateTimeExact "yyyy-MM-dd HH:mm:ss")
            |> Parser.parse """{ "prop": "2025-05-13 17:28:45" }"""
            |> Expect.ok
        Expect.equal actual expected

    [<Fact>]
    let ``Should parse string as DateTimeOffset`` () =
        let expected = DateTime.Parse("2025-05-13T17:28:45+02:00")
        let actual =
            Parse.req "prop" Parse.dateTime
            |> Parser.parse """{ "prop": "2025-05-13T17:28:45+02:00" }"""
            |> Expect.ok
        Expect.equal actual expected

    [<Fact>]
    let ``Should parse string as DateTimeOffset exact`` () =
        let expected = DateTimeOffset.Parse("2025-05-13T17:28:00+02:00")
        let actual =
            Parse.req "prop" (Parse.dateTimeOffsetExact "yyyy-MM-dd HH:mm zzz")
            |> Parser.parse """{ "prop": "2025-05-13 17:28 +02:00" }"""
            |> Expect.ok
        Expect.equal actual expected

    [<Fact>]
    let ``Should parse array as list`` () =
        let expected = [ 1; 2; 3; ]
        let actual =
            Parse.req "prop" (Parse.list Parse.int)
            |> Parser.parse """{ "prop": [ 1, 2, 3 ] }"""
            |> Expect.ok
        Expect.equal actual expected

    [<Fact>]
    let ``Should parse array as array`` () =
        let expected = [| 1; 2; 3; |]
        let actual =
            Parse.req "prop" (Parse.array Parse.int)
            |> Parser.parse """{ "prop": [ 1, 2, 3 ] }"""
            |> Expect.ok
        Expect.equal actual expected

    [<Fact>]
    let ``Should parse array as Set`` () =
        let expected = set [ 1; 2; 3; ]
        let actual =
            Parse.req "prop" (Parse.set Parse.int)
            |> Parser.parse """{ "prop": [ 1, 2, 3 ] }"""
            |> Expect.ok
        Expect.equal actual expected

    [<Fact>]
    let ``Should parse array length as int`` () =
        let expected = 3
        let actual =
            Parse.req "prop" Parse.arrayLength
            |> Parser.parse """{ "prop": [ 1, 2, 3 ] }"""
            |> Expect.ok
        Expect.equal actual expected

    [<Fact>]
    let ``Should parse element kind as JsonValueKind`` () =
        let expected = JsonValueKind.Number
        let actual =
            Parse.req "prop" Parse.kind
            |> Parser.parse """{ "prop": 1 }"""
            |> Expect.ok
        Expect.equal actual expected

    [<Fact>]
    let ``Should parse element as JsonElement`` () =
        let expected = JsonDocument.Parse("""{ "prop2": 1 }""").RootElement
        let actual =
            Parse.req "prop" Parse.element
            |> Parser.parse """{ "prop": { "prop2": 1 } }"""
            |> Expect.ok
        Expect.isTrue <| JsonElement.DeepEquals(expected, actual)

    [<Fact>]
    let ``Should not parse element and return unit`` () =
        let expected = ()
        let actual =
            Parse.req "prop" Parse.none
            |> Parser.parse """{ "prop": 1 }"""
            |> Expect.ok
        Expect.equal actual expected