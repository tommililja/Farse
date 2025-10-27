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

    type ByteEnum =
        | Something = 1uy

    [<Fact>]
    let ``Should parse number as byte enum`` () =
        let expected = ByteEnum.Something
        let actual =
            Parse.req "prop" Parse.byteEnum
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

    type SByteEnum =
        | Something = 1y

    [<Fact>]
    let ``Should parse number as sbyte enum`` () =
        let expected = SByteEnum.Something
        let actual =
            Parse.req "prop" Parse.sbyteEnum
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

    type IntEnum =
        | Something = 1

    [<Fact>]
    let ``Should parse number as int enum`` () =
        let expected = IntEnum.Something
        let actual =
            Parse.req "prop" Parse.intEnum
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

    type Int16Enum =
        | Something = 1s

    [<Fact>]
    let ``Should parse number as int16 enum`` () =
        let expected = Int16Enum.Something
        let actual =
            Parse.req "prop" Parse.int16Enum
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

    type Int64Enum =
        | Something = 1L

    [<Fact>]
    let ``Should parse number as int64 enum`` () =
        let expected = Int64Enum.Something
        let actual =
            Parse.req "prop" Parse.int64Enum
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

    type UInt16Enum =
        | Something = 1us

    [<Fact>]
    let ``Should parse number as uint16 enum`` () =
        let expected = UInt16Enum.Something
        let actual =
            Parse.req "prop" Parse.uint16Enum
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

    type UInt32Enum =
        | Something = 1u

    [<Fact>]
    let ``Should parse number as uint32 enum`` () =
        let expected = UInt32Enum.Something
        let actual =
            Parse.req "prop" Parse.uint32Enum
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

    type UInt64Enum =
        | Something = 1UL

    [<Fact>]
    let ``Should parse number as uint64 enum`` () =
        let expected = UInt64Enum.Something
        let actual =
            Parse.req "prop" Parse.uint64Enum
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
    let ``Should parse string as TimeSpan`` () =
        let expected = TimeSpan.Parse("1:23:45")
        let actual =
            Parse.req "prop" Parse.timeSpan
            |> Parser.parse """{ "prop": "1:23:45" }"""
            |> Expect.ok
        Expect.equal actual expected

    [<Fact>]
    let ``Should parse string as TimeSpan exact`` () =
        let expected = TimeSpan.Parse("01:23:45")
        let actual =
            Parse.req "prop" (Parse.timeSpanExact "hhmmss")
            |> Parser.parse """{ "prop": "012345" }"""
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
    let ``Should parse array as seq`` () =
        let expected = seq [ 1; 2; 3; ]
        let actual =
            Parse.req "prop" (Parse.seq Parse.int)
            |> Parser.parse """{ "prop": [ 1, 2, 3 ] }"""
            |> Expect.ok
        Expect.equalSeq actual expected

    [<Fact>]
    let ``Should parse object as Map`` () =
        let expected = Map.ofSeq [ "key1", 1; "key2", 2; "key3", 3; ]
        let actual =
            Parse.req "prop" (Parse.map Parse.int)
            |> Parser.parse """{ "prop": { "key1": 1, "key2": 2, "key3": 3 } }"""
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
    let ``Should parse object property count as int`` () =
        let expected = 3
        let actual =
            Parse.req "prop" Parse.propertyCount
            |> Parser.parse """{ "prop": { "one": 1, "two": 2, "three": 3 } }"""
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
    let ``Should parse element as string`` () =
        let expected = """{ "prop2": 1 }"""
        let actual =
            Parse.req "prop" Parse.rawText
            |> Parser.parse """{ "prop": { "prop2": 1 } }"""
            |> Expect.ok
        Expect.equal actual expected

    type StringEnum =
        | Something = 1

    [<Fact>]
    let ``Should parse string as enum`` () =
        let expected = StringEnum.Something
        let actual =
            Parse.req "prop" Parse.enum
            |> Parser.parse """{ "prop": "something" }"""
            |> Expect.ok
        Expect.equal actual expected

    [<Fact>]
    let ``Should not parse element and return unit`` () =
        let expected = ()
        let actual =
            Parse.req "prop" Parse.none
            |> Parser.parse """{ "prop": 1 }"""
            |> Expect.ok
        Expect.equal actual expected

    [<Fact>]
    let ``Should not parse element and return value`` () =
        let expected = 1
        let actual =
            Parse.req "prop" (Parse.noneWith 1)
            |> Parser.parse """{ "prop": true }"""
            |> Expect.ok
        Expect.equal actual expected

    [<Fact>]
    let ``Should parse value with custom parser`` () =
        let parser : Parser<int> =
            Parse.custom (fun element ->
                match element.TryGetInt32() with
                | true, int -> Ok <| int + 1
                | _ -> Error None
            ) ExpectedKind.Number
        let expected = 2
        let actual =
            Parse.req "prop" parser
            |> Parser.parse """{ "prop": 1 }"""
            |> Expect.ok
        Expect.equal expected actual

    [<Fact>]
    let ``Should Ok when value is valid`` () =
        let fromInt (x:int) = Ok x
        let expected = 1
        let actual =
            Parse.req "prop" (Parse.valid Parse.int fromInt)
            |> Parser.parse """{ "prop": 1 }"""
            |> Expect.ok
        Expect.equal actual expected

    [<Fact>]
    let ``Should Error when value is not valid`` () =
        let fromInt (_:int) = Error "Not valid."
        Parse.req "prop" (Parse.valid Parse.int fromInt)
        |> Parser.parse """{ "prop": 1 }"""
        |> Expect.error