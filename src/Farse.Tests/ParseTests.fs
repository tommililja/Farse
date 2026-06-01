namespace Farse.Tests

open System.Numerics
open Xunit
open Farse
open System
open System.Collections.Generic
open System.Text.Json

module ParseTests =

    module Custom =

        [<Fact>]
        let ``Should succeed when value is custom`` () =
            Parse.custom (_.GetInt32() >> Ok) ExpectedKind.Number
            |> Parser.parse "1"
            |> Expect.ok
            |> Expect.equal 1

        [<Fact>]
        let ``Should succeed when value is custom with any kind`` () =
            Parse.custom (fun element ->
                match element.TryGetInt32() with
                | true, x -> Ok x
                | _ -> Error "Invalid value."
            ) ExpectedKind.Any
            |> Parser.parse "1"
            |> Expect.ok
            |> Expect.equal 1

        [<Fact>]
        let ``Should fail when value is not a number`` () =
            Parse.custom (fun element ->
                match element.TryGetInt32() with
                | true, x -> Ok x
                | _ -> Error "Invalid value."
            ) ExpectedKind.Number
            |> Parser.parse "true"
            |> Expect.isError

        [<Fact>]
        let ``Should fail when value is invalid`` () =
            Parse.custom (fun element ->
                match element.TryGetInt32() with
                | true, x -> Ok x
                | _ -> Error "Invalid value."
            ) ExpectedKind.Number
            |> Parser.parse "1.1"
            |> Expect.isError

        [<Fact>]
        let ``Should fail when value is throwing exception`` () =
            Parse.custom (_.GetString() >> Ok) ExpectedKind.Number
            |> Parser.parse "1"
            |> Expect.isError

    module Int =

        [<Fact>]
        let ``Should succeed when value is int`` () =
            Parse.int
            |> Parser.parse "1"
            |> Expect.ok
            |> Expect.equal 1

        [<Fact>]
        let ``Should fail when value is invalid`` () =
            Parse.int
            |> Parser.parse "1.1"
            |> Expect.isError

        [<Fact>]
        let ``Should fail when value is not a number`` () =
            Parse.int
            |> Parser.parse "true"
            |> Expect.isError

    module Int16 =

        [<Fact>]
        let ``Should succeed when value is int16`` () =
            Parse.int16
            |> Parser.parse "1"
            |> Expect.ok
            |> Expect.equal 1s

        [<Fact>]
        let ``Should fail when value is invalid`` () =
            Parse.int16
            |> Parser.parse "1.1"
            |> Expect.isError

        [<Fact>]
        let ``Should fail when value is not a number`` () =
            Parse.int16
            |> Parser.parse "true"
            |> Expect.isError

    module Int64 =

        [<Fact>]
        let ``Should succeed when value is int64`` () =
            Parse.int64
            |> Parser.parse "1"
            |> Expect.ok
            |> Expect.equal 1L

        [<Fact>]
        let ``Should fail when value is invalid`` () =
            Parse.int64
            |> Parser.parse "1.1"
            |> Expect.isError

        [<Fact>]
        let ``Should fail when value is not a number`` () =
            Parse.int64
            |> Parser.parse "true"
            |> Expect.isError

    module UInt16 =

        [<Fact>]
        let ``Should succeed when value is uint16`` () =
            Parse.uint16
            |> Parser.parse "1"
            |> Expect.ok
            |> Expect.equal 1us

        [<Fact>]
        let ``Should fail when value is invalid`` () =
            Parse.uint16
            |> Parser.parse "1.1"
            |> Expect.isError

        [<Fact>]
        let ``Should fail when value is not a number`` () =
            Parse.uint16
            |> Parser.parse "true"
            |> Expect.isError

    module UInt32 =

        [<Fact>]
        let ``Should succeed when value is uint32`` () =
            Parse.uint32
            |> Parser.parse "1"
            |> Expect.ok
            |> Expect.equal 1u

        [<Fact>]
        let ``Should fail when value is invalid`` () =
            Parse.uint32
            |> Parser.parse "1.1"
            |> Expect.isError

        [<Fact>]
        let ``Should fail when value is not a number`` () =
            Parse.uint32
            |> Parser.parse "true"
            |> Expect.isError

    module UInt64 =

        [<Fact>]
        let ``Should succeed when value is uint64`` () =
            Parse.uint64
            |> Parser.parse "1"
            |> Expect.ok
            |> Expect.equal 1UL

        [<Fact>]
        let ``Should fail when value is invalid`` () =
            Parse.uint64
            |> Parser.parse "1.1"
            |> Expect.isError

        [<Fact>]
        let ``Should fail when value is not a number`` () =
            Parse.uint64
            |> Parser.parse "true"
            |> Expect.isError

    module Float =

        [<Fact>]
        let ``Should succeed when value is float`` () =
            Parse.float
            |> Parser.parse "1.1"
            |> Expect.ok
            |> Expect.equal 1.1

        [<Fact>]
        let ``Should fail when value is invalid`` () =
            Parse.float
            |> Parser.parse "\"a\""
            |> Expect.isError

        [<Fact>]
        let ``Should fail when value is not a number`` () =
            Parse.float
            |> Parser.parse "true"
            |> Expect.isError

    module Float32 =

        [<Fact>]
        let ``Should succeed when value is float32`` () =
            Parse.float32
            |> Parser.parse "1.1"
            |> Expect.ok
            |> Expect.equal 1.1f

        [<Fact>]
        let ``Should fail when value is invalid`` () =
            Parse.float32
            |> Parser.parse "\"a\""
            |> Expect.isError

        [<Fact>]
        let ``Should fail when value is not a number`` () =
            Parse.float32
            |> Parser.parse "true"
            |> Expect.isError

    module Decimal =

        [<Fact>]
        let ``Should succeed when value is decimal`` () =
            Parse.decimal
            |> Parser.parse "1.1"
            |> Expect.ok
            |> Expect.equal 1.1m

        [<Fact>]
        let ``Should fail when value is invalid`` () =
            Parse.decimal
            |> Parser.parse "\"a\""
            |> Expect.isError

        [<Fact>]
        let ``Should fail when value is not a number`` () =
            Parse.decimal
            |> Parser.parse "true"
            |> Expect.isError

    module Byte =

        [<Fact>]
        let ``Should succeed when value is byte`` () =
            Parse.byte
            |> Parser.parse "1"
            |> Expect.ok
            |> Expect.equal 1uy

        [<Fact>]
        let ``Should fail when value is invalid`` () =
            Parse.byte
            |> Parser.parse "256"
            |> Expect.isError

        [<Fact>]
        let ``Should fail when value is not a number`` () =
            Parse.byte
            |> Parser.parse "true"
            |> Expect.isError

    module SByte =

        [<Fact>]
        let ``Should succeed when value is sbyte`` () =
            Parse.sbyte
            |> Parser.parse "1"
            |> Expect.ok
            |> Expect.equal 1y

        [<Fact>]
        let ``Should fail when value is invalid`` () =
            Parse.sbyte
            |> Parser.parse "128"
            |> Expect.isError

        [<Fact>]
        let ``Should fail when value is not a number`` () =
            Parse.sbyte
            |> Parser.parse "true"
            |> Expect.isError

    module Char =

        [<Fact>]
        let ``Should succeed when value is char`` () =
            Parse.char
            |> Parser.parse "\"a\""
            |> Expect.ok
            |> Expect.equal 'a'

        [<Fact>]
        let ``Should fail when value is invalid`` () =
            Parse.char
            |> Parser.parse "\"ab\""
            |> Expect.isError

    module String =

        [<Fact>]
        let ``Should succeed when value is string`` () =
            Parse.string
            |> Parser.parse "\"value\""
            |> Expect.ok
            |> Expect.equal "value"

    module StringNonEmpty =

        [<Fact>]
        let ``Should succeed when value is stringNonEmpty`` () =
            Parse.stringNonEmpty
            |> Parser.parse "\"a\""
            |> Expect.ok
            |> Expect.equal "a"

        [<Fact>]
        let ``Should fail when value is invalid`` () =
            Parse.stringNonEmpty
            |> Parser.parse "\"\""
            |> Expect.isError

        [<Fact>]
        let ``Should return Error when parsing empty string`` () =
            Prop.req "prop" Parse.stringNonEmpty
            |> Parser.parse """{ "prop": "" }"""
            |> Expect.errorString

        [<Fact>]
        let ``Should return Error when parsing whitespace string`` () =
            Prop.req "prop" Parse.stringNonEmpty
            |> Parser.parse """{ "prop": "  " }"""
            |> Expect.errorString

    module Regex =

        [<Fact>]
        let ``Should succeed when value is regex`` () =
            Parse.regex "^[0-9]+$"
            |> Parser.parse "\"123\""
            |> Expect.ok
            |> Expect.equal "123"

        [<Fact>]
        let ``Should fail when value is invalid`` () =
            Parse.regex "^[0-9]+$"
            |> Parser.parse "\"abc\""
            |> Expect.isError

        [<Fact>]
        let ``Should parse string with regex`` () =
            let expected = "12345"
            let actual =
                Prop.req "prop" (Parse.regex "^[0-9]+$")
                |> Parser.parse """{ "prop": "12345" }"""
                |> Expect.ok
            Expect.equal actual expected

        [<Fact>]
        let ``Should return Error when regex does not match`` () =
            Prop.req "prop" (Parse.regex "^[0-9]+$")
            |> Parser.parse """{ "prop": "abc" }"""
            |> Expect.errorString

    module Number =

        [<Fact>]
        let ``Should succeed when value is number`` () =
            Parse.number<int>
            |> Parser.parse "\"1\""
            |> Expect.ok
            |> Expect.equal 1

        [<Fact>]
        let ``Should fail when value is invalid`` () =
            Parse.number<int>
            |> Parser.parse "\"a\""
            |> Expect.isError

        [<Fact>]
        let ``Should parse string as int`` () =
            let expected = BigInteger.Parse("1")
            let actual =
                Prop.req "prop" Parse.number<bigint>
                |> Parser.parse """{ "prop": "1" }"""
                |> Expect.ok
            Expect.equal actual expected

        [<Fact>]
        let ``Should return Error when string is not a number`` () =
            Prop.req "prop" Parse.number<int>
            |> Parser.parse """{ "prop": "invalid" }"""
            |> Expect.errorString

    module Base64Bytes =

        [<Fact>]
        let ``Should succeed when value is base64Bytes`` () =
            Parse.base64Bytes
            |> Parser.parse "\"SGVsbG8=\""
            |> Expect.ok
            |> Expect.equal ("Hello" |> System.Text.Encoding.UTF8.GetBytes)

        [<Fact>]
        let ``Should parse base64 string byte array`` () =
            let expected = Convert.FromBase64String("aGVsbG8gc3RyYW5nZXIh")
            let actual =
                Prop.req "prop" Parse.base64Bytes
                |> Parser.parse """{ "prop": "aGVsbG8gc3RyYW5nZXIh" }"""
                |> Expect.ok
            Expect.equal actual expected

    module Bigint =

        [<Fact>]
        let ``Should succeed when value is bigint`` () =
            Parse.bigint
            |> Parser.parse "123456789012345678901234567890"
            |> Expect.ok
            |> Expect.equal (System.Numerics.BigInteger.Parse "123456789012345678901234567890")

        [<Fact>]
        let ``Should fail when value is invalid`` () =
            Parse.bigint
            |> Parser.parse "\"a\""
            |> Expect.isError

    module Bool =

        [<Fact>]
        let ``Should succeed when value is bool`` () =
            Parse.bool
            |> Parser.parse "true"
            |> Expect.ok
            |> Expect.equal true

        [<Fact>]
        let ``Should fail when value is invalid`` () =
            Parse.bool
            |> Parser.parse "1"
            |> Expect.isError

    module Guid =

        [<Fact>]
        let ``Should succeed when value is guid`` () =
            let guid = Guid.NewGuid()
            Parse.guid
            |> Parser.parse (sprintf "\"%s\"" (guid.ToString()))
            |> Expect.ok
            |> Expect.equal guid

        [<Fact>]
        let ``Should fail when value is invalid`` () =
            Parse.guid
            |> Parser.parse "\"invalid-guid\""
            |> Expect.isError

    module Unit =

        [<Fact>]
        let ``Should succeed when value is unit`` () =
            Parse.unit
            |> Parser.parse "null"
            |> Expect.ok
            |> Expect.equal ()

    module None =

        [<Fact>]
        let ``Should succeed when value is none`` () =
            Parse.none
            |> Parser.parse "null"
            |> Expect.ok
            |> Expect.equal ()

        [<Fact>]
        let ``Should not parse element and return unit`` () =
            let expected = ()
            let actual =
                Prop.req "prop" Parse.none
                |> Parser.parse """{ "prop": 1 }"""
                |> Expect.ok
            Expect.equal actual expected

    module Enum =

        type StringEnum =
            | A = 0

        [<Fact>]
        let ``Should parse string as enum`` () =
            let expected = StringEnum.A
            let actual =
                Prop.req "prop" Parse.enum<StringEnum>
                |> Parser.parse """{ "prop": "something" }"""
                |> Expect.ok
            Expect.equal actual expected

    module IntEnum =

        type IntEnum =
            | A = 1

        [<Fact>]
        let ``Should succeed when value is intEnum`` () =
            Parse.intEnum<IntEnum>
            |> Parser.parse "1"
            |> Expect.ok
            |> Expect.equal IntEnum.A

        [<Fact>]
        let ``Should parse number as int enum`` () =
            let expected = IntEnum.A
            let actual =
                Prop.req "prop" Parse.intEnum
                |> Parser.parse """{ "prop": 1 }"""
                |> Expect.ok
            Expect.equal actual expected

    module Int16Enum =

        type Int16Enum =
            | A = 1s

        [<Fact>]
        let ``Should parse number as int16 enum`` () =
            let expected = Int16Enum.A
            let actual =
                Prop.req "prop" Parse.int16Enum
                |> Parser.parse """{ "prop": 1 }"""
                |> Expect.ok
            Expect.equal actual expected

    module Int64Enum =

        type Int64Enum =
            | A = 1L

        [<Fact>]
        let ``Should parse number as int64 enum`` () =
            let expected = Int64Enum.A
            let actual =
                Prop.req "prop" Parse.int64Enum
                |> Parser.parse """{ "prop": 1 }"""
                |> Expect.ok
            Expect.equal actual expected

    module UInt16Enum =

        type UInt16Enum =
            | A = 1us

        [<Fact>]
        let ``Should parse number as uint16 enum`` () =
            let expected = UInt16Enum.A
            let actual =
                Prop.req "prop" Parse.uint16Enum
                |> Parser.parse """{ "prop": 1 }"""
                |> Expect.ok
            Expect.equal actual expected

    module UInt32Enum =

        type UInt32Enum =
            | A = 1u

        [<Fact>]
        let ``Should parse number as uint32 enum`` () =
            let expected = UInt32Enum.A
            let actual =
                Prop.req "prop" Parse.uint32Enum
                |> Parser.parse """{ "prop": 1 }"""
                |> Expect.ok
            Expect.equal actual expected


    module UInt64Enum =

        type UInt64Enum =
            | A = 1UL

        [<Fact>]
        let ``Should parse number as uint64 enum`` () =
            let expected = UInt64Enum.A
            let actual =
                Prop.req "prop" Parse.uint64Enum
                |> Parser.parse """{ "prop": 1 }"""
                |> Expect.ok
            Expect.equal actual expected

    module ByteEnum =

        type ByteEnum =
            | A = 1uy

        [<Fact>]
        let ``Should parse number as byte enum`` () =
            let expected = ByteEnum.A
            let actual =
                Prop.req "prop" Parse.byteEnum
                |> Parser.parse """{ "prop": 1 }"""
                |> Expect.ok
            Expect.equal actual expected

    module SByteEnum =

        type SByteEnum =
            | A = 1y

        [<Fact>]
        let ``Should parse number as sbyte enum`` () =
            let expected = SByteEnum.A
            let actual =
                Prop.req "prop" Parse.sbyteEnum
                |> Parser.parse """{ "prop": 1 }"""
                |> Expect.ok
            Expect.equal actual expected

    module TimeOnly =

        [<Fact>]
        let ``Should succeed when value is timeOnly`` () =
            Parse.timeOnly
            |> Parser.parse "\"12:00:00\""
            |> Expect.ok
            |> Expect.equal (TimeOnly(12, 0, 0))

        [<Fact>]
        let ``Should parse string as TimeOnly`` () =
            let expected = TimeOnly.Parse("17:28:45")
            let actual =
                Prop.req "prop" Parse.timeOnly
                |> Parser.parse """{ "prop": "2025-05-13T17:28:45" }"""
                |> Expect.ok
            Expect.equal actual expected

    module TimeOnlyExact =

        [<Fact>]
        let ``Should succeed when value is timeOnlyExact`` () =
            Parse.timeOnlyExact "HH:mm"
            |> Parser.parse "\"12:00\""
            |> Expect.ok
            |> Expect.equal (TimeOnly(12, 0))

        [<Fact>]
        let ``Should parse string as TimeOnly exact`` () =
            let expected = TimeOnly.Parse("17:28:45")
            let actual =
                Prop.req "prop" (Parse.timeOnlyExact "HHmmss")
                |> Parser.parse """{ "prop": "172845" }"""
                |> Expect.ok
            Expect.equal actual expected

        [<Fact>]
        let ``Should return Error when parsing a TimeOnly exact with an incorrect format`` () =
            Prop.req "prop" (Parse.timeOnlyExact "HHmmss")
            |> Parser.parse """{ "prop": "17:28:45" }"""
            |> Expect.errorString

    module TimeSpan =

        [<Fact>]
        let ``Should succeed when value is timeSpan`` () =
            Parse.timeSpan
            |> Parser.parse "\"01:00:00\""
            |> Expect.ok
            |> Expect.equal (TimeSpan(1, 0, 0))

        [<Fact>]
        let ``Should parse string as TimeSpan`` () =
            let expected = TimeSpan.Parse("1:23:45")
            let actual =
                Prop.req "prop" Parse.timeSpan
                |> Parser.parse """{ "prop": "1:23:45" }"""
                |> Expect.ok
            Expect.equal actual expected

    module TimeSpanExact =

        [<Fact>]
        let ``Should succeed when value is timeSpanExact`` () =
            Parse.timeSpanExact "c"
            |> Parser.parse "\"1.00:00:00\""
            |> Expect.ok
            |> Expect.equal (TimeSpan(1, 0, 0, 0))

        [<Fact>]
        let ``Should parse string as TimeSpan exact`` () =
            let expected = TimeSpan.Parse("01:23:45")
            let actual =
                Prop.req "prop" (Parse.timeSpanExact "hhmmss")
                |> Parser.parse """{ "prop": "012345" }"""
                |> Expect.ok
            Expect.equal actual expected

        [<Fact>]
        let ``Should return Error when parsing a TimeSpan exact with an incorrect format`` () =
            Prop.req "prop" (Parse.timeSpanExact @"hh\mm\ss")
            |> Parser.parse """{ "prop": "01:28:45" }"""
            |> Expect.errorString

    module DateOnly =

        [<Fact>]
        let ``Should succeed when value is dateOnly`` () =
            Parse.dateOnly
            |> Parser.parse "\"2021-01-01\""
            |> Expect.ok
            |> Expect.equal (DateOnly(2021, 1, 1))

        [<Fact>]
        let ``Should parse string as DateOnly`` () =
            let expected = DateOnly.Parse("2025-05-13")
            let actual =
                Prop.req "prop" Parse.dateOnly
                |> Parser.parse """{ "prop": "2025-05-13T17:28:45" }"""
                |> Expect.ok
            Expect.equal actual expected

    module DateOnlyExact =

        [<Fact>]
        let ``Should succeed when value is dateOnlyExact`` () =
            Parse.dateOnlyExact "yyyy-MM-dd"
            |> Parser.parse "\"2021-01-01\""
            |> Expect.ok
            |> Expect.equal (DateOnly(2021, 1, 1))

        [<Fact>]
        let ``Should parse string as DateOnly exact`` () =
            let expected = DateOnly.Parse("2025-05-13")
            let actual =
                Prop.req "prop" (Parse.dateOnlyExact "yyyyMMdd")
                |> Parser.parse """{ "prop": "20250513" }"""
                |> Expect.ok
            Expect.equal actual expected

        [<Fact>]
        let ``Should return Error when parsing a DateOnly exact with an incorrect format`` () =
            Prop.req "prop" (Parse.dateOnlyExact "yyyyMMdd")
            |> Parser.parse """{ "prop": "2025-05-13" }"""
            |> Expect.errorString

    module DateTime =

        [<Fact>]
        let ``Should succeed when value is dateTime`` () =
            let dt = DateTime(2021, 1, 1, 12, 0, 0)
            Parse.dateTime
            |> Parser.parse (sprintf "\"%04d-%02d-%02dT%02d:%02d:%02d\"" dt.Year dt.Month dt.Day dt.Hour dt.Minute dt.Second)
            |> Expect.ok
            |> Expect.equal dt

        [<Fact>]
        let ``Should parse string as DateTime`` () =
            let expected = DateTime.Parse("2025-05-13T17:28:45")
            let actual =
                Prop.req "prop" Parse.dateTime
                |> Parser.parse """{ "prop": "2025-05-13T17:28:45" }"""
                |> Expect.ok
            Expect.equal actual expected

    module DateTimeUtc =

        [<Fact>]
        let ``Should succeed when value is dateTimeUtc`` () =
            let dt = DateTime(2021, 1, 1, 12, 0, 0, DateTimeKind.Utc)
            Parse.dateTimeUtc
            |> Parser.parse (sprintf "\"%04d-%02d-%02dT%02d:%02d:%02dZ\"" dt.Year dt.Month dt.Day dt.Hour dt.Minute dt.Second)
            |> Expect.ok
            |> Expect.equal dt

        [<Fact>]
        let ``Should parse string as DateTime UTC`` () =
            let now = DateTime(DateOnly(2025, 05, 25), TimeOnly(10, 00))
            let expected = now.ToUniversalTime()
            let actual =
                Prop.req "prop" Parse.dateTimeUtc
                |> Parser.parse $$"""{ "prop": "%%s{{now.ToString("yyyy-MM-ddTHH:mm")}}" }"""
                |> Expect.ok
            Expect.equal actual expected

    module DateTimeExact =

        [<Fact>]
        let ``Should succeed when value is dateTimeExact`` () =
            let dt = DateTime(2021, 1, 1, 12, 0, 0)
            Parse.dateTimeExact "yyyy-MM-dd HH:mm:ss"
            |> Parser.parse "\"2021-01-01 12:00:00\""
            |> Expect.ok
            |> Expect.equal dt

        [<Fact>]
        let ``Should parse string as DateTime exact`` () =
            let expected = DateTime.Parse("2025-05-13T17:28:45")
            let actual =
                Prop.req "prop" (Parse.dateTimeExact "yyyy-MM-dd HH:mm:ss")
                |> Parser.parse """{ "prop": "2025-05-13 17:28:45" }"""
                |> Expect.ok
            Expect.equal actual expected

        [<Fact>]
        let ``Should return Error when parsing a DateTime exact with an incorrect format`` () =
            Prop.req "prop" (Parse.dateTimeExact "yyyy-MM-dd HH:mm:ss")
            |> Parser.parse """{ "prop": "2025-05-13T17:28:45" }"""
            |> Expect.errorString

    module DateTimeOffset =

        [<Fact>]
        let ``Should succeed when value is dateTimeOffset`` () =
            let dto = DateTimeOffset(2021, 1, 1, 12, 0, 0, TimeSpan.FromHours(1.0))
            Parse.dateTimeOffset
            |> Parser.parse (sprintf "\"%04d-%02d-%02dT%02d:%02d:%02d+01:00\"" dto.Year dto.Month dto.Day dto.Hour dto.Minute dto.Second)
            |> Expect.ok
            |> Expect.equal dto

        [<Fact>]
        let ``Should parse string as DateTimeOffset`` () =
            let expected = DateTime.Parse("2025-05-13T17:28:45+02:00")
            let actual =
                Prop.req "prop" Parse.dateTime
                |> Parser.parse """{ "prop": "2025-05-13T17:28:45+02:00" }"""
                |> Expect.ok
            Expect.equal actual expected

    module DateTimeOffsetExact =

        [<Fact>]
        let ``Should succeed when value is dateTimeOffsetExact`` () =
            let dto = DateTimeOffset(2021, 1, 1, 12, 0, 0, TimeSpan.FromHours(1.0))
            Parse.dateTimeOffsetExact "yyyy-MM-dd HH:mm:ss zzz"
            |> Parser.parse "\"2021-01-01 12:00:00 +01:00\""
            |> Expect.ok
            |> Expect.equal dto

        [<Fact>]
        let ``Should parse string as DateTimeOffset exact`` () =
            let expected = DateTimeOffset.Parse("2025-05-13T17:28:00+02:00")
            let actual =
                Prop.req "prop" (Parse.dateTimeOffsetExact "yyyy-MM-dd HH:mm zzz")
                |> Parser.parse """{ "prop": "2025-05-13 17:28 +02:00" }"""
                |> Expect.ok
            Expect.equal actual expected

        [<Fact>]
        let ``Should return Error when parsing a DateTimeOffset exact with an incorrect format`` () =
            Prop.req "prop" (Parse.dateTimeOffsetExact "yyyyMMdd HH:mm:ss zzz")
            |> Parser.parse """{ "prop": "2025-05-13 17:28 +02:00" }"""
            |> Expect.errorString

    module List =

        [<Fact>]
        let ``Should succeed when value is list`` () =
            Parse.list Parse.int
            |> Parser.parse "[1, 2, 3]"
            |> Expect.ok
            |> Expect.equal [1; 2; 3]

        [<Fact>]
        let ``Should parse array as list`` () =
            let expected = [ 1; 2; 3; ]
            let actual: _ list =
                Prop.req "prop" (Parse.list Parse.int)
                |> Parser.parse """{ "prop": [ 1, 2, 3 ] }"""
                |> Expect.ok
            Expect.equal actual expected

    module Array =

        [<Fact>]
        let ``Should succeed when value is array`` () =
            Parse.array Parse.int
            |> Parser.parse "[1, 2, 3]"
            |> Expect.ok
            |> Expect.equal [|1; 2; 3|]

        [<Fact>]
        let ``Should parse array as array`` () =
            let expected = [| 1; 2; 3; |]
            let actual: _ array =
                Prop.req "prop" (Parse.array Parse.int)
                |> Parser.parse """{ "prop": [ 1, 2, 3 ] }"""
                |> Expect.ok
            Expect.equal actual expected

    module Set =

        [<Fact>]
        let ``Should succeed when value is set`` () =
            Parse.set Parse.int
            |> Parser.parse "[1, 2, 3]"
            |> Expect.ok
            |> Expect.equal (Set.ofList [1; 2; 3])

        [<Fact>]
        let ``Should parse array as Set`` () =
            let expected = set [ 1; 2; 3; ]
            let actual: _ Set =
                Prop.req "prop" (Parse.set Parse.int)
                |> Parser.parse """{ "prop": [ 1, 2, 3 ] }"""
                |> Expect.ok
            Expect.equal actual expected

    module Seq =

        [<Fact>]
        let ``Should succeed when value is seq`` () =
            Parse.seq Parse.int
            |> Parser.parse "[1, 2, 3]"
            |> Expect.ok
            |> Expect.equalSeq [1; 2; 3]

        [<Fact>]
        let ``Should parse array as seq`` () =
            let expected = seq [ 1; 2; 3; ]
            let actual: _ seq =
                Prop.req "prop" (Parse.seq Parse.int)
                |> Parser.parse """{ "prop": [ 1, 2, 3 ] }"""
                |> Expect.ok
            Expect.equalSeq actual expected

    module Index =

        [<Fact>]
        let ``Should succeed when value is index`` () =
            Parse.index 1 Parse.int
            |> Parser.parse "[1, 2, 3]"
            |> Expect.ok
            |> Expect.equal 2

        [<Fact>]
        let ``Should parse array at index`` () =
            let expected = 1
            let actual =
                Prop.req "prop" (Parse.index 0 Parse.int)
                |> Parser.parse """{ "prop": [ 1, 2, 3 ] }"""
                |> Expect.ok
            Expect.equal actual expected

        [<Fact>]
        let ``Should return Error when index is out of range`` () =
            Prop.req "prop" (Parse.index 3 Parse.int)
            |> Parser.parse """{ "prop": [ 1, 2, 3 ] }"""
            |> Expect.errorString

        [<Fact>]
        let ``Should return Error when index parsing fails`` () =
            Prop.req "prop" (Parse.index 1 Parse.int)
            |> Parser.parse """{ "prop": [ 1, "2", 3 ] }"""
            |> Expect.errorString

    module Map =

        [<Fact>]
        let ``Should succeed when value is map`` () =
            Parse.map Parse.int
            |> Parser.parse "{\"a\": 1, \"b\": 2}"
            |> Expect.ok
            |> Expect.equal (Map.ofList [("a", 1); ("b", 2)])

        [<Fact>]
        let ``Should parse object as Map`` () =
            let expected = Map.ofSeq [ "key1", 1; "key2", 2; "key3", 3; ]
            let actual: Map<_,_> =
                Prop.req "prop" (Parse.map Parse.int)
                |> Parser.parse """{ "prop": { "key1": 1, "key2": 2, "key3": 3 } }"""
                |> Expect.ok
            Expect.equal actual expected

        [<Fact>]
        let ``Should return Error when parsing Map fails`` () =
            Prop.req "prop" (Parse.map Parse.int)
            |> Parser.parse """{ "prop": { "key1": "1", "key2": "2", "key3": 3 } }"""
            |> Expect.errorString

        [<Fact>]
        let ``Should return Error when parsing Map with duplicate keys`` () =
            Prop.req "prop" (Parse.map Parse.int)
            |> Parser.parse """{ "prop": { "key1": 1, "key1": 1, "key2": 2, "key2": 2, "key3": 3 } }"""
            |> Expect.errorString

    module Dict =

        [<Fact>]
        let ``Should succeed when value is dict`` () =
            Parse.dict Parse.int
            |> Parser.parse "{\"a\": 1, \"b\": 2}"
            |> Expect.ok
            |> (fun r -> Expect.equal (r.["a"]) 1; Expect.equal (r.["b"]) 2)

        [<Fact>]
        let ``Should parse object as Dictionary`` () =
            let expected = dict [ "key1", 1; "key2", 2; "key3", 3; ]
            let actual: IDictionary<_,_> =
                Prop.req "prop" (Parse.dict Parse.int)
                |> Parser.parse """{ "prop": { "key1": 1, "key2": 2, "key3": 3 } }"""
                |> Expect.ok
            Expect.equalSeq actual expected

        [<Fact>]
        let ``Should return Error when parsing Dictionary with duplicate keys`` () =
            Prop.req "prop" (Parse.dict Parse.int)
            |> Parser.parse """{ "prop": { "key1": 1, "key1": 1, "key2": 2, "key2": 2, "key3": 3 } }"""
            |> Expect.errorString

        [<Fact>]
        let ``Should return Error when parsing Dictionary fails`` () =
            Prop.req "prop" (Parse.dict Parse.int)
            |> Parser.parse """{ "prop": { "key1": "1", "key2": "2", "key3": 3 } }"""
            |> Expect.errorString

    module KeyValuePairs =

        [<Fact>]
        let ``Should succeed when value is keyValuePairs`` () =
            Parse.keyValuePairs Parse.int
            |> Parser.parse "{\"a\": 1, \"b\": 2}"
            |> Expect.ok
            |> Expect.equalSeq [KeyValuePair.Create("a", 1); KeyValuePair.Create("b", 2)]

        [<Fact>]
        let ``Should parse object as KeyValuePair seq`` () =
            let expected = seq [ "key1", 1; "key2", 2; "key3", 3; ] |> Seq.map KeyValuePair
            let actual: _ seq =
                Prop.req "prop" (Parse.keyValuePairs Parse.int)
                |> Parser.parse """{ "prop": { "key1": 1, "key2": 2, "key3": 3 } }"""
                |> Expect.ok
            Expect.equalSeq actual expected

        [<Fact>]
        let ``Should return Error when parsing KeyValuePair seq with duplicate keys`` () =
            Prop.req "prop" (Parse.keyValuePairs Parse.int)
            |> Parser.parse """{ "prop": { "key1": 1, "key1": 1, "key2": 2, "key2": 2, "key3": 3 } }"""
            |> Expect.errorString

        [<Fact>]
        let ``Should return Error when parsing KeyValuePairs fails`` () =
            Prop.req "prop" (Parse.keyValuePairs Parse.int)
            |> Parser.parse """{ "prop": { "key1": "1", "key2": "2", "key3": 3 } }"""
            |> Expect.errorString

    module Tuples =

        [<Fact>]
        let ``Should succeed when value is tuples`` () =
            Parse.tuples Parse.int
            |> Parser.parse "{\"a\": 1, \"b\": 2}"
            |> Expect.ok
            |> Expect.equalSeq [("a", 1); ("b", 2)]

        [<Fact>]
        let ``Should parse object as tuple seq`` () =
            let expected = seq [ "key1", 1; "key2", 2; "key3", 3; ]
            let actual: _ seq =
                Prop.req "prop" (Parse.tuples Parse.int)
                |> Parser.parse """{ "prop": { "key1": 1, "key2": 2, "key3": 3 } }"""
                |> Expect.ok
            Expect.equalSeq actual expected

        [<Fact>]
        let ``Should return Error when parsing tuple seq with duplicate keys`` () =
            Prop.req "prop" (Parse.tuples Parse.int)
            |> Parser.parse """{ "prop": { "key1": 1, "key1": 1, "key2": 2, "key2": 2, "key3": 3 } }"""
            |> Expect.errorString

        [<Fact>]
        let ``Should return Error when parsing tuples fails`` () =
            Prop.req "prop" (Parse.tuples Parse.int)
            |> Parser.parse """{ "prop": { "key1": "1", "key2": "2", "key3": 3 } }"""
            |> Expect.errorString

    module Keys =

        [<Fact>]
        let ``Should succeed when value is keys`` () =
            Parse.keys
            |> Parser.parse "{\"a\": 1, \"b\": 2}"
            |> Expect.ok
            |> Expect.equalSeq ["a"; "b"]

        [<Fact>]
        let ``Should parse objects keys as string seq`` () =
            let expected = seq [ "key1"; "key2"; "key3" ]
            let actual: _ seq =
                Prop.req "prop" Parse.keys
                |> Parser.parse """{ "prop": { "key1": 1, "key2": 2, "key3": 3 } }"""
                |> Expect.ok
            Expect.equalSeq actual expected

        [<Fact>]
        let ``Should return Error when trying to parse string seq with duplicate keys`` () =
            Prop.req "prop" Parse.keys
            |> Parser.parse """{ "prop": { "key1": 1, "key1": 1, "key2": 2, "key2": 2, "key3": 3 } }"""
            |> Expect.errorString

    module Tuple2 =

        [<Fact>]
        let ``Should succeed when value is tuple2`` () =
            Parse.tuple2 Parse.int Parse.string
            |> Parser.parse "[1, \"a\"]"
            |> Expect.ok
            |> Expect.equal (1, "a")

        [<Fact>]
        let ``Should parse array as tuple`` () =
            let expected = "1", 1
            let actual =
                Prop.req "prop" (Parse.tuple2 Parse.string Parse.int)
                |> Parser.parse """{ "prop": [ "1", 1 ] }"""
                |> Expect.ok
            Expect.equal actual expected

        [<Fact>]
        let ``Should return Error when parsing tuple with incorrect length`` () =
            Prop.req "prop" (Parse.tuple2 Parse.string Parse.int)
            |> Parser.parse """{ "prop": [ 1, 1, 1 ] }"""
            |> Expect.errorString

        [<Fact>]
        let ``Should return Error when tuple2 parsing fails`` () =
            Prop.req "prop" (Parse.tuple2 Parse.int Parse.int)
            |> Parser.parse """{ "prop": [ "1", "1" ] }"""
            |> Expect.errorString

    module Tuple3 =

        [<Fact>]
        let ``Should succeed when value is tuple3`` () =
            Parse.tuple3 Parse.int Parse.string Parse.bool
            |> Parser.parse "[1, \"a\", true]"
            |> Expect.ok
            |> Expect.equal (1, "a", true)

        [<Fact>]
        let ``Should parse array as tuple of three`` () =
            let expected = "1", 1, 1
            let actual =
                Prop.req "prop" (Parse.tuple3 Parse.string Parse.int Parse.int)
                |> Parser.parse """{ "prop": [ "1", 1, 1 ] }"""
                |> Expect.ok
            Expect.equal actual expected

        [<Fact>]
        let ``Should return Error when tuple3 parsing fails`` () =
            Prop.req "prop" (Parse.tuple3 Parse.int Parse.int Parse.int)
            |> Parser.parse """{ "prop": [ "1", "1", 1 ] }"""
            |> Expect.errorString

    module Tuple4 =

        [<Fact>]
        let ``Should succeed when value is tuple4`` () =
            Parse.tuple4 Parse.int Parse.string Parse.bool Parse.float
            |> Parser.parse "[1, \"a\", true, 1.1]"
            |> Expect.ok
            |> Expect.equal (1, "a", true, 1.1)

        [<Fact>]
        let ``Should parse array as tuple of four`` () =
            let expected = "1", 1, 1, 1
            let actual =
                Prop.req "prop" (Parse.tuple4 Parse.string Parse.int Parse.int Parse.int)
                |> Parser.parse """{ "prop": [ "1", 1, 1, 1 ] }"""
                |> Expect.ok
            Expect.equal actual expected

        [<Fact>]
        let ``Should return Error when tuple4 parsing fails`` () =
            Prop.req "prop" (Parse.tuple4 Parse.int Parse.int Parse.int Parse.int)
            |> Parser.parse """{ "prop": [ "1", "1", 1, 1 ] }"""
            |> Expect.errorString

    module Tuple5 =

        [<Fact>]
        let ``Should succeed when value is tuple5`` () =
            Parse.tuple5 Parse.int Parse.string Parse.bool Parse.float Parse.int
            |> Parser.parse "[1, \"a\", true, 1.1, 2]"
            |> Expect.ok
            |> Expect.equal (1, "a", true, 1.1, 2)

        [<Fact>]
        let ``Should parse array as tuple of five`` () =
            let expected = "1", 1, 1, 1, 1
            let actual =
                Prop.req "prop" (Parse.tuple5 Parse.string Parse.int Parse.int Parse.int Parse.int)
                |> Parser.parse """{ "prop": [ "1", 1, 1, 1, 1 ] }"""
                |> Expect.ok
            Expect.equal actual expected

        [<Fact>]
        let ``Should return Error when tuple5 parsing fails`` () =
            Prop.req "prop" (Parse.tuple5 Parse.int Parse.int Parse.int Parse.int Parse.int)
            |> Parser.parse """{ "prop": [ "1", "1", 1, 1, 1 ] }"""
            |> Expect.errorString

    module OneOf =

        type TestDu =
            | A of int * int
            | B of {| Prop: string |}
            | C

        [<Fact>]
        let ``Should parse one-of as discriminated union`` () =
            let expected = A (1, 2)
            let actual =
                Prop.req "prop" (Parse.oneOf "disc" [
                    "a", parser {
                        let! a = Prop.req "prop2" Parse.int
                        let! b = Prop.req "prop3" Parse.int

                        return A (a, b)
                    }
                ])
                |> Parser.parse """{ "prop": { "disc": "a", "prop2": 1, "prop3": 2 } }"""
                |> Expect.ok
            Expect.equal actual expected

        [<Fact>]
        let ``Should return Error when one-of parser fails`` () =
            let a =
                parser {
                    let! a = Prop.req "prop2" Parse.int
                    let! b = Prop.req "prop3" Parse.int

                    return a, b
                }
            Prop.req "prop" (Parse.oneOf "disc" [ "a", a ])
            |> Parser.parse """{ "prop": { "disc": "a", "prop2": "1", "prop3": 2 } }"""
            |> Expect.errorString

        [<Fact>]
        let ``Should return Error when one-of is not an object`` () =
            Prop.req "prop" (Parse.oneOf "disc" [ "missing", Parse.int ])
            |> Parser.parse """{ "prop": [] }"""
            |> Expect.errorString

        [<Fact>]
        let ``Should return Error when one-of discriminator is not matched`` () =
            Prop.req "prop" (Parse.oneOf "disc" [ "missing", Parse.int ])
            |> Parser.parse """{ "prop": { "disc": "a", "prop2": 1, "prop3": 2 } }"""
            |> Expect.errorString

        [<Fact>]
        let ``Should return Error when one-of discriminator is not found`` () =
            Prop.req "prop" (Parse.oneOf "missing" [ "a", Parse.int ])
            |> Parser.parse """{ "prop": { "disc": "a", "prop2": 1, "prop3": 2 } }"""
            |> Expect.errorString

    module Self =

        type Tree =
            | Leaf of int
            | Branch of Tree * Tree

        [<Fact>]
        let ``Should parse one-of as recursive discriminated union`` () =
            let expected = Branch (Leaf 1, Branch (Leaf 2, Leaf 3))
            let actual =
                Parse.self (fun self ->
                    Parse.oneOf "type" [
                        "leaf",
                            parser {
                                let! value = Prop.req "value" Parse.int
                                return Leaf value
                            }
                        "branch",
                            parser {
                                let! left = Prop.req "left" self
                                let! right = Prop.req "right" self
                                return Branch (left, right)
                            }
                    ]
                )
                |> Parser.parse
                   """
                        {
                            "type": "branch",
                            "left": { "type": "leaf", "value": 1 },
                            "right": {
                                "type": "branch",
                                "left": { "type": "leaf", "value": 2 },
                                "right": { "type": "leaf", "value": 3 }
                            }
                        }
                   """
                |> Expect.ok
            Expect.equal actual expected

    module Attempt =

        type TestDu =
            | A of int * int
            | B of {| Prop: string |}
            | C

        [<Fact>]
        let ``Should succeed when value is attempt`` () =
            Parse.attempt [Parse.int |> Parser.map box; Parse.bool |> Parser.map box]
            |> Parser.parse "true"
            |> Expect.ok
            |> Expect.equal (box true)

        [<Fact>]
        let ``Should attempt to parse discriminated union`` () =
            let a =
                parser {
                    let! a = Prop.req "prop2" Parse.int
                    let! b = Prop.req "prop3" Parse.int

                    return A (a, b)
                }

            let b = parser {
                let! a = Prop.req "missing" Parse.string

                return B {| Prop = a |}
            }

            let expected = A (1, 2)
            let actual =
                Prop.req "prop" (Parse.attempt [ b; a ])
                |> Parser.parse """{ "prop": { "prop2": 1, "prop3": 2 } }"""
                |> Expect.ok
            Expect.equal actual expected

        [<Fact>]
        let ``Should return error when all attempts fail`` () =
            Prop.req "prop" (Parse.attempt [ Parser.fail "first"; Parser.fail "second"; Parser.fail "third" ])
            |> Parser.parse """{ "prop": { "prop2": 1, "prop3": 2 } }"""
            |> Expect.errorString

    module Nil =

        [<Fact>]
        let ``Should succeed and return default when value is null`` () =
            Parse.nil Parse.int 0
            |> Parser.parse "null"
            |> Expect.ok
            |> Expect.equal 0

        [<Fact>]
        let ``Should return default value when value is null`` () =
            let expected = 1
            let actual =
                Prop.req "prop" (Parse.nil Parse.int 1)
                |> Parser.parse """{ "prop": null }"""
                |> Expect.ok
            Expect.equal actual expected

        [<Fact>]
        let ``Should not return default value when value is not null`` () =
            let expected = 2
            let actual =
                Prop.req "prop" (Parse.nil Parse.int 1)
                |> Parser.parse """{ "prop": 2 }"""
                |> Expect.ok
            Expect.equal actual expected

    module Option =

        [<Fact>]
        let ``Should succeed when value is option`` () =
            Parse.option Parse.int
            |> Parser.parse "1"
            |> Expect.ok
            |> Expect.equal (Some 1)

        [<Fact>]
        let ``Should parse optional null value as None`` () =
            let expected = None
            let actual =
                Prop.req "prop" (Parse.option Parse.int)
                |> Parser.parse """{ "prop": null }"""
                |> Expect.ok
            Expect.equal actual expected

        [<Fact>]
        let ``Should parse optional value as Some int`` () =
            let expected = Some 1
            let actual =
                Prop.req "prop" (Parse.option Parse.int)
                |> Parser.parse """{ "prop": 1 }"""
                |> Expect.ok
            Expect.equal actual expected

    module Refine =

        [<Fact>]
        let ``Should succeed when value is refine`` () =
            Parse.refine Parse.int (fun x -> if x > 0 then Ok x else Error "Must be positive")
            |> Parser.parse "1"
            |> Expect.ok
            |> Expect.equal 1

        [<Fact>]
        let ``Should return Error when refined value is not valid`` () =
            Prop.req "prop" (Parse.refine Parse.int (fun _ -> Error "Not valid."))
            |> Parser.parse """{ "prop": 1 }"""
            |> Expect.errorString

        [<Fact>]
        let ``Should return Ok when refined value is valid`` () =
            let expected = 1
            let actual =
                Prop.req "prop" (Parse.refine Parse.int Ok)
                |> Parser.parse """{ "prop": 1 }"""
                |> Expect.ok
            Expect.equal actual expected

    module Verify =

        [<Fact>]
        let ``Should succeed when value is verify`` () =
            Parse.verify Parse.int (fun x -> x > 0) "Must be positive"
            |> Parser.parse "1"
            |> Expect.ok
            |> Expect.equal 1

        [<Fact>]
        let ``Should return Error when verified value is not valid`` () =
            Prop.req "prop" (Parse.verify Parse.int (fun x -> x > 0) "Not valid.")
            |> Parser.parse """{ "prop": 0 }"""
            |> Expect.errorString

        [<Fact>]
        let ``Should return Ok when verified value is valid`` () =
            let expected = 1
            let actual =
                Prop.req "prop" (Parse.verify Parse.int (fun x -> x > 0) "Error")
                |> Parser.parse """{ "prop": 1 }"""
                |> Expect.ok
            Expect.equal actual expected

    module Kind =

        [<Fact>]
        let ``Should succeed when value is kind`` () =
            Parse.kind
            |> Parser.parse "1"
            |> Expect.ok
            |> Expect.equal JsonValueKind.Number

        [<Fact>]
        let ``Should parse element kind as JsonValueKind`` () =
            let expected = JsonValueKind.Number
            let actual =
                Prop.req "prop" Parse.kind
                |> Parser.parse """{ "prop": 1 }"""
                |> Expect.ok
            Expect.equal actual expected

    module Element =

        [<Fact>]
        let ``Should succeed when value is element`` () =
            Parse.element
            |> Parser.parse "1"
            |> Expect.ok
            |> (fun e -> Expect.equal e.ValueKind JsonValueKind.Number)

        [<Fact>]
        let ``Should parse element as JsonElement`` () =
            let expected = JsonDocument.Parse("""{ "prop2": 1 }""").RootElement
            let actual =
                Prop.req "prop" Parse.element
                |> Parser.parse """{ "prop": { "prop2": 1 } }"""
                |> Expect.ok
            Expect.isTrue <| JsonElement.DeepEquals(expected, actual)

    module RawText =

        [<Fact>]
        let ``Should succeed when value is rawText`` () =
            Parse.rawText
            |> Parser.parse "{\"a\": 1}"
            |> Expect.ok
            |> Expect.equal "{\"a\": 1}"

        [<Fact>]
        let ``Should parse element as string`` () =
            let expected = """{ "prop2": 1 }"""
            let actual =
                Prop.req "prop" Parse.rawText
                |> Parser.parse """{ "prop": { "prop2": 1 } }"""
                |> Expect.ok
            Expect.equal actual expected

    module ArrayLength =

        [<Fact>]
        let ``Should succeed when value is arrayLength`` () =
            Parse.arrayLength
            |> Parser.parse "[1, 2, 3]"
            |> Expect.ok
            |> Expect.equal 3

        [<Fact>]
        let ``Should parse array length as int`` () =
            let expected = 3
            let actual =
                Prop.req "prop" Parse.arrayLength
                |> Parser.parse """{ "prop": [ 1, 2, 3 ] }"""
                |> Expect.ok
            Expect.equal actual expected

    module PropertyCount =

        [<Fact>]
        let ``Should succeed when value is propertyCount`` () =
            Parse.propertyCount
            |> Parser.parse "{\"a\": 1, \"b\": 2}"
            |> Expect.ok
            |> Expect.equal 2

        [<Fact>]
        let ``Should parse object property count as int`` () =
            let expected = 3
            let actual =
                Prop.req "prop" Parse.propertyCount
                |> Parser.parse """{ "prop": { "one": 1, "two": 2, "three": 3 } }"""
                |> Expect.ok
            Expect.equal actual expected