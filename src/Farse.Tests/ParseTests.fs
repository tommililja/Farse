namespace Farse.Tests

open System
open System.Collections.Generic
open System.Numerics
open System.Text.Json
open Expecto.Flip
open Xunit
open Farse

module ParseTests =

    module Custom =

        [<Fact>]
        let ``Should parse element with custom parser`` () =
            let expected = 1
            let actual =
                Parse.custom (_.GetInt32() >> Ok) ExpectedKind.Number
                |> Parser.parse "1"
                |> Expect.wantOk $"Expected %s{nameof Parser.parse} to succeed."
            Expect.equal Msg.none actual expected

        [<Fact>]
        let ``Should parse with value with custom parser when ExpectedKind is Any`` () =
            let expected = 1
            let actual =
                Parse.custom (fun element ->
                    match element.TryGetInt32() with
                    | true, x -> Ok x
                    | _ -> Error "Invalid value."
                ) ExpectedKind.Any
                |> Parser.parse "1"
                |> Expect.wantOk $"Expected %s{nameof Parser.parse} to succeed."
            Expect.equal Msg.none actual expected

        [<Fact>]
        let ``Should fail when element is not the right kind`` () =
            Parse.custom (fun element ->
                match element.TryGetInt32() with
                | true, x -> Ok x
                | _ -> Error "Invalid value."
            ) ExpectedKind.Number
            |> Parser.parse "true"
            |> Expect.wantErrorString

        [<Fact>]
        let ``Should fail when value is invalid`` () =
            Parse.custom (fun element ->
                match element.TryGetInt32() with
                | true, x -> Ok x
                | _ -> Error "Invalid value."
            ) ExpectedKind.Number
            |> Parser.parse "1.1"
            |> Expect.wantErrorString

        [<Fact>]
        let ``Should fail when a exception is thrown`` () =
            Parse.custom (_.GetString() >> Ok) ExpectedKind.Number
            |> Parser.parse "1"
            |> Expect.wantErrorString

    module Int =

        [<Fact>]
        let ``Should parse number as int`` () =
            let expected = Int32.MaxValue
            let actual =
                Parse.int
                |> Parser.parse "2147483647"
                |> Expect.wantOk $"Expected %s{nameof Parser.parse} to succeed."
            Expect.equal Msg.none actual expected

        [<Fact>]
        let ``Should fail when value is invalid`` () =
            Parse.int
            |> Parser.parse "1.1"
            |> Expect.wantErrorString

        [<Fact>]
        let ``Should fail when value is not a number`` () =
            Parse.int
            |> Parser.parse "true"
            |> Expect.wantErrorString

    module Int16 =

        [<Fact>]
        let ``Should parse number as int16`` () =
            let expected = Int16.MaxValue
            let actual =
                Parse.int16
                |> Parser.parse "32767"
                |> Expect.wantOk $"Expected %s{nameof Parser.parse} to succeed."
            Expect.equal Msg.none actual expected

        [<Fact>]
        let ``Should fail when value is invalid`` () =
            Parse.int16
            |> Parser.parse "1.1"
            |> Expect.wantErrorString

        [<Fact>]
        let ``Should fail when value is not a number`` () =
            Parse.int16
            |> Parser.parse "true"
            |> Expect.wantErrorString

    module Int64 =

        [<Fact>]
        let ``Should parse number as int64`` () =
            let expected = Int64.MaxValue
            let actual =
                Parse.int64
                |> Parser.parse "9223372036854775807"
                |> Expect.wantOk $"Expected %s{nameof Parser.parse} to succeed."
            Expect.equal Msg.none actual expected

        [<Fact>]
        let ``Should fail when value is invalid`` () =
            Parse.int64
            |> Parser.parse "1.1"
            |> Expect.wantErrorString

        [<Fact>]
        let ``Should fail when value is not a number`` () =
            Parse.int64
            |> Parser.parse "true"
            |> Expect.wantErrorString

    module UInt16 =

        [<Fact>]
        let ``Should parse number as uint16`` () =
            let expected = UInt16.MaxValue
            let actual =
                Parse.uint16
                |> Parser.parse "65535"
                |> Expect.wantOk $"Expected %s{nameof Parser.parse} to succeed."
            Expect.equal Msg.none actual expected

        [<Fact>]
        let ``Should fail when value is invalid`` () =
            Parse.uint16
            |> Parser.parse "1.1"
            |> Expect.wantErrorString

        [<Fact>]
        let ``Should fail when value is not a number`` () =
            Parse.uint16
            |> Parser.parse "true"
            |> Expect.wantErrorString

    module UInt32 =

        [<Fact>]
        let ``Should parse number as uint32`` () =
            let expected = UInt32.MaxValue
            let actual =
                Parse.uint32
                |> Parser.parse "4294967295"
                |> Expect.wantOk $"Expected %s{nameof Parser.parse} to succeed."
            Expect.equal Msg.none actual expected

        [<Fact>]
        let ``Should fail when value is invalid`` () =
            Parse.uint32
            |> Parser.parse "1.1"
            |> Expect.wantErrorString

        [<Fact>]
        let ``Should fail when value is not a number`` () =
            Parse.uint32
            |> Parser.parse "true"
            |> Expect.wantErrorString

    module UInt64 =

        [<Fact>]
        let ``Should parse number as uint64`` () =
            let expected = UInt64.MaxValue
            let actual =
                Parse.uint64
                |> Parser.parse "18446744073709551615"
                |> Expect.wantOk $"Expected %s{nameof Parser.parse} to succeed."
            Expect.equal Msg.none actual expected

        [<Fact>]
        let ``Should fail when value is invalid`` () =
            Parse.uint64
            |> Parser.parse "1.1"
            |> Expect.wantErrorString

        [<Fact>]
        let ``Should fail when value is not a number`` () =
            Parse.uint64
            |> Parser.parse "true"
            |> Expect.wantErrorString

    module Float =

        [<Fact>]
        let ``Should parse number as float`` () =
            let expected = Double.MaxValue
            let actual =
                Parse.float
                |> Parser.parse "1.7976931348623157E+308"
                |> Expect.wantOk $"Expected %s{nameof Parser.parse} to succeed."
            Expect.equal Msg.none actual expected

        [<Fact>]
        let ``Should fail when value is not a number`` () =
            Parse.float
            |> Parser.parse "true"
            |> Expect.wantErrorString

    module Float32 =

        [<Fact>]
        let ``Should parse number as float32`` () =
            let expected = Single.MaxValue
            let actual =
                Parse.float32
                |> Parser.parse "3.40282346639e+38"
                |> Expect.wantOk $"Expected %s{nameof Parser.parse} to succeed."
            Expect.equal Msg.none actual expected

        [<Fact>]
        let ``Should fail when value is not a number`` () =
            Parse.float32
            |> Parser.parse "true"
            |> Expect.wantErrorString

    module Decimal =

        [<Fact>]
        let ``Should parse number as decimal`` () =
            let expected = Decimal.MaxValue
            let actual =
                Parse.decimal
                |> Parser.parse (string Decimal.MaxValue)
                |> Expect.wantOk $"Expected %s{nameof Parser.parse} to succeed."
            Expect.equal Msg.none actual expected

        [<Fact>]
        let ``Should fail when value is invalid`` () =
            Parse.decimal
            |> Parser.parse "1e999"
            |> Expect.wantErrorString

        [<Fact>]
        let ``Should fail when value is not a number`` () =
            Parse.decimal
            |> Parser.parse "true"
            |> Expect.wantErrorString

    module Byte =

        [<Fact>]
        let ``Should parse number as byte`` () =
            let expected = Byte.MaxValue
            let actual =
                Parse.byte
                |> Parser.parse "255"
                |> Expect.wantOk $"Expected %s{nameof Parser.parse} to succeed."
            Expect.equal Msg.none actual expected

        [<Fact>]
        let ``Should fail when value is invalid`` () =
            Parse.byte
            |> Parser.parse "256"
            |> Expect.wantErrorString

        [<Fact>]
        let ``Should fail when value is not a number`` () =
            Parse.byte
            |> Parser.parse "true"
            |> Expect.wantErrorString

    module SByte =

        [<Fact>]
        let ``Should parse number as sbyte`` () =
            let expected = SByte.MaxValue
            let actual =
                Parse.sbyte
                |> Parser.parse "127"
                |> Expect.wantOk $"Expected %s{nameof Parser.parse} to succeed."
            Expect.equal Msg.none actual expected

        [<Fact>]
        let ``Should fail when value is invalid`` () =
            Parse.sbyte
            |> Parser.parse "128"
            |> Expect.wantErrorString

        [<Fact>]
        let ``Should fail when value is not a number`` () =
            Parse.sbyte
            |> Parser.parse "true"
            |> Expect.wantErrorString

    module Char =

        [<Fact>]
        let ``Should parse string as char`` () =
            let expected = 'a'
            let actual =
                Parse.char
                |> Parser.parse "\"a\""
                |> Expect.wantOk $"Expected %s{nameof Parser.parse} to succeed."
            Expect.equal Msg.none actual expected

        [<Fact>]
        let ``Should fail when value is invalid`` () =
            Parse.char
            |> Parser.parse "\"ab\""
            |> Expect.wantErrorString

        [<Fact>]
        let ``Should fail when element is not a string`` () =
            Parse.char
            |> Parser.parse "1"
            |> Expect.wantErrorString

    module String =

        [<Fact>]
        let ``Should parse string as string`` () =
            let expected = "value"
            let actual =
                Parse.string
                |> Parser.parse "\"value\""
                |> Expect.wantOk $"Expected %s{nameof Parser.parse} to succeed."
            Expect.equal Msg.none actual expected

        [<Fact>]
        let ``Should fail when element is not a string`` () =
            Parse.string
            |> Parser.parse "1"
            |> Expect.wantErrorString

    module StringNonEmpty =

        [<Fact>]
        let ``Should parse string as non-empty string`` () =
            let expected = "value"
            let actual =
                Parse.stringNonEmpty
                |> Parser.parse "\"value\""
                |> Expect.wantOk $"Expected %s{nameof Parser.parse} to succeed."
            Expect.equal Msg.none actual expected

        [<Fact>]
        let ``Should fail when string is empty`` () =
            Parse.stringNonEmpty
            |> Parser.parse "\"\""
            |> Expect.wantErrorString

        [<Fact>]
        let ``Should fail when string is whitespace`` () =
            Parse.stringNonEmpty
            |> Parser.parse "\"  \""
            |> Expect.wantErrorString

        [<Fact>]
        let ``Should fail when element is not a string`` () =
            Parse.stringNonEmpty
            |> Parser.parse "1"
            |> Expect.wantErrorString

    module Regex =

        [<Fact>]
        let ``Should parse string as string with regex`` () =
            let expected = "12345"
            let actual =
                Parse.regex "^[0-9]+$"
                |> Parser.parse "\"12345\""
                |> Expect.wantOk $"Expected %s{nameof Parser.parse} to succeed."
            Expect.equal Msg.none actual expected

        [<Fact>]
        let ``Should fail when regex is invalid`` () =
            Parse.regex "^[0-9+$"
            |> Parser.parse "\"abc\""
            |> Expect.wantErrorString

        [<Fact>]
        let ``Should fail when element is not a string`` () =
            Parse.regex "^[0-9]+$"
            |> Parser.parse "1"
            |> Expect.wantErrorString

        [<Fact>]
        let ``Should fail when regex does not match`` () =
            Parse.regex "^[0-9]+$"
            |> Parser.parse "\"abc\""
            |> Expect.wantErrorString

    module Number =

        [<Fact>]
        let ``Should parse string as bigint`` () =
            let expected = BigInteger.Parse("1")
            let actual =
                Parse.number<bigint>
                |> Parser.parse "\"1\""
                |> Expect.wantOk $"Expected %s{nameof Parser.parse} to succeed."
            Expect.equal Msg.none actual expected

        [<Fact>]
        let ``Should fail when value is invalid`` () =
            Parse.number<int>
            |> Parser.parse "\"a\""
            |> Expect.wantErrorString

        [<Fact>]
        let ``Should fail when element is not a string`` () =
            Parse.number<int>
            |> Parser.parse "1"
            |> Expect.wantErrorString

    module Base64Bytes =

        [<Fact>]
        let ``Should parse string as base64 byte array`` () =
            let actual = Convert.FromBase64String("aGVsbG8gc3RyYW5nZXIh")
            let expected =
                Parse.base64Bytes
                |> Parser.parse "\"aGVsbG8gc3RyYW5nZXIh\""
                |> Expect.wantOk $"Expected %s{nameof Parser.parse} to succeed."
            Expect.equal Msg.none actual expected

        [<Fact>]
        let ``Should fail when element is not a string`` () =
            Parse.base64Bytes
            |> Parser.parse "1"
            |> Expect.wantErrorString

        [<Fact>]
        let ``Should fail when parsing fails`` () =
            Parse.base64Bytes
            |> Parser.parse "\"abc\""
            |> Expect.wantErrorString

    module BigInt =

        [<Fact>]
        let ``Should parse number as bigint`` () =
            let expected = BigInteger.Parse("123456789012345678901234567890")
            let actual =
                Parse.bigint
                |> Parser.parse "123456789012345678901234567890"
                |> Expect.wantOk $"Expected %s{nameof Parser.parse} to succeed."
            Expect.equal Msg.none actual expected

        [<Fact>]
        let ``Should fail when value is invalid`` () =
            Parse.bigint
            |> Parser.parse "1.1"
            |> Expect.wantErrorString

        [<Fact>]
        let ``Should fail when element is not a number`` () =
            Parse.bigint
            |> Parser.parse "true"
            |> Expect.wantErrorString

    module Bool =

        [<Fact>]
        let ``Should parse true as bool`` () =
            let expected = true
            let actual =
                Parse.bool
                |> Parser.parse "true"
                |> Expect.wantOk $"Expected %s{nameof Parser.parse} to succeed."
            Expect.equal Msg.none actual expected

        [<Fact>]
        let ``Should fail when element is not a bool`` () =
            Parse.bool
            |> Parser.parse "1"
            |> Expect.wantErrorString

    module Guid =

        [<Fact>]
        let ``Should parse string as Guid`` () =
            let expected = Guid.Parse("fb245a37-2de1-4cc5-b41b-1c6e68866b68")
            let actual =
                Parse.guid
                |> Parser.parse "\"fb245a37-2de1-4cc5-b41b-1c6e68866b68\""
                |> Expect.wantOk $"Expected %s{nameof Parser.parse} to succeed."
            Expect.equal Msg.none actual expected

        [<Fact>]
        let ``Should fail when value is invalid`` () =
            Parse.guid
            |> Parser.parse "\"invalid\""
            |> Expect.wantErrorString

        [<Fact>]
        let ``Should fail when element is not a string`` () =
            Parse.guid
            |> Parser.parse "1"
            |> Expect.wantErrorString

    module Unit =

        [<Fact>]
        let ``Should parse null as unit`` () =
            let expected = ()
            let actual =
                Parse.unit
                |> Parser.parse "null"
                |> Expect.wantOk $"Expected %s{nameof Parser.parse} to succeed."
            Expect.equal Msg.none actual expected

    module None =

        [<Fact>]
        let ``Should not parse element and return unit`` () =
            let expected = ()
            let actual =
                Parse.none
                |> Parser.parse "1"
                |> Expect.wantOk $"Expected %s{nameof Parser.parse} to succeed."
            Expect.equal Msg.none actual expected

    module Enum =

        type StringEnum =
            | A = 0

        [<Fact>]
        let ``Should parse number as string enum`` () =
            let expected = StringEnum.A
            let actual =
                Parse.enum
                |> Parser.parse "\"A\""
                |> Expect.wantOk $"Expected %s{nameof Parser.parse} to succeed."
            Expect.equal Msg.none actual expected

        [<Fact>]
        let ``Should fail when value is invalid`` () =
            Parse.enum<StringEnum>
            |> Parser.parse "\"B\""
            |> Expect.wantErrorString

        [<Fact>]
        let ``Should fail when element is not a string`` () =
            Parse.enum<StringEnum>
            |> Parser.parse "true"
            |> Expect.wantErrorString

    module IntEnum =

        type IntEnum =
            | A = 1

        [<Fact>]
        let ``Should parse number as int enum`` () =
            let expected = IntEnum.A
            let actual =
                Parse.intEnum
                |> Parser.parse "1"
                |> Expect.wantOk $"Expected %s{nameof Parser.parse} to succeed."
            Expect.equal Msg.none actual expected

        [<Fact>]
        let ``Should fail when value is invalid`` () =
            Parse.intEnum<IntEnum>
            |> Parser.parse "2"
            |> Expect.wantErrorString

        [<Fact>]
        let ``Should fail when parsing fails`` () =
            Parse.intEnum<IntEnum>
            |> Parser.parse "1.1"
            |> Expect.wantErrorString

        [<Fact>]
        let ``Should fail when element is not a number`` () =
            Parse.intEnum<IntEnum>
            |> Parser.parse "true"
            |> Expect.wantErrorString

    module Int16Enum =

        type Int16Enum =
            | A = 1s

        [<Fact>]
        let ``Should parse number as int16 enum`` () =
            let expected = Int16Enum.A
            let actual =
                Parse.int16Enum
                |> Parser.parse "1"
                |> Expect.wantOk $"Expected %s{nameof Parser.parse} to succeed."
            Expect.equal Msg.none actual expected

        [<Fact>]
        let ``Should fail when value is invalid`` () =
            Parse.int16Enum<Int16Enum>
            |> Parser.parse "2"
            |> Expect.wantErrorString

        [<Fact>]
        let ``Should fail when parsing fails`` () =
            Parse.int16Enum<Int16Enum>
            |> Parser.parse "1.1"
            |> Expect.wantErrorString

        [<Fact>]
        let ``Should fail when element is not a number`` () =
            Parse.int16Enum<Int16Enum>
            |> Parser.parse "true"
            |> Expect.wantErrorString

    module Int64Enum =

        type Int64Enum =
            | A = 1L

        [<Fact>]
        let ``Should parse number as int64 enum`` () =
            let expected = Int64Enum.A
            let actual =
                Parse.int64Enum
                |> Parser.parse "1"
                |> Expect.wantOk $"Expected %s{nameof Parser.parse} to succeed."
            Expect.equal Msg.none actual expected

        [<Fact>]
        let ``Should fail when value is invalid`` () =
            Parse.int64Enum<Int64Enum>
            |> Parser.parse "2"
            |> Expect.wantErrorString

        [<Fact>]
        let ``Should fail when parsing fails`` () =
            Parse.int64Enum<Int64Enum>
            |> Parser.parse "1.1"
            |> Expect.wantErrorString

        [<Fact>]
        let ``Should fail when element is not a number`` () =
            Parse.int64Enum<Int64Enum>
            |> Parser.parse "true"
            |> Expect.wantErrorString

    module UInt16Enum =

        type UInt16Enum =
            | A = 1us

        [<Fact>]
        let ``Should parse number as uint16 enum`` () =
            let expected = UInt16Enum.A
            let actual =
                Parse.uint16Enum
                |> Parser.parse "1"
                |> Expect.wantOk $"Expected %s{nameof Parser.parse} to succeed."
            Expect.equal Msg.none actual expected

        [<Fact>]
        let ``Should fail when value is invalid`` () =
            Parse.uint16Enum<UInt16Enum>
            |> Parser.parse "2"
            |> Expect.wantErrorString

        [<Fact>]
        let ``Should fail when parsing fails`` () =
            Parse.uint16Enum<UInt16Enum>
            |> Parser.parse "1.1"
            |> Expect.wantErrorString

        [<Fact>]
        let ``Should fail when element is not a number`` () =
            Parse.uint16Enum<UInt16Enum>
            |> Parser.parse "true"
            |> Expect.wantErrorString

    module UInt32Enum =

        type UInt32Enum =
            | A = 1u

        [<Fact>]
        let ``Should parse number as uint32 enum`` () =
            let expected = UInt32Enum.A
            let actual =
                Parse.uint32Enum
                |> Parser.parse "1"
                |> Expect.wantOk $"Expected %s{nameof Parser.parse} to succeed."
            Expect.equal Msg.none actual expected

        [<Fact>]
        let ``Should fail when value is invalid`` () =
            Parse.uint32Enum<UInt32Enum>
            |> Parser.parse "2"
            |> Expect.wantErrorString

        [<Fact>]
        let ``Should fail when parsing fails`` () =
            Parse.uint32Enum<UInt32Enum>
            |> Parser.parse "1.1"
            |> Expect.wantErrorString

        [<Fact>]
        let ``Should fail when element is not a number`` () =
            Parse.uint32Enum<UInt32Enum>
            |> Parser.parse "true"
            |> Expect.wantErrorString

    module UInt64Enum =

        type UInt64Enum =
            | A = 1UL

        [<Fact>]
        let ``Should parse number as uint64 enum`` () =
            let expected = UInt64Enum.A
            let actual =
                Parse.uint64Enum
                |> Parser.parse "1"
                |> Expect.wantOk $"Expected %s{nameof Parser.parse} to succeed."
            Expect.equal Msg.none actual expected

        [<Fact>]
        let ``Should fail when value is invalid`` () =
            Parse.uint64Enum<UInt64Enum>
            |> Parser.parse "2"
            |> Expect.wantErrorString

        [<Fact>]
        let ``Should fail when parsing fails`` () =
            Parse.uint64Enum<UInt64Enum>
            |> Parser.parse "1.1"
            |> Expect.wantErrorString

        [<Fact>]
        let ``Should fail when element is not a number`` () =
            Parse.uint64Enum<UInt64Enum>
            |> Parser.parse "true"
            |> Expect.wantErrorString

    module ByteEnum =

        type ByteEnum =
            | A = 1uy

        [<Fact>]
        let ``Should parse number as byte enum`` () =
            let expected = ByteEnum.A
            let actual =
                Parse.byteEnum
                |> Parser.parse "1"
                |> Expect.wantOk $"Expected %s{nameof Parser.parse} to succeed."
            Expect.equal Msg.none actual expected

        [<Fact>]
        let ``Should fail when value is invalid`` () =
            Parse.byteEnum<ByteEnum>
            |> Parser.parse "2"
            |> Expect.wantErrorString

        [<Fact>]
        let ``Should fail when parsing fails`` () =
            Parse.byteEnum<ByteEnum>
            |> Parser.parse "1.1"
            |> Expect.wantErrorString

        [<Fact>]
        let ``Should fail when element is not a number`` () =
            Parse.byteEnum<ByteEnum>
            |> Parser.parse "true"
            |> Expect.wantErrorString

    module SByteEnum =

        type SByteEnum =
            | A = 1y

        [<Fact>]
        let ``Should parse number as sbyte enum`` () =
            let expected = SByteEnum.A
            let actual =
                Parse.sbyteEnum
                |> Parser.parse "1"
                |> Expect.wantOk $"Expected %s{nameof Parser.parse} to succeed."
            Expect.equal Msg.none actual expected

        [<Fact>]
        let ``Should fail when value is invalid`` () =
            Parse.sbyteEnum<SByteEnum>
            |> Parser.parse "2"
            |> Expect.wantErrorString

        [<Fact>]
        let ``Should fail when parsing fails`` () =
            Parse.sbyteEnum<SByteEnum>
            |> Parser.parse "1.1"
            |> Expect.wantErrorString

        [<Fact>]
        let ``Should fail when element is not a number`` () =
            Parse.sbyteEnum<SByteEnum>
            |> Parser.parse "true"
            |> Expect.wantErrorString

    module TimeOnly =

        [<Fact>]
        let ``Should parse string as TimeOnly`` () =
            let expected = TimeOnly.Parse("17:28:45")
            let actual =
                Parse.timeOnly
                |> Parser.parse "\"2025-05-13T17:28:45\""
                |> Expect.wantOk $"Expected %s{nameof Parser.parse} to succeed."
            Expect.equal Msg.none actual expected

        [<Fact>]
        let ``Should fail when parsing fails`` () =
            Parse.timeOnly
            |> Parser.parse "\"17:2845\""
            |> Expect.wantErrorString

        [<Fact>]
        let ``Should fail when element is not a string`` () =
            Parse.timeOnly
            |> Parser.parse "true"
            |> Expect.wantErrorString

    module TimeOnlyExact =

        [<Fact>]
        let ``Should parse string as TimeOnly exact`` () =
            let expected = TimeOnly.Parse("17:28:45")
            let actual =
                Parse.timeOnlyExact "HHmmss"
                |> Parser.parse "\"172845\""
                |> Expect.wantOk $"Expected %s{nameof Parser.parse} to succeed."
            Expect.equal Msg.none actual expected

        [<Fact>]
        let ``Should fail when format is incorrect`` () =
            Parse.timeOnlyExact "HH:mm:ss"
            |> Parser.parse "\"172845\""
            |> Expect.wantErrorString

        [<Fact>]
        let ``Should fail when element is not a string`` () =
            Parse.timeOnlyExact "HHmmss"
            |> Parser.parse "1"
            |> Expect.wantErrorString

    module TimeSpan =

        [<Fact>]
        let ``Should parse string as TimeSpan`` () =
            let expected = TimeSpan.Parse("1:23:45")
            let actual =
                Parse.timeSpan
                |> Parser.parse "\"1:23:45\""
                |> Expect.wantOk $"Expected %s{nameof Parser.parse} to succeed."
            Expect.equal Msg.none actual expected

        [<Fact>]
        let ``Should fail when parsing fails`` () =
            Parse.timeOnly
            |> Parser.parse "\"1:2345\""
            |> Expect.wantErrorString

        [<Fact>]
        let ``Should fail when element is not a string`` () =
            Parse.timeSpan
            |> Parser.parse "true"
            |> Expect.wantErrorString

    module TimeSpanExact =

        [<Fact>]
        let ``Should parse string as TimeSpan exact`` () =
            let expected = TimeSpan.Parse("01:23:45")
            let actual =
                Parse.timeSpanExact "hhmmss"
                |> Parser.parse "\"012345\""
                |> Expect.wantOk $"Expected %s{nameof Parser.parse} to succeed."
            Expect.equal Msg.none actual expected

        [<Fact>]
        let ``Should fail when format is incorrect`` () =
            Parse.timeSpanExact @"hh:mm:ss"
            |> Parser.parse "\"012845\""
            |> Expect.wantErrorString

        [<Fact>]
        let ``Should fail when element is not a string`` () =
            Parse.timeSpanExact @"hh\mm\ss"
            |> Parser.parse "1"
            |> Expect.wantErrorString

    module DateOnly =

        [<Fact>]
        let ``Should parse string as DateOnly`` () =
            let expected = DateOnly.Parse("2025-05-13")
            let actual =
                Parse.dateOnly
                |> Parser.parse "\"2025-05-13T17:28:45\""
                |> Expect.wantOk $"Expected %s{nameof Parser.parse} to succeed."
            Expect.equal Msg.none actual expected

        [<Fact>]
        let ``Should fail when parsing fails`` () =
            Parse.dateOnly
            |> Parser.parse "\"2025-0513\""
            |> Expect.wantErrorString

        [<Fact>]
        let ``Should fail when element is not a string`` () =
            Parse.dateOnly
            |> Parser.parse "1"
            |> Expect.wantErrorString

    module DateOnlyExact =

        [<Fact>]
        let ``Should parse string as DateOnly exact`` () =
            let expected = DateOnly.Parse("2025-05-13")
            let actual =
                Parse.dateOnlyExact "yyyyMMdd"
                |> Parser.parse "\"20250513\""
                |> Expect.wantOk $"Expected %s{nameof Parser.parse} to succeed."
            Expect.equal Msg.none actual expected

        [<Fact>]
        let ``Should fail when format is incorrect`` () =
            Parse.dateOnlyExact "yyyyMMdd"
            |> Parser.parse "\"2025-05-13\""
            |> Expect.wantErrorString

        [<Fact>]
        let ``Should fail when element is not a string`` () =
            Parse.dateOnlyExact "yyyyMMdd"
            |> Parser.parse "1"
            |> Expect.wantErrorString

    module DateTime =

        [<Fact>]
        let ``Should parse string as DateTime`` () =
            let expected = DateTime.Parse("2025-05-13T17:28:45")
            let actual =
                Parse.dateTime
                |> Parser.parse "\"2025-05-13T17:28:45\""
                |> Expect.wantOk $"Expected %s{nameof Parser.parse} to succeed."
            Expect.equal Msg.none actual expected

        [<Fact>]
        let ``Should fail when parsing fails`` () =
            Parse.dateTime
                |> Parser.parse "\"2025-05-13T172845\""
            |> Expect.wantErrorString

        [<Fact>]
        let ``Should fail when element is not a string`` () =
            Parse.dateTime
            |> Parser.parse "1"
            |> Expect.wantErrorString

    module DateTimeUtc =

        [<Fact>]
        let ``Should parse string as DateTime UTC`` () =
            let now = DateTime(DateOnly(2025, 05, 25), TimeOnly(10, 00))
            let expected = now.ToUniversalTime()
            let actual =
                Parse.dateTimeUtc
                |> Parser.parse "\"2025-05-25T10:00:00\""
                |> Expect.wantOk $"Expected %s{nameof Parser.parse} to succeed."
            Expect.equal Msg.none actual expected

        [<Fact>]
        let ``Should fail when parsing fails`` () =
            Parse.dateTimeUtc
            |> Parser.parse "\"2025-05-25100000\""
            |> Expect.wantErrorString

        [<Fact>]
        let ``Should fail when element is not a string`` () =
            Parse.dateTimeUtc
            |> Parser.parse "1"
            |> Expect.wantErrorString

    module DateTimeExact =

        [<Fact>]
        let ``Should parse string as DateTimeExact`` () =
            let expected = DateTime.Parse("2025-05-13T17:28:45")
            let actual =
                Parse.dateTimeExact "yyyy-MM-dd HH:mm:ss"
                |> Parser.parse "\"2025-05-13 17:28:45\""
                |> Expect.wantOk $"Expected %s{nameof Parser.parse} to succeed."
            Expect.equal Msg.none actual expected

        [<Fact>]
        let ``Should fail when format is incorrect`` () =
            Parse.dateTimeExact "yyyy-MM-dd HH:mm:ss"
            |> Parser.parse "\"2025-05-13T17:28:45\""
            |> Expect.wantErrorString

        [<Fact>]
        let ``Should fail when element is not a string`` () =
            Parse.dateTimeExact "yyyy-MM-dd HH:mm:ss"
            |> Parser.parse "1"
            |> Expect.wantErrorString

    module DateTimeOffset =

        [<Fact>]
        let ``Should parse string as DateTimeOffset`` () =
            let expected = DateTime.Parse("2025-05-13T17:28:45+02:00")
            let actual =
                Parse.dateTime
                |> Parser.parse "\"2025-05-13T17:28:45+02:00\""
                |> Expect.wantOk $"Expected %s{nameof Parser.parse} to succeed."
            Expect.equal Msg.none actual expected

        [<Fact>]
        let ``Should fail whe parsing fails`` () =
            Parse.dateTime
            |> Parser.parse "\"2025-05-13T17:28:4502:00\""
            |> Expect.wantErrorString

        [<Fact>]
        let ``Should fail when element is not a string`` () =
            Parse.dateTime
            |> Parser.parse "1"
            |> Expect.wantErrorString

    module DateTimeOffsetExact =

        [<Fact>]
        let ``Should parse string as DateTimeOffset exact`` () =
            let expected = DateTimeOffset.Parse("2025-05-13T17:28:00+02:00")
            let actual =
                Parse.dateTimeOffsetExact "yyyy-MM-dd HH:mm zzz"
                |> Parser.parse "\"2025-05-13 17:28 +02:00\""
                |> Expect.wantOk $"Expected %s{nameof Parser.parse} to succeed."
            Expect.equal Msg.none actual expected

        [<Fact>]
        let ``Should fail when format is incorrect`` () =
            Parse.dateTimeOffsetExact "yyyyMMdd HH:mm:ss zzz"
            |> Parser.parse "\"2025-05-13 17:28 +02:00\""
            |> Expect.wantErrorString

        [<Fact>]
        let ``Should fail when element is not a string`` () =
            Parse.dateTimeOffsetExact "\"yyyyMMdd HH:mm:ss zzz\""
            |> Parser.parse "1"
            |> Expect.wantErrorString

    module Choose =

        [<Fact>]
        let ``Should parse array as value seq`` () =
            let expected = seq [ 1; 2; 3; ]
            let actual: _ seq =
                Parse.choose Parse.int
                |> Parser.parse """[ 1, null, 2, 3, "4" ]"""
                |> Expect.wantOk $"Expected %s{nameof Parser.parse} to succeed."
            Expect.sequenceEqual Msg.none actual expected

        [<Fact>]
        let ``Should fail when element is not an array`` () =
            Parse.choose Parse.int
            |> Parser.parse "{}"
            |> Expect.wantErrorString

    module List =

        [<Fact>]
        let ``Should parse array as value list`` () =
            let expected = [ 1; 2; 3; ]
            let actual: _ list =
                Parse.list Parse.int
                |> Parser.parse "[ 1, 2, 3 ]"
                |> Expect.wantOk $"Expected %s{nameof Parser.parse} to succeed."
            Expect.sequenceEqual Msg.none actual expected

        [<Fact>]
        let ``Should fail when parsing fails`` () =
            Parse.list Parse.int
            |> Parser.parse """[ "1", "2", "3" ]"""
            |> Expect.wantErrorString

        [<Fact>]
        let ``Should fail when element is not an array`` () =
            Parse.list Parse.int
            |> Parser.parse "{}"
            |> Expect.wantErrorString

    module Array =

        [<Fact>]
        let ``Should parse array as value array`` () =
            let expected = [| 1; 2; 3; |]
            let actual: _ array =
                Parse.array Parse.int
                |> Parser.parse "[ 1, 2, 3 ]"
                |> Expect.wantOk $"Expected %s{nameof Parser.parse} to succeed."
            Expect.sequenceEqual Msg.none actual expected

        [<Fact>]
        let ``Should fail when parsing fails`` () =
            Parse.array Parse.int
            |> Parser.parse """[ "1", "2", "3" ]"""
            |> Expect.wantErrorString

        [<Fact>]
        let ``Should fail when element is not an array`` () =
            Parse.array Parse.int
            |> Parser.parse "{}"
            |> Expect.wantErrorString

    module Set =

        [<Fact>]
        let ``Should parse array as value Set`` () =
            let expected = Set [ 1; 2; 3; ]
            let actual: _ Set =
                Parse.set Parse.int
                |> Parser.parse "[ 1, 2, 3 ]"
                |> Expect.wantOk $"Expected %s{nameof Parser.parse} to succeed."
            Expect.sequenceEqual Msg.none actual expected

        [<Fact>]
        let ``Should fail when parsing fails`` () =
            Parse.set Parse.int
            |> Parser.parse """[ "1", "2", "3" ]"""
            |> Expect.wantErrorString

        [<Fact>]
        let ``Should fail when element is not an array`` () =
            Parse.set Parse.int
            |> Parser.parse "{}"
            |> Expect.wantErrorString

    module Seq =

        [<Fact>]
        let ``Should parse array as value seq`` () =
            let expected = seq [ 1; 2; 3; ]
            let actual: _ seq =
                Parse.seq Parse.int
                |> Parser.parse "[ 1, 2, 3 ]"
                |> Expect.wantOk $"Expected %s{nameof Parser.parse} to succeed."
            Expect.sequenceEqual Msg.none actual expected

        [<Fact>]
        let ``Should fail when parsing fails`` () =
            Parse.seq Parse.int
            |> Parser.parse """[ "1", "2", "3" ]"""
            |> Expect.wantErrorString

        [<Fact>]
        let ``Should fail when element is not an array`` () =
            Parse.seq Parse.int
            |> Parser.parse "{}"
            |> Expect.wantErrorString

    module Index =

        [<Fact>]
        let ``Should parse array at index`` () =
            let expected = 1
            let actual =
                Parse.index 0 Parse.int
                |> Parser.parse "[ 1, 2, 3 ]"
                |> Expect.wantOk $"Expected %s{nameof Parser.parse} to succeed."
            Expect.equal Msg.none actual expected

        [<Fact>]
        let ``Should fail when index is out of range`` () =
            Parse.index 3 Parse.int
            |> Parser.parse "[ 1, 2, 3 ]"
            |> Expect.wantErrorString

        [<Fact>]
        let ``Should fail when parsing fails`` () =
            Parse.index 1 Parse.int
            |> Parser.parse """[ 1, "2", 3 ]"""
            |> Expect.wantErrorString

        [<Fact>]
        let ``Should fail when element is not an array`` () =
            Parse.index 1 Parse.int
            |> Parser.parse "{}"
            |> Expect.wantErrorString

    module Map =

        [<Fact>]
        let ``Should parse object properties as IDictionary`` () =
            let expected = Map [ "key1", 1; "key2", 2; "key3", 3 ]
            let actual: Map<_,_> =
                Parse.map Parse.int
                |> Parser.parse """{ "key1": 1, "key2": 2, "key3": 3 }"""
                |> Expect.wantOk $"Expected %s{nameof Parser.parse} to succeed."
            Expect.sequenceEqual Msg.none actual expected

        [<Fact>]
        let ``Should fail when element is not an object`` () =
            Parse.map Parse.int
            |> Parser.parse "[]"
            |> Expect.wantErrorString

        [<Fact>]
        let ``Should fail when parsing duplicate keys`` () =
            Parse.map Parse.int
            |> Parser.parse """{ "key1": 1, "key1": 1, "key2": 2, "key2": 2, "key3": 3 }"""
            |> Expect.wantErrorString

        [<Fact>]
        let ``Should fail when parsing fails`` () =
            Parse.map Parse.int
            |> Parser.parse """{ "key1": "1", "key2": "2", "key3": 3 }"""
            |> Expect.wantErrorString

    module Dict =

        [<Fact>]
        let ``Should parse object properties as IDictionary`` () =
            let expected = seq [ "key1", 1; "key2", 2; "key3", 3 ] |> dict
            let actual: IDictionary<_,_> =
                Parse.dict Parse.int
                |> Parser.parse """{ "key1": 1, "key2": 2, "key3": 3 }"""
                |> Expect.wantOk $"Expected %s{nameof Parser.parse} to succeed."
            Expect.sequenceEqual Msg.none actual expected

        [<Fact>]
        let ``Should fail when element is not an object`` () =
            Parse.dict Parse.int
            |> Parser.parse "[]"
            |> Expect.wantErrorString

        [<Fact>]
        let ``Should fail when parsing duplicate keys`` () =
            Parse.dict Parse.int
            |> Parser.parse """{ "key1": 1, "key1": 1, "key2": 2, "key2": 2, "key3": 3 }"""
            |> Expect.wantErrorString

        [<Fact>]
        let ``Should fail when parsing fails`` () =
            Parse.dict Parse.int
            |> Parser.parse """{ "key1": "1", "key2": "2", "key3": 3 }"""
            |> Expect.wantErrorString

    module KeyValuePairs =

        [<Fact>]
        let ``Should parse object properties as KeyValuePair seq`` () =
            let expected = seq [ "key1", 1; "key2", 2; "key3", 3 ] |> Seq.map KeyValuePair
            let actual: _ seq =
                Parse.keyValuePairs Parse.int
                |> Parser.parse """{ "key1": 1, "key2": 2, "key3": 3 }"""
                |> Expect.wantOk $"Expected %s{nameof Parser.parse} to succeed."
            Expect.sequenceEqual Msg.none actual expected

        [<Fact>]
        let ``Should fail when element is not an object`` () =
            Parse.keyValuePairs Parse.int
            |> Parser.parse "[]"
            |> Expect.wantErrorString

        [<Fact>]
        let ``Should fail when parsing duplicate keys`` () =
            Parse.keyValuePairs Parse.int
            |> Parser.parse """{ "key1": 1, "key1": 1, "key2": 2, "key2": 2, "key3": 3 }"""
            |> Expect.wantErrorString

        [<Fact>]
        let ``Should fail when parsing fails`` () =
            Parse.keyValuePairs Parse.int
            |> Parser.parse """{ "key1": "1", "key2": "2", "key3": 3 }"""
            |> Expect.wantErrorString

    module Tuples =

        [<Fact>]
        let ``Should parse object properties as tuple seq`` () =
            let expected = seq [ "key1", 1; "key2", 2; "key3", 3 ]
            let actual: _ seq =
                Parse.tuples Parse.int
                |> Parser.parse """{ "key1": 1, "key2": 2, "key3": 3 }"""
                |> Expect.wantOk $"Expected %s{nameof Parser.parse} to succeed."
            Expect.sequenceEqual Msg.none actual expected

        [<Fact>]
        let ``Should fail when element is not an object`` () =
            Parse.tuples Parse.int
            |> Parser.parse "[]"
            |> Expect.wantErrorString

        [<Fact>]
        let ``Should fail when parsing duplicate keys`` () =
            Parse.tuples Parse.int
            |> Parser.parse """{ "key1": 1, "key1": 1, "key2": 2, "key2": 2, "key3": 3 }"""
            |> Expect.wantErrorString

        [<Fact>]
        let ``Should fail when parsing fails`` () =
            Parse.tuples Parse.int
            |> Parser.parse """{ "key1": "1", "key2": "2", "key3": 3 }"""
            |> Expect.wantErrorString

    module Keys =

        [<Fact>]
        let ``Should parse object keys as string seq`` () =
            let expected = seq [ "key1"; "key2"; "key3" ]
            let actual: _ seq =
                Parse.keys
                |> Parser.parse """{ "key1": 1, "key2": 2, "key3": 3 }"""
                |> Expect.wantOk $"Expected %s{nameof Parser.parse} to succeed."
            Expect.sequenceEqual Msg.none actual expected

        [<Fact>]
        let ``Should fail when element is not an object`` () =
            Parse.keys
            |> Parser.parse "[]"
            |> Expect.wantErrorString

        [<Fact>]
        let ``Should fail when parsing duplicate keys`` () =
            Parse.keys
            |> Parser.parse """{ "key1": 1, "key1": 1, "key2": 2, "key2": 2, "key3": 3 }"""
            |> Expect.wantErrorString

    module Tuple2 =

        [<Fact>]
        let ``Should parse array as tuple of 2`` () =
            let expected = "1", 1
            let actual =
                Parse.tuple2 Parse.string Parse.int
                |> Parser.parse """[ "1", 1 ]"""
                |> Expect.wantOk $"Expected %s{nameof Parser.parse} to succeed."
            Expect.equal Msg.none actual expected

        [<Fact>]
        let ``Should fail when parsing fails`` () =
            Parse.tuple2 Parse.int Parse.int
            |> Parser.parse """[ "1", 1, 1 ]"""
            |> Expect.wantErrorString

        [<Fact>]
        let ``Should fail when length is invalid`` () =
            Parse.tuple2 Parse.int Parse.int
            |> Parser.parse "[ 1 ]"
            |> Expect.wantErrorString

        [<Fact>]
        let ``Should fail when element is not an array`` () =
            Parse.tuple2 Parse.int Parse.int
            |> Parser.parse "{}"
            |> Expect.wantErrorString

    module Tuple3 =

        [<Fact>]
        let ``Should parse array as tuple of 3`` () =
            let expected = "1", 1, 1
            let actual =
                Parse.tuple3 Parse.string Parse.int Parse.int
                |> Parser.parse """[ "1", 1, 1 ]"""
                |> Expect.wantOk $"Expected %s{nameof Parser.parse} to succeed."
            Expect.equal Msg.none actual expected

        [<Fact>]
        let ``Should fail when parsing fails`` () =
            Parse.tuple3 Parse.int Parse.int Parse.int
            |> Parser.parse """[ "1", 1, 1, 1 ]"""
            |> Expect.wantErrorString

        [<Fact>]
        let ``Should fail when length is invalid`` () =
            Parse.tuple3 Parse.int Parse.int Parse.int
            |> Parser.parse "[ 1, 1 ]"
            |> Expect.wantErrorString

        [<Fact>]
        let ``Should fail when element is not an array`` () =
            Parse.tuple3 Parse.int Parse.int Parse.int
            |> Parser.parse "{}"
            |> Expect.wantErrorString

    module Tuple4 =

        [<Fact>]
        let ``Should parse array as tuple of 4`` () =
            let expected = "1", 1, 1, 1
            let actual =
                Parse.tuple4 Parse.string Parse.int Parse.int Parse.int
                |> Parser.parse """[ "1", 1, 1, 1 ]"""
                |> Expect.wantOk $"Expected %s{nameof Parser.parse} to succeed."
            Expect.equal Msg.none actual expected

        [<Fact>]
        let ``Should fail when parsing fails`` () =
            Parse.tuple4 Parse.int Parse.int Parse.int Parse.int
            |> Parser.parse """[ "1", 1, 1, 1 ]"""
            |> Expect.wantErrorString

        [<Fact>]
        let ``Should fail when length is invalid`` () =
            Parse.tuple4 Parse.int Parse.int Parse.int Parse.int
            |> Parser.parse "[ 1, 1, 1 ]"
            |> Expect.wantErrorString

        [<Fact>]
        let ``Should fail when element is not an array`` () =
            Parse.tuple4 Parse.int Parse.int Parse.int Parse.int
            |> Parser.parse "{}"
            |> Expect.wantErrorString

    module Tuple5 =

        [<Fact>]
        let ``Should parse array as tuple of five`` () =
            let expected = "1", 1, 1, 1, 1
            let actual =
                Parse.tuple5 Parse.string Parse.int Parse.int Parse.int Parse.int
                |> Parser.parse """[ "1", 1, 1, 1, 1 ]"""
                |> Expect.wantOk $"Expected %s{nameof Parser.parse} to succeed."
            Expect.equal Msg.none actual expected

        [<Fact>]
        let ``Should fail when parsing fails`` () =
            Parse.tuple5 Parse.int Parse.int Parse.int Parse.int Parse.int
            |> Parser.parse """[ "1", "1", 1, 1, 1 ]"""
            |> Expect.wantErrorString

        [<Fact>]
        let ``Should fail when length is invalid`` () =
            Parse.tuple5 Parse.int Parse.int Parse.int Parse.int Parse.int
            |> Parser.parse "[ 1, 1, 1, 1 ]"
            |> Expect.wantErrorString

        [<Fact>]
        let ``Should fail when element is not an array`` () =
            Parse.tuple5 Parse.int Parse.int Parse.int Parse.int Parse.int
            |> Parser.parse "{}"
            |> Expect.wantErrorString

    module OneOf =

        type TestDu =
            | A of int * int
            | B of {| Prop: string |}
            | C

        [<Fact>]
        let ``Should parse object as discriminated union`` () =
            let expected = A (1, 2)
            let actual =
                Parse.oneOf "disc" [
                    "a", parser {
                        let! a = Prop.req "prop2" Parse.int
                        let! b = Prop.req "prop3" Parse.int

                        return A (a, b)
                    }
                ]
                |> Parser.parse """{ "disc": "a", "prop2": 1, "prop3": 2 }"""
                |> Expect.wantOk $"Expected %s{nameof Parser.parse} to succeed."
            Expect.equal Msg.none actual expected

        [<Fact>]
        let ``Should fail when parsing fails`` () =
            let a =
                parser {
                    let! a = Prop.req "prop2" Parse.int
                    let! b = Prop.req "prop3" Parse.int

                    return a, b
                }
            Parse.oneOf "disc" [ "a", a ]
            |> Parser.parse """{ "disc": "a", "prop2": "1", "prop3": 2 }"""
            |> Expect.wantErrorString

        [<Fact>]
        let ``Should fail when element is not an object`` () =
            Parse.oneOf "disc" [ "missing", Parse.int ]
            |> Parser.parse "[]"
            |> Expect.wantErrorString

        [<Fact>]
        let ``Should fail when discriminator is not matched`` () =
            Parse.oneOf "disc" [ "missing", Parse.int ]
            |> Parser.parse """{ "disc": "a", "prop2": 1, "prop3": 2 }"""
            |> Expect.wantErrorString

        [<Fact>]
        let ``Should fail when discriminator is not found`` () =
            Parse.oneOf "missing" [ "a", Parse.int ]
            |> Parser.parse """{ "disc": "a", "prop2": 1, "prop3": 2 }"""
            |> Expect.wantErrorString

    module Self =

        type Tree =
            | Leaf of int
            | Branch of Tree * Tree

        [<Fact>]
        let ``Should parse object as recursive discriminated union`` () =
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
                |> Expect.wantOk $"Expected %s{nameof Parser.parse} to succeed."
            Expect.equal Msg.none actual expected

    module Attempt =

        type TestDu =
            | A of int * int
            | B of {| Prop: string |}
            | C

        [<Fact>]
        let ``Should attempt to parse discriminated union`` () =
            let a =
                parser {
                    let! a = Prop.req "prop2" Parse.int
                    let! b = Prop.req "prop3" Parse.int

                    return A (a, b)
                }

            let b =
                parser {
                    let! b = Prop.req "prop" Parse.string

                    return B {| Prop = b |}
                }

            let expected = A (1, 2)
            let actual =
                Parse.attempt [ b; a ]
                |> Parser.parse """{ "prop2": 1, "prop3": 2 }"""
                |> Expect.wantOk $"Expected %s{nameof Parser.parse} to succeed."
            Expect.equal Msg.none actual expected

        [<Fact>]
        let ``Should fail when all parsers fail`` () =
            let parser = Parser.fail "msg"
            Parse.attempt [ parser; parser; parser ]
            |> Parser.parse """{ "prop2": 1, "prop3": 2 }"""
            |> Expect.wantErrorString

        [<Fact>]
        let ``Should fail when list is empty`` () =
            Parse.attempt []
            |> Parser.parse """{ "prop2": 1, "prop3": 2 }"""
            |> Expect.wantErrorString

    module Nil =

        [<Fact>]
        let ``Should parse null as default value`` () =
            let expected = 1
            let actual =
                Parse.nil Parse.int 1
                |> Parser.parse "null"
                |> Expect.wantOk $"Expected %s{nameof Parser.parse} to succeed."
            Expect.equal Msg.none actual expected

        [<Fact>]
        let ``Should parse element as value`` () =
            let expected = 1
            let actual =
                Parse.nil Parse.int 2
                |> Parser.parse "1"
                |> Expect.wantOk $"Expected %s{nameof Parser.parse} to succeed."
            Expect.equal Msg.none actual expected

    module Option =

        [<Fact>]
        let ``Should parse null as None`` () =
            let expected = None
            let actual =
                Parse.option Parse.int
                |> Parser.parse "null"
                |> Expect.wantOk $"Expected %s{nameof Parser.parse} to succeed."
            Expect.equal Msg.none actual expected

        [<Fact>]
        let ``Should parse value as Some value`` () =
            let expected = Some 1
            let actual =
                Parse.option Parse.int
                |> Parser.parse "1"
                |> Expect.wantOk $"Expected %s{nameof Parser.parse} to succeed."
            Expect.equal Msg.none actual expected

        [<Fact>]
        let ``Should fail when parsing fails`` () =
            Parse.option Parse.int
            |> Parser.parse "1.1"
            |> Expect.wantErrorString

    module Refine =

        [<Fact>]
        let ``Should parse refined value`` () =
            let expected = 1
            let actual =
                Parse.refine Parse.int Ok
                |> Parser.parse "1"
                |> Expect.wantOk $"Expected %s{nameof Parser.parse} to succeed."
            Expect.equal Msg.none actual expected

        [<Fact>]
        let ``Should fail when validation fails`` () =
            Parse.refine Parse.byte (fun _ -> Error "msg")
            |> Parser.parse "1"
            |> Expect.wantErrorString

        [<Fact>]
        let ``Should fail when parsing fails`` () =
            Parse.refine Parse.int Ok
            |> Parser.parse "1.1"
            |> Expect.wantErrorString

    module Verify =

        [<Fact>]
        let ``Should parse verified value`` () =
            let expected = 1
            let actual =
                Parse.verify Parse.int (fun x -> x > 0) "msg"
                |> Parser.parse "1"
                |> Expect.wantOk $"Expected %s{nameof Parser.parse} to succeed."
            Expect.equal Msg.none actual expected

        [<Fact>]
        let ``Should fail when predicate returns false`` () =
            Parse.verify Parse.byte (fun x -> x = 0uy) "msg"
            |> Parser.parse "1"
            |> Expect.wantErrorString

        [<Fact>]
        let ``Should fail when parsing fails`` () =
            Parse.verify Parse.int (fun x -> x > 0) "msg"
            |> Parser.parse "true"
            |> Expect.wantErrorString

    module Kind =

        [<Fact>]
        let ``Should parse element as JsonValueKind`` () =
            let expected = JsonValueKind.Number
            let actual =
                Parse.kind
                |> Parser.parse "1"
                |> Expect.wantOk $"Expected %s{nameof Parser.parse} to succeed."
            Expect.equal Msg.none actual expected

    module Element =

        [<Fact>]
        let ``Should parse element as JsonElement`` () =
            let expected = JsonDocument.Parse("""{ "prop": 1 }""").RootElement
            let actual =
                Parse.element
                |> Parser.parse """{ "prop": 1 }"""
                |> Expect.wantOk $"Expected %s{nameof Parser.parse} to succeed."
            Expect.isTrue Msg.none <| JsonElement.DeepEquals(expected, actual)

    module RawText =

        [<Fact>]
        let ``Should parse element as string`` () =
            let expected = """{ "prop": 1 }"""
            let actual =
                Parse.rawText
                |> Parser.parse """{ "prop": 1 }"""
                |> Expect.wantOk $"Expected %s{nameof Parser.parse} to succeed."
            Expect.equal Msg.none actual expected

    module ArrayLength =

        [<Fact>]
        let ``Should parse array length as int`` () =
            let expected = 3
            let actual =
                Parse.arrayLength
                |> Parser.parse "[ 1, 2, 3 ]"
                |> Expect.wantOk $"Expected %s{nameof Parser.parse} to succeed."
            Expect.equal Msg.none actual expected

        [<Fact>]
        let ``Should fail when element is not an array`` () =
            Parse.arrayLength
            |> Parser.parse "{}"
            |> Expect.wantErrorString

    module PropertyCount =

        [<Fact>]
        let ``Should parse object property count as int`` () =
            let expected = 3
            let actual =
                Parse.propertyCount
                |> Parser.parse """{ "prop": 1, "prop2": 2, "prop3": 3 }"""
                |> Expect.wantOk $"Expected %s{nameof Parser.parse} to succeed."
            Expect.equal Msg.none actual expected

        [<Fact>]
        let ``Should fail when element is not an object`` () =
            Parse.propertyCount
            |> Parser.parse "[]"
            |> Expect.wantErrorString