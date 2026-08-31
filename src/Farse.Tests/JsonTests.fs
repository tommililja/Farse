namespace Farse.Tests

open System
open System.Globalization
open System.IO
open System.Numerics
open System.Text.Json
open System.Text.Json.Nodes
open System.Threading
open Expecto.Flip
open Xunit
open Farse

module JsonTests =

    [<Fact>]
    let ``Should sort properties in ascending order`` () =
        Data.example
        |> Json.sort
        |> Json.asString Indented
        |> Expect.string

    [<Fact>]
    let ``Should be equal after properties are sorted`` () =
        let a = JObj [ "a", JNum 1; "b", JNum 2 ]
        let b = JObj [ "b", JNum 2; "a", JNum 1 ]
        let equal = Json.equal a b
        Expect.isTrue "Expected values to be equal." equal

    [<Fact>]
    let ``Should not be equal after properties are sorted`` () =
        let a = JObj [ "a", JNum 1; "b", JNum 2 ]
        let b = JObj [ "b", JNum 1; "a", JNum 2 ]
        let equal = Json.equal a b
        Expect.isFalse "Expected values to not be equal." equal

    [<Fact>]
    let ``Should not be equal and return a message`` () =
        let x =
            JObj [
                "same", JNum 2
                "changed", JNum 1
                "onlyInX", JStr "present"
                "nested",
                    JObj [
                        "same", JBit true
                        "changed", JBit true
                        "onlyInX", JStr "present"
                    ]
                "sameArray", JArr [ JNum 1; JNum 2 ]
                "sameLengthArray", JArr [ JObj [ "id", JNum 1; "role", JStr "Engineer" ]; JNum 2 ]
                "diffLengthArray", JArr [ JNum 1; JNum 2; JNum 3 ]
                "nullBoth", JNil
                "nullVsValue", JNil
            ]

        let y =
            JObj [
                "same", JNum 1
                "changed", JNum 2
                "onlyInB", JStr "present"
                "nested",
                    JObj [
                        "same", JBit true
                        "changed", JBit false
                        "onlyInY", JStr "present"
                    ]
                "sameArray", JArr [ JNum 1; JNum 2 ]
                "sameLengthArray", JArr [ JObj [ "id", JNum 1; "role", JStr "Manager" ]; JNum 2 ]
                "diffLengthArray", JArr [ JNum 1; JNum 2 ]
                "nullBoth", JNil
                "nullVsValue", JStr "now a string"
            ]

        Json.diff x y
        |> Expect.wantSome $"Expected %s{nameof Json.diff} to return a message."
        |> Expect.string

    [<Fact>]
    let ``Should be equal when object keys are in a different order`` () =
        let x =
            JObj [
                "id", JNum 1
                "name", JStr "Alice"
                "active", JBit true
                "nested",
                    JObj [
                        "x", JNum 1
                        "y", JNum 2
                    ]
                "tags", JArr [ JStr "a"; JStr "b" ]
            ]

        let y =
            JObj [
                "tags", JArr [ JStr "a"; JStr "b" ]
                "nested",
                    JObj [
                        "y", JNum 2
                        "x", JNum 1
                    ]
                "active", JBit true
                "id", JNum 1
                "name", JStr "Alice"
            ]

        Json.diff x y
        |> Expect.isNone Msg.none

    [<Fact>]
    let ``Should create Json from JsonElement`` () =
        let expected = Json.asString Indented Data.example
        let actual =
            JsonElement.Parse expected
            |> Json.fromElement
            |> Json.asString Indented
        Expect.equal Msg.none expected actual

    [<Fact>]
    let ``Should create Json from string`` () =
        let expected = Json.asString Indented Data.example
        let actual =
            Json.fromString expected
            |> Expect.wantOk $"Expected %s{nameof Json.fromString} to succeed."
            |> Json.asString Indented
        Expect.equal Msg.none expected actual

    [<Fact>]
    let ``Should fail to create Json from string when JSON is invalid``() =
        "invalid"
        |> Json.fromString
        |> Expect.isError $"Expected %s{nameof Json.fromString} to fail."

    [<Fact>]
    let ``Should fail to create Json from string when JSON is null``() =
        null
        |> Json.fromString
        |> Expect.isError $"Expected %s{nameof Json.fromString} to fail."

    [<Fact>]
    let ``Should fail to create Json from string when JSON is empty``() =
        String.Empty
        |> Json.fromString
        |> Expect.isError $"Expected %s{nameof Json.fromString} to fail."

    [<Fact>]
    let ``Should create Json from stream async`` () =
        task {
            let expected = Json.asString Indented Data.example
            let! actual =
                expected
                |> MemoryStream.ofString
                |> Json.fromStreamAsync CancellationToken.None
                |> Task.map (
                    Expect.wantOk $"Expected %s{nameof Json.fromStreamAsync} to succeed."
                    >> Json.asString Indented
                )
            Expect.equal Msg.none expected actual
        }

    [<Fact>]
    let ``Should fail to create Json from stream async when JSON is invalid`` () =
        "invalid"
        |> MemoryStream.ofString
        |> Json.fromStreamAsync CancellationToken.None
        |> Task.map (Expect.isError $"Expected %s{nameof Json.fromStreamAsync} to fail.")

    [<Fact>]
    let ``Should fail to create Json from stream async when JSON is empty`` () =
        String.Empty
        |> MemoryStream.ofString
        |> Json.fromStreamAsync CancellationToken.None
        |> Task.map (Expect.isError $"Expected %s{nameof Json.fromStreamAsync} to fail.")

    [<Fact>]
    let ``Should create Json from bytes``() =
        let expected = Json.asString Indented Data.example
        let actual =
            expected
            |> String.asBytes
            |> Json.fromBytes
            |> Expect.wantOk $"Expected %s{nameof Json.fromBytes} to succeed."
            |> Json.asString Indented
        Expect.equal Msg.none expected actual

    [<Fact>]
    let ``Should fail to create Json from bytes when JSON is invalid``() =
        "invalid"
        |> String.asBytes
        |> Json.fromBytes
        |> Expect.isError $"Expected %s{nameof Json.fromBytes} to fail."

    [<Fact>]
    let ``Should convert Json to JsonNode`` () =
        let expected =
            Data.example
            |> Json.asString Indented
            |> JsonNode.Parse
        let actual =
            Data.example
            |> Json.asJsonNode
        Expect.isTrue Msg.none (JsonNode.DeepEquals(expected, actual))

    [<Fact>]
    let ``Should convert Json to JsonElement`` () =
        let expected =
            Data.example
            |> Json.asString Indented
            |> JsonElement.Parse
        let actual =
            Data.example
            |> Json.asJsonElement
        Expect.isTrue Msg.none (JsonElement.DeepEquals(expected, actual))

    [<Fact>]
    let ``Should convert Json to JsonDocument`` () =
        let expected =
            Data.example
            |> Json.asString Indented
            |> JsonDocument.Parse
        use actual =
            Data.example
            |> Json.asJsonDocument
        Expect.isTrue Msg.none (JsonElement.DeepEquals(expected.RootElement, actual.RootElement))

    [<Fact>]
    let ``Should convert Json to indented JSON string`` () =
        Data.example
        |> Json.asString Indented
        |> Expect.string

    [<Fact>]
    let ``Should convert Json to custom JSON string`` () =
        let options =
            JsonSerializerOptions(
                WriteIndented = true,
                IndentSize = 1,
                IndentCharacter = char 9
            )
            |> Custom

        Data.example
        |> Json.asString options
        |> Expect.string

    [<Fact>]
    let ``Should convert Json to raw JSON string`` () =
        Data.example
        |> Json.asString Raw
        |> Expect.string

    [<Fact>]
    let ``Should write Json as string to writer`` () =
        task {
            let stream = new MemoryStream()
            use writer = new Utf8JsonWriter(stream)
            Json.writeTo writer Data.example
            do! writer.FlushAsync()
            let expected = Json.asString Raw Data.example
            let actual = stream.ToArray() |> String.ofBytes
            Expect.equal Msg.none expected actual
        }

    [<Fact>]
    let ``Should convert Json to bytes`` () =
        let expected = Json.asString Indented Data.example
        let actual = Json.asBytes Indented Data.example |> String.ofBytes
        Expect.equal Msg.none expected actual

    module JStr =

        [<Fact>]
        let ``Should create string`` () =
            JStr "string"
            |> Json.asString Indented
            |> Expect.string

        [<Fact>]
        let ``Should create empty string`` () =
            JStr.empty
            |> Json.asString Indented
            |> Expect.string

        [<Fact>]
        let ``Should create string when Some`` () =
            JStr.option id (Some "1")
            |> Json.asString Indented
            |> Expect.string

        [<Fact>]
        let ``Should create null when None`` () =
            JStr.option id None
            |> Json.asString Indented
            |> Expect.string

        [<Fact>]
        let ``Should create string array`` () =
            JStr.array id [ "1"; "2"; "3" ]
            |> Json.asString Indented
            |> Expect.string

        [<Fact>]
        let ``Should create string singleton`` () =
            JStr.singleton id "1"
            |> Json.asString Indented
            |> Expect.string

    module JNum =

        [<Fact>]
        let ``Should create number`` () =
            JNum<int> 5
            |> Json.asString Indented
            |> Expect.string

        [<Fact>]
        let ``Should create number with zero`` () =
            JNum.zero
            |> Json.asString Indented
            |> Expect.string

        [<Fact>]
        let ``Should create number when Some`` () =
            JNum.option<int, int> id (Some 1)
            |> Json.asString Indented
            |> Expect.string

        [<Fact>]
        let ``Should create null when None`` () =
            JNum.option<int, int> id None
            |> Json.asString Indented
            |> Expect.string

        [<Fact>]
        let ``Should create number array`` () =
            JNum.array id [ 1; 2; 3 ]
            |> Json.asString Indented
            |> Expect.string

        [<Fact>]
        let ``Should create number singleton`` () =
            JNum.singleton id 1
            |> Json.asString Indented
            |> Expect.string

    [<Fact>]
    let ``Should format numbers correctly when converting to string`` () =
        [
            JNum Int16.MaxValue, "32767"
            JNum Int32.MaxValue, "2147483647"
            JNum Int64.MaxValue, "9223372036854775807"
            JNum Int16.MinValue, "-32768"
            JNum Int32.MinValue, "-2147483648"
            JNum Int64.MinValue, "-9223372036854775808"
            JNum UInt16.MaxValue, "65535"
            JNum UInt32.MaxValue, "4294967295"
            JNum UInt64.MaxValue, "18446744073709551615"
            JNum Int128.MaxValue, "170141183460469231731687303715884105727"
            JNum Int128.MinValue, "-170141183460469231731687303715884105728"
            JNum Byte.MaxValue, "255"
            JNum SByte.MaxValue, "127"
            JNum Single.MaxValue, "3.40282347E+38"
            JNum Single.MinValue, "-3.40282347E+38"
            JNum Double.MaxValue, "1.7976931348623157E+308"
            JNum Double.MinValue, "-1.7976931348623157E+308"
            JNum Half.MinValue, "-65504"
            JNum Half.MaxValue, "65504"
            JNum Decimal.MaxValue, "79228162514264337593543950335"
            JNum Decimal.MinValue, "-79228162514264337593543950335"
            JNum (Decimal.Parse("12345678900.12345678900", CultureInfo.InvariantCulture)), "12345678900.12345678900"
            JNum (BigInteger.Parse("99999999999999999999999999999")), "99999999999999999999999999999"
            JNum (BigInteger.Parse("-99999999999999999999999999999")), "-99999999999999999999999999999"
        ]
        |> List.iter (fun (json, expected) ->
            let actual = Json.asString Raw json
            Expect.equal Msg.none expected actual
        )

    module JBit =

        let ``Should create bool`` () =
            JBit true
            |> Json.asString Indented
            |> Expect.string

        [<Fact>]
        let ``Should create bool when Some`` () =
            JBit.option id (Some true)
            |> Json.asString Indented
            |> Expect.string

        [<Fact>]
        let ``Should create null when None`` () =
            JBit.option id None
            |> Json.asString Indented
            |> Expect.string

        [<Fact>]
        let ``Should create bool array`` () =
            JBit.array id [ true; false; true ]
            |> Json.asString Indented
            |> Expect.string

        [<Fact>]
        let ``Should create bool singleton`` () =
            JBit.singleton id true
            |> Json.asString Indented
            |> Expect.string

    module JObj =

        let ``Should create object`` () =
            JObj [ "1", JNum 1; "2", JNum 2; "3", JNum 3 ]
            |> Json.asString Indented
            |> Expect.string

        [<Fact>]
        let ``Should create empty object`` () =
            JObj.empty
            |> Json.asString Indented
            |> Expect.string

        [<Fact>]
        let ``Should create object from value`` () =
            JObj.from (fun x -> [ "value", JNum x ]) 1
            |> Json.asString Indented
            |> Expect.string

        [<Fact>]
        let ``Should create object when Some`` () =
            JObj.option (fun x -> [ "value", JNum x ]) (Some 1)
            |> Json.asString Indented
            |> Expect.string

        [<Fact>]
        let ``Should create null when None`` () =
            JObj.option id None
            |> Json.asString Indented
            |> Expect.string

        [<Fact>]
        let ``Should create object array`` () =
            JObj.array (fun x -> [ "value", JNum x ]) [ 1; 2; 3 ]
            |> Json.asString Indented
            |> Expect.string

        [<Fact>]
        let ``Should create object singleton`` () =
            JObj.singleton (fun (n, v) -> [ n, JNum v ]) ("value", 1)
            |> Json.asString Indented
            |> Expect.string

    module JArr =

        let ``Should create array`` () =
            JArr [ JNum 1; JNum 2; JNum 3 ]
            |> Json.asString Indented
            |> Expect.string

        [<Fact>]
        let ``Should create empty array`` () =
            JArr.empty
            |> Json.asString Indented
            |> Expect.string

    module JNil =

        let ``Should create null`` () =
            JNil
            |> Json.asString Indented
            |> Expect.string