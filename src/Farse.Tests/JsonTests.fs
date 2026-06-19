namespace Farse.Tests

open System
open System.Globalization
open System.IO
open System.Numerics
open System.Text
open System.Text.Json
open System.Threading
open Expecto.Flip
open Xunit
open Farse

module JsonTests =

    let example =
        JObj [
            "id", JStr "c8eae96a-025d-4bc9-88f8-f204e95f2883"
            "name", JStr "Alice"
            "age", JNil
            "email", JStr "alice@domain.com"
            "profiles",
                JArr [
                    JStr "01458283-b6e3-4ae7-ae54-a68eb587cdc0"
                    JStr "927eb20f-cd62-470c-aafc-c3ce6b9248b0"
                    JStr "bf00d1e2-ee53-4969-9507-86bed7e96432"
                ]
            "subscription",
                JObj [
                    "plan", JStr "pro"
                    "isCanceled", JBit false
                    "renewsAt", JStr "2026-12-25T10:30:00Z"
                ]
            "tags",
                JArr [
                    JStr "beta"
                    JStr "verified"
                ]
        ]

    [<Fact>]
    let ``Should sort properties in ascending order`` () =
        example
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
    let ``Should create Json from JsonElement`` () =
        let expected = Json.asString Indented example
        let actual =
            JsonElement.Parse expected
            |> Json.fromElement
            |> Json.asString Indented
        Expect.equal Msg.none actual expected

    [<Fact>]
    let ``Should create Json from string`` () =
        let expected = Json.asString Indented example
        let actual =
            Json.fromString expected
            |> Expect.wantOk $"Expected %s{nameof Json.fromString} to succeed."
            |> Json.asString Indented
        Expect.equal Msg.none actual expected

    [<Fact>]
    let ``Should fail to create Json from string when JSON is invalid``() =
        "invalid"
        |> Json.fromString
        |> Expect.isError $"Expected %s{nameof Json.fromString} to fail."

    [<Fact>]
    let ``Should create Json from stream async`` () =
        task {
            let expected = Json.asString Indented example
            let! actual =
                MemoryStream.create expected
                |> Json.fromStreamAsync CancellationToken.None
                |> Task.map (
                    Expect.wantOk $"Expected %s{nameof Json.fromStreamAsync} to succeed."
                    >> Json.asString Indented
                )
            Expect.equal Msg.none actual expected
        }

    [<Fact>]
    let ``Should fail to create Json from stream async when JSON is invalid`` () =
        MemoryStream.create "invalid"
        |> Json.fromStreamAsync CancellationToken.None
        |> Task.map (Expect.isError $"Expected %s{nameof Json.fromStreamAsync} to fail.")

    [<Fact>]
    let ``Should create Json from bytes``() =
        let expected = Json.asString Indented example
        let actual =
            Encoding.UTF8.GetBytes expected
            |> Json.fromBytes
            |> Expect.wantOk $"Expected %s{nameof Json.fromBytes} to succeed."
            |> Json.asString Indented
        Expect.equal Msg.none actual expected

    [<Fact>]
    let ``Should fail to create Json from bytes when JSON is invalid``() =
        Encoding.UTF8.GetBytes("invalid")
        |> Json.fromBytes
        |> Expect.isError $"Expected %s{nameof Json.fromBytes} to fail."

    [<Fact>]
    let ``Should convert Json to JsonNode`` () =
        let expected = Json.asString Indented example
        let actual =
            Json.fromString expected
            |> Expect.wantOk $"Expected %s{nameof Json.fromString} to succeed."
            |> Json.asJsonNode
            |> _.ToJsonString(JsonSerializerOptions(
                WriteIndented = true,
                IndentSize = 4)
            )
        Expect.equal Msg.none actual expected

    [<Fact>]
    let ``Should convert Json to indented JSON string`` () =
        example
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

        example
        |> Json.asString options
        |> Expect.string

    [<Fact>]
    let ``Should convert Json to raw JSON string`` () =
        example
        |> Json.asString Raw
        |> Expect.string

    [<Fact>]
    let ``Should write Json as string to writer`` () =
        task {
            let stream = new MemoryStream()
            use writer = new Utf8JsonWriter(stream)
            Json.asStringTo writer example
            do! writer.FlushAsync()
            let expected = Json.asString Raw example
            let actual = Encoding.UTF8.GetString(stream.ToArray())
            Expect.equal Msg.none actual expected
        }

    [<Fact>]
    let ``Should convert Json to bytes`` () =
        let expected = Json.asString Indented example
        let actual = Json.asBytes Indented example |> Encoding.UTF8.GetString
        Expect.equal Msg.none actual expected

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
            JStr.nil id (Some "1")
            |> Json.asString Indented
            |> Expect.string

        [<Fact>]
        let ``Should create null when None`` () =
            JStr.nil id None
            |> Json.asString Indented
            |> Expect.string

        [<Fact>]
        let ``Should create string array`` () =
            JStr.arr id [ "1"; "2"; "3" ]
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
            JNum.nil<int, int> id (Some 1)
            |> Json.asString Indented
            |> Expect.string

        [<Fact>]
        let ``Should create null when None`` () =
            JNum.nil<int, int> id None
            |> Json.asString Indented
            |> Expect.string

        [<Fact>]
        let ``Should create number array`` () =
            JNum.arr id [ 1; 2; 3 ]
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
            JNum Byte.MaxValue, "255"
            JNum SByte.MaxValue, "127"
            JNum Single.MaxValue, "3.40282347E+38"
            JNum Single.MinValue, "-3.40282347E+38"
            JNum Double.MaxValue, "1.7976931348623157E+308"
            JNum Double.MinValue, "-1.7976931348623157E+308"
            JNum Decimal.MaxValue, "79228162514264337593543950335"
            JNum Decimal.MinValue, "-79228162514264337593543950335"
            JNum (Decimal.Parse("12345678900.12345678900", CultureInfo.InvariantCulture)), "12345678900.12345678900"
            JNum (BigInteger.Parse("99999999999999999999999999999")), "99999999999999999999999999999"
            JNum (BigInteger.Parse("-99999999999999999999999999999")), "-99999999999999999999999999999"
        ]
        |> List.iter (fun (json, expected) ->
            let actual = Json.asString Raw json
            Expect.equal Msg.none actual expected
        )

    module JBit =

        let ``Should create bool`` () =
            JBit true
            |> Json.asString Indented
            |> Expect.string

        [<Fact>]
        let ``Should create bool when Some`` () =
            JBit.nil id (Some true)
            |> Json.asString Indented
            |> Expect.string

        [<Fact>]
        let ``Should create null when None`` () =
            JBit.nil id None
            |> Json.asString Indented
            |> Expect.string

        [<Fact>]
        let ``Should create bool array`` () =
            JBit.arr id [ true; false; true ]
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
        let ``Should create object when Some`` () =
            JObj.nil (fun x -> [ "value", JNum x ]) (Some 1)
            |> Json.asString Indented
            |> Expect.string

        [<Fact>]
        let ``Should create null when None`` () =
            JObj.nil id None
            |> Json.asString Indented
            |> Expect.string

        [<Fact>]
        let ``Should create object array`` () =
            JObj.arr (fun x -> [ "value", JNum x ]) [ 1; 2; 3 ]
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