namespace Farse.Tests

open System
open System.Globalization
open System.IO
open System.Numerics
open System.Text
open System.Text.Json
open System.Threading
open Xunit
open Farse

module JsonTests =

    let private json =
        JObj [
            "id", JStr "c8eae96a-025d-4bc9-88f8-f204e95f2883"
            "name", JStr "Alice"
            "age", JNil
            "email", JStr "alice@domain.com"
            "profiles", JStr.arr id [
                "01458283-b6e3-4ae7-ae54-a68eb587cdc0"
                "927eb20f-cd62-470c-aafc-c3ce6b9248b0"
                "bf00d1e2-ee53-4969-9507-86bed7e96432"
            ]
            "subscription", JObj [
                "plan", JStr "Pro"
                "isCanceled", JBit false
                "renewsAt", JStr "2026-12-25T10:30:00Z"
            ]
            "tags", JArr [
                JStr "beta"
                JStr "verified"
            ]
        ]

    [<Fact>]
    let ``Should return Ok when creating Json`` () =
        let expected = File.ReadAllText("Example.json")
        let actual =
            expected
            |> Json.fromString
            |> Expect.ok
            |> Json.asString Indented
        Expect.equal actual expected

    [<Fact>]
    let ``Should return Ok when creating Json async`` () =
        task {
            let expected = File.ReadAllText("Example.json")
            let! actual =
                MemoryStream.create expected
                |> Json.fromStreamAsync CancellationToken.None
                |> Task.map (Expect.ok >> Json.asString Indented)
            Expect.equal actual expected
        }

    [<Fact>]
    let ``Should return Ok when creating Json from bytes`` () =
        let bytes = Json.asString Indented json |> Encoding.UTF8.GetBytes
        let actual = Json.fromBytes bytes |> Expect.ok
        Expect.equal actual json

    [<Fact>]
    let ``Should return Error when creating Json from invalid JSON`` () =
        """{ "prop" 1 }"""
        |> Json.fromString
        |> Expect.error

    [<Fact>]
    let ``Should return Error when creating Json async from invalid JSON`` () =
        MemoryStream.create """{ "prop" 1 }"""
        |> Json.fromStreamAsync CancellationToken.None
        |> Task.map Expect.error

    [<Fact>]
    let ``Should create indented JSON string`` () =
        json
        |> Json.asString Indented
        |> Expect.string

    [<Fact>]
    let ``Should create custom JSON string`` () =
        let options =
            JsonSerializerOptions(
                WriteIndented = true,
                IndentSize = 1,
                IndentCharacter = char 9
            )
            |> Custom

        json
        |> Json.asString options
        |> Expect.string

    [<Fact>]
    let ``Should create raw JSON string`` () =
        json
        |> Json.asString Raw
        |> Expect.string

    [<Fact>]
    let ``Should write JSON string to writer`` () =
        task {
            let stream = new MemoryStream()
            use writer = new Utf8JsonWriter(stream)
            Json.asStringTo writer json
            do! writer.FlushAsync()
            let expected = Json.asString Raw json
            let actual = Encoding.UTF8.GetString(stream.ToArray())
            Expect.equal actual expected
        }

    [<Fact>]
    let ``Should return bytes from JSON string`` () =
        let expected = Json.asString Indented json |> Encoding.UTF8.GetBytes
        let actual = Json.asBytes Indented json
        Expect.equal actual expected

    [<Fact>]
    let ``Comparing two Json values should return true`` () =
        let json = File.ReadAllText("Example.json")
        let a = Json.fromString json
        let b = Json.fromString json
        Expect.equal a b

    [<Fact>]
    let ``Should sort properties in ascending order`` () =
        let json =
            File.ReadAllText("Example.json")
            |> Json.fromString
            |> Expect.ok
        json
        |> Json.sort
        |> Json.asString Indented
        |> Expect.string

    module JStr =

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
        ]
        |> List.iter (fun (json, expected) ->
            let actual = Json.asString Raw json
            Expect.equal actual expected
        )

    module JBit =

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

        [<Fact>]
        let ``Should create empty object`` () =
            JObj.empty
            |> Json.asString Indented
            |> Expect.string

        [<Fact>]
        let ``Should create object when Some`` () =
            JObj.nil (fun x -> [ "value", JStr x ]) (Some "1")
            |> Json.asString Indented
            |> Expect.string

        [<Fact>]
        let ``Should create null when None`` () =
            JObj.nil id None
            |> Json.asString Indented
            |> Expect.string

        [<Fact>]
        let ``Should create object array`` () =
            JObj.arr (fun x -> [ "value", JStr x ]) [ "1"; "2"; "3" ]
            |> Json.asString Indented
            |> Expect.string

        [<Fact>]
        let ``Should create object singleton`` () =
            JObj.singleton id [ "value", JStr "1" ]
            |> Json.asString Indented
            |> Expect.string

    module JArr =

        [<Fact>]
        let ``Should create empty array`` () =
            JArr.empty
            |> Json.asString Indented
            |> Expect.string

    module JNil =

        let ``Should create null value`` () =
            JNil
            |> Json.asString Indented
            |> Expect.string