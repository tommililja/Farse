namespace Farse.Tests

open System.IO
open System.Text.Json
open System.Threading
open Xunit
open Farse

module JsonTests =

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
        JObj [ "value", JNum 1 ]
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

        JObj [ "value", JNum 1 ]
        |> Json.asString options
        |> Expect.string

    [<Fact>]
    let ``Should create raw JSON string`` () =
        JObj [ "value", JNum 1 ]
        |> Json.asString Raw
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
        let ``Should create number with zero`` () =
            JNum.zero
            |> Json.asString Indented
            |> Expect.string

        [<Fact>]
        let ``Should create number when Some`` () =
            JNum.nil id (Some 1)
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