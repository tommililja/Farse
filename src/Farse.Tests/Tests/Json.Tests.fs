namespace Farse.Tests

open System.Text.Json
open Xunit
open Farse

module JsonTests =

    [<Theory>]
    [<InlineData(2)>]
    [<InlineData(4)>]
    [<InlineData(8)>]
    let ``Should create indented JSON string`` size =
        JObj [ "value", JNum 1 ]
        |> Json.asString (Indented size)
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
        let ``Should create string when Some`` () =
            JStr.nil id (Some "1")
            |> Json.asString preset
            |> Expect.string

        [<Fact>]
        let ``Should create null when None`` () =
            JStr.nil id None
            |> Json.asString preset
            |> Expect.string

        [<Fact>]
        let ``Should create string array`` () =
            JStr.arr id [ "1"; "2"; "3" ]
            |> Json.asString preset
            |> Expect.string

    module JNum =

        [<Fact>]
        let ``Should create number when Some`` () =
            JNum.nil id (Some 1)
            |> Json.asString preset
            |> Expect.string

        [<Fact>]
        let ``Should create null when None`` () =
            JNum.nil<int, int> id None
            |> Json.asString preset
            |> Expect.string

        [<Fact>]
        let ``Should create number array`` () =
            JNum.arr id [ 1; 2; 3 ]
            |> Json.asString preset
            |> Expect.string

    module JBit =

        [<Fact>]
        let ``Should create bool when Some`` () =
            JBit.nil id (Some true)
            |> Json.asString preset
            |> Expect.string

        [<Fact>]
        let ``Should create null when None`` () =
            JBit.nil id None
            |> Json.asString preset
            |> Expect.string

        [<Fact>]
        let ``Should create bool array`` () =
            JBit.arr id [ true; false; true ]
            |> Json.asString preset
            |> Expect.string

    module JObj =

        [<Fact>]
        let ``Should create object when Some`` () =
            JObj.nil (fun x -> [ "value", JStr x ]) (Some "1")
            |> Json.asString preset
            |> Expect.string

        [<Fact>]
        let ``Should create null when None`` () =
            JObj.nil id None
            |> Json.asString preset
            |> Expect.string

        [<Fact>]
        let ``Should create object array`` () =
            JObj.arr (fun x -> [ "value", JStr x ]) [ "1"; "2"; "3" ]
            |> Json.asString preset
            |> Expect.string

    module JNil =

        let ``Should create null value`` () =
            JNil.none
            |> Json.asString preset
            |> Expect.string