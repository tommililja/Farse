namespace Farse.Tests

open System
open System.Text.Json
open System.Threading
open Expecto.Flip
open Xunit
open Farse

module ParserTests =

    [<Fact>]
    let ``Should run parser against an element`` () =
        let expected = ()
        let actual =
            Parse.unit
            |> Parser.run (JsonElement.Parse("null"))
            |> Expect.wantOk $"Expected %s{nameof Parser.run} to succeed."
        Expect.equal Msg.none actual expected

    [<Fact>]
    let ``Should create Parser from value`` () =
        let expected = ()
        let actual =
            expected
            |> Parser.from
            |> Parser.parse "1"
            |> Expect.wantOk $"Expected %s{nameof Parser.parse} to succeed."
        Expect.equal Msg.none actual expected

    [<Fact>]
    let ``Should create Parser from Result`` () =
        let expected = Ok ()
        let actual =
            expected
            |> Parser.fromResult
            |> Parser.parse "1"
        Expect.equal Msg.none actual expected

    [<Fact>]
    let ``Should create Parser from Result that fails`` () =
        Error "msg"
        |> Parser.fromResult
        |> Parser.parse "1"
        |> Expect.wantErrorString

    [<Fact>]
    let ``Should create Parser that fails`` () =
        "msg"
        |> Parser.fail
        |> Parser.parse "1"
        |> Expect.wantErrorString

    [<Fact>]
    let ``Should recover with a default value when a Parser fails`` () =
        let expected = 1
        let actual =
            Parser.fail "msg"
            |> Parser.recover 1
            |> Parser.parse "1"
            |> Expect.wantOk $"Expected %s{nameof Parser.parse} to succeed."
        Expect.equal Msg.none actual expected

    [<Fact>]
    let ``Should not recover with a default value when a Parser succeeds`` () =
        let expected = 1
        let actual =
            Parse.int
            |> Parser.recover 2
            |> Parser.parse "1"
            |> Expect.wantOk $"Expected %s{nameof Parser.parse} to succeed."
        Expect.equal Msg.none actual expected

    [<Fact>]
    let ``Should bind parsers`` () =
        let expected = 2
        let actual =
            Parse.int
            |> Parser.bind (fun x -> Parser.from (x + 1))
            |> Parser.parse "1"
            |> Expect.wantOk $"Expected %s{nameof Parser.parse} to succeed."
        Expect.equal Msg.none actual expected

    [<Fact>]
    let ``Should map Parser value`` () =
        let expected = "1"
        let actual =
            Parse.int
            |> Parser.map string
            |> Parser.parse "1"
            |> Expect.wantOk $"Expected %s{nameof Parser.parse} to succeed."
        Expect.equal Msg.none actual expected

    [<Fact>]
    let ``Should ignore Parser value`` () =
        let expected = ()
        let actual =
            Parse.int
            |> Parser.ignore<int>
            |> Parser.parse "1"
            |> Expect.wantOk $"Expected %s{nameof Parser.parse} to succeed."
        Expect.equal Msg.none actual expected

    [<Fact>]
    let ``Should not ignore Parser value when a Parser fails`` () =
        Parser.fail "msg"
        |> Parser.ignore<int>
        |> Parser.parse "1"
        |> Expect.wantErrorString

    [<Fact>]
    let ``Should set default value when None`` () =
        let expected = 1
        let actual =
            Parse.option Parse.int
            |> Parser.defaultValue 1
            |> Parser.parse "null"
            |> Expect.wantOk $"Expected %s{nameof Parser.parse} to succeed."
        Expect.equal Msg.none actual expected

    [<Fact>]
    let ``Should not set default value when Some`` () =
        let expected = 1
        let actual =
            Parse.option Parse.int
            |> Parser.defaultValue 2
            |> Parser.parse "1"
            |> Expect.wantOk $"Expected %s{nameof Parser.parse} to succeed."
        Expect.equal Msg.none actual expected

    [<Fact>]
    let ``Should not set default value when a Parser fails`` () =
        Parser.fail "msg"
        |> Parser.defaultValue 1
        |> Parser.parse "1"
        |> Expect.wantErrorString

    [<Fact>]
    let ``Should get default value when None`` () =
        let expected = 1
        let actual =
            Parse.option Parse.int
            |> Parser.defaultWith (fun () -> 1)
            |> Parser.parse "null"
            |> Expect.wantOk $"Expected %s{nameof Parser.parse} to succeed."
        Expect.equal Msg.none actual expected

    [<Fact>]
    let ``Should not get default value when Some`` () =
        let expected = 1
        let actual =
            Parse.option Parse.int
            |> Parser.defaultWith (fun () -> 2)
            |> Parser.parse "1"
            |> Expect.wantOk $"Expected %s{nameof Parser.parse} to succeed."
        Expect.equal Msg.none actual expected

    [<Fact>]
    let ``Should not get default value when a Parser fails`` () =
        Parser.fail "msg"
        |> Parser.defaultWith (fun () -> 1)
        |> Parser.parse "1"
        |> Expect.wantErrorString

    [<Fact>]
    let ``Should parse JSON`` () =
        let expected = 1
        let actual =
            Parse.int
            |> Parser.parse "1"
            |> Expect.wantOk $"Expected %s{nameof Parser.parse} to succeed."
        Expect.equal Msg.none actual expected

    [<Fact>]
    let ``Should fail when parsing invalid JSON`` () =
        Parse.int
        |> Parser.parse "invalid"
        |> Expect.wantErrorString

    [<Fact>]
    let ``Should fail when parsing a null string`` () =
        Parse.int
        |> Parser.parse null
        |> Expect.wantErrorString

    [<Fact>]
    let ``Should fail when parsing an empty string`` () =
        Parse.int
        |> Parser.parse String.Empty
        |> Expect.wantErrorString

    [<Fact>]
    let ``Should parse JSON async`` () =
        Parse.int
        |> Parser.parseAsync (MemoryStream.create "1") CancellationToken.None
        |> Task.map (fun x ->
            let expected = 1
            let actual = Expect.wantOk $"Expected %s{nameof Parser.parseAsync} to succeed." x
            Expect.equal Msg.none actual expected
        )

    [<Fact>]
    let ``Should fail when parsing invalid JSON async`` () =
        Parse.int
        |> Parser.parseAsync (MemoryStream.create "invalid") CancellationToken.None
        |> Task.bind Expect.wantErrorString

    [<Fact>]
    let ``Should fail when parsing a null stream async`` () =
        Parse.int
        |> Parser.parseAsync null CancellationToken.None
        |> Task.bind Expect.wantErrorString

    [<Fact>]
    let ``Should fail when parsing an empty string async`` () =
        Parse.int
        |> Parser.parseAsync (MemoryStream.create String.Empty) CancellationToken.None
        |> Task.bind Expect.wantErrorString