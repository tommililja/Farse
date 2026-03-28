namespace Farse.Tests

open System.Threading
open Xunit
open Farse

module ParserTests =

    [<Fact>]
    let ``Should create Parser and return expected value`` () =
        let expected = ()
        let actual =
            expected
            |> Parser.from
            |> Parser.parse "1"
            |> Expect.ok
        Expect.equal actual expected

    [<Fact>]
    let ``Should create Parser and return expected Result`` () =
        let expected = Ok ()
        let actual =
            expected
            |> Parser.fromResult
            |> Parser.parse "1"
        Expect.equal actual expected

    [<Fact>]
    let ``Should bind Parser value and return expected value`` () =
        let expected = 2
        let actual =
            Parse.int
            |> Parser.bind (fun x -> Parser.from (x + 1))
            |> Parser.parse "1"
            |> Expect.ok
        Expect.equal actual expected

    [<Fact>]
    let ``Should map Parser value and return expected value`` () =
        let expected = "1"
        let actual =
            Parse.int
            |> Parser.map string
            |> Parser.parse "1"
            |> Expect.ok
        Expect.equal actual expected

    [<Fact>]
    let ``Should ignore Parser value and return unit`` () =
        let expected = ()
        let actual =
            Parse.int
            |> Parser.ignore<int>
            |> Parser.parse "1"
            |> Expect.ok
        Expect.equal actual expected

    [<Fact>]
    let ``Should set default value for optional value`` () =
        let expected = 1
        let actual =
            Parse.optional Parse.int
            |> Parser.defaultValue 1
            |> Parser.parse "null"
            |> Expect.ok
        Expect.equal actual expected

    [<Fact>]
    let ``Should recover from an error with a default value`` () =
        let expected = 1
        let actual =
            Parser.fail "msg"
            |> Parser.recover 1
            |> Parser.parse "null"
            |> Expect.ok
        Expect.equal actual expected

    [<Fact>]
    let ``Should return Ok when parsing JSON async`` () =
        task {
            let expected = 1
            let! actual =
                Prop.req "prop" Parse.int
                |> Parser.parseAsync (MemoryStream.create """{ "prop": 1 }""") CancellationToken.None
                |> Task.map Expect.ok
            Expect.equal actual expected
        }