namespace Farse.Tests

open Xunit
open Farse

module Parser =

    [<Fact>]
    let ``Should create parser and return expected value`` () =
        let expected = ()
        let actual =
            expected
            |> Parser.from
            |> Parser.parse "1"
            |> Expect.ok
        Expect.equal actual expected

    [<Fact>]
    let ``Should bind parsers and return expected value`` () =
        let expected = 2
        let actual =
            Parse.int
            |> Parser.bind (fun x -> Parser.from (x + 1))
            |> Parser.parse "1"
            |> Expect.ok
        Expect.equal actual expected

    [<Fact>]
    let ``Should map parser and return expected value`` () =
        let expected = "1"
        let actual =
            Parse.int
            |> Parser.map string
            |> Parser.parse "1"
            |> Expect.ok
        Expect.equal actual expected

    [<Fact>]
    let ``Should ignore parser value and return unit`` () =
        let expected = ()
        let actual =
            Parse.int
            |> Parser.ignore
            |> Parser.parse "1"
            |> Expect.ok
        Expect.equal actual expected

    [<Fact>]
    let ``Should return ok with expected value when validation succeeds`` () =
        let expected = 1
        let actual =
            Parse.int
            |> Parser.validate Ok
            |> Parser.parse "1"
            |> Expect.ok
        Expect.equal actual expected

    [<Fact>]
    let ``Should return expected error when validation fails`` () =
        let expected = "Error"
        let actual =
            Parse.int
            |> Parser.validate (fun _ -> Error expected)
            |> Parser.parse "1"
            |> Expect.error
        Expect.equal actual expected